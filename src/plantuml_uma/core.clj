(ns plantuml-uma.core
  (:require [cheshire.core :as json]
            [clojure.contrib.humanize :as human]
            clojure.string)
  (:import (java.io FileOutputStream)
           (net.sourceforge.plantuml SourceStringReader)
           (javax.swing ImageIcon JLabel JFrame)
           (java.awt Toolkit BorderLayout)))

(defn http-interesting [x]
  (let [http (-> x
                 (get "_source")
                 (get "layers")
                 (get "http"))
        request-method (first (keep #(get % "http.request.method") (vals http)))
        response-code (first (keep #(get % "http.response.code") (vals http)))]
    (-> http
        (select-keys ["http.user_agent"
                      "http.request.full_uri"
                      "http.response_in"
                      "http.set_cookie"
                      "http.host"
                      "http.content_type"])
        (assoc :response-code response-code)
        (assoc :method request-method))))

(defn other-interesting [x]
  {:id             (get-in x ["_source" "layers" "frame" "frame.number"])
   :time           (long (* 1000 (Double/parseDouble (get-in x ["_source" "layers" "frame" "frame.time_epoch"]))))
   :frame-size     (Long/parseLong (get-in x ["_source" "layers" "frame" "frame.len"]))
   :destination-ip (get-in x ["_source" "layers" "ip" "ip.dst_host"])})

(defn interesting [x]
  (merge (http-interesting x) (other-interesting x)))

(defn uri-ends [what]
  (fn [x]
    (some-> x
            ^String (get "http.request.full_uri")
            (.matches (str ".*\\." what "$")))))

(def ignore-request?
  (some-fn
    (fn [x]
      (some-> x
              (get :destination-ip)
              (= "192.168.0.14")))
    (uri-ends "js")
    (uri-ends "ico")
    (uri-ends "js.map")
    (uri-ends "png")
    (fn [x]
      (some-> x
              (get "http.request.full_uri")
              (.contains "login-status-iframe.html")))
    (fn [x]
      (some-> x
              (get "http.request.full_uri")
              (.contains "sockjs-node")))))

(defn remove-boring-frames [all-frames]
  (let [to-ignore (filter ignore-request? (map interesting all-frames))
        ids-to-remove (set (remove nil? (mapcat (juxt :id #(get % "http.response_in")) to-ignore)))]
    (remove
      (comp ids-to-remove :id)
      (map interesting all-frames))))

(defn from? [req]
  (if (.contains (get req "http.user_agent" "") "Mozilla")
    :browser
    :backend))

(defn to? [req]
  (cond
    (.contains (get req "http.request.full_uri" "") "http://t1.lumen.localhost:3030/api/") :backend
    (.contains (get req "http.request.full_uri" "") "http://t1.lumen.localhost:3030/env") :backend
    (.contains (get req "http.request.full_uri" "") "localhost:8080") :keycloak
    :default :nginx))

(defn join-req-and-resp [interesting-frames]
  (let [reqs (filter #(get % "http.response_in") interesting-frames)
        resp (fn [req] (first (filter
                                (comp
                                  (partial = (get req "http.response_in"))
                                  :id) interesting-frames)))]
    (->> reqs
         (map (juxt identity resp))
         (sort-by (comp #(Long/parseLong %) :id first))
         (map (fn [[req res]] [(assoc req :from (from? req) :to (to? req))
                               (assoc res :from (from? req) :to (to? req))])))))

(defn traffic-size [request-response-pairs]
  (->> request-response-pairs
       (group-by (comp (juxt :from :to) first))
       (map (fn [[from-to request-response-pairs]]
              [from-to
               {:from-to  from-to
                :from->to (reduce + (map (comp :frame-size first) request-response-pairs))
                :to->from (reduce + (map (comp :frame-size second) request-response-pairs))}]))
       (into {})))

(defn ->plantuml [request-response-pairs]
  (let [response? (fn [req-or-resp] (:response-code req-or-resp))
        steps (sort-by (comp #(Long/parseLong %) :id) (apply concat request-response-pairs))
        content-type (fn [type-str] (cond
                                      (nil? type-str) nil
                                      (clojure.string/includes? type-str "json") "json"
                                      (clojure.string/includes? type-str "html") "html"
                                      :default (throw (RuntimeException. type-str))))
        path (fn [url] (.getPath (java.net.URL. url)))
        sequence-diagram-step (fn [{:keys [from to method frame-size] :as req-or-res}]
                                (let [frame-size-str (str " (" (human/filesize frame-size) ")")]
                                  (if-not (response? req-or-res)
                                    [(str (name from) " -> " (name to)
                                          ": " (path (get req-or-res "http.request.full_uri"))
                                          (when-not (= "GET" method) (str " [" method "]"))
                                          frame-size-str)]
                                    [(let [resp-code (:response-code req-or-res)
                                           content-type (content-type (get req-or-res "http.content_type"))
                                           cookie (some-> (get req-or-res "http.set_cookie") (clojure.string/split #"=") first)
                                           request (ffirst (filter (fn [[_ res]]
                                                                     (= (:id res) (:id req-or-res)))
                                                                   request-response-pairs))]
                                       (try
                                         (str (name from) " <-- " (name to)
                                              ": " resp-code " " content-type
                                              (when cookie (str "  [" cookie "]"))
                                              frame-size-str)
                                         (catch Exception e
                                           (println req-or-res)
                                           (throw e))))])))
        sizes (traffic-size request-response-pairs)
        sizes-in-order (map (fn [from-to] (merge
                                            {:from-to  from-to
                                             :from->to 0
                                             :to->from 0}
                                            (get sizes from-to)))
                            [[:browser :nginx]
                             [:browser :backend]
                             [:browser :keycloak]
                             [:backend :keycloak]])
        size-note-fn (fn [{:keys [from-to from->to to->from]}]
                       (str "note over " (name (first from-to))
                            ", " (name (second from-to))
                            ": ->" (human/filesize from->to)
                            "/<-" (human/filesize to->from)))]
    (clojure.string/join
      "\n"
      (concat
        ["@startuml\n"
         "actor browser"
         "participant nginx"
         "participant backend"
         "participant keycloak"]
        (mapcat sequence-diagram-step steps)
        (map size-note-fn sizes-in-order)
        ["@enduml"]))))

(defn create-image! [input-file output-file]
  (let [uml (-> input-file
                remove-boring-frames
                join-req-and-resp
                ->plantuml)
        out (FileOutputStream. (clojure.java.io/file output-file))]
    (-> (SourceStringReader. uml)
        (.generateImage out))
    (.close out)))

(def input-file "your input json file path")
(def output-img "the result image")

(defonce data (json/parse-string (slurp input-file)))

(create-image! data output-img)

(defonce img (ImageIcon. output-img))

(defonce jframe (doto
                  (JFrame. "img")
                  (.add (JLabel. img) BorderLayout/CENTER)
                  (.pack)
                  (.setVisible true)))

(dotimes [_ 2]
  (let [img-icon (.getImage (Toolkit/getDefaultToolkit) output-img)
        max-height 1450
        img-icon (if (> (.getHeight img-icon) max-height)
                   (.getScaledInstance img-icon (/ (* (.getWidth img-icon) max-height)
                                                   (.getHeight img-icon)) max-height 1)
                   img-icon)]
    (.setImage img img-icon))
  (-> img .getImage .flush)
  (.repaint jframe))

