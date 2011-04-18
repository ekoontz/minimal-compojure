(ns base.lib
  (:use [hiccup core page-helpers])
  (:require
   [clojure.set :as set]
   [clojure.string :as string]))

(defn req-tr [key-value-pair]
  (let [key (first key-value-pair)
        val (second key-value-pair)]
    (str "<tr><th>" key "</th><td>" val "</td></tr>")))

(defn reqdata [request]
  (html
   [:div
     [:table
      (string/join " " (seq
                        (map req-tr
                             (map (fn [key]
                                    (list key (get request key)))
                                  (set/difference (set (keys request))
                                                  (set (list )))))))]]))
