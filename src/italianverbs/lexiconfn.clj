(ns italianverbs.lexiconfn
  (:use [hiccup core page-helpers]
	[somnium.congomongo])
  (:require
   [clojure.string :as string]
   [italianverbs.morphology :as morphology]
   [clojure.contrib.str-utils2 :as str-utils]))

(mongo! :db "mydb")


(defn italian [lexeme]
  (get (nth lexeme 1) :lexicon))

(defn synsem [lexeme]
  (nth lexeme 1))

(defn english [lexeme]
  (get (nth lexeme 1) :english))

;; CRUD-like functions:
(defn add [italian english & [featuremap]]
  (let [featuremap
	(merge featuremap
	       (if english
		 (assoc {} :italian italian :english english)
		 (assoc {} :italian italian)))]
    (let [function-to-symbol featuremap]
      (insert! :lexicon function-to-symbol)
      featuremap)))

(defn add2 [italian & [types result]]
  (if (first types)
    (add2
     italian
     (rest types)
     (merge (first types) result))
    (add italian nil result)))

(defn get [italian]
  (fetch-one :lexicon :where {:italian italian}))

(defn clear []
  (destroy! :lexicon {}))


