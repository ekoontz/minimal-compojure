(ns italianverbs.lexiconfn
  (:use [hiccup core page-helpers]
	[somnium.congomongo])
  (:require
   [clojure.string :as string]
   [clojure.contrib.str-utils2 :as str-utils]))

(mongo! :db "mydb")

(defn italian [lexeme]
  (get (nth lexeme 1) :lexicon))

(defn synsem [lexeme]
  (nth lexeme 1))

(defn english [lexeme]
  (get (nth lexeme 1) :english))

;; CRUD-like functions:
;; italian and english are strings, featuremap is a map of key->values.
(defn add [italian english & [featuremap types result]]
  (if (first types)
    (add
     italian
     english
     featuremap
     (rest types)
     (merge (first types) result))
    (let [featuremap
          (merge featuremap
                 (merge result
                        (if english
                          (assoc {} :italian italian :english english)
                          (assoc {} :italian italian))))]
      (let [function-to-symbol featuremap]
        (insert! :lexicon function-to-symbol)
        featuremap))))

;; _italian and _english are strings; _types is a list of symbols (each of which is a map of key-values);
;; _result is an accumulator which is the merge of all of the maps in _types.
;; Key-values in earlier types have precedence over those in later types
;; (i.e. the later key-value pair do NOT override original value for that key).
(defn add-as [italian english & [types result]]
  (if (first types)
    (add-as
     italian
     english
     (rest types)
     (merge (first types) result))
    (add italian nil (merge {:english english} result))))

;; _italian is a string; _types is a list of symbols (each of which is a map of key-values);
;; _result is an accumulator which contains the merge of all of the maps
;; in _types.
;; no _english param needed; _result should be assumed to contain a :root key-value.
(defn add-infl [italian & [types result]]
  (if (first types)
    (add-infl
     italian
     (rest types)
     (merge (first types) result))
    (add italian nil result)))

(defn get [italian]
  (fetch-one :lexicon :where {:italian italian}))

(defn clear []
  (destroy! :lexicon {}))


