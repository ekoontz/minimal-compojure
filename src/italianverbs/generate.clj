(ns italianverbs.generate
  (:use [hiccup core page-helpers]
        [somnium.congomongo])
  (:require
   [clojure.string :as string]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.grammar :as grammar]
   [italianverbs.generate :as generate]
   [italianverbs.morphology :as morphology]
   [clojure.contrib.str-utils2 :as str-utils]))

"find a function which might really be a function, or might be a string that
 needs to be converted to a function whose name is that string."
(defn find-fn [fn]
  (cond
   (nil? fn)
   {:cat :error :note
    (str "function is null")}
   (string? fn)
   (symbol fn)
   true fn))

(defn np [ & [fs]]
  (let [noun (grammar/choose-lexeme (merge fs {:cat :noun}))
        ;; use _genfn to generate an argument (determiner) given _noun.
        genfn (get noun :genfn)]
    (let [determiner (apply (eval (find-fn genfn)) (list noun))]
      (if determiner
        (grammar/combine noun determiner 'right)
        noun))))

(defn pp [ & [fs]]
  (let [prep (grammar/choose-lexeme (merge fs {:cat :prep}))
        ;; (eventually) use _genfn to generate an argument (np) given _prep.
        genfn (get prep :genfn)]
    (let [np (np {:case {:$ne :nom}})]
      (grammar/combine prep np 'left))))

(defn vp [ & [fs]]
  (let [verb-fs (merge
                 fs
                 {:cat :verb
                  :infl :infinitive})
        verb
        (nth (fetch :lexicon :where verb-fs)
             (rand-int (count (fetch :lexicon :where verb-fs))))]
    (grammar/combine verb (pp) 'left)))

(defn sentence []
  (let [subject
        (np {:case {:$ne :acc}})]
    (let [subject
          (merge
           {:head
            (merge 
             {:case :nom}
             (morphology/get-head subject))}
           subject)
          vp (vp)]
      (grammar/combine vp subject 'right))))
      

