(ns italianverbs.generate
  (:use [hiccup core page-helpers]
	[somnium.congomongo])
  (:require
   [clojure.string :as string]
   [italianverbs.lexiconfn :as lexfn]
   [italianverbs.grammar :as grammar]
   [italianverbs.morphology :as morphology]
   [clojure.contrib.str-utils2 :as str-utils]))

(defn choose-lexeme [struct]
  ;; do a query based on the given struct,
  ;; and choose a random element that satisfies the query.
  (let [results (fetch :lexicon :where struct)]
    (if (= (count results) 0)
      {:cat :error :note (str "choose lexeme: no results found for " struct)}
      (nth results (rand-int (count results))))))

;; return a feature structure just like sign, but with :left and :right set.
(defn pos [sign left right]
  (if sign
    (merge sign {:left left :right right})
    {:left left :right right :cat :error :note "null sign given to (pos)"}))

(defn generate-np [offset]
  (let [lexeme (choose-lexeme {:cat :noun})
        genfn (get lexeme :genfn)]
    (let [noun
          (pos lexeme
               (+ offset 1)
               (+ offset 2))]
      ;; choose a determiner that agrees with the noun in number and gender.
      (let [determiner
            (pos
             (choose-lexeme
              {:gender (get noun :gender)
               :number (get noun :number)
               :cat :det
               :def :def
               })
             offset
             (+ offset 1))]
        (grammar/combine noun determiner)))))

(defn generate-vp [offset]
  (let [verb-fs {:cat :verb
		 :infl :infinitive
		 :fn "verb-vo"}
	verb
	(nth (fetch :lexicon :where verb-fs)
	     (rand-int (count (fetch :lexicon :where verb-fs))))]
    (let [verb-with-pos
	  (pos verb
	       offset
	       (+ 1 offset))
	  parent (grammar/combine
		  verb-with-pos
		  (generate-np (+ 1 offset)))]
    parent)))
      
(defn sentence []
  (let [subject
	(pos
	 (nth (fetch :lexicon :where {:cat :pronoun})
	      (rand-int (count (fetch :lexicon :where {:cat :pronoun}))))
	 0 1)] ;; fixme: left is always 0, but right (1)
    ;; varies depending on length of subject.
    (grammar/combine (generate-vp 1) subject)))

(defn linearize [signs & [offset]]
  (let [offset (if offset offset 0)]
    (if (> (count signs) 0)
      (cons
       (pos (first signs) offset (+ 1 offset))
       (linearize (rest signs) (+ 1 offset))))))

