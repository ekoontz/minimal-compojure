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

;; return a feature structure just like sign, but with :left and :right set.
(defn pos [sign left right]
  (if sign
    (merge sign {:left left :right right})
    {:left left :right right :cat :error :note "null sign given to (pos)"}))

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

(defn generate-np [offset]
  (let [noun (grammar/choose-lexeme {:cat :noun})
        ;; use _genfn to generate an argument (determiner) given _noun.
        genfn (get noun :genfn)]
    (let [determiner
          (apply (eval (find-fn genfn)) (list noun))]
      (if determiner
        (grammar/combine
         (pos noun (+ offset 1) (+ offset 2))
         (pos determiner offset (+ offset 1)))
        (pos noun offset (+ offset 1))))))

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
  ;; fixme: :left (beginning of sentence) is always 0,
  ;; but :right (end of subject/beginning of vp)
  ;; varies depending on length of subject.
  (let [subject
	(pos
     (generate-np 0)
;	 (nth (fetch :lexicon :where {:cat :pronoun})
;	      (rand-int (count (fetch :lexicon :where {:cat :pronoun}))))
	 0 1)]
    (grammar/combine (generate-vp 1) subject)))

(defn linearize [signs & [offset]]
  (let [offset (if offset offset 0)]
    (if (> (count signs) 0)
      (cons
       (pos (first signs) offset (+ 1 offset))
       (linearize (rest signs) (+ 1 offset))))))

