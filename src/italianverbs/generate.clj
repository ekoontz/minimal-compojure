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

(defn span [fs]
  (cond
   (nil? (get fs :head)) 1
   true
   2))

;; return a feature structure just like sign, but with :left and :right set.
(defn pos [sign left & [right]]
  (if sign
    (let [right
          (if right right
              (+ left (span sign)))]
      (merge {:left left :right right} sign)) ;; overwrites existing :left and :right, if any.
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

(defn np [offset & [fs]]
  (let [noun (grammar/choose-lexeme (merge fs {:cat :noun}))
        ;; use _genfn to generate an argument (determiner) given _noun.
        genfn (get noun :genfn)]
    (let [determiner (apply (eval (find-fn genfn)) (list noun))
          det-span (span determiner)
          noun-span (span noun)]
      (if determiner
        (grammar/combine
         (pos noun (+ offset det-span) (+ offset det-span noun-span))
         (pos determiner offset (+ offset det-span)))
        (pos noun offset (+ 1 offset))))))

(defn vp [offset & [fs]]
  (let [verb-fs (merge
                 fs
                 {:cat :verb
                  :infl :infinitive
                  :fn "verb-vo"})
        verb
        (nth (fetch :lexicon :where verb-fs)
             (rand-int (count (fetch :lexicon :where verb-fs))))]
    ;; FIXME: should count size of verb (not just "+ 1"')
    (let [verb
          (pos verb
               offset)
          np
          (np (+ 1 offset)
              {:case {:$ne :nom}})]
      (grammar/combine
        verb
        (merge
         {:case :acc}
         np)))))

(defn sentence []
  ;; fixme: :left (beginning of sentence) is always 0,
  ;; but :right (end of subject/beginning of vp)
  ;; varies depending on length of subject.
  (let [subject
        (np 0 {:case {:$ne :acc}})]
    (let [subject
          (merge
           {:head
            (merge 
             {:case :nom}
             (morphology/get-head subject))}
           subject)]
    (grammar/combine (vp (get subject :right)) subject))))
    
(defn linearize [signs & [offset]]
  (let [offset (if offset offset 0)]
    (if (> (count signs) 0)
      (cons
       (pos (first signs) offset (+ 1 offset))
       (linearize (rest signs) (+ 1 offset))))))

