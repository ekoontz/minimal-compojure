(ns italianverbs.generate
  (:use [hiccup core page-helpers]
        [somnium.congomongo]
        [italianverbs.morphology])
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


(defn pp [ & [fs]]
  (let [prep (grammar/choose-lexeme (merge fs {:cat :prep}))
        ;; (eventually) use _genfn to generate an argument (np) given _prep.
        genfn (get prep :genfn)]
    (let [np (grammar/np {:case {:$ne :nom}})]
      (grammar/combine prep np 'left))))

(defn vp [ & [fs]]
  (let [verb-fs (merge
                 fs
                 {:italian "mangiare"
                  :cat :verb
                  :infl :infinitive})
        verb
        (nth (fetch :lexicon :where verb-fs)
             (rand-int (count (fetch :lexicon :where verb-fs))))]
    (let [genfn (get verb :genfn)]
      (let [arg (apply (eval (find-fn genfn)) (list verb))]
;        (grammar/combine verb arg 'left)))))
    ;; TODO: generate pp if this verb allows it lexically.

    ;; generate a VP given this verb.
;    (grammar/combine verb (pp) 'left)))
;    (grammar/combine 
;     (grammar/combine verb (np) 'left)
;     (pp) 'left)))
;    (grammar/combine verb (np) 'left)))
    (cond
;     (= (get (morphology/get-head verb) :subcat) 'np-pp)
;     (grammar/combine 
;      (grammar/combine verb (np) 'left)
;      (pp) 'left)
     true
     (grammar/combine verb arg 'left))))))

(defn vp-with-adjunct-pp [ & [fs]]
  (let [verb-fs (merge
                 fs
                 {:italian "leggere"
                  :cat :verb
                  :infl :infinitive})
        verb
        (nth (fetch :lexicon :where verb-fs)
             (rand-int (count (fetch :lexicon :where verb-fs))))
        pp (pp)]
    (let [genfn (get verb :genfn)]
      (let [arg (apply (eval (find-fn genfn)) (list verb))]
        (cond
         true
         (grammar/combine
          (vp fs)
          pp
          'left))))))

    
(defn sentence []
  (let [subject
        ;; (np) generates a random noun phrase: in this case, one whose case is NOT accusative.
        (grammar/np {:case {:$ne :acc}})]
    (let [subject
          (merge
           {:head
            (merge 
             {:case :nom}
             (morphology/get-head subject))}
           subject)
          ;; (vp) generates a random verb phase
          ;;          vp (vp)]
          vp (vp-with-adjunct-pp)]
      (grammar/combine vp subject 'right))))
      

