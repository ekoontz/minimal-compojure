;; CHANGES TO THIS FILE REQUIRES RESTARTING RING (for some reason).
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
    (let [np (grammar/np {:case {:$ne :nom}
                          :place true})]
      (grammar/combine prep np))))

(defn sv [head comp]
  (grammar/right head comp))

(defn vo [head comp]
  (grammar/left head comp))

(defn vp-pp [head comp]
  (grammar/left head comp))

(defn det-n [head comp]
  (grammar/right head comp))
  
(defn vp [ & [fs]]
  (let [verb-fs (merge
                 fs
                 {:cat :verb
                  :italian "pranzare"
                  :infl :infinitive})
        verb
        (nth (fetch :lexicon :where verb-fs)
             (rand-int (count (fetch :lexicon :where verb-fs))))]
    (if (get verb :obj)
      (grammar/combine verb (grammar/choose-object verb) vo)
      verb)))

(defn vp-with-adjunct-pp [ & [fs]]
  (let [vp (vp fs)]
    (grammar/combine vp (pp) vp-pp)))
    
(defn sentence []
  (let [vp (vp-with-adjunct-pp)]
    (let [subject
          (grammar/np
           (merge
            {:case {:$ne :acc}}
            (get (get-root-head vp) :subj)))]
      (if vp
        (grammar/combine vp subject sv)
        {:cat :error
         :error "vp-with-adjunct-pp returned null."}))))

      

