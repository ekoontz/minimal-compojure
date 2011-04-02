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
  (merge
   (grammar/right head comp)
   {:english (string/join " "
                          (list (get comp :english)
                                (conjugate-english-verb head comp)))
    :italian (string/join " "
                          (list (get comp :italian)
                                (conjugate-italian-verb head comp)))}))

  
    

      

