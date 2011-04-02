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

    

      

