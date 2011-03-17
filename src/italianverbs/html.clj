(ns italianverbs.html
  (:use [hiccup core page-helpers]
	[somnium.congomongo])
  (:require [clojure.string :as string]
	    [italianverbs.lexiconfn :as lex]
	    [clojure.contrib.str-utils2 :as str-utils]))

(defn verb-row [italian]
  (html  
   [:tr 
   [:th italian] [:td (get (lex/get italian) :english)] 
    ]))

(defn verb-table [lexicon]
  (html [:table 
	(for [verb (sort (keys lexicon))]
	     (verb-row verb))]))

