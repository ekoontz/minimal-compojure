(ns italianverbs.html
  (:use [hiccup core page-helpers]
	[somnium.congomongo]
	[italianverbs.lexicon]
	[italianverbs.grammar])
  (:require [clojure.string :as string]
	    [italianverbs.lexicon :as lexicon]
	    [clojure.contrib.str-utils2 :as str-utils]))

(defn verb-row [italian]
  (html  
   [:tr 
   [:th italian] [:td (get (get-from-lexicon italian) :english)] 
    ]))

(defn verb-table [lexicon]
  (html [:table 
	(for [verb (sort (keys lexicon))]
	     (verb-row verb))]))

