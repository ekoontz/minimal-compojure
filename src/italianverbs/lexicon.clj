(ns italianverbs.lexicon
    (:use [hiccup core page-helpers])
    )


;; figure out differences between hash-map and hash-set..
(def lexicon (hash-map))

(defn verb-row [italian]
  (html  
   [:tr 
   [:th italian] [:td (get lexicon italian)] 
   ]))

(defn add-verb [italian english]
  (def lexicon (assoc lexicon italian english)))

(add-verb "dire" "to say")
(add-verb "fare" "to do")
(add-verb "scrivere" "to write")
(add-verb "correggere" "to correct")
(add-verb "leggere" "to read")
(add-verb "mangiere" "to eat")
(add-verb "parlere" "to speak")
(add-verb "pranzare" "to eat lunch")
(add-verb "smettere" "to quit")

(defn verb-table []
  (html [:table 
	(for [verb (sort (keys lexicon))]
	     (verb-row verb))]))

(defn show [] 
  (verb-table))

