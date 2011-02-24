(ns italianverbs.lexicon
    (:use [hiccup core page-helpers])
    )


;; figure out differences between hash-map and hash-set..
;; using hash-map since I'd expect that to have unit-time key lookup.
(def lexicon (hash-map))
(def lexicon-i2e (hash-map))

(defn verb-row [italian]
  (html  
   [:tr 
   [:th italian] [:td (get lexicon italian)] 
   ]))

(defn add-verb [italian english]
  (def lexicon (assoc lexicon italian english)))

(defn add-lexeme [italian english & [featuremap]]
  (def lexicon-i2e (assoc lexicon-i2e italian (assoc featuremap :english english))))

(add-verb "dimenticare" "to forget")
(add-verb "dire" "to say")
(add-verb "fare" "to do")
(add-verb "scrivere" "to write")
(add-verb "correggere" "to correct")
(add-verb "leggere" "to read")
(add-verb "mangiere" "to eat")
(add-verb "parlere" "to speak")
(add-verb "pranzare" "to eat lunch")
(add-verb "smettere" "to quit")
(add-verb "lunghi" "long")

(defn intrans []) ;; e.g. "sleep"
(defn trans []) ;; e.g. "forget"
(defn trans2 []) ;; e.g. "give"

;; infinitive verbs
(add-lexeme "dimenticare" "to forget" {:cat :verb :infl :infinitive :fn intrans})
(add-lexeme "dire" "to say" {:cat :verb :infl :infinitive :fn trans})
(add-lexeme "fare" "to do" {:cat :verb :infl :infinitive})
(add-lexeme "scrivere" "to write" {:cat :verb :infl :infinitive})
(add-lexeme "correggere" "to correct" {:cat :verb :infl :infinitive})
(add-lexeme "leggere" "to read" {:cat :verb :infl :infinitive})
(add-lexeme "mangiere" "to eat" {:cat :verb :infl :infinitive})
(add-lexeme "parlere" "to speak" {:cat :verb :infl :infinitive})
(add-lexeme "pranzare" "to eat lunch" {:cat :verb :infl :infinitive})
(add-lexeme "smettere" "to quit" {:cat :verb :infl :infinitive})

;; pronouns
(add-lexeme "io" "i" {:person "1st" :number "singular" :cat :noun})
(add-lexeme "tu" "you" {:person "2nd" :number "singular"  :cat :noun})
(add-lexeme "lui" "he" {:person "3rd" :number "singular" :cat :noun})
(add-lexeme "noi" "we" {:person "1st" :number "plural" :cat :noun})
(add-lexeme "voi" "you all" {:person "2nd" :number "plural" :cat :noun})
(add-lexeme "loro" "they" {:person "3rd" :number "plural" :cat :noun})

;; nouns
(add-lexeme "uomo" "man"
	    {:cat :adjective})
(add-lexeme "donna" "woman"
	    {:cat :adjective})

;; sentences

(add-lexeme "ha gli occhi azzuri" "he has blue eyes"
	    {:person "3rd" :number "singular" :cat :verb})
(add-lexeme "ha i cappelli non molte lunghi" "he has not very long hair"
	    {:person "3rd" :number "singular" :cat :verb})
(add-lexeme "ha il naso alla francese" "he has a french nose" 
	    {:person "3rd" :number "singular" :cat :verb})
(add-lexeme "non lo so" "i don't know"
	    {:cat :verb})
(add-lexeme "come sono?" "how?"
	    {:cat :verb})
(add-lexeme "cosa fa?" "what?"
	    {:cat :verb})

;; adjectives
(add-lexeme "alto" "tall"
	    {:cat :adjective})
(add-lexeme "basso" "shortd"
	    {:cat :adjective})
(add-lexeme "giovano" "young"
	    {:cat :adjective})
(add-lexeme "anziano" "old"
	    {:cat :adjective})
(add-lexeme "margra" "lean"
	    {:cat :adjective})
(add-lexeme "grasso" "fat"
	    {:cat :adjective})
(add-lexeme "bello" "beautiful"
	    {:cat :adjective})
(add-lexeme "bruto" "ugly"
	    {:cat :adjective})
(add-lexeme "carino" "cute"
	    {:cat :adjective})
(add-lexeme "lunghi" "corti"
	    {:cat :adjective})
(add-lexeme "calvo" "bald"
	    {:cat :adjective})
(add-lexeme "bruno" "brown"
	    {:cat :adjective})
(add-lexeme "bianchi" "white"
	    {:cat :adjective})
(add-lexeme "di mezza eta" "middle-aged"
	    {:cat :adjective})
(add-lexeme "qui" "here"
	    {:cat :adjective})

(defn verb-table [lexicon]
  (html [:table 
	(for [verb (sort (keys lexicon))]
	     (verb-row verb))]))

