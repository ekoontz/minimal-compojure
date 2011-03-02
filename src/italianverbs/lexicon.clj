(ns italianverbs.lexicon
    (:use [hiccup core page-helpers])
    (:require [clojure.string :as string]))

;; figure out differences between hash-map and hash-set..
;; using hash-map since I'd expect that to have unit-time key lookup.
(def lexicon-i2e (hash-map))

(defn italian [lexeme]
  (get (nth lexeme 1) :lexicon))

(defn synsem [lexeme]
  (nth lexeme 1))

(defn english [lexeme]
  (get (nth lexeme 1) :english))

(defn verb-row [italian]
  (html  
   [:tr 
   [:th italian] [:td (get (get lexicon-i2e italian) :english)] 
   ]))

(defn add-lexeme [italian english & [featuremap]]
  (def lexicon-i2e
    (assoc lexicon-i2e
      italian
      (assoc featuremap :english english :italian italian))))

(defn intrans []) ;; e.g. "sleep"

(defn trans [head arg]  ;; e.g. "forget"
  (assoc {}
    :infl :present
    :cat (get head :cat)
    :english
    (string/join " "
		 (list (get arg :english)
		       (cond (= (get arg :person) :1st)
			     "write"
			     (= (get arg :person) :2nd)
			     "write"
			     (= (get arg :person) :3rd)
			     "writes"
			     true
			     "to write")))
    :italian
    (string/join " "
		 (list (get arg :italian)
		       (cond (= (get arg :person) :1st)
			     "scrivo"
			     (= (get arg :person) :2nd)
			     "scrivi"
			     (= (get arg :person) :3rd)
			     "scriva"
			     true
			     "scrivere")))))

(defn remove-to [english-verb-phrase]
  ;; regexp goes here: /^to /
  english-verb-phrase)

(defn conjugate-english [verb subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  (cond (= (get subject :person) :1st)
	;; use regexp: "to write" -> "write"
	(remove-to (str (get verb :english)))
	(= (get subject :person) :2nd)
	(remove-to (str (get verb :english)))
	(= (get subject :person) :3rd)
	(str (remove-to (str (get verb :english))) "s")
	true
	"to write"))

(defn conjugate-italian [verb subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  (cond (= (get subject :person) :1st)
	;; use regexp: "scrivere" -> "scrivo"
	(str (get verb :italian))
	(= (get subject :person) :2nd)
	(str (get verb :italian) "")
	(= (get subject :person) :3rd)
	(str (get verb :italian) "s")
	true
	"to write"))

(defn trans-sv [head arg]  ;; e.g. "forget","writes a book"
  (assoc {}
    :infl :present
    :cat (get head :cat)
    :english
    (string/join " "
		 (list (get arg :english)
		       (conjugate-english head arg)))
    :italian
    (string/join " "
		 (list (get arg :italian)
		       (conjugate-italian head arg)))))

(defn trans-vo [head arg]  ;; e.g. "forget","writes a book"
  (assoc {}
    :infl :present
    :cat (get head :cat)
    :english
    (string/join " "
		 (list 
		  (cond (= (get arg :person) :1st)
			"write"
			(= (get arg :person) :2nd)
			"write"
			(= (get arg :person) :3rd)
			"writes"
			true
			(str "to write "))
		  (get arg :english)))

    :italian
    (string/join " "
		 (list 

		  (cond (= (get arg :person) :1st)
			"scrivo"
			(= (get arg :person) :2nd)
			"scrivi"
			(= (get arg :person) :3rd)
			"scriva"
			true
			(str "scrivere "))
		  (get arg :italian)))))


(defn trans2 []) ;; e.g. "give"

;; verbs
(add-lexeme "dimenticare" "to forget" {:cat :verb :infl :infinitive :fn intrans})
(add-lexeme "dire" "to say" {:cat :verb :infl :infinitive :fn trans})
(add-lexeme "fare" "to do" {:cat :verb :infl :infinitive})


(add-lexeme "scrivere" "to write" {:cat :verb :infl :infinitive :fn trans-vo})

(add-lexeme "scrivere il libro" "to write the book" {:cat :verb :infl :sv :fn trans-sv})

(add-lexeme "correggere" "to correct" {:cat :verb :infl :infinitive})
(add-lexeme "leggere" "to read" {:cat :verb :infl :infinitive})
(add-lexeme "mangiere" "to eat" {:cat :verb :infl :infinitive})
(add-lexeme "parlere" "to speak" {:cat :verb :infl :infinitive})
(add-lexeme "pranzare" "to eat lunch" {:cat :verb :infl :infinitive})
(add-lexeme "smettere" "to quit" {:cat :verb :infl :infinitive})

;; pronouns
(add-lexeme "io" "i" {:person :1st :number :singular :cat :noun})
(add-lexeme "tu" "you" {:person :2nd :number :singular  :cat :noun})
(add-lexeme "lui" "he" {:person :3rd :number :singular :cat :noun})
(add-lexeme "noi" "we" {:person :1st :number :plural :cat :noun})
(add-lexeme "voi" "you all" {:person :2nd :number :plural :cat :noun})
(add-lexeme "loro" "they" {:person :3rd :number :plural :cat :noun})

;; nouns
(add-lexeme "uomo" "man"
	    {:cat :noun})
(add-lexeme "donna" "woman"
	    {:cat :noun})

(add-lexeme "il libro" "the book"
	    {:cat :noun,
	     :writable true})

;; adjectives
(add-lexeme "bianco" "white"
	    {:cat :adjective})
(add-lexeme "noro" "black"
	    {:cat :adjective})

;; sentences

(add-lexeme "ha gli occhi azzuri" "he has blue eyes"
	    {:person "3rd" :number :singular :cat :verb})
(add-lexeme "ha i cappelli non molte lunghi" "he has not very long hair"
	    {:person "3rd" :number :singular :cat :verb})
(add-lexeme "ha il naso alla francese" "he has a french nose" 
	    {:person "3rd" :number :singular :cat :verb})
(add-lexeme "non lo so" "i don't know"
	    {:cat :verb})
(add-lexeme "come sono?" "how?"
	    {:cat :verb})
(add-lexeme "cosa fa?" "what?"
	    {:cat :verb})

;; adjectives
(add-lexeme "alto" "tall"
	    {:cat :adjective})
(add-lexeme "basso" "short"
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
(add-lexeme "lunghi" "long"
	    {:cat :adjective})
(add-lexeme "corti" "short"
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

