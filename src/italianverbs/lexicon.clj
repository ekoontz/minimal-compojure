(ns italianverbs.lexicon
    (:use [hiccup core page-helpers])
    (:require [clojure.string :as string]
	      [clojure.contrib.str-utils2 :as str-utils]))

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
  (let [regex #"to (.*)"]
    (str-utils/replace english-verb-phrase regex (fn [[_ rest]] (str rest)))))

(defn add-s-to-first-word [english-verb-phrase]
  (let [regex #"^([^ ]*)([ ]?)(.*)"]
    (str-utils/replace english-verb-phrase regex (fn [[_ word space rest]] (str word "s" space rest)))))

(defn conjugate-english [verb subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  (let [english (get verb :english)]
    (cond (= (get subject :person) :1st)
	  (remove-to english)
	  (= (get subject :person) :2nd)
	  (remove-to english)
	  (= (get subject :person) :3rd)
	  (add-s-to-first-word (remove-to english))
	  true
	  "to write")))

(defn ere-1st [italian-verb-phrase]
 (let [regex #"^([^ ]*)ere([ ]?)(.*)"]
   (str-utils/replace italian-verb-phrase regex (fn [[_ stem space rest]] (str stem "o" space rest)))))
(defn ere-2nd [italian-verb-phrase]
 (let [regex #"^([^ ]*)ere([ ]?)(.*)"]
   (str-utils/replace italian-verb-phrase regex (fn [[_ stem space rest]] (str stem "i" space rest)))))
(defn ere-3rd [italian-verb-phrase]
 (let [regex #"^([^ ]*)ere([ ]?)(.*)"]
   (str-utils/replace italian-verb-phrase regex (fn [[_ stem space rest]] (str stem "e" space rest)))))

(defn conjugate-italian [verb subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  (let [italian (get verb :italian)]
    (cond (= (get subject :person) :1st)
	  (ere-1st italian)
	  (= (get subject :person) :2nd)
	  (ere-2nd italian)
	  (= (get subject :person) :3rd)
	  (ere-3rd italian)
	  true
	  (str subject italian "<i>infinitivo</i>"))))

(defn trans-sv [head arg]  ;; e.g. "forget","writes a book"
  (assoc {}
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
    :cat (get head :cat)
    :fn trans-sv
    :english
    (string/join " "
		 (list 
		  (get head :english)
		  (get arg :english)))

    :italian
    (string/join " "
		 (list 
		  (get head :italian)
		  (get arg :italian)))))


(defn trans2 []) ;; e.g. "give"

;; verbs
(add-lexeme "dimenticare" "to forget" {:cat :verb :infl :infinitive :fn intrans})
(add-lexeme "dire" "to say" {:cat :verb :infl :infinitive :fn trans})
(add-lexeme "fare" "to do" {:cat :verb :infl :infinitive})


(add-lexeme "scrivere" "to write" {:cat :verb :infl :infinitive :fn trans-vo})

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
(add-lexeme "forte" "strong"
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

