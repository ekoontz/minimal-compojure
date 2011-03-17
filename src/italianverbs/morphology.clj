(ns italianverbs.morphology
  (:use [hiccup core page-helpers]
	[somnium.congomongo]
	[italianverbs.grammar])
  (:require
   [clojure.string :as string]
   [italianverbs.html :as html]
   [clojure.contrib.str-utils2 :as str-utils]))

(defn get-head [sign]
  (if (get sign :head)
    (get sign :head)
    sign))

(defn remove-to [english-verb-phrase]
  (let [regex #"to (.*)"]
    (str-utils/replace english-verb-phrase regex (fn [[_ rest]] (str rest)))))

(defn add-s-to-first-word [english-verb-phrase]
  ;; FIXME: look at (get english-verb-phrase :head)
  (let [regex #"^([^ ]*)([o])([ ]?)(.*)"
	with-e
	(str-utils/replace
	 english-verb-phrase
	 regex
	 (fn [[_ word vowel space rest]] (str word (if vowel (str vowel "e")) space rest)))]
    (let [regex #"^([^ ]*)([ ]?)(.*)"]
      (str-utils/replace
       with-e
       regex
       (fn [[_ word space rest]] (str word "s" space rest))))))

(defn conjugate-english-verb [verb-head subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  (let [english (get verb-head :english)]
    (cond
     (and (not (= (get subject :cat) "noun"))
	  (not (= (get subject :cat) "pronoun")))
     {:cat :error
      :note  (str ":cat != :noun for " subject)}
     (= (get subject :person) "1st")
     (remove-to english)
     (= (get subject :person) "2nd")
     (remove-to english)
     (and
      (= (get subject :person) "3rd")
      (= (get subject :number) "singular")) 
     (add-s-to-first-word (remove-to english))
     true
     (remove-to english))))

(defn conjugate-italian-verb-regular [verb-head subject]
   (let [root-form (get verb-head :italian)
	 regex #"^([^ ]*)([aei])re([ ]?)(.*)"]
     (cond

      (and (= (get subject :person) "1st")
	   (= (get subject :number) "singular"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem "o" space rest)))

      (and (= (get subject :person) "1st")
	   (= (get subject :number) "plural"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem "i" "amo" space rest)))


      (and (= (get subject :person) "2nd")
	   (= (get subject :number) "singular"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem "i" space rest)))

      (and (= (get subject :person) "2nd")
	   (= (get subject :number) "plural"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem vowel "te" space rest)))

      
      (and (= (get subject :person) "3rd")
	   (= (get subject :number) "singular"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem "e" space rest)))

      (and (= (get subject :person) "3rd")
	   (= (get subject :number) "plural"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem vowel "no" space rest)))
      true
      (str "(conjugate-italian-verb-regular=>(can't conjugate this..)"
	   verb-head
	   subject))))
					;	   (tablize verb-head)
;	   (tablize subject)))))

(defn conjugate-italian-verb [verb-phrase subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  ;; takes two feature structures and returns a string.
  (let [italian (get verb-phrase :italian)
	italian-head (get (get verb-phrase :head) :italian)]
    (let [italian (if italian-head italian-head italian)]
      (let [irregular
	    (fetch-one :lexicon
		       :where {:cat :verb
			       :infl :present
			       :person (get subject :person)
			       :number (get subject :number)
			       :root.italian (get (get-head verb-phrase) :italian)
			       }
		       )]
	(if irregular
	  (str (get irregular :italian)
	       " "
	       (get (get verb-phrase :comp) :italian))
	  (str
	   (conjugate-italian-verb-regular
	    (get-head verb-phrase) subject) " "
	   (get (get verb-phrase :comp) :italian)))))))
 
(defn plural-masc [italian]
 (let [regex #"^([^ ]*)o([ ]?)(.*)"]
   (str-utils/replace
    italian
    regex (fn [[_ stem space rest]] (str stem "i" space rest)))))

(defn plural-fem [italian]
 (let [regex #"^([^ ]*)a([ ]?)(.*)"]
   (str-utils/replace
    italian
    regex (fn [[_ stem space rest]] (str stem "e" space rest)))))

(defn conjugate-it [head]
  (cond (= (get head :cat) "noun")
	(cond (= (get head :gender) "masc")
	      (cond (= (get head :number) "plural")
		    (plural-masc (get head :italian))
		    true
		    (get head :italian))
	      (= (get head :gender) "fem")
	      (cond (= (get head :number) "plural")
		    (plural-fem (get head :italian))
		    true
		    (get head :italian))
	      true
	      "??(not masc or fem)")
	true
	(str "??(cat != noun)"
	     (get head :cat)
	     (= (get head :cat) "noun"))))

(defn conjugate-en [head arg]
  (str (get arg :english)
       " "
       (cond (= (get head :cat) "noun")
	     (cond (= (get head :number) "plural")
		   (str (get head :english) "s")
		   true
		   (get head :english))
	     true
	     (str "??(cat != noun)"
		  (get head :cat)
		  (= (get head :cat) "noun")))))

