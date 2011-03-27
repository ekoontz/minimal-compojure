(ns italianverbs.morphology
  (:use [hiccup core page-helpers]
	[somnium.congomongo])
  (:require
   [clojure.string :as string]
   [italianverbs.html :as html]
   [clojure.contrib.str-utils2 :as str-utils]))

(defn get-head [sign]
  (if (get sign :head)
    (get sign :head)
    sign))

(defn remove-to [english-verb-phrase]
  (let [english (get english-verb-phrase :english)]
    (let [regex #"^to[ ]+(.*)"]
      (let [string
            (str-utils/replace english regex (fn [[_ rest]] (str rest)))]
        (merge
         {:remove-to string}
         english-verb-phrase)))))

(defn add-s-to-first-word [english-verb-phrase]
  (let [english-verb-string (get english-verb-phrase :english)]
    (let [regex #"^[ ]*([^ ]+)[ ]*(.*)"
          with-s
          (str-utils/replace
           english-verb-string
           regex
           (fn [[_ first-word rest]]
             (str first-word (if (re-find #"o$" first-word) "e") "s" " " rest)))]
      (merge
       {:add-s with-s}
       english-verb-phrase))))
    
(defn conjugate-english-verb [verb-head subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  (let [english (get verb-head :english)
        remove-to (remove-to verb-head)]
    (cond
     (and (not (= (get (get-head subject) :cat) "noun"))
          (not (= (get (get-head subject) :cat) "pronoun")))
     {:cat :error
      :note  (str "<tt><i>error: :cat of '" (get subject :english) "' != :noun</i>."
                  "(<b>conjugate-english-verb</b> " (get verb-head :english)
                  "," (get subject :english) ")</tt>")}
     (= (get (get-head subject) :person) "1st")
     (get remove-to :remove-to)
     (= (get (get-head subject) :person) "2nd")
     (get remove-to :remove-to)
     (and
      (= (get (get-head subject) :person) "3rd")
      (= (get (get-head subject) :number) "singular"))
     (get
      (add-s-to-first-word
       (merge
        remove-to
        {:english (get remove-to :remove-to)}))
      :add-s)
     true ;; 3rd plural
     (get remove-to :remove-to))))

(defn conjugate-italian-verb-regular [verb-head subject-head]
   (let [root-form (get verb-head :italian)
	 regex #"^([^ ]*)([aei])re([ ]?)(.*)"]
     (cond

      (and (= (get subject-head :person) "1st")
	   (= (get subject-head :number) "singular"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem "o" space rest)))

      (and (= (get subject-head :person) "1st")
	   (= (get subject-head :number) "plural"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem "i" "amo" space rest)))
      (and (= (get subject-head :person) "2nd")
	   (= (get subject-head :number) "singular"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem "i" space rest)))

      (and (= (get subject-head :person) "2nd")
	   (= (get subject-head :number) "plural"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem vowel "te" space rest)))

      
      (and (= (get subject-head :person) "3rd")
	   (= (get subject-head :number) "singular"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem "e" space rest)))

      (and (= (get subject-head :person) "3rd")
	   (= (get subject-head :number) "plural"))
      (str-utils/replace root-form regex
			 (fn [[_ stem vowel space rest]] (str stem vowel "no" space rest)))
      true
      (str "<tt><i>error: :person or :number value was not matched</i>. (<b>conjugate-italian-verb-regular</b> " (get verb-head :italian) ",(phrase with head:'" (get subject-head :italian) "'))</i></tt>"))))

(defn get-root-head [sign]
  (cond
   (get sign :head)
   (get-root-head (get sign :head))
   true
   sign))

(defn conjugate-italian-verb [verb-phrase subject]
  ;; conjugate verb based on subject and eventually verb's features (such as tense)
  ;; takes two feature structures and returns a string.
  (let [italian (get verb-phrase :italian)
        italian-head (get (get-head verb-phrase) :italian)
        ;; all we need is the head, which has the relevant grammatical information, not the whole subject
        subject (get-head subject)] 
    (let [italian (if italian-head italian-head italian)]
      (let [irregular
	    (fetch-one :lexicon
		       :where {:cat :verb
                       :infl :present
                       :person (get subject :person)
                       :number (get subject :number)
                       :root.italian (get (get-root-head verb-phrase) :italian)
			       }
		       )]
	(if irregular
	  (str (get irregular :italian))
	  (str
	   (conjugate-italian-verb-regular
	    (get-head verb-phrase) subject)))))))
 
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

