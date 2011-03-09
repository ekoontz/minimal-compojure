(ns italianverbs.test
    (:use 
     [hiccup core page-helpers]
     [italianverbs.grammar]
     [italianverbs.lexicon]
     [somnium.congomongo])
    (:require 
     [clojure.string :as string]
     [italianverbs.quiz :as quiz]))

(mongo! :db "mydb")

;; useful library functions
(defn show-answer [question] (get question :answer))
(defn wrap-div [string]
  (str "<div class='test'>" string "</div>"))
  
(defn lex-thead [lexeme]
  (str "<tr>"
       "<th>italian</th>"
       "<th>english</th>"
       "<th>person</th>"
       "<th>number</th>"
       "<th>cat</th>"
       "<th>infl</th>"
       "<th>writable</th>"
       "<th>fn</th>"
       "</tr>"))

(defn lex-row [lexeme]
  (str "<tr>"
       "<td>" (nth lexeme 0) "</td>"
       "<td>" (get (nth lexeme 1) :english) "</td>"
       "<td>" (get (nth lexeme 1) :person) "</td>"
       "<td>" (get (nth lexeme 1) :number) "</td>"
       "<td>" (get (nth lexeme 1) :cat) "</td>"
       "<td>" (get (nth lexeme 1) :infl) "</td>"
       "<td>" (get (nth lexeme 1) :writable) "</td>"
       "<td>" (get (nth lexeme 1) :fn) "</td>"
       "</tr>"))

(defn answer-row [question]
  (let [correctness (if (= (get question :guess) (get question :answer)) "correct" "incorrect")]
       (str "<tr><td>" 
	    (get question :answer) 
	    "</td><td>" 
	    (get question :guess) 
	    "</td><td class='" correctness "'>"
	    correctness
	    "</td></tr>")))

;; tests
(defn answertable []
  (str "<table>" (string/join " " (map answer-row (fetch :question))) "</table>"))

(defn correct []
  (str "correct guesses: " (count (mapcat quiz/each-correct (fetch :question)))
       " out of : " (count (fetch :question))))

(defn show-lexicon-as-feature-structures []
  (string/join " "
	       (map (fn [lexeme]
		      (fs lexeme))
		    (fetch :lexicon))))

(defn io-andare []
  (let [subject (get-from-lexicon "io")
	verb-phrase (get-from-lexicon "andare")
	parent (combine verb-phrase subject)]
    (tablize parent
	     (list subject
		   verb-phrase))))

(defn tu-andare []
  (let [subject (get-from-lexicon "tu")
	verb-phrase (get-from-lexicon "andare")
	parent (combine verb-phrase subject)]
    (tablize parent
	     (list subject
		   verb-phrase))))


(defn io-pranzare []
  (let [subject (get-from-lexicon "io")
	verb-phrase (get-from-lexicon "pranzare")
	parent (combine verb-phrase subject)]
    (tablize parent
	     (list subject
		   verb-phrase))))

(defn lui-scrivo-il-libro []
  (let [subject (get-from-lexicon "lui")
	object (combine
		(get-from-lexicon "libro")
		(get-from-lexicon "il"))
	verb-phrase (combine (get-from-lexicon "scrivere")
			     object)
	parent (combine verb-phrase subject)]
    (tablize parent
	     (list subject
		   (tablize verb-phrase
			    (list (get-from-lexicon "scrivere")
				  (tablize object
					   (list
					    (get-from-lexicon "il")
					    (get-from-lexicon "libro")))))))))

(defn io-scrivo-il-libro []
  (let [subject (get-from-lexicon "io")
	object (combine
		(get-from-lexicon "libro")
		(get-from-lexicon "il"))
	verb-phrase (combine (get-from-lexicon "scrivere")
			     object)
	parent (combine verb-phrase subject)]
    (tablize parent
	     (list subject
		   (tablize verb-phrase
			    (list (get-from-lexicon "scrivere")
				  (tablize object
					   (list
					    (get-from-lexicon "il")
					    (get-from-lexicon "libro")))))))))

(defn scrivo-il-libro []
  (let [object (combine
		(get-from-lexicon "libro")
		(get-from-lexicon "il"))
	verb-phrase (combine (get-from-lexicon "scrivere")
			     object)]
    (tablize verb-phrase
	     (list (get-from-lexicon "scrivere")
		   (tablize object
			    (list
			     (get-from-lexicon "il")
			     (get-from-lexicon "libro")))))))

(defn choose-determiner [struct]
  ;; do a query based on the given struct,
  ;; and choose a random element that satisfies the query.
  (let [results (fetch :lexicon :where struct)]
    (nth results (rand-int (count results) 0))))

(defn det-libro []
  (let [determiner
	(choose-determiner
	 (assoc {}
	   :cat :det
	   :def :def))
	object (combine
		(get-from-lexicon "libro")
		determiner)]
    (tablize object
	     (list
	      determiner
	      (get-from-lexicon "libro")))))

(defn il-libro []
  (let [object (combine
		(get-from-lexicon "libro")
		(get-from-lexicon "il"))]
	(tablize object
		 (list
		  (get-from-lexicon "il")
		  (get-from-lexicon "libro")))))

(defn generate-noun-phrase []
  (let [noun
	(nth (fetch :lexicon :where {:cat :noun})
	     (rand-int (count (fetch :lexicon :where {:cat :noun}))))
	article-unif
	(assoc {}
	  :cat :det
	  :def :def)
	article
	(nth (fetch :lexicon :where {:cat :det})
	     (rand-int (count (fetch :lexicon :where {:cat :det}))))
	parent (combine noun article)]
    (tablize parent
	     (list article noun))))

(defn generate-sentence []
  (let [subject
	(nth (fetch :lexicon :where {:cat :pronoun})
	     (rand-int (count (fetch :lexicon :where {:cat :pronoun}))))
	verb
	(nth (fetch :lexicon :where {:cat :verb :infl :infinitive
				     :fn "trans-sv"}) 
	     (rand-int (count (fetch :lexicon :where {:cat :verb :infl :infinitive :fn "trans-sv"}))))
	parent (combine verb subject)]
    (tablize parent
	     (list subject 
		   verb))))

(def tests
  (list
   (det-libro)
   (il-libro)
   (generate-sentence)
;   (generate-noun-phrase)
;   (io-andare)
;   (tu-andare)
;   (scrivo-il-libro)
   (io-scrivo-il-libro)
;   (lui-scrivo-il-libro)

;   (io-pranzare)
;   (show-lexicon-as-feature-structures)

;   (correct)
					;   (answertable))
   ))

  