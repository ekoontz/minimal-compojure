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


(defn show-lexicon-as-table []
  (str "<table>" 
       (string/join " " (map lex-thead (list (first lexicon-i2e))))
       (string/join " " (map lex-row lexicon-i2e))
       "</table>"))

(defn show-lexicon-as-feature-structures []
  (string/join " " (map (fn [x] (fs (synsem x))) lexicon-i2e)))

(defn io-pranzare []
  (let [subject (get lexicon-i2e "io")
	verb-phrase (get lexicon-i2e "pranzare")
	parent (combine verb-phrase subject)]
    (tablize parent
	     (list subject
		   verb-phrase))))

(defn io-scrivo-il-libro []
  (let [subject (get lexicon-i2e "io")
	verb-phrase (combine (get lexicon-i2e "scrivere")
			     (get lexicon-i2e "il libro"))
	parent (combine verb-phrase subject)]
    (tablize parent
	     (list subject
		   (tablize verb-phrase
			    (list (get lexicon-i2e "scrivere")
				  (get lexicon-i2e "il libro")))))))

(defn lui-scrivo-il-libro []
  (let [subject (get lexicon-i2e "lui")
	verb-phrase (combine (get lexicon-i2e "scrivere")
			     (get lexicon-i2e "il libro"))
	parent (combine verb-phrase subject)]
    (tablize parent
	     (list subject
		   (tablize verb-phrase
			    (list (get lexicon-i2e "scrivere")
				  (get lexicon-i2e "il libro")))))))
							     
(def tests
  (list
   (io-pranzare)
   (lui-scrivo-il-libro)
   (io-scrivo-il-libro)
   (show-lexicon-as-feature-structures)
   (show-lexicon-as-table)
   (correct)
   (answertable)))

  