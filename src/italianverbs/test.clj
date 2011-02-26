(ns italianverbs.test
    (:use 
     [hiccup core page-helpers]
     [somnium.congomongo])
    (:require 
     [clojure.string :as string]
     [italianverbs.lexicon :as lexicon]
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


(defn lexicon []
  (str "<table>" 
       (string/join " " (map lex-thead (list (first lexicon/lexicon-i2e))))
       (string/join " " (map lex-row lexicon/lexicon-i2e))
       "</table>"))

(defn getkeyvals [keys lexeme-struct]
  (if (> (count keys) 0)
    (cons
     (list (first keys)
	   (get lexeme-struct (first keys)))
     (getkeyvals (rest keys) lexeme-struct))))

(defn fs-tr [key-val-pair]
  (let [key (first key-val-pair)
	val (second key-val-pair)]
    (str "<tr> <th> " key "</th>  <td>" val "</td></tr>")))

;(defn fs-table [lex-struct]

(defn fs [lexeme]
  (str "<table class='fs'>"
       "<tr> <th>italian</th>  <td>"
       (lexicon/italian lexeme)
       "</td></tr>"
       (string/join " " (seq (map fs-tr (getkeyvals (keys (lexicon/synsem lexeme))
						    (lexicon/synsem lexeme)))))
       "</table>"))

(defn lexicon-fs []
  (string/join " " (map fs lexicon/lexicon-i2e)))

(defn combine [subject verb]
  (string/join (list "io" " " "scrivere")))

(defn io-scrivo []
  (let [result (combine (get lexicon/lexicon-i2e "io")
			(get lexicon/lexicon-i2e "scrivere"))]
    (string/join " "
		 (list (fs (list "io" (get lexicon/lexicon-i2e "io")))
		       (fs (list "scrivere" (get lexicon/lexicon-i2e "scrivere")))
		       result))))