(ns italianverbs.test
    (:use 
     [hiccup core page-helpers]
     [italianverbs.lexicon]
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
       (string/join " " (map lex-thead (list (first lexicon-i2e))))
       (string/join " " (map lex-row lexicon-i2e))
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
    (if (= key :fn)
      (str "<tr> <th> " key "</th>  <td>" "(fn)" "</td></tr>")
      (str "<tr> <th> " key "</th>  <td>" val "</td></tr>"))))

;(defn fs-table [lex-struct]

(defn fs [lexeme]
  (str "<table class='fs'>"
       (string/join " " (seq (map fs-tr (getkeyvals (keys lexeme)
						    lexeme))))
       "</table>"))

(defn lexicon-fs []
  (string/join " " (map (fn [x] (fs (synsem x))) lexicon-i2e)))

(defn combine-sv [subject verb]
  (if (get verb :fn)
    (apply (get verb :fn) (list verb subject))
    {:cat :error :note
     (str "null pointer: no function for this verb :" verb  )}))

(defn combine-vo [verb object]
  (if (get verb :fn)
    (apply (get verb :fn) (list verb object))
    {:cat :error :note
     (str "null pointer: no function for this verb :" verb  )}))

(defn io-scrivo-il-libro []
  (let [subject (get lexicon-i2e "io")
	verb (get lexicon-i2e "scrivere il libro")
	result (combine-sv subject verb)]
    (str
     "<div class='syntax'><table class='syntax'>"
     "<tr><td style='padding-left:25%' colspan='2'>" (fs result) "</td></tr>"
     "<tr><td>" (fs subject) "</td><td>" (fs verb) "</td></tr>"
     "</table></div>")))

(defn scrivo-il-libro []
  (let [verb (get lexicon-i2e "scrivere")
	object (get lexicon-i2e "il libro")
	result (combine-vo verb object)]
;	result object]
    (str
     "<div class='syntax'><table class='syntax'>"
     "<tr><td style='padding-left:25%' colspan='2'>" (fs result) "</td></tr>"
     "<tr><td>" (fs verb) "</td><td>" (fs object) "</td></tr>"
     "</table></div>")))

(def tests
  (list
;   (io-scrivo-il-libro)
   (scrivo-il-libro)
   (lexicon-fs)
   (lexicon)
   (correct)
   (answertable)))

  