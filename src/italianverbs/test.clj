(ns italianverbs.test
  (:use [compojure.core]
	[hiccup core page-helpers]
	[somnium.congomongo])
  (:require [clojure.string :as string]))
 
(mongo! :db "mydb")

(defn show-answer [question] (get question :answer))

(defn lex-row [question]
  (let [correctness (if (= (get question :guess) (get question :answer)) "correct" "incorrect")]
       (str "<tr><td>" 
	    (get question :answer) 
	    "</td><td>" 
	    (get question :guess) 
	    "</td><td class='" correctness "'>"
	    correctness
	    "</td></tr>")))
  
(defn answertable []
  (str "<table>" (string/join " " (map lex-row (fetch :question))) "</table>"))

(defn correct []
  (str "correct guesses: " (count (mapcat each-correct (fetch :question)))
       " out of : " (count (fetch :question))))

(defn wrap-div [string]
  (str "<div class='test'>" string "</div>"))


 