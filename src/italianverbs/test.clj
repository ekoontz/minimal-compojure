(ns italianverbs.test
  (:use [compojure.core]
	[hiccup core page-helpers]
	[somnium.congomongo])
  (:require [clojure.string :as string]))
 
(mongo! :db "mydb")

(defn show-answer [question] (get question :answer))

(defn lex-row [question]
  (str "<tr><td>" 
       (get question :answer) 
       "</td><td>" 
       (get question :guess) 
       "</td><td>" 
       (if (= (get question :guess) (get question :answer)) "good!")
       "</td></tr>"))

(defn each-correct [question]
  (if (= (get question :guess) (get question :answer)) '(true) nil))

(defn test2 []
  (string/join " " (map show-answer (fetch :question))))

(defn test3 []
  (str "<table>" (string/join " " (map lex-row (fetch :question))) "</table>"))

(defn correct []
  (str (count (mapcat each-correct (fetch :question)))))

(defn wrap-div [string]
  (str "<div class='test'>" string "</div>"))


 