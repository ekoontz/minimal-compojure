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

(defn test2 []
  (string/join " " (map show-answer (fetch :question))))

(defn test3 []
  (str "<table>" (string/join " " (map lex-row (fetch :question))) "</table>"))


 