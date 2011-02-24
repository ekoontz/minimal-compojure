(ns italianverbs.test
  (:use [compojure.core]
	[hiccup core page-helpers]
	[somnium.congomongo]
	[clojure.string :as string]
	[foo.html]))

(mongo! :db "mydb")

(defn show-answer [question] (get question :answer))

(defn test2 []
  (join " " (map show-answer (fetch :question))))


