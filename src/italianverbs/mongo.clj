(ns italianverbs.mongo
    (:use [somnium.congomongo]
	  [hiccup core]))

(defn mongotest []
  (mongo! 
   :db "mydb")
  (insert! :users    
	   {:name "eugene"
	   :lastlogin "whenever"})
  (html [:h2 "mongo test (more).."]))

