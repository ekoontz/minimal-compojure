(ns italianverbs.session
    (:use 
    [hiccup core page-helpers]
    [somnium.congomongo])
    (:require [italianverbs.lexicon :as lexicon])
    (:import (java.security 
	      NoSuchAlgorithmException
	      MessageDigest)
	     (java.math BigInteger)))

(defn find-or-insert-user [username]
  (let [found (fetch-one :users :where {:name username})]
       (if found found
	 (insert! :users {:name username :lastlogin "never"}))))

(defn start-session [username]
  (last-activity username)
  (insert! :session {:user username :start "now"}))


(defn new [username] ;; create a new session for the given user.
  (let [newuser (find-or-insert-user username)
        newsession (start-session username)]
       {:name (get newuser :name)}))

(defn last-activity [username]
  (let [my-user (fetch-one :users :where {:name username})]
       (update! :users my-user (merge my-user {:lastlogin "reallynow"}))))
