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

(defn new [username] ;; create a new session for the given user.
  (let [newuser (find-or-insert-user username)
        newsession (insert! :session {:user username :start "now"})]
       {:name (get newuser :name)}))

(defn last-activity [userid])
;  (update! :user my-robot (merge my-robot { :name "asimo" }))
;  (update! :users {_id: userid},{$set:{:lastlogin "foo42"}}))
