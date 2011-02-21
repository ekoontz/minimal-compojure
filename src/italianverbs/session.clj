(ns italianverbs.session
    (:use 
    [hiccup core page-helpers]
    [somnium.congomongo])
    (:require [italianverbs.lexicon :as lexicon])
    (:import (java.security 
	      NoSuchAlgorithmException
	      MessageDigest)
	     (java.math BigInteger)))

(defn new [username] ;; create a new session for the given user.
  (let [newuser (insert! :users {:name username :lastlogin "foo"})
        newsession (insert! :session {:user username :start "now"})]
       {:name (get newuser :name)}))

(defn last-activity [userid])
;  (update! :user my-robot (merge my-robot { :name "asimo" }))
;  (update! :users {_id: userid},{$set:{:lastlogin "foo42"}}))
