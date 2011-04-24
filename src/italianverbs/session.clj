(ns italianverbs.session
    (:use 
    [hiccup core page-helpers]
    [somnium.congomongo])
    (:require [italianverbs.lexicon :as lexicon]
              [somnium.congomongo :as congomongo])
    (:import (java.security 
              NoSuchAlgorithmException
              MessageDigest)
             (java.math BigInteger)))

(defn find-or-insert-user [username]
  (let [found (fetch-one :users :where {:name username})]
       (if found found
	 (insert! :users {:name username :lastlogin "never"}))))

;; TODO : figure out date/time in Clojure.
(defn last-activity [username]
  (let [my-user (fetch-one :users :where {:name username})]
       (update! :users my-user (merge my-user {:lastlogin "reallynow"}))))

(defn start-session [username cookie]
  (last-activity username)
  (insert! :session {:user username :start "now"
                     :cookie cookie}))

(defn get-session [username request]
  "get a session for user _username_ based on cookie in request."
  (let [fetch (fetch-one :session)]
    (if fetch
      (get fetch :user))))

(defn new [username request] ;; create a new session for the given user.
  (let [newuser (find-or-insert-user username)
        newsession (start-session username (get (get request :cookies) "ring-session"))]
       {:name (get newuser :name)}))

(defn clear [request]
  "remove session with cookie in request; return nil."
  (let [cookie (if (get request :cookies)
                 (if (get (get request :cookies) "ring-session")
                   (if (get (get (get request :cookies) "ring-session") :value)
                     (get (get (get request :cookies) "ring-session") :value))))]
    (if cookie
      (destroy! :session {:cookie {:value cookie}})
    nil)))


