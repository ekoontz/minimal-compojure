(ns foo.core
  (:use compojure.core)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]))

(def banner (str "<html><title>Welcome to Foo</title><body><div><h1>Welcome to the Foo app</h1></div>"))

(defn message [msg] (str "<div style='width:40%;background:#ededed;border:2px solid #caecae'>" msg "</div>"))
(defn sessiondata [data] (str "<div style='float:right;border:1px dashed #caecae'><pre>" (pr-str data) "</pre></div>"))

(def footer "<div style='margin:1em;padding:0.5em;border:1px solid #ededed'>
<p><a href='/'>Main</a></p>
<p><a href='/test/'>Tests</a></p>
<p><a href='/form/'>Form Processing</a></p>
<p><a href='/session/set/'>Session set</a></p>
<p><a href='/session/clear/'>Session clear</a></p>
<div style='float:right'>Powered by Compojure</div></body></html>")

(defroutes main-routes
  (GET "/" 
       {session :session}
       {
       :session session
       :body (str banner (message "Welcome.") (sessiondata session) footer)
       })

  (GET "/test/" [] (str banner "<h2>Tests? Yeah, tests are good.</h2>" footer))

  (GET "/session/"
       {session :session}
       {
       :body (str banner (sessiondata session) footer)
       })

  (GET "/session/set/"  
       {session :session}
       {
       :session {:val 123 :fruit "grape"}
       :body (str banner (message "Session set.") (sessiondata session) footer)
       })

  (GET "/session/clear/"  
       {session :session}
       {
       :session {}
       :body (str banner (message "Session cleared.") footer)
       })

  (route/resources "/")
  (route/not-found (str banner "<h2>Sorry, page not found.</h2>" footer)))

; http://weavejester.github.com/compojure/compojure.handler-api.html
; site function

;Usage: (site routes & [opts])

;Create a handler suitable for a standard website. This adds the
;following middleware to your routes:
;  - wrap-session
;  - wrap-cookies
;  - wrap-multipart-params
;  - wrap-params
;  - wrap-nested-params
;  - wrap-keyword-params
; A map of options may also be provided. These keys are provided:
;  :session - a map of session middleware options

(def app
  (handler/site main-routes))

