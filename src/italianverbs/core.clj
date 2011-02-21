(ns italianverbs.core
  (:use [compojure.core]
	[italianverbs.mongo]
	[foo.html])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
	    [foo.lib :as foolib]
	    [foo.html :as foohtml]
	    [italianverbs.lexicon :as lexicon]))

(defroutes main-routes

;   "A handler processes the request map and returns a response map."
; http://groups.google.com/group/compojure/browse_thread/thread/3c507da23540da6e
  (GET "/" 
       ;; request map: access it with (get request X),
       ;; where X in {:session,:request-method,:uri,...}
       request

       ;; response map
       { :session (get request :session)
         :body (foohtml/page "Welcome"
		     "Welcome to Italian Verbs."
		     request)
       }
       )

  (GET "/lexicon/" 
       request
       ;; response map
       { :session (get request :session)
         :body (foohtml/page "Lexicon"
		     (lexicon/show)
		     request)
       }
       )

  (GET "/test/" 
       request
       { :session (get request :session)
         :body (page "mongotest" (mongotest) request)
	 })

  (GET "/session/set/"  
       request
       {
       :session {:val 123 :fruit "grape"}
       :status 302
       :headers {"Location" "/?msg=set"}
       })

  (GET "/session/clear/" 
       {} 
       {
       :session {}
       :status 302
       :headers {"Location" "/?msg=cleared"}
       })

  (route/resources "/")
  (route/not-found (foohtml/page "not found." "Sorry, page not found.")))

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

