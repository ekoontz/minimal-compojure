(ns foo.core
  (:use compojure.core)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]))

(load-file "src/foo/html.clj")
(load-file "src/foo/lib.clj")

(defroutes main-routes

;   "A handler processes the request map and returns a response map."
; http://groups.google.com/group/compojure/browse_thread/thread/3c507da23540da6e
  (GET "/" 
       ;; request map: access it with (get request X),
       ;; where X in {:session,:request-method,:uri,...}
       request

       ;; response map
       { :session (get request :session)
         :body (page (str "Welcome to Foo."
			  (sessiondata (get request :session))
			  (reqdata (get request :request-method)
				   (get request :uri))))
       }
       )

  (GET "/test/" 
       request
       { :session (get request :session)
         :body (page (str "Tests go here."
			  (sessiondata (get request :session)) 
			  (reqdata (get request :request-method) 
				   (get request :uri))))
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
  (route/not-found (page "Sorry, page not found.")))

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

