(ns italianverbs.core
  (:use [compojure.core]
        [somnium.congomongo]
        [base.html])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [base.lib :as baselib]
            [italianverbs.quiz :as quiz]
            [italianverbs.test :as test]
            [italianverbs.session :as session]
            [italianverbs.lexiconfn :as lexfn]))

(defroutes main-routes

;   "A handler processes the request map and returns a response map."
; http://groups.google.com/group/compojure/browse_thread/thread/3c507da23540da6e
  (GET "/" 
       ;; request map: access it with (get request X),
       ;; where X in {:session,:request-method,:uri,...}
       request

       ;; response map
       {:session (get request :session)
        :body (let [username (get (get request :session) :name)]
                (page "Welcome"
                      (str "Welcome to Italian Verbs" (if username (str ", " username)) ".")
                      request))
        }
       )

  (GET "/lexicon/" 
       request
       ;; response map
       { :session (get request :session)
         :body (page "Lexicon"
                     (lexfn/show-lexicon-as-feature-structures)
		     request)
       }
       )

  (GET "/quiz/" 
       request
       ;; response map
       { :session (get request :session)
         :body (page "Quiz"
                     (quiz/run request)
                     request)
       }
       )

  (POST "/quiz/" 
       request
       ;; response map
       {:session (get request :session)
        :body (page "Quiz"
                    (quiz/run request)
                    request)
        }
       )

  (GET "/quiz/clear/" 
       request
       ;; response map
       { :session (session/clear-questions (get request :session))
         :status 302
         :headers {"Location" "/quiz/"}
       }
       )

  (GET "/test/" 
       request
       {:session (session/get-session "Eugene" request)
        :body (page "test" 
                    (map test/wrap-div 
                         (flatten test/tests))
                    request)
        })

  (POST "/test/" 
       request
       {:session (get request :session)
        :body (page "test" 
                    (map test/wrap-div 
                         test/tests)
                    request)
        })
  
;; TODO: make this a POST with 'username' and 'password' params.
  (GET "/session/set/"  
       request
       {
       :session (session/new "Eugene" request)
       :status 302
       :headers {"Location" "/?msg=set"}
       })

  (GET "/session/clear/" 
       request 
       {
       :session (session/clear request)
       :status 302
       :headers {"Location" "/?msg=cleared"}
       })

  (route/resources "/")
  (route/not-found (page "Page not found." "Sorry, page not found.")))

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

