(ns italianverbs.core
  (:use [compojure.core]
        [somnium.congomongo]
        [base.html])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [base.lib :as baselib]
            [clojure.string :as string]
            [italianverbs.html :as ihtml]
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
        :body (let [username (get (get request :cookie) :name)]
                (page "Welcome"
                      (str "Welcome to Italian Verbs" (if username (str ", " username)) ".")
                      request))
        }
       )

  (GET "/lexicon/" 
       request
       ;; response map
       {:session (get request :session)
        :body
        (do ;"reload lexicon into mongodb and then render it as HTML."
          (load-file "src/italianverbs/lexicon.clj")
          (page "Lexicon"
                (string/join " "
                             (map (fn [lexeme]
                                    (ihtml/fs lexeme))
                                  (fetch :lexicon :sort {"italian" 1})))))
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

  (POST "/quiz/filter" ;; for now just run quiz.
       request
       ;; response map
       {:session (get request :session)
        :body (page "Quiz"
                    (quiz/filter request)
                    request)
        }
       )

  
  (POST "/quiz/clear" 
       request
       ;; response map
       { :session (quiz/clear-questions (session/request-to-session request))
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
       {:session (get request :cookie)
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
  (route/not-found (page "Non posso trovare. Page not found." "Non passo trovare. Sorry, page not found.")))

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

