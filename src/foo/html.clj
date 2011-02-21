(ns foo.html
    (:use [hiccup core page-helpers])
    (:require [foo.lib :as foolib]))

(defn message [msg] 
  (html
   [:div msg]))

(defn sessiondata [data] 
  (html
   [:div.sessiondata
    [:h4 "Session"]
    [:pre (pr-str data)]
    [:a {:href "/session/clear/"} "Clear"]
    [:a {:href "/session/set/"} "New"]]))

(def footer 
     (html
      [:div.footer

        [:div 
	  [:a {:href "/"} "Main"  ] ] 

        [:div 
	  [:a {:href "/quiz/"} "Quiz"]] 

        [:div 
	  [:a {:href "/lexicon/"} "Lexicon"  ] ] 

        [:div 
	  [:a {:href "/test/"} "Test"  ] ] 

        [:div 
	  [:a {:href "/form/"} "Forms"  ] ] 

      ] 
      [:div.poweredby
        "Powered by " [:a {:href "http://github.com/weavejester/compojure"}
                      "Compojure" ] ] ))

(defn page [title & [content request]]
  (html5
   [:head 
   [:title "Verbi italiani &#0187; " title]
   (include-css "/css/style.css")]
   [:body
   [:div
   [:h1 "Verbi italiani &#0187; " title]]
   [:div#content content]
   (if request
       [:div.http
         (sessiondata (get request :session))
	 (foolib/reqdata (get request :request-method) 
			 (get request :uri)
			 (get request :query-string))])
   footer ]))


