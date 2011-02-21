(ns foo.core
    (:use [hiccup core page-helpers]))

(defn message [msg] 
  (html
   [:div msg]))

(defn sessiondata [data] 
  (html
   [:div {:style "text-align:right;position:absolute;top:1px;right:1px;padding:1px;border:1px solid #caecae;width:30%;float:right"}
    [:h4 {:style "padding:0;margin:0"} "Session"]
    [:pre (pr-str data)]
    [:a {:style "padding-right:1em" :href "/session/clear/"} "Clear"]
    [:a {:style "padding-right:1em" :href "/session/set/"} "New"]]))

(def footer 
     (html
      [:div {:style "width:60%;float:right;margin:1em;padding:0.5em;border:1px solid #ededed"}

        [:div {:style "margin-right:1em;width:auto;float:left"}
	  [:a {:href "/"} "Main"  ] ] 

        [:div {:style "margin-right:1em;width:auto;float:left"}
	  [:a {:href "/test/"} "Test"  ] ] 

        [:div {:style "margin-right:1em;width:auto;float:left"}
	  [:a {:href "/form/"} "Forms"  ] ] 

      ] 
      [:div {:style "float:right;text-align:right;width:100%"}
        "Powered by " [:a {:href "http://github.com/weavejester/compojure"}
                      "Compojure" ] ] ))

(defn page [content]
     (html5
      [:head 
        [:title "Welcome to Foo"]
      ]
      [:body
        [:div
	  [:h1 "Welcome to the Foo app"]]
        content 
	footer ]))

