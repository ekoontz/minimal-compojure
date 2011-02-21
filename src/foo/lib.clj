(ns foo.core
    (:use [hiccup core page-helpers]))

(defn reqdata [request-method & [ uri query-string]] 
  (html
   [:div {:style "padding:3px;width:40%;background:#ededed;border:2px solid #caecae;margin-top:0.5em"}
     [:table
       [:tr
         [:th "Method" ]
	 [:td request-method ]
       ]
       [:tr
         [:th "Uri" ]
	 [:td uri ]
       ]
       [:tr
         [:th "Query String" ]
	 [:td query-string ]
       ]

     ] 
   ]))

