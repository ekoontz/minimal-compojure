(ns base.lib
    (:use [hiccup core page-helpers]))

(defn reqdata [request-method & [ uri query-string]] 
  (html
   [:div.reqdata
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

