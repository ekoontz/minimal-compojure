(ns italianverbs.quiz
    (:use 
     [hiccup core page-helpers]
     [somnium.congomongo])
    (:require [italianverbs.lexicon :as lexicon]))

(defn wrapchoice [word & [ istrue ] ]
  ;; FIXME: url-encode word.
  (let [href_prefix "/quiz/?"
;        english word]
        english (get word :english)]
       (html [:div {:class "guess"}
	       [:h3 [:a {:href (str href_prefix "guess=" english)} english]
  	       (if istrue [:i.debug true])
	       ]])))

(defn show-choice [lexicon remaining answer show-true-before]
  (if (>= remaining 0)
      (str
       (if (= show-true-before remaining)
	   (wrapchoice answer true))
       (if (> remaining 0)
	   (let [choice (rand-int (count lexicon))]
		(str 
		 (html [:div.debug (str "remaining: " remaining)])
		 (wrapchoice (get lexicon (nth (keys lexicon) choice)))
		 (show-choice (dissoc lexicon (nth (keys lexicon) choice)) (- remaining 1)
			answer show-true-before)))))))

(defn store-question [index lexicon]
      (insert! :question {:q-index index 
                          :question (nth (keys lexicon) index) 
	                  :answer (get (get lexicon (nth (keys lexicon) index)) :english)
                         }))
(defn each-correct [question]
  (if (= (get question :guess) (get question :answer)) '(true) nil))

(defn show-history-rows [qs count last]
   (if (first qs)
       (let
	  [row (first qs)
	   correctness (if (and (get row :guess) (not (= (get row :guess) "")))
			   (if (= (get row :answer) (get row :guess))
			       "correct"
			     "incorrect"))]
	  (html
	   [:tr 
	     [:td count][:th (get row :question)  ] 
	     [:td (if (not (= count last)) (get row :answer))]  
	     [:td {:class correctness} (get row :guess)]
           ]
	   (show-history-rows (rest qs) (+ 1 count) last)))))

(defn show-history []
  (let 
      [total (fetch-count :question)
       skip (if (> total 10) (- total 10) 0)
       qs (fetch :question :sort {:_id 1} :limit 10 :skip skip )]
      (html
       [:div#stats
         [:table
           [:tr [:th "Correct:"]
	        [:td (count (mapcat each-correct (fetch :question))) "/" total ]]
         ]
	 [:div.controls [:a {:href "/quiz/clear/"} "Clear"]]
       ]      
       [:table
        [:tr
	  [:td]
	  [:th "question"]
          [:th "answer" ]
	  [:th "guess" ]
	]
       (show-history-rows qs (+ 1 skip) total)
       ]
       )))

(defn next-question [index lexicon]
  (let 
      [italian-verb (nth (keys lexicon) index)
       english-verb (get lexicon italian-verb)
       number-of-guesses 4
       true-before (rand-int (+ 1 number-of-guesses))
       store-question (store-question index lexicon)
       ]
       
      (html 
       [:div.debug (str "index:" index "(" italian-verb ")" ) ] 
       [:div.debug (str "english:" english-verb)]
       [:div.debug (str "lexicon size:" (count lexicon))]
       [:div.debug (str "# guesses:" number-of-guesses)]
       [:div.debug (str "true@ :" true-before)]
       [:div {:class "question major"}
       [:h2 [:i italian-verb]]
         (show-choice (dissoc lexicon italian-verb) 
		number-of-guesses english-verb true-before) 
       ]

       [:div.history
         (show-history)
       ]

 )

 ) 
)

(defn evaluate-guess [ guess ]
  ;; get last question.
  (let [question 
    (if (not (= (fetch :question :sort {:_id -1}) '()))
	(nth (fetch :question :sort {:_id -1}) 0))]
	(if question
	    (update! :question question (merge question {:guess guess})))))

(defn quiz [ last-guess]
  (evaluate-guess last-guess)      
  (next-question (rand-int (count (fetch :lexicon)) (fetch :lexicon))))

(defn url-decode [string]
  (.replaceAll string "(%20)" " "))

(defn get-params [pairs]
  (if (first pairs)
      (let [keyval (re-seq #"[^=]+" (first pairs))]
	   (merge 
	    {(first keyval) (url-decode (second keyval))}
	    (get-params (rest pairs))))
    {}))

(defn get-param-map [query-string]
  (if query-string
      (get-params (re-seq #"[^&]+" query-string))))

(defn run [ query-string ]
  (let [params (get-param-map query-string)]
       (html  
	;; get 'guess' from query-string (e.g. from "guess=to%20eat")
	;; pass the users's guess to (quiz), which will evaluate it.
	[:div (quiz (get params "guess"))])))


