(ns italianverbs.quiz
    (:use 
     [hiccup core page-helpers]
     [somnium.congomongo])
    (:require [italianverbs.lexicon :as lexicon]
              [italianverbs.grammar :as gram]))

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
	                  :answer (get (get lexicon (nth (keys lexicon) index)) :english)}))

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

(defn evaluate-guess [ guess ]
  ;; get last question.
  (let [question 
    (if (not (= (fetch :question :sort {:_id -1}) '()))
	(nth (fetch :question :sort {:_id -1}) 0))]
	(if question
	    (update! :question question (merge question {:guess guess})))))

(defn get-next-question-id [user]
  "get the question id for the next question for this user."
  "43")

(defn quiz [last-guess request]
  (let [next-question (gram/generate)]
    (html
     [:div.quiz
      [:h2 "Question"]
      [:form {:method "post" :action "/quiz/"}
       [:table
        [:tr
         [:td [:h1 (get next-question :english)]]]
        [:tr
         [:td
          [:input {:name "guess" :size "50"}]]]]
       [:div
        [:input {:type "hidden" :name "question_id" :value (get-next-question-id {:foo "bar"})}]
        [:input.submit {:type "submit" :value "riposta"}]]]]

     [:div.history
      [:h2 "History"]
      (evaluate-guess last-guess)])))


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

(defn run [ query-string request]
  (let [query-string query-string
        params (get-param-map query-string)]
    (html  
     ;; get 'guess' from query-string (e.g. from "guess=to%20eat")
     ;; pass the users's guess to (quiz), which will evaluate it.
     [:div (quiz (get params "guess") request)])))


