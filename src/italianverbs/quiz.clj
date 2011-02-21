(ns italianverbs.quiz
    (:use [hiccup core page-helpers])
    (:require [italianverbs.lexicon :as lexicon]))

(defn wrapchoice [word & [ istrue ] ]
  ;; FIXME: url-encode word.
  (let [href_prefix "/quiz?"]
       (html [:div.guess 
	       [:h3 [:a {:href (str href_prefix "guess=" word)} word]
  	       (if istrue [:i true])
	       ]])))


(defn guess [lexicon remaining answer show-true-before]
  (if (>= remaining 0)
      (str
       (if (= show-true-before remaining)
	   (wrapchoice answer true))
       (if (> remaining 0)
	   (let [choice (rand-int (count lexicon))]
		(str 
		 (html [:div.debug (str "remaining: " remaining)])
		 (wrapchoice (get lexicon (nth (keys lexicon) choice)))
		 (guess (dissoc lexicon (nth (keys lexicon) choice)) (- remaining 1)
			answer show-true-before)))))))

(defn next-question [index lexicon]
  (let 
      [italian-verb (nth (keys lexicon) index)
       english-verb (get lexicon italian-verb)
       number-of-guesses 4
       true-before (rand-int (+ 1 number-of-guesses))]
      (html 
       [:div.debug (str "index:" index "(" italian-verb ")" ) ] 
       [:div.debug (str "english:" english-verb)]
       [:div.debug (str "lexicon size:" (count lexicon))]
       [:div.debug (str "# guesses:" number-of-guesses)]
       [:div.debug (str "true@ :" true-before)]
       [:div.question
       [:h2 [:i italian-verb]]
         (guess (dissoc lexicon italian-verb) 
		number-of-guesses english-verb true-before)])))

(defn quiz []
  (next-question (rand-int (count lexicon/lexicon)) lexicon/lexicon))

(defn run []
  (html  
   [:div (quiz)]))


