(ns italianverbs.test
    (:use 
     [hiccup core page-helpers]
     [italianverbs.grammar]
     [italianverbs.lexicon]
     [somnium.congomongo])
    (:require 
     [clojure.string :as string]
     [italianverbs.quiz :as quiz]))

(mongo! :db "mydb")

;; useful library functions
(defn show-answer [question] (get question :answer))
(defn wrap-div [string]
  (str "<div class='test'>" string "</div>"))
  
(defn lex-thead [lexeme]
  (str "<tr>"
       "<th>italian</th>"
       "<th>english</th>"
       "<th>person</th>"
       "<th>number</th>"
       "<th>cat</th>"
       "<th>infl</th>"
       "<th>writable</th>"
       "<th>fn</th>"
       "</tr>"))

(defn lex-row [lexeme]
  (str "<tr>"
       "<td>" (nth lexeme 0) "</td>"
       "<td>" (get (nth lexeme 1) :english) "</td>"
       "<td>" (get (nth lexeme 1) :person) "</td>"
       "<td>" (get (nth lexeme 1) :number) "</td>"
       "<td>" (get (nth lexeme 1) :cat) "</td>"
       "<td>" (get (nth lexeme 1) :infl) "</td>"
       "<td>" (get (nth lexeme 1) :writable) "</td>"
       "<td>" (get (nth lexeme 1) :fn) "</td>"
       "</tr>"))

(defn answer-row [question]
  (let [correctness (if (= (get question :guess) (get question :answer)) "correct" "incorrect")]
       (str "<tr><td>" 
	    (get question :answer) 
	    "</td><td>" 
	    (get question :guess) 
	    "</td><td class='" correctness "'>"
	    correctness
	    "</td></tr>")))

;; tests
(defn answertable []
  (str "<table>" (string/join " " (map answer-row (fetch :question))) "</table>"))

(defn correct []
  (str "correct guesses: " (count (mapcat quiz/each-correct (fetch :question)))
       " out of : " (count (fetch :question))))

(defn show-lexicon-as-feature-structures []
  (string/join " "
	       (map (fn [lexeme]
		      (fs lexeme))
		    (fetch :lexicon))))

(defn io-andare []
  (let [subject (get-from-lexicon "io")
	verb-phrase (get-from-lexicon "andare")]
    (combine verb-phrase subject)))

(defn tu-andare []
  (let [subject (get-from-lexicon "tu")
	verb-phrase (get-from-lexicon "andare")]
    (combine verb-phrase subject)))

(defn io-pranzare []
  (let [subject (get-from-lexicon "io")
	verb-phrase (get-from-lexicon "pranzare")]
    (combine verb-phrase subject)))

(defn lui-scrivo-il-libro []
  (let [subject (get-from-lexicon "lui")
	object (combine
		(get-from-lexicon "libro")
		(get-from-lexicon "il"))
	verb-phrase (combine (get-from-lexicon "scrivere")
			     object)]
    (combine verb-phrase subject)))

(defn io-scrivo-il-libro []
  (let [subject (get-from-lexicon "io")
	object (combine
		(get-from-lexicon "libro")
		(get-from-lexicon "il"))
	verb-phrase (combine (get-from-lexicon "scrivere")
			     object)]
    (combine verb-phrase subject)))

(defn scrivo-il-libro []
  (let [object (combine
		(get-from-lexicon "libro")
		(get-from-lexicon "il"))
	verb-phrase (combine (get-from-lexicon "scrivere")
			     object)]
    verb-phrase))

(defn choose-lexeme [struct]
  ;; do a query based on the given struct,
  ;; and choose a random element that satisfies the query.
  (let [results (fetch :lexicon :where struct)]
    (nth results (rand-int (count results)))))

(defn generate-np []
  (let [noun
	(merge {:left 1
		:right 2}
	       (choose-lexeme
		(assoc {}
		  :cat :noun)))]
    ;; choose a determiner that agrees with the noun in number and gender.
    (let [determiner
	  (merge {:left 0
		  :right 1}
		 (choose-lexeme
		  (assoc {}
		    :gender (get noun :gender)
		    :number (get noun :number)
		    :cat :det
		    :def :def)))]
      (combine noun determiner))))

(defn generate-vp []
  (let [verb-fs {:cat :verb
		 :infl :infinitive
		 :fn "trans-vo"}
	verb
	(nth (fetch :lexicon :where verb-fs)
	     (rand-int (count (fetch :lexicon :where verb-fs))))
	parent (combine
		(merge
		 {:left 0
		  :right 1}
		 verb)
		(merge
		 {:left 1
		  :right 2}
		 (generate-np)))]
    parent))
 
(defn generate-sentence []
  (let [subject
	(nth (fetch :lexicon :where {:cat :pronoun})
	     (rand-int (count (fetch :lexicon :where {:cat :pronoun}))))]
    (combine (generate-vp) subject)))

(defn reload-button []
  (str "<form action='/test/' method='post'><input type='submit' value='Reload'/>  </form> "))

(def tests
  (list
;   (reload-button) ; reload button does not work yet (results are still cached)
   "<div> <h2>random sentences</h2></div>"
;   (tablize (io-facio-la-donna))
   (tablize (generate-sentence))
   (tablize (generate-sentence))
   (tablize (generate-sentence))
   (tablize (generate-sentence))
   "<div> <h2>fixed sentences</h2></div>"
   (tablize (io-andare))
   (tablize (tu-andare))
   (tablize (io-pranzare))
   (tablize (scrivo-il-libro))
   (tablize (lui-scrivo-il-libro))


   (show-lexicon-as-feature-structures)

   (correct)
   (answertable)
   ))

  