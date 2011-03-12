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

;; useful library functions: will move elsewhere after testing.
(defn show-answer [question] (get question :answer))
(defn wrap-div [string]
  (str "<div class='test'>" string "</div>"))
(defn pos [node left right]
  (positionalize node left right))
(defn positionalize [node left right]
  (merge {:left left :right right}
	 node))

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
  (let [subject (pos (get-from-lexicon "io") 0 1)
	verb-phrase (pos (get-from-lexicon "andare") 1 2)]
    (combine verb-phrase subject)))

(defn tu-andare []
  (let [subject (pos (get-from-lexicon "tu") 0 1)
	verb-phrase (pos (get-from-lexicon "andare") 1 2)]
    (combine verb-phrase subject)))

(defn io-pranzare []
  (let [subject (pos (get-from-lexicon "io") 0 1)
	verb-phrase (pos (get-from-lexicon "pranzare") 1 2)]
    (combine verb-phrase subject)))

(defn lui-scrivo-il-libro []
  (let [subject (pos (get-from-lexicon "lui") 0 1)
	object (combine
		(pos (get-from-lexicon "libro") 3 4)
		(pos (get-from-lexicon "il") 2 3))
	verb-phrase (combine
		     (pos (get-from-lexicon "scrivere") 1 2)
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

(defn generate-np [offset]
  (let [noun
	(positionalize 
	 (choose-lexeme
	  (assoc {} :cat :noun))
	  (+ offset 1)
	  (+ offset 2))]
    ;; choose a determiner that agrees with the noun in number and gender.
    (let [determiner
	  (positionalize
	   (choose-lexeme
	    (assoc {}
	      :gender (get noun :gender)
	      :number (get noun :number)
	      :cat :det
	      :def :def))
	   offset
	   (+ offset 1))]
      (combine noun determiner))))

(defn generate-vp [offset]
  (let [verb-fs {:cat :verb
		 :infl :infinitive
		 :fn "trans-vo"}
	verb
	(positionalize
	 (nth (fetch :lexicon :where verb-fs)
	      (rand-int (count (fetch :lexicon :where verb-fs))))
	 offset
	 (+ offset 1))
	parent (combine
		 verb
		 (generate-np 2))]
    parent))
 
(defn generate-sentence []
  (let [subject
	(positionalize
	 (nth (fetch :lexicon :where {:cat :pronoun})
	      (rand-int (count (fetch :lexicon :where {:cat :pronoun}))))
	 0 1)] ;; fixme: left is always 0, but right (1)
    ;; varies depending on length of subject.
    (combine (generate-vp 1) subject)))

(defn reload-button []
  (str "<form action='/test/' method='post'><input type='submit' value='Reload'/>  </form> "))

;; current thing I'm debugging..
(defn bugs []
   "<div> <h2>bugs</h2></div>"
   (let [subject (positionalize (get-from-lexicon "lui") 0 1)
	 object (combine
		 (positionalize (get-from-lexicon "donna") 3 4)
		 (positionalize (get-from-lexicon "la") 2 3))
	 verb-phrase (combine
		      (positionalize (get-from-lexicon "correggere") 1 2)
		      object)]
;     (tablize verb-phrase)))
     (tablize (combine verb-phrase subject))))

(def tests
  (list
;   (reload-button) ; reload button does not work yet (results are still cached)

  ; (bugs)
   "<div> <h2>random sentences</h2></div>"
   (tablize (generate-sentence))
   (tablize (generate-sentence))
   (tablize (generate-sentence))
   (tablize (generate-sentence))
   "<div> <h2>fixed sentences</h2></div>"
   (tablize (io-andare))
   (tablize (tu-andare))
   (tablize (io-pranzare))
;   (tablize (scrivo-il-libro))
   (tablize (lui-scrivo-il-libro))


   (show-lexicon-as-feature-structures)

   (correct)
   (answertable)
   ))

  