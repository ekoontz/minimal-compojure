(ns italianverbs.test
    (:use 
     [hiccup core page-helpers]
     [italianverbs.grammar]
     [somnium.congomongo])
    (:require
     [italianverbs.lexiconfn :as lexfn]
     [clojure.string :as string]
     [italianverbs.quiz :as quiz]))

(mongo! :db "mydb")

;; useful library functions: will move elsewhere after testing.
(defn show-answer [question] (get question :answer))
(defn wrap-div [string]
  (str "<div class='test'>" string "</div>"))

;; return a feature structure just like node, but with :left and :right set.
(defn pos [sign left right]
  (if sign
    (merge sign {:left left :right right})
    {:left left :right right :cat :error :note "null sign given to (pos)"}))

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
		    (fetch :lexicon :sort {"italian" 1}))))

;; fixme: change name to "compose-sv-sentence" or something.
(defn conjugate [pronoun infinitive]
  (let [subject (pos pronoun 0 1)
	verb-phrase (pos infinitive 1 2)]
    (combine verb-phrase subject)))

(defn io-pranzo []
  (let [subject (pos (lexfn/get "io") 0 1)
	verb-phrase (pos (lexfn/get "pranzare") 1 2)]
    (combine verb-phrase subject)))

(defn lui-scrivo-il-libro []
  (let [subject (pos (lexfn/get "lui") 0 1)
	object (combine
		(pos (lexfn/get "libro") 3 4)
		(pos (lexfn/get "il") 2 3))
	verb-phrase (combine
		     (pos (lexfn/get "scrivere") 1 2)
		     object)]
    (combine verb-phrase subject)))

(def in-italia
  (let [prep (lexfn/get "in")
	noun (lexfn/get "Italia")]
    (combine prep noun)))

(def andare-in-italia
  (let [verb (pos (lexfn/get "andare") 0 1)]
    (combine verb
	     (pos in-italia 1 3))))

(defn lui-vado-in-italia []
  (combine
   (combine
    (pos (lexfn/get "andare") 1 2)
    (pos in-italia 2 4))
   (pos (lexfn/get "lui") 0 1)))

(defn io-mangio-il-pane []
  (let [subject (pos (lexfn/get "io") 0 1)
	object (combine
		(pos (lexfn/get "pane") 3 4)
		(pos (lexfn/get "il") 2 3))
	verb-phrase (combine (pos (lexfn/get "mangiare") 1 2)
			     object)]
    (combine verb-phrase subject)))

(defn lui-mangio-la-pasta []
  (let [subject (pos (lexfn/get "lui") 0 1)
	object (combine
		(pos (lexfn/get "pasta") 3 4)
		(pos (lexfn/get "la") 2 3))
	verb-phrase (combine (pos (lexfn/get "mangiare") 1 2)
			     object)]
    (combine 
     (combine verb-phrase subject)
     (pos in-italia 4 6))))

(defn io-scrivo-il-libro []
  (let [subject (lexfn/get "io")
	object (combine
		(lexfn/get "libro")
		(lexfn/get "il"))
	verb-phrase (combine (lexfn/get "scrivere")
			     object)]
    (combine verb-phrase subject)))

(defn choose-lexeme [struct]
  ;; do a query based on the given struct,
  ;; and choose a random element that satisfies the query.
  (let [results (fetch :lexicon :where struct)]
    (if (= (count results) 0)
      {:cat :error :note (str "choose lexeme: no results found for " struct)}
      (nth results (rand-int (count results))))))

(defn generate-np [offset]
  (let [noun
	(pos 
	 (choose-lexeme
	  {:cat :noun})
	 (+ offset 1)
	 (+ offset 2))]
    ;; choose a determiner that agrees with the noun in number and gender.
    (let [determiner
	  (pos
	   (choose-lexeme
	    {:gender (get noun :gender)
	     :number (get noun :number)
	     :cat :det
	     :def :def
	     })
	   offset
	   (+ offset 1))]
;      noun)))
      (combine noun determiner))))

(defn generate-vp [offset]
  (let [verb-fs {:cat :verb
		 :infl :infinitive
		 :fn "verb-vo"}
	verb
	(nth (fetch :lexicon :where verb-fs)
	     (rand-int (count (fetch :lexicon :where verb-fs))))]
    (let [verb-with-pos
	  (pos verb
	       offset
	       (+ 1 offset))
	  parent (combine
		  verb-with-pos
		  (generate-np (+ 1 offset)))]
    parent)))
      
(defn generate-sentence []
  (let [subject
	(pos
	 (nth (fetch :lexicon :where {:cat :pronoun})
	      (rand-int (count (fetch :lexicon :where {:cat :pronoun}))))
	 0 1)] ;; fixme: left is always 0, but right (1)
    ;; varies depending on length of subject.
    (combine (generate-vp 1) subject)))

(defn reload-button []
  (str "<form action='/test/' method='post'><input type='submit' value='Reload'/>  </form> "))

(defn linearize [signs & [offset]]
  (let [offset (if offset offset 0)]
    (if (> (count signs) 0)
      (cons
       (pos (first signs) offset (+ 1 offset))
       (linearize (rest signs) (+ 1 offset))))))

;; current thing I'm debugging..
(defn bugs []
  (let [sentence
	(list (lexfn/get "io")
	      (lexfn/get "dimenticare")
	      (lexfn/get "Italia"))
	linearized
	(linearize sentence)]
    (str
     "<div> <h2>bugs</h2></div>"
     (tablize (combine
	       (nth linearized 0)
	       (combine
		(nth linearized 1)
		(nth linearized 2)))))))
      
(defn conjugation [verb] ;; verb should be the infinitive form of a verb.
  (str
   "<div class='conjugation'>"
   (tablize verb)
   "<table class='fs conjugation'>"
   "<tr>"
   "<th>io</th>"
   "<td>"
   (get (conjugate (lexfn/get "io") verb) :italian)
   "</td>"
   "</tr>"
   "<tr>"
       "<th>tu</th>"
       "<td>"
       (get (conjugate (lexfn/get "tu") verb) :italian)
       "</td>"
       "</tr>"
       "<tr>"
       "<th>lui/lei</th>"
       "<td>"
       (get (conjugate (lexfn/get "lui") verb) :italian)
       "</td>"
       "</tr>"
       "<tr>"
       "<th>noi</th>"
       "<td>"
       (get (conjugate (lexfn/get "noi") verb) :italian)
       "</td>"
       "</tr>"
       "<tr>"
       "<th>voi</th>"
       "<td>"
       (get (conjugate (lexfn/get "voi") verb) :italian)
       "</td>"
       "</tr>"
       "<tr>"
       "<th>loro</th>"
       "<td>"
       (get (conjugate (lexfn/get "loro") verb) :italian)
       "</td>"
       "</tr>"
       "</table>"
       "</div>"
       ))

(def tests
  (list
;   (reload-button) ; reload button does not work yet (results are still cached)

;   (bugs)
;   "<div class='section'> <h2>conjugations</h2></div>"
;   (conjugation (lexfn/get "andare"))
;   (conjugation (lexfn/get "volare"))
;   (conjugation (lexfn/get "fare"))
;   (conjugation (lexfn/get "venire"))
;   (conjugation (lexfn/get "dire"))
   "<div class='section'> <h2>random sentences</h2></div>"
   (tablize (generate-sentence))
   (tablize (generate-sentence))
   (tablize (generate-sentence))
   (tablize (generate-sentence))
   (tablize (generate-sentence))
   (tablize (generate-sentence))
   (tablize (generate-sentence))
   (tablize (generate-sentence))

   "<div class='section'> <h2>fixed sentences</h2></div>"
   (tablize (lui-vado-in-italia))
   (tablize (io-mangio-il-pane))
   (tablize (lui-mangio-la-pasta))
   (tablize (io-pranzo))
   (tablize (lui-scrivo-il-libro))

   (show-lexicon-as-feature-structures)

   (correct)
   (answertable)
   ))

  