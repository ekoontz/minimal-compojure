 (ns italianverbs.test
    (:use 
     [hiccup core page-helpers]
     [italianverbs.grammar]
     [somnium.congomongo])
    (:require
     [italianverbs.lexiconfn :as lexfn]
     [italianverbs.generate :as gen]
     [clojure.string :as string]
     [italianverbs.quiz :as quiz]))

(mongo! :db "mydb")

;; useful library functions: will move elsewhere after testing.
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
		    (fetch :lexicon :sort {"italian" 1}))))

;; fixme: change name to "compose-sv-sentence" or something.
(defn conjugate [pronoun infinitive]
  (let [subject (gen/pos pronoun 0 1)
	verb-phrase (gen/pos infinitive 1 2)]
    (combine verb-phrase subject)))

(defn io-pranzo []
  (let [subject (gen/pos (lexfn/get "io") 0 1)
	verb-phrase (gen/pos (lexfn/get "pranzare") 1 2)]
    (combine verb-phrase subject)))

(defn lui-scrivo-il-libro []
  (let [subject (gen/pos (lexfn/get "lui") 0 1)
	object (combine
		(gen/pos (lexfn/get "libro") 3 4)
		(gen/pos (lexfn/get "il") 2 3))
	verb-phrase (combine
		     (gen/pos (lexfn/get "scrivere") 1 2)
		     object)]
    (combine verb-phrase subject)))

(def in-italia
  (let [prep (lexfn/get "in")
	noun (lexfn/get "Italia")]
    (combine prep noun)))

(def andare-in-italia
  (let [verb (gen/pos (lexfn/get "andare") 0 1)]
    (combine verb
	     (gen/pos in-italia 1 3))))

(defn lui-vado-in-italia []
  (combine
   (combine
    (gen/pos (lexfn/get "andare") 1 2)
    (gen/pos in-italia 2 4))
   (gen/pos (lexfn/get "lui") 0 1)))

(defn io-mangio-il-pane []
  (let [subject (gen/pos (lexfn/get "io") 0 1)
	object (combine
		(gen/pos (lexfn/get "pane") 3 4)
		(gen/pos (lexfn/get "il") 2 3))
	verb-phrase (combine (gen/pos (lexfn/get "mangiare") 1 2)
			     object)]
    (combine verb-phrase subject)))

(defn lui-mangio-la-pasta-in-italia []
  (let [subject (gen/pos (lexfn/get "lui") 0 1)
	object (combine
		(gen/pos (lexfn/get "pasta") 3 4)
		(gen/pos (lexfn/get "la") 2 3))
	verb-phrase (combine (gen/pos (lexfn/get "mangiare") 1 2)
			     object)]
    (combine 
     (combine verb-phrase subject)
     (gen/pos in-italia 4 6))))

(defn io-scrivo-il-libro []
  (let [subject (lexfn/get "io")
	object (combine
		(lexfn/get "libro")
		(lexfn/get "il"))
	verb-phrase (combine (lexfn/get "scrivere")
			     object)]
    (combine verb-phrase subject)))

(defn reload-button []
  (str "<form action='/test/' method='post'><input type='submit' value='Reload'/>  </form> "))

;; current thing I'm debugging..
(defn bugs []
  (let [sentence
	(list (lexfn/get "lui")
	      (lexfn/get "dimenticare")
	      (lexfn/get "il")
	      (lexfn/get "libro"))
	linearized
	(gen/linearize sentence)]
    (str
     "<div> <h2>bugs</h2></div>"
     (tablize (combine
	       (combine
		(nth linearized 1)
		(combine
		 (nth linearized 3)
		 (nth linearized 2)))
	       (nth linearized 0))))))

(defn bugs []
  (let [sentence
        (list (lexfn/get "il")
              (lexfn/get "libro")
              (lexfn/get "dimenticare")
              (lexfn/get "Italia"))
	linearized
	(gen/linearize sentence)]
    (str
     "<div> <h2>bugs</h2></div>" ;; 
     (tablize (combine ;; [ [C [C il] [H libro] ] [H [H dimenticare] [C Italia] ] ]
               (combine
                (nth linearized 2)    ;; dimenticare
                (nth linearized 3))   ;; Italia
               (combine
                (nth linearized 1)
                (nth linearized 0)))))))

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

   (bugs)
   "<div class='section'> <h2>conjugations</h2></div>"

   ;(conjugation (lexfn/get "andare"))
   ;(conjugation (lexfn/get "volare"))
   ;(conjugation (lexfn/get "fare"))
   ;(conjugation (lexfn/get "venire"))
   ;(conjugation (lexfn/get "dire"))
   "<div class='section'> <h2>random sentences</h2></div>"
   (tablize (gen/sentence))
   (tablize (gen/sentence))
   (tablize (gen/sentence))
   (tablize (gen/sentence))
   (tablize (gen/sentence))
   (tablize (gen/sentence))
   (tablize (gen/sentence))
   (tablize (gen/sentence))
   (tablize (gen/sentence))
   (tablize (gen/sentence))
   (tablize (gen/sentence))

   "<div class='section'> <h2>fixed sentences</h2></div>"
   (tablize (lui-vado-in-italia))
   (tablize (io-mangio-il-pane))
   (tablize (lui-mangio-la-pasta-in-italia))
   (tablize (io-pranzo))
   (tablize (lui-scrivo-il-libro))

   (show-lexicon-as-feature-structures)

   (correct)
   (answertable)
   ))

  