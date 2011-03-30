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
    (combine infinitive pronoun 'right))

(defn io-pranzo []
  (combine (lexfn/get "pranzare")
           (lexfn/get "io" {:case {:$ne :acc}}) 'right))

(defn lui-scrivo-il-libro []
  (let [subject (lexfn/get "lui" {:case {:$ne :acc}})
        object (combine
                (lexfn/get "libro")
                (lexfn/get "il") 'right gen/det-n)
        verb-phrase (combine (lexfn/get "scrivere") object 'left gen/vo)]
    (combine verb-phrase subject 'right)))

(def in-italia
  (let [prep (lexfn/get "in")
	noun (lexfn/get "Italia")]
    (combine
     prep noun 'left)))

(def andare-in-italia
  (combine (lexfn/get "andare")
           in-italia 'left))

(defn lui-vado-in-italia []
  (combine
   (combine
    (lexfn/get "andare") in-italia 'left)
   (lexfn/get "lui" {:case {:$ne :acc}}) 'right))

(defn io-mangio-il-pane []
  (let [subject (lexfn/get "io" {:case {:$ne :acc}})
        object (combine
                (lexfn/get "pane")
                (lexfn/get "il") 'right)
        verb-phrase (combine (lexfn/get "mangiare")
                             object 'left)]
    (combine verb-phrase subject 'right)))

(defn lui-mangio-la-pasta-in-italia []
  (let [subject (lexfn/get "lui" {:case {:$ne :acc}})
        object (combine
                (lexfn/get "pasta")
                (lexfn/get "la") 'right)
        verb-phrase (combine (lexfn/get "mangiare")
                             object
                             'left)]
    (combine 
     (combine verb-phrase subject 'right)
     in-italia 'left)))

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

(defn bugs []
  nil)

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

(defn conjugations []
  (list 
   "<div class='section'> <h2>conjugations</h2></div>"
   (conjugation (lexfn/get "andare"))
   (conjugation (lexfn/get "volare"))
   (conjugation (lexfn/get "fare"))
   (conjugation (lexfn/get "venire"))
   (conjugation (lexfn/get "dire"))))

(defn random-sentences-1 [num]
  (if (> num 0)
    (cons
     (tablize (gen/sentence))
     (random-sentences-1 (- num 1)))))

(defn random-sentences [num]
  (list
   "<div class='section'> <h2>random sentences</h2></div>"
   (random-sentences-1 num)))

(def tests
  (list
;   (reload-button) ; reload button does not work yet (results are still cached)

   ;(bugs)

   ;(conjugations)

   (random-sentences 20)
   
   "<div class='section'> <h2>fixed sentences</h2></div>"
 ;  (tablize (lui-vado-in-italia))
 ;  (tablize (io-mangio-il-pane))
 ;  (tablize (lui-mangio-la-pasta-in-italia))
 ;  (tablize (io-pranzo))
 ;  (tablize (lui-scrivo-il-libro))

   (show-lexicon-as-feature-structures)

   (correct)
   (answertable)
   ))

  