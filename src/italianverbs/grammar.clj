(ns italianverbs.grammar
  (:use [somnium.congomongo])
  (:require
   [clojure.set :as set]
   [italianverbs.morphology :as morphology]
   [clojure.string :as string]))

(defn fs-tr [key-val-pair]
  (let [key (first key-val-pair)
	val (second key-val-pair)]
    (str "<tr> <th> " key "</th>  <td>" val "</td></tr>")))

(defn fs [lexeme]
  (str "<table class='fs'>"
       (string/join " " (seq (map fs-tr
				  (map (fn [key]
					 (cond
					  (= key :_id) nil
					  (= key :children) nil
;					  (= key :left) nil
;					  (= key :right) nil
; uncomment for debugging.
;					  (= key :fn) nil
					  (= key :head)
					  (list key
                            (fs (get lexeme key)))
                      (or (= key :head-debug) (= key :comp-debug))
                      (list key
                            (fs (get lexeme key)))
					  (= key :root)
					  (list key
                            (fs (get lexeme key)))
					  (= key :comp) nil
					  true
					  (list key
                            (get lexeme key))))
				       (cons
                        :italian
                        (if (get lexeme :english)
                          (cons :english
                                (set/difference
                                 (set (keys lexeme))
                                 #{:english :italian}))
                          (set/difference
                           (set (keys lexeme))
                           #{:english :italian})))))))
       "</table>"))

(defn combine [head comp]
  (let [linear-order (cond
                      (= (get head :right)
                         (get comp :left))
                      (list head comp)
                      (= (get head :left)
                         (get comp :right))
                      (list comp head)
                      true
                      (list
                       {:cat :error
                        :head-debug head
                        :comp-debug comp
                        :note (str "<tt><b>(combine '"
                                   (get head :italian) "',"  "'" (get comp :italian) "'</b>) <i>head and comp not adjacent:</i>"
                                   "head:[" (get head :left) "," (get head :right) "];"
                                   "comp:[" (get comp :left) "," (get comp :right) "];")
                                   
                        }))
        fn (cond
            (nil? (get head :fn))
            {:cat :error :note
             (str "no function for this head :" head )}
            (string? (get head :fn))
            (eval (symbol (get head :fn)))
            true (get head :fn))]
    (merge
     (apply fn (list head comp))
     {:head (if (get head :head) (get head :head) head)
      :left (get (first linear-order) :left)
      :right (get (second linear-order) :right)
      :children linear-order})))
  
(defn tablize [parent]
  (let
      [children (get parent :children)]
      (str
     "<div class='syntax'><table class='syntax'>"
     "<tr><td style='padding-left:5%;width:90%' colspan='" (count children) "'>"
       (fs parent)
     "</td></tr>"
     "<tr>"
     ;; now show syntactic children for this parent.
     (string/join " " (map (fn [child] (str "<td>"
					    (cond (string? child)
						  child
						  (get child :children)
						  (tablize child)
						  true
						  (fs child))
					    "</td>")) children))
     "</tr>"
     "</table></div>")))

(defn gramhead [sign]
  (if (get sign :head)
    (get sign :head)
    sign))

(defn unify-np [head arg]
  (if (and
       (= (get (gramhead head) :gender)
          (get (gramhead arg) :gender))
       (= (get (gramhead head) :number)
          (get (gramhead arg) :number)))
    {
     :head head
     }
    {
     :cat :fail
     ;; FIXME: rewrite as (defn diagnosis [head arg])
     :note (str (get head :gender) " != " (get arg :gender)
                " or "
                (get head :number) " != " (get arg :number))
     }))


(defn prep-fn [head arg]  ;; e.g. "[in Italia]","[a lavorare]"
  {:head head
   :comp arg
   :english
   (string/join " "
		(list 
		 (get head :english)
		 (get arg :english)))
   
   :italian
   (string/join " "
		(list 
		 (get head :italian)
		 (get arg :italian)))})

(defn noun-fn [head arg]  ;; e.g. "il libro"
  (merge
   (unify-np head arg)
   {:english
    (morphology/conjugate-en head arg)
    :italian
    (string/join " "
                 (list (get arg :italian)
                       (morphology/conjugate-it head)))}))

;; following 3 fns should probably be in generate.clj.
(defn np-no-det [noun]
  ;; for nouns that do not take determiners.
  nil)

(defn choose-lexeme [struct]
  ;; do a query based on the given struct,
  ;; and choose a random element that satisfies the query.
  (let [results (fetch :lexicon :where struct)]
    (if (= (count results) 0)
      {:cat :error :note (str "choose lexeme: no results found for " struct)}
      (nth results (rand-int (count results))))))

(defn np-det [noun]
  (choose-lexeme
   {:gender (get noun :gender)
    :number (get noun :number)
    :cat :det}))

(defn verb-sv [head comp]  ;; e.g. "i [sleep]","he [writes a book]"
  (cond
   ;; unfortunately we have to check
   ;; for either the :-form or the quoted-string below:
   (or (= (get (morphology/get-head comp) :cat) :noun)
       (= (get (morphology/get-head comp) :cat) "noun")
       (= (get (morphology/get-head comp) :cat) :pronoun)
       (= (get (morphology/get-head comp) :cat) "pronoun"))

   {:fn "verb-sv"
    :english
    (string/join " "
		 (list 
		  (get comp :english)
		  (morphology/conjugate-english-verb (morphology/get-head head) comp)
		  (get (get head :comp) :english)))
    :italian
    (string/join " "
		 (list
		  (get comp :italian)
		  (morphology/conjugate-italian-verb head comp)
          (get (get head :comp) :italian)))}
   (= (get (morphology/get-head comp) :cat) "prep")
   {:fn "verb-sv"
    :head head
    :comp comp
    :italian
    (str
     (get head :italian)
     " "
     (get comp :italian))
     :english
    (str
     (get head :english)
     " "
     (get comp :english))}
   true
   {:cat :error
    :note (str
           "<tt><i>error: verb does not know what to do with this argument.</i>(<b>verb-sv</b> "
           "'" (get head :english) "','" (get comp :english) "'"
           ")</i>."
           "<p>get-head comp :cat=" (get (morphology/get-head comp) :cat) "</p>"
           "</tt>")}))

(defn verb-vo [head arg]  ;; e.g. "[sees a house]","[writes a book]"
  {
   :fn verb-sv
   :head head
   :comp arg
   :english
   (string/join " "
                (list 
                 (get head :english)
                 (get arg :english)))
   :italian
   (string/join " "
                (list 
                 (get head :italian)
                 (get arg :italian)))
   })

(defn verb-pp [head arg]  ;; e.g. "[speaks to the man]"
  {
   :fn verb-sv
   :head head
   :comp arg
   :english
   (string/join " "
                (list 
                 (get head :english)
                 (get arg :english)))
   :italian
   (string/join " "
                (list 
                 (get head :italian)
                 (get arg :italian)))
   })
