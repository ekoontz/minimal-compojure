(ns italianverbs.grammar
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
			:note (str "head and comp not adjacent:"
				   (get head :right) "!=" (get comp :left))
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

(defn unify-np [head arg]
  (if (and
       (= (get head :gender)
	  (get arg :gender))
       (= (get head :number)
	  (get arg :number)))
    (assoc {}
      :head head)
    (assoc {}
      :cat :fail
      ;; FIXME: rewrite as (defn diagnosis [head arg])
      :note (str (get head :gender) " != " (get arg :gender)
		 " or "
		 (get head :number) " != " (get arg :number)))))


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
   (assoc {}
    :english
    (morphology/conjugate-en head arg)
    :italian
    (string/join " "
		 (list (get arg :italian)
		       (morphology/conjugate-it head))))))

(defn verb-sv [head comp]  ;; e.g. "i [sleep]","he [writes a book]"
  (cond
   (or (= (get (morphology/get-head comp) :cat) "noun")
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
		  (morphology/conjugate-italian-verb head comp)))}
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
    :note "verb does not know what to do with this argument."}))

(defn verb-vo [head arg]  ;; e.g. "[sees a house]","[writes a book]"
  (assoc {}
    :infl :infinitive
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
		  (get arg :italian)))))
