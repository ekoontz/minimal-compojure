(ns italianverbs.grammar
  (:require
   [clojure.set :as set]
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
					  (= key :left) nil
					  (= key :right) nil
					  (= key :fn) nil
					  (= key :head)
					  (list key
						(fs (get lexeme key)))
					  (= key :comp) nil
					  true
					  (list key
						(get lexeme key))))
				       (cons
					:italian
					(cons :english
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
		       {:cat :error :note "head and comp not adjacent"}))
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
  