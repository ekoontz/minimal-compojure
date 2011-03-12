(ns italianverbs.grammar
  (:require
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
					  (= key :head) nil
					  (= key :comp) nil
					  true
					  (list key
						(get lexeme key))))
				       (keys lexeme)))))
       "</table>"))

(defn combine [head comp]
  (let [ls (if (= (get head :left)
		  (get comp :right))
	     (list head comp)
	     (list comp head))]
    (cond
     (nil? (get head :fn))
     {:cat :error :note
      (str "no function for this head :" head )}
     (string? (get head :fn))
     (merge
      (apply (eval (symbol (get head :fn))) (list head comp))
      {:left (get head :left)
       :right (get comp :right)
       :children (list head comp)}) 
     true
     (merge
      (apply (get head :fn) (list head comp))
      {:left (get head :left)
       :right (get comp :right)
       :children (list head comp)}))))
  
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
  