(ns italianverbs.grammar
  (:require
   [clojure.string :as string]))

(defn fs-tr [key-val-pair]
  (let [key (first key-val-pair)
	val (second key-val-pair)]
    (if (= key :fn)
      (str "<tr> <th>" key "</th>  <td>" "(fn)" "</td></tr>")
      (str "<tr> <th> " key "</th>  <td>" val "</td></tr>"))))

(defn fs [lexeme]
  (str "<table class='fs'>"
       (string/join " " (seq (map fs-tr
				  (map (fn [key]
					 (cond
					  (= key :_id) nil
					  true
					  (list key
						(get lexeme key))))
				       (keys lexeme)))))
       "</table>"))

(defn combine [head comp]
  (cond
   (symbol? (get head :fn))
   (apply (eval (get head :fn)) (list head comp))
   (nil? (get head :fn))
   {:cat :error :note
    (str "no function for this head :" head )}
   (string? (get head :fn))
;   {:cat :debug :note (str "it's a string.." (get head :fn))}
   (apply (eval (symbol (get head :fn))) (list head comp))
   true
   (apply (get head :fn) (list head comp))))

(defn tablize [parent children]
  (str
   "<div class='syntax'><table class='syntax'>"
   "<tr><td style='padding-left:5%;width:90%' colspan='" (count children) "'>" (fs parent) "</td></tr>"
   "<tr>"
   (string/join " " (map (fn [child] (str "<td>"
					  (cond (string? child)
						child
						true
						(fs child))
					  "</td>")) children))
   "</tr>"
   "</table></div>"))
