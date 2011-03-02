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
					 (list key
					       (get lexeme key)))
				       (keys lexeme)))))
       "</table>"))

(defn combine [head comp]
  (if (get head :fn)
    (apply (get head :fn) (list head comp))
    {:cat :error :note
     (str "no function for this head :" head )}))

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
