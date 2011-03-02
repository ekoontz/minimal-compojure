(ns italianverbs.grammar)

(defn combine-sv [subject verb]
  (if (get verb :fn)
    (apply (get verb :fn) (list verb subject))
    {:cat :error :note
     (str "null pointer: no function for this verb :" verb  )}))

(defn combine-vo [verb object]
  (if (get verb :fn)
    (apply (get verb :fn) (list verb object))
    {:cat :error :note
     (str "null pointer: no function for this verb :" verb  )}))
