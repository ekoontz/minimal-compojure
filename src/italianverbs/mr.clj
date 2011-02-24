;(ns italianverbs.mr
;  (:use clojure.test
;        somnium.congomongo
;        somnium.congomongo.config
;        somnium.congomongo.util
;        somnium.congomongo.coerce
;        clojure.contrib.pprint)
;  (:use [clojure.contrib.json :only (read-json json-str)])
;  (:use [clojure.contrib.duck-streams :only (slurp*)])
;  (:import [com.mongodb BasicDBObject]))

(mongo! :db "mydb")

; does not work yet.
;(let [mapfn
;     "function() {
;        emit(this.guess, {count: this.guess});
;     }"
;     reducefn
;     "function(key, values){
;              var total = 0;
;              for ( var i=0; i<values.length; i++ ){
;                  total += values[i].count;
;              }
;              return { count : total };
;          }"]
;    (map-reduce :mr mapfn reducefn)))



