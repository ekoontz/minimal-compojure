(ns foo.core
  (:use compojure.core)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]))

(defroutes main-routes
  (GET "/" [] "<h1>Welcome to the foo app..</h1>")
  (GET "/test/" [] "<h1>Tests? Yeah, tests are good.</h1>")
  (route/resources "/")
  (route/not-found "<h2>Page not found</h2>"))

(def app
  (handler/site main-routes))