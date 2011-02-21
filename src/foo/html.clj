(ns foo.core)
(def banner (str "<html><head><title>Welcome to Foo</title></head><body><div><h1>Welcome to the Foo app</h1></div>"))
(defn message [msg] (str "<div style='padding:3px;width:40%;background:#ededed;border:2px solid #caecae'>" msg "</div>"))

(defn sessiondata [data] 
  (str 
   "<div style='text-align:right;position:absolute;top:0;right:0;float:right;padding:0;border:1px solid #caecae';width:10%>"
   "<h4 style='padding:0;margin:0'>Session</h4><pre>" 
   (pr-str data) 
   "</pre>"
   "<a href='/session/clear/'>Clear</a>
    <a style='padding-left:1em' href='/session/set/'>New</a>"
   "</div>"))

(def footer "<div style='margin:1em;padding:0.5em;border:1px solid #ededed'>
<p><a href='/'>Main</a></p>
<p><a href='/test/'>Tests</a></p>
<p><a href='/form/'>Form Processing</a></p>
<div style='float:right'>Powered by Compojure</div></body></html>")

