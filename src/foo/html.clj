(ns foo.core)
(def banner (str "<html><head><title>Welcome to Foo</title></head><body><div><h1>Welcome to the Foo app</h1></div>"))
(defn message [msg] (str "<div style='padding:3px;width:40%;background:#ededed;border:2px solid #caecae'>" msg "</div>"))

(defn sessiondata [data] 
  (str 
   "<div style='text-align:right;position:absolute;top:1;right:1;padding:1;border:1px solid #caecae';width:10%>"
   "<h4 style='padding:0;margin:0'>Session</h4><pre>" 
   (pr-str data) 
   "</pre>"
   "<a href='/session/clear/'>Clear</a>
    <a style='padding-left:1em' href='/session/set/'>New</a>"
   "</div>"))

(def footer 
     "<div style='width:60%;float:right;margin:1em;padding:0.5em;border:1px solid #ededed'>
        <div style='margin-right:1em;width:auto;float:left'><a href='/'>Main</a></div>
	<div style='margin-right:1em;width:auto;float:left'><a href='/test/'>Tests</a></div>
	<div style='margin-right:1em;width:auto;float:left'><a href='/form/'>Form Processing</a></div>
      </div>
      <div style='float:right;width:100%'>Powered by Compojure</div>
      </body></html>")

