(ns foo.core)
(defn reqdata [request-method & uri] 
      (str 
      "<div style='padding:3px;width:40%;background:#ededed;border:2px solid #caecae;margin-top:0.5em'>" 
      "<table>"
      "<tr>"
      "<th>method</th>"
      "<td>" request-method "</td>"
      "</tr>"
      "<tr>"
      "<th>uri</th>"
      "<td>" uri "</td>"
      "</tr>"
      "</table>"
      "</div>"))
