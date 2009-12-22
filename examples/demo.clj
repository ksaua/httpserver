(ns demo
  (:require httpserver))

(defn index [params] 
  (str "<html></p>"
       (:text (:postvars params))
       "<form method=\"post\" action=\"/\"><input type=\"text\" name=\"text\"><input type=\"submit\"></form></html>"))

(defn parameters [params]
  (str "<p>" params "</p>"))
  

(def handlers [["/" index]
               ["/params" parameters]])

(httpserver/start 8080 handlers)
