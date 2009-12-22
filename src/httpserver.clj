(ns httpserver)

(import '(java.net ServerSocket)
        '(java.io BufferedReader InputStreamReader PrintWriter)
        '(java.lang Thread))

(use '[clojure.contrib.str-utils :only (chomp)]
     '[clojure.contrib.str-utils2 :only (split, split-lines, join)])

(def *404* (fn [params] (str "404: " (:url params))))

(defn parse-key-value [line]
  "Parses a key-value string to a set:
   key1=value1&key2=value2  ->  {:key1 value1, :key2 value2}"
  (loop [keyvalues (split line #"&|=") sq {}]
    (if (empty? keyvalues)
      sq
      (recur (rest (rest keyvalues)) (conj sq {(keyword (first keyvalues)) (first (rest keyvalues))})))))

(defn parse-param [line]
  (defn colon-param [line]
    (let [s (split line #": ")]
      [(first s) (join ": " (rest s))]))

  (let [data (split (chomp line) #" ")]
    (if (or (= (first data) "GET") (= (first data) "POST"))
      {:method (first data) :url (nth data 1) :http-version (nth data 2)}
      (let [[variable data] (colon-param line)]
        (if (= variable "Content-Length")
          {:Content-Length (Integer. data)}
          {(keyword variable) data})))))

(defn dispatch [handlers url]
  (let [handler (first (filter (fn [[regex _]] (re-find regex url)) handlers))]
   (if (nil? handler)
     *404*
     (last handler))))

(defn output-handler [handlers params output]
  (. output println
     ((dispatch handlers (:url params)) params)))

(defn input-handler [reader]
  "Reads from the stream until it says it's finished"
  (defn read-n-chars [bufferedreader n]
    (let [chars (char-array n)]
      (do 
        (.read bufferedreader chars 0 n)
        (apply str (seq chars)))))

  (loop [text (. reader readLine) http-params {}]
    (if (= "" text)
      (if (nil? (:Content-Length http-params))
        http-params
        (conj http-params 
              {:postvars (parse-key-value (read-n-chars reader (:Content-Length http-params)))}))
      (recur (. reader readLine) (conj http-params (parse-param text))))))

(defn connection-handler [connection handlers]
  (let [input (BufferedReader. (InputStreamReader. (. connection getInputStream)))
        output (PrintWriter. (. connection getOutputStream) true)]
    (do
      (output-handler handlers (input-handler input) output)
      (. connection close))))

(defn start [port handlers]
  (let [listen (ServerSocket. port)
        handlers (map (fn [[regexp function]] [(re-pattern (str regexp "$")) function]) handlers)] 
    (do
      (loop []
        (let [connection (. listen accept)]
          (do
            (.start (Thread. (fn [] (connection-handler connection handlers))))
            (recur))))
      (. listen close))))

