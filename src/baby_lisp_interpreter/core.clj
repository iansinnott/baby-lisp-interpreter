(ns baby-lisp-interpreter.core
  (:gen-class)
  (:require [clojure.string :as str]))

(declare parse read-seq parse-token)

(defn tokenize [^String src]
  (-> src
      (str/replace #"(\(|\))" " $1 ") ; Surround parens with " " so all tokens are space separated
      (str/split #"\s+") ; Spit on one or more spaces (implications for multiline code?)
      (->> (filter #(not (empty? %)))))) ; Filter out empty strings

(defn numeric? [x]
  (= (str/replace x #"[^0-9]" "") x))

(numeric? "23")
(numeric? "t23")

; TODO
(defn parse-token [t]
  (cond
    (numeric? t) (Integer. t)
    :else (format "Symbol<%s>" t)))

; TODO
(defn read-seq [tokens]
  (loop [coll tokens ; Scope this in locally so we can recur on it
         result []]
    (let [[t & ts] coll]
      (cond
        (nil? t) (throw (Exception. "Could not find closing `)`"))
        (= t ")") [result, ts] ; NOTE Closing paren is implicitly skipped by leaving it out of both result and ts
        :else (let [[exp, new_ts] (parse coll)]
                (recur new_ts (conj result exp)))))))

; NOTE: We expect parse to be passed tokens representing a valid expression.
; Valid expressions in our mini-lisp cannot start with `)`
(defn parse [tokens]
  (let [[t & ts] tokens]
    (println (str "parsing EXP: " t))
    (case t
      "(" (read-seq ts) ; NOTE by passing on s only we discard the paren since its not needed
      ")" (throw (Exception. "Unexpected `)`")) ; See NOTE
      [(parse-token t), ts]))) ; NOTE We cannot use :else here, otherwise it would try to match t as the keyword :else and throw a "No matching clause"

(defn traverse [& args] (first args))

(defn transform [& args] (first args))

(defn generate-code [& args] (first args))


(defn -compile [^String src]
  (-> src
      (tokenize)
      (parse)
      (transform)
      (generate-code)))

(defn -main
  "Compile and eval a baby lisp language"
  [& args]
  (clojure.pprint/pprint (-compile "(add (dec (+ 2 4)) (inc 2))")))

(-main)