(ns baby-lisp-interpreter.core
  (:gen-class)
  (:require [clojure.string :as str]))

; Must declare any "cyclical" function reference, i.e as is the case with
; parse<->read-seq
(declare parse-exp read-seq parse-token)

(defn tokenize [^String src]
  (-> src
      (str/replace #"(\(|\))" " $1 ") ; Surround parens with " " so all tokens are space separated
      (str/split #"\s+") ; Spit on one or more spaces (implications for multiline code?)
      (->> (filter #(not (empty? %)))))) ; Filter out empty strings

(defn numeric? [x]
  (= (str/replace x #"[^0-9]" "") x))

(numeric? "23")
(numeric? "t23")

; NOTE: Keywords are not necessary to encode symbols, they are just something
; different than symbols. I thought it was a bit unfair to use the existing
; clojure symbols since we're trying to write a full 
(defn parse-token
  "Parse an individual token"
  [t]
  (cond
    (numeric? t) (Integer. t)
    :else (keyword t))) ; See NOTE

(defn read-seq
  "Parse the sequene of tokens within parens"
  [tokens]
  (loop [coll tokens ; Scope this in locally so we can recur on it
         result []]
    (let [[t & ts] coll]
      (cond
        ; If t is nil then we're at the end of the sequence and have not yet hit
        ; the base-case, thus an error.
        (nil? t) (throw (Exception. "Could not find closing `)`"))

        ; Base case. Closing paren is implicitly skipped by leaving it out of
        ; both result and ts.
        (= t ")") [result, ts]

        ; Otherwise recur, not just here which is essentially a loop, but by
        ; using parse-exp which also calls this fn
        :else (let [[exp, new_ts] (parse-exp coll)]
                (recur new_ts (conj result exp)))))))

; NOTE: We expect parse to be passed tokens representing a valid expression.
; Valid expressions in our mini-lisp cannot start with `)`
(defn parse-exp [tokens]
  (let [[t & ts] tokens]
    (case t
      "(" (read-seq ts) ; NOTE by passing on s only we discard the paren since its not needed
      ")" (throw (Exception. "Unexpected `)`")) ; See NOTE
      [(parse-token t), ts]))) ; NOTE We cannot use :else here, otherwise it would try to match t as the keyword :else and throw a "No matching clause"

(def parse (comp
            parse-exp
            tokenize))

(defn traverse [& args] (first args))

(defn transform [& args] (first args))

(defn generate-code [& args] (first args))

; compile is already built-in to clojure, so we need a new name
(defn -compile [^String src]
  (-> src
      (parse)
      (transform)
      (generate-code)))

(defn -main
  "Compile and eval a baby lisp language"
  [& args]
  (println (-compile "(+ (dec (+ 2 4)) (inc 2))")))

(-main)