(ns baby-lisp-interpreter.compiler
  (:gen-class)
  (:require [clojure.string :as str]))

; Must declare any "cyclical" function reference, i.e as is the case with
; parse<->read-seq
(declare parse-exp read-seq parse-token)

(defn tokenize [^String src]
  (-> src
      (str/replace #";.*" "") ; Super rudimentary comment support. Ideally we would want comments in the AST
      (str/replace #"(\(|\))" " $1 ") ; Surround parens with " " so all tokens are space separated
      (str/split #"\s+") ; Spit on one or more spaces (implications for multiline code?)
      (->> (filter #(not (empty? %)))))) ; Filter out empty strings

;; (tokenize "; This is just a comment\n(define a 2)\n(+ 1 a)")

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

        ; Maybe base case. Closing paren is implicitly skipped by leaving it out
        ; of both result and ts.
        (= t ")") [result, ts] ; Recur. This means we have more expressions to process. Ex: Multiple top-level exps

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

; This construct simply ensures that adjacent expressions get parsed
(defn parse-exp-all [tokens]
  (loop [parsed []
         remaining tokens]
    (if (empty? remaining)
      parsed
      (let [[a b] (parse-exp remaining)]
        (recur (conj parsed a) b)))))

(parse-exp-all (tokenize "; This is just a comment\n(define a 2)\n(+ 1 a)"))

(def parse (comp
            parse-exp-all
            tokenize))

; NOTE: Using an atom allows modifying the env, which I will need to support
; variable declarations.
; NOTE: For now there is clearly not much here, and that's intentional. My
; thinking is that the point here isn't to define a super interesting language,
; but to learn how to build an interpreter. To that end a simple env is still
; sufficient.
; Clearly if we're interpretting clojure in clojure there are mostly just going
; to be a bunch of direct mappings from keywords to their exact function
; counterpart.
(def env (atom {:+ +
                :- -
                :* *
                :/ /
                :> >
                :< <
                :>= >=
                :<= <=
                := =
                :define (fn [k v] (swap! env assoc (keyword k) v))
                :print print
                :car first
                :cdr rest
                :inc inc
                :dec dec}))

; `eval` is taken in clojure
(defn lisp-eval [exp env]
  (cond
    (number? exp) exp
    (keyword? exp) (exp @env)
    (vector? exp) (let [fname (first exp)
                        f (lisp-eval fname env)
                        args (map #(lisp-eval % env) (rest exp))]
                    (if (nil? f)
                      (throw (Exception. (str "Symbol not found: " fname)))
                      (apply f args)))
    :else (throw (Exception. (str "Unknown expression:" exp)))))

(defn lisp-run [^String src]
  (-> src
      (parse)
      (lisp-eval env)))

(defn traverse [& args] (first args))

(defn transform [& args] (first args))

(defn generate-code [& args] (first args))

; compile is already built-in to clojure, so we need a new name
(defn lisp-compile [^String src]
  (-> src
      (parse)
      (transform)
      (generate-code)))