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

(defn string-like? [^String x]
  (and (= (first x) \") (= (last x) \")))

(defn strip-quotes [^String x]
  (-> x
      (str/replace #"(^\"|\"$)" "")))

(comment
  "Comments are left here so you can try things out"
  (tokenize "(+ 1 2)")
  (-> "\"sup\""
      tokenize
      first
      first
      class))

(defn numeric? [x]
  (= (str/replace x #"[^0-9]" "") x))

; NOTE: Keywords are not necessary to encode symbols, they are just something
; different than symbols. I thought it was a bit unfair to use the existing
; clojure symbols since we're trying to write a full
(defn parse-token
  "Parse an individual token"
  [t]
  (cond
    (numeric? t) (Integer. t)
    (string-like? t) (strip-quotes t)
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
                :print print
                :car first
                :cdr rest
                :inc inc
                :dec dec}))

; `eval` is taken in clojure
(defn lisp-eval [exp env]
  (cond
    ;; Primatives
    (number? exp) exp
    (string? exp) exp

    ;; An environment lookup. I.e. a variable reference
    ;; NOTE: For now I'm just flagging lookup errors. Can consider throwing
    ;; later
    (keyword? exp) (exp @env :LOOKUP_ERROR) ; See NOTE

    ;; Define a var in the global scope
    ;; NOTE It's very important to recurse on the value passed to define. This
    ;; threw me off for a while when implementing lambdas. You call define and
    ;; it defines something all right but what it defines is just the AST for
    ;; the lambda, not the actual lambda. So we need to recurse. Makes sense
    ;; after the fact of course. Shows what trouble can arrise form using only
    ;; simple values initially. It just so happened that they had all been
    ;; strings or numbers, so it didn't matter that they weren't recursed on.
    (= :define (first exp)) (let [[_ k raw-v] exp
                                  v (lisp-eval raw-v env)] ;; See NOTE
                              (swap! env assoc k v)
                              (println "[INFO] Bound " k " -> " (prn-str v)))

    ;; If expression
    (= :if (first exp)) (let [[condition succ fail] (-> exp rest vec)
                              passed (lisp-eval condition env)]
                          (if passed (lisp-eval succ env) (lisp-eval fail env)))

    ;; The quote helper. Quote a list
    (= :quote (first exp)) (let [[_ xs] exp] xs)

    ;; Define a function (anon function)
    ;; NOTE Since Atoms are reference types and lisp-eval expects an atom we
    ;; need to create a new local atom when we evaluate
    (= :lambda (first exp)) (let [[_ args body] exp] (fn [& vals]
                                                       (let [local-env (zipmap args vals)
                                                             merged-env (atom (merge @env local-env))] ;; See NOTE
                                                         (lisp-eval body merged-env))))

    ;; Function call
    (vector? exp) (let [fname (first exp)
                        f (lisp-eval fname env)
                        args (map (fn
                                    [x]
                                    (let [v (lisp-eval x env)]
                                      (if (= v :LOOKUP_ERROR)
                                        (throw (Exception. (str "Unknown Symbol: " x)))
                                        v)))
                                  (rest exp))]
                    (if (nil? f)
                      (throw (Exception. (str "Symbol is not a function: " fname)))
                      (apply f args)))

    ;; Something invalid or unsupported
    :else (throw (Exception. (str "Unknown expression:" exp)))))

(defn lisp-eval-all [expressions env]
  (reduce #(lisp-eval %2 env) "" expressions))

(defn lisp-run [^String src]
  (-> src
      (parse)
      (lisp-eval-all env)))

(comment
  (parse "; This is just a comment\n(define a 2)\n(+ 1 a)")
  (parse "(+ 1 a)")
  (lisp-run "(+ 1 1)")
  (lisp-run "(inc 2)\n(print (+ 1 2) (inc 5))")
  (try (lisp-run "(print %ENV)")
       (catch Exception e (println (str "Failed: " (.getMessage e)))))
  (try (lisp-run "(define a 4)\n(print %ENV)")
       (catch Exception e (println (str "Failed: " (.getMessage e))))))

(defn traverse [& args] (first args))

(defn transform [& args] (first args))

(defn generate-code [& args] (first args))

; compile is already built-in to clojure, so we need a new name
(defn lisp-compile [^String src]
  (-> src
      (parse)
      (transform)
      (generate-code)))

(comment
  (let [conditional  "(if (= 1 (- 2 1)) (print \"t\") (print \"f\"))"]
    (let [[a b c] (-> conditional
                      parse
                      first
                      rest
                      vec)]
      (println "a" a)
      (println "b" b)
      (println "c" c))
    (-> conditional parse (lisp-eval-all env)))
  (-> "(quote (3 2 1))"
      lisp-run)

  (->  "(define runme (lambda (x y) (+ x y)))\n(print (runme 2 3))"
       lisp-run)

  (-> "((lambda () (print \"Func\")))"
      lisp-run))
