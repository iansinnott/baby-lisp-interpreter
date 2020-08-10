(ns baby-lisp-interpreter.repl
  (:gen-class)
  (:require [baby-lisp-interpreter.core :as core]))

; NOTE: Using an atom allows modifying the env, which is needed to support
; variable declarations
(def env (atom {:+ +
                :- -
                :inc inc
                :dec dec}))

; `eval` is taken in clojure
(defn -eval [exp env]
  (cond
    (number? exp) exp
    (keyword? exp) (exp @env)
    (vector? exp) (let [fname (first exp)
                        f (-eval fname env)
                        args (map #(-eval % env) (rest exp))]
                    (if (nil? f)
                      (throw (Exception. (str "Function not found or symbol not invokable: " fname)))
                      (apply f args)))
    :else (throw (Exception. (str "Unknown expression:" exp)))))

(defn -run-repl-code [^String src]
  (-> src
      (core/parse)
      (-eval env)))

(defn -main
  "Compile and eval a baby lisp language"
  [code]
  (println (-run-repl-code code)))

;; (-main "283")
;; (core/parse "(+ 1 2)")
;; (-run-repl-code "(+ 1 2)")
;; (-run-repl-code "(+ (dec (+ 2 4)) (inc 2))")
;; (core/parse "(+ (dec (+ 2 4)) (inc 2))")