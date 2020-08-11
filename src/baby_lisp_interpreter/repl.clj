(ns baby-lisp-interpreter.repl
  (:gen-class)
  (:require [baby-lisp-interpreter.core :as core]))

;; For now, since I don't know how to arrange clojure projects, run this using:
;; $ lein run -m baby-lisp-interpreter.repl

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

; `repl` is taken in clojure
; (flush) is needed for displaying the prompt. Print stays in a buffer otherwise.
; See: https://clojuredocs.org/clojure.core/read-line
(defn -repl []
  (let [prompt (fn [] (do (print "> ") (flush) (read-line)))]
    (loop [input (prompt)]
      (case input
        "exit" (do (println "Exiting") (println "Goodbye!"))
        (recur (do (println (-run-repl-code input)) (prompt)))))))

(defn -main
  "Compile and eval a baby lisp language"
  []
  (-repl))

;; (-main "283")
;; (core/parse "(+ 1 2)")
;; (-run-repl-code "(+ 1 2)")
;; (-run-repl-code "(+ (dec (+ 2 4)) (inc 2))")
;; (core/parse "(+ (dec (+ 2 4)) (inc 2))")