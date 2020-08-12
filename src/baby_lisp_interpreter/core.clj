(ns baby-lisp-interpreter.core
  (:gen-class)
  (:require
   [clojure.pprint]
   [baby-lisp-interpreter.compiler :as compiler]))

(defn -print-help []
  (println
   "USAGE:" "\n"
   "  run <filepath>         -- Interprets and runs the file at filepath" "\n"
   "  ast <filepath|string>  -- Parses and prints the AST" "\n"
   "  repl                   -- Start an interactive REPL"))

; `repl` is taken in clojure
; (flush) is needed for displaying the prompt. Print stays in a buffer otherwise.
; See: https://clojuredocs.org/clojure.core/read-line
(defn start-repl []
  (let [prompt (fn [] (do (print "> ") (flush) (read-line)))]
    (loop [input (prompt)]
      (case input
        "exit" (println "Goodbye!")
        (recur (do (println (compiler/lisp-run input)) (prompt)))))))

(defn -filepath-or-src [s]
  (try (slurp s)
       (catch Exception _ s)))

(defn -main
  "Compile and eval a baby lisp language"
  [& args]
  (let [cmd (first args)]
    (case cmd
      "repl" (start-repl)
      "ast" (let [s (second args)
                  src (-filepath-or-src s)]
              (clojure.pprint/pprint (compiler/parse src)))
      "run" (let [s (second args)]
              (if (nil? s)
                (throw (Exception. "[ERR] Must provide a path to the file to run or string"))
                (let [code-str (-filepath-or-src s)]
                  (compiler/lisp-run code-str))))
      (-print-help))))

;; (-main "run" "(+ 2 4)")