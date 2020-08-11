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
        "exit" (do (println "Exiting") (println "Goodbye!"))
        (recur (do (println (compiler/lisp-run input)) (prompt)))))))

(defn -main
  "Compile and eval a baby lisp language"
  [& args]
  (let [cmd (first args)]
    (case cmd
      "repl" (start-repl)
      "ast" (let [filepath-or-src (second args)
                  src (try (slurp filepath-or-src)
                           (catch Exception _ filepath-or-src))]
              (clojure.pprint/pprint (compiler/parse src)))
      "run" (let [filepath (second args)]
              (if (nil? filepath)
                (throw (Exception. "[ERR] Must provide a path to the file to run"))
                (let [code-str (slurp filepath)]
                  (println
                   "GOT ALL THIS CODE:"
                   "-----------------"
                   code-str))))
      (-print-help))))

;; (-main)