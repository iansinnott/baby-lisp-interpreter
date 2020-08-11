(ns baby-lisp-interpreter.core
  (:gen-class)
  (:require
   [clojure.pprint]
   [baby-lisp-interpreter.compiler :as compiler]
   [baby-lisp-interpreter.repl :as lisp-repl]))

(defn -print-help []
  (println
   "USAGE:" "\n"
   "  run <filepath>         -- Interprets and runs the file at filepath" "\n"
   "  ast <filepath|string>  -- Parses and prints the AST" "\n"
   "  repl                   -- Start an interactive REPL"))


(defn -main
  "Compile and eval a baby lisp language"
  [& args]
  (let [cmd (first args)]
    (case cmd
      "repl" (lisp-repl/start-repl)
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