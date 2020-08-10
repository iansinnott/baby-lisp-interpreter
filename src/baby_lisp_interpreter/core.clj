(ns baby-lisp-interpreter.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn tokenize [^String src]
  (-> src
      (str/replace #"(\(|\))" " $1 ")
      (str/split #"\s+")
      (->> (filter #(not (empty? %))))))

(defn parse [& args] (first args))

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
  (clojure.pprint/pprint (-compile "(add 1 2)")))

(-main)