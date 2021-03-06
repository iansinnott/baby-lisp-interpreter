(ns sicp.ch1)

(defn largest-n [n coll]
  (->> coll
       sort
       (take-last n)))

(defn square [x] (* x x))

(defn sum-square-largest-2
  [& args]
  (reduce + (map square (largest-n 2 args))))

(comment
  (sum-square-largest-2 2 3 4))

(defn p [] (p))

(defn text-xy [x y]
  (if (= 0 x) 0 y))

(text-xy 0 (p))
