;; Comp/Ling 445 Assignment 1
;; Author: fangnx

;; Problem 1
(defn abs [x] (Math/sqrt(* x x)))

;; Problem 2
(defn take-square [x] (* x x))

(defn sum-of-squares [x y]
  (+ (take-square x) (take-square y)))

;; Problem 3
(def exp-13-1 (+ 0 13))
(def exp-13-2 (* 1 13))
(def exp-13-3 (- 20 7))
(def exp-13-4 (+ 6.5 6.5))

;; Problem 4
(defn third [l] (first (rest (rest l))))

;; Problem 5
(def compose 
  (fn [f, g] 
    (fn [x] (f (g x)))))

;; Problem 6
(defn first-two [l] (list (first l) (first (rest l))))

;; Problem 7
(defn remove-second [l] (cons (first l) (rest (rest l))))

;; Problem 8
(defn add-to-end [l, x] 
 (concat l [x]))

;; Problem 9
(defn reverse [l]
  (if (empty? l) []
    (concat (reverse (rest l)) [(first l)])))

;; Problem 10
(defn count-to-1 [n]
 (if (= n 0) '()
  (concat [n] (count-to-1 (- n 1)))))  

;; Problem 11
(defn count-to-n [n]
  (reverse (count-to-1 n))) 

;; Problem 12
(defn get-max [l]
  (reduce (fn [a, b] (if (> a b) a b)) l))

;; Problem 13
(defn greater-than-five? [l]
  (map (fn [x] (if (> x 5) true false)) l))

;; Problem 14
(defn concat-three [x, y, z]
  (concat 

;; Problem 15
(defn sequence-to-power [x, n]
  (if (= n 0) 
    '()
    (concat x (sequence-to-power x (- n 1)))))

;; Problem 16
(defn in-L? [x]
  (if (empty? x)
    true
    (if (= (first x) 'a)
      (in-L? (rest x))
      false)))








