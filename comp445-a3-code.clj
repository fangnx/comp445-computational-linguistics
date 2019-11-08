;; COMP/LING 445 Assignment 3
;; Author: Naxin Fang

(def vocabulary '(call me ishmael))
(def theta1 (list (/ 1 2) (/ 1 4) (/ 1 4)))
(def theta2 (list (/ 1 4) (/ 1 2) (/ 1 4)))
(def thetas (list theta1 theta2))
(def theta-prior (list (/ 1 2) (/ 1 2)))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn score-categorical [outcome outcomes params]
  (if (empty? params)
      (throw "no matching outcome")
      (if (= outcome (first outcomes))
          (first params)
          (score-categorical outcome (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
  (if (empty? lst)
      base
      (f (first lst)
         (list-foldr f base (rest lst)))))

(defn score-BOW-sentence [sen probabilities]
  (list-foldr 
   (fn [word rest-score] 
     (+ (log2 (score-categorical word vocabulary probabilities))
        rest-score))
   0
   sen))

(defn score-corpus [corpus probabilities]
  (list-foldr
   (fn [sen rst]
     (+ (score-BOW-sentence sen probabilities) rst))
   0
   corpus))

(defn logsumexp [log-vals]
  (let [mx (apply max log-vals)]
    (+ mx
      (log2
        (apply +
          (map (fn [z] (Math/pow 2 z)) 
            (map (fn [x] (- x mx)) log-vals)))))))

;; Problem 1
(def my-corpus '((call me) (call ishmael)))

(defn index-in-list [w lst index]
  (if (empty? lst)
    -1
    (if (= w (first lst))
      index
      (index-in-list w (rest lst) (+ index 1)))))

(defn theta-corpus-joint [theta corpus theta-probs]
  (logsumexp [(score-corpus corpus theta) (nth theta-probs (index-in-list theta thetas 0))]))

(println (theta-corpus-joint theta1 my-corpus theta-prior))

;; Problem 2
(defn compute-marginal [corpus theta-probs]
  (logsumexp (map (fn [t] (theta-corpus-joint t corpus theta-probs)) thetas)))

(println (compute-marginal my-corpus theta-prior))

;; Problem 3
(defn compute-conditional-prob [theta corpus theta-probs]
  (/ (theta-corpus-joint theta corpus theta-probs) (compute-marginal corpus theta-probs)))

(println (compute-conditional-prob theta1 my-corpus theta-prior))

;; Problem 4
(defn compute-conditional-dist [corpus theta-probs]
  (map 
    (fn [t] (compute-conditional-prob t corpus theta-probs)) 
    thetas))

(println (compute-conditional-dist my-corpus theta-prior))