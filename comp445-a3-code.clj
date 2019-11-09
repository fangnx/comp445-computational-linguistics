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
  (+ (score-corpus corpus theta)
     (log2 (nth theta-probs (index-in-list theta thetas 0)))))

; (println (score-BOW-sentence (first my-corpus) theta1))
; (println (score-corpus my-corpus theta1))

(println 'Problem 1)
(println (theta-corpus-joint theta1 my-corpus theta-prior))
(println (theta-corpus-joint theta2 my-corpus theta-prior))

;; Problem 2
(defn compute-marginal [corpus theta-probs]
  (logsumexp (map (fn [t] (theta-corpus-joint t corpus theta-probs)) thetas)))

(println 'Problem 2)
(println (compute-marginal my-corpus theta-prior))

;; Problem 3
(defn compute-conditional-prob [theta corpus theta-probs]
  (- (theta-corpus-joint theta corpus theta-probs) (compute-marginal corpus theta-probs)))

(println 'Problem 3)
(println (compute-conditional-prob theta1 my-corpus theta-prior))

;; Problem 4
(defn compute-conditional-dist [corpus theta-probs]
  (map
   (fn [t] (compute-conditional-prob t corpus theta-probs))
   thetas))

;; Problem 5
(println 'Problem 4/5)
(println (compute-conditional-dist my-corpus theta-prior))
(println (map (fn [lp] (Math/pow 2 lp)) (compute-conditional-dist my-corpus theta-prior)))

;; Problem 6
(defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
  (let [conditional-dist (map (fn [lp] (Math/pow 2 lp)) (compute-conditional-dist observed-corpus theta-probs))]
    (compute-marginal new-corpus conditional-dist)))

(println 'Problem 6)
(println (compute-posterior-predictive my-corpus my-corpus theta-prior))

(defn normalize [params]
  (let [sum (apply + params)]
    (map (fn [x] (/ x sum)) params)))

(defn flip [weight]
  (if (< (rand 1) weight)
    true
    false))

(defn sample-categorical [outcomes params]
  (if (flip (first params))
    (first outcomes)
    (sample-categorical (rest outcomes)
                        (normalize (rest params)))))

(defn repeat [f n]
  (if (= n 0)
    '()
    (cons (f) (repeat f (- n 1)))))

(defn sample-BOW-sentence [len probabilities]
  (if (= len 0)
    '()
    (cons (sample-categorical vocabulary probabilities)
          (sample-BOW-sentence (- len 1) probabilities))))

; (println (sample-BOW-sentence 10 theta1))

;; Problem 7
(defn sample-BOW-corpus [theta sent-len corpus-len]
  ; (repeat (sample-BOW-sentence sent-len theta) corpus-len))
  (repeat (fn [] (sample-BOW-sentence sent-len theta)) corpus-len))

(println 'Problem 7)
(println (sample-BOW-corpus theta1 2 2))

;; Problem 8
(defn sample-theta-corpus [sent-len corpus-len theta-probs]
  (let [theta theta-probs]
    (list theta (sample-BOW-corpus theta sent-len corpus-len))))

(println 'Problem 8)
(println (sample-theta-corpus 4 4 theta1))

(defn get-theta [theta-corpus]
  (first theta-corpus))

(defn get-corpus [theta-corpus]
  (first (rest theta-corpus)))

(defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
  (repeat (fn [] (sample-theta-corpus sent-len corpus-len theta-probs)) sample-size))

;; Problem 9



;; Problem 10

;; Problem 11

;; Problem 12
