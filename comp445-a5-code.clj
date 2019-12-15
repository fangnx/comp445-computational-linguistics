;; COMP/LING 445 Assignment 5
;; Author: Naxin Fang

(def hidden-states '(Start N V))

(def vocabulary '(Call me Ishmael))

(def theta-transition-Start '(0 0.9 0.1))
(def theta-transition-N '(0 0.3 0.7))
(def theta-transition-V '(0 0.8 0.2))

(def theta-transition-dists-1
  (list theta-transition-Start theta-transition-N theta-transition-V))

(def theta-observation-Start '(0 0 0))
(def theta-observation-N '(0.1 0.5 0.4))
(def theta-observation-V '(0.8 0.1 0.1))

(def theta-observation-dists-1
  (list theta-observation-Start theta-observation-N theta-observation-V))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn dist-lookup [state states dists]
  (if (= state (first states))
    (first dists)
    (dist-lookup state (rest states) (rest dists))))

(defn logsumexp [log-vals]
  (let [mx (apply max log-vals)]
    (+ mx
       (log2
        (apply +
               (map (fn [z] (Math/pow 2 z))
                    (map (fn [x] (- x mx)) log-vals)))))))

(defn logscore-categorical [outcome outcomes params]
  (if (= outcome (first outcomes))
    (log2 (first params))
    (logscore-categorical outcome (rest outcomes) (rest params))))

(defn index-in-list [w lst index]
  (if (empty? lst)
    -1
    (if (= w (first lst))
      index
      (index-in-list w (rest lst) (+ index 1)))))

;; Problem 1
(defn score-next-state-word [curr-state next-state next-obs transtion-dists obs-dists]
  (logsumexp
   [(nth (dist-lookup next-state hidden-states theta-observation-dists-1) 
    (index-in-list next-obs vocabulary 0))
   (nth (dist-lookup curr-state hidden-states theta-transition-dists-1) 
    (index-in-list next-state hidden-states 0))]))

(println 'Problem 1)
(println (score-next-state-word 'Start 'N 'me theta-transition-dists-1 theta-observation-dists-1))
; (println (dist-lookup 'N hidden-states theta-observation-dists-1))
; (println (dist-lookup 'Start hidden-states theta-transition-dists-1))

;; Problem 2
(defn compute-next-observation-marginal [curr-state next-obs transition-dists obs-dists]
  (logsumexp 
    (map 
      (fn [s] (score-next-state-word curr-state s next-obs transition-dists obs-dists)) 
        hidden-states)))

(println 'Problem 2)
(println (compute-next-observation-marginal 'Start 'me theta-transition-dists-1 theta-observation-dists-1))

;; Problem 3
(defn score-next-states-words-recur [curr-state next-k-states next-k-obs transition-dists obs-dists]
  (if (empty? next-k-states) 
    []
    (cons 
      (Math/pow 2 (score-next-state-word curr-state (first next-k-states) (first next-k-obs) transition-dists obs-dists))
      (score-next-states-words-recur curr-state (rest next-k-states) (rest next-k-obs) transition-dists obs-dists))))

(defn score-next-states-words [curr-state next-k-states next-k-obs transition-dists obs-dists]
  (logsumexp (score-next-states-words-recur curr-state next-k-states next-k-obs transition-dists obs-dists)))

(println 'Problem 3)
(println (score-next-states-words 'Start ['N 'N 'V] ['me 'Ishmael 'me] theta-transition-dists-1 theta-observation-dists-1))

