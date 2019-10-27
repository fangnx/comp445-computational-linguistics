;; COMP/LING 445 Assignment 1
;; Author: Naxin Fang

;; Problem 1
(def moby-word-tokens '(CALL me Ishmael . Some years ago never mind
     how long precisely having little or no money in my purse , and
     nothing particular to interest me on shore , I thought I would
     sail about a little and see the watery part of the world .  It is
     a way I have of driving off the spleen , and regulating the
     circulation . Whenever I find myself growing grim about the mouth
     whenever it is a damp , drizzly November in my soul whenever I
     find myself involuntarily pausing before coffin warehouses , and
     bringing up the rear of every funeral I meet and especially
     whenever my hypos get such an upper hand of me , that it requires
     a strong moral principle to prevent me from deliberately stepping
     into the street , and methodically knocking people's hats off
     then , I account it high time to get to sea as soon as I can .
     This is my substitute for pistol and ball . With a philosophical
     flourish Cato throws himself upon his sword I quietly take to the
     ship .  There is nothing surprising in this . If they but knew it
     , almost all men in their degree , some time or other , cherish
     very nearly the same feelings toward the ocean with me .))

(defn member-of-list? [w l]
  (if (empty? l)
    false
    (if (= w (first l))
      true
      (member-of-list? w (rest l)))))

(defn get-vocabulary [word-tokens vocab]
  (if (empty? word-tokens)
    vocab
    (if (member-of-list? (first word-tokens) vocab)
      (get-vocabulary (rest word-tokens) vocab)
      (get-vocabulary (rest word-tokens) (conj vocab (first word-tokens))))))

(def moby-vocab (get-vocabulary moby-word-tokens []))

; (println moby-vocab)

;; Problem 2
(defn get-count-of-word [w word-tokens count]
  (if (empty? word-tokens)
    count
    (if (= w (first word-tokens))
      (get-count-of-word w (rest word-tokens) (+ count 1))
      (get-count-of-word w (rest word-tokens) count))))

(defn get-word-counts [vocab word-tokens]
  (let [count-word (fn [w] 
    (get-count-of-word w word-tokens 0))]
    (map count-word vocab)))

;; Problem 3
(def moby-word-frequencies (get-word-counts moby-vocab moby-word-tokens))

; (println moby-word-frequencies)

;; Problem 4
(defn flip [p]
  (if (< (rand 1) p)
    true
    false))

(defn normalize [params]
  (let [sum (apply + params)]
    (map (fn [x] (/ x sum)) params)))

(defn sample-categorical [outcomes params]
  (if (flip (first params))
    (first outcomes)
    (sample-categorical (rest outcomes)
            (normalize (rest params)))))

(defn create-uniform-distribution [outcomes]
  (let [num-outcomes (count outcomes)]
    (map (fn [x] (/ 1 num-outcomes)) outcomes)))

(defn sample-uniform-BOW-sentence [n vocab]
  (if (= n 0)
    '()
    (cons (sample-categorical vocab (create-uniform-distribution vocab))
          (sample-uniform-BOW-sentence (- n 1) vocab))))

; (println (sample-uniform-BOW-sentence 4 (list 'the 'a 'every)))

;; Problem 5
(defn score-categorical [outcome outcomes params]
  (if (empty? params)
    (throw "Error: No matching outcome.")
    (if (= outcome (first outcomes))
      (first params)
      (score-categorical outcome (rest outcomes) (rest params)))))

(defn foldr [f base lst]
  (if (empty? lst)
    base
    (f (first lst) (foldr f base (rest lst)))))

(defn compute-uniform-BOW-prob [vocab sentence] 
  (foldr
    (fn [word score] 
      (* (score-categorical word vocab (create-uniform-distribution vocab)) score))
      1
      sentence))

; (println (compute-uniform-BOW-prob (list 'the 'a 'every) (list 'every 'every)))

;; Problem 6
(def sen1 (sample-uniform-BOW-sentence 3 moby-vocab))
(def sen2 (sample-uniform-BOW-sentence 3 moby-vocab))
(def sen3 (sample-uniform-BOW-sentence 3 moby-vocab))
(def prob-sen1 (compute-uniform-BOW-prob moby-vocab sen1))
(def prob-sen2 (compute-uniform-BOW-prob moby-vocab sen2))
(def prob-sen3 (compute-uniform-BOW-prob moby-vocab sen3))

(print sen1) (println prob-sen1)
(print sen2) (println prob-sen2)
(print sen3) (println prob-sen3)

;; Problem 7
(def moby-word-probabilities (normalize moby-word-frequencies))

(println moby-word-probabilities)

;; Problem 8
(defn sample-BOW-sentence [len vocabulary probabilities]
  (if (= len 0)
    '()
    (cons (sample-categorical vocabulary probabilities)
      (sample-BOW-sentence (- len 1) vocabulary probabilities))))

(def sen11 (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))
(def sen12 (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))
(def sen13 (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))
(def sen14 (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))
(def sen15 (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))
(def sen16 (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))

(println sen11)
(println sen12)
(println sen13)
(println sen14)
(println sen15)
(println sen16)

;; Problem 9
(defn index-in-list [w lst index]
  (if (empty? lst)
    -1
    (if (= w (first lst))
      index
      (index-in-list w (rest lst) (+ index 1)))))

(defn lookup-probability [w outcomes probs]
  (let [index (index-in-list w outcomes 0)]
    (if (= index -1)
      (throw "Error: no matching outcome.")
      (nth probs index))))

; (println (lookup-probability 'a (list 'the 'a 'every) (list 0.2 0.5 0.3)))

;; Problem 10
(defn product [l]
  (apply * l))

(defn compute-BOW-prob [sentence vocab probabilities]
  (product 
    (map (fn [w] (lookup-probability w vocab probabilities)) sentence)))

;; Problem 11
(println (compute-BOW-prob sen11 moby-vocab moby-word-probabilities))
(println (compute-BOW-prob sen12 moby-vocab moby-word-probabilities))
(println (compute-BOW-prob sen13 moby-vocab moby-word-probabilities))
(println (compute-BOW-prob sen14 moby-vocab moby-word-probabilities))
(println (compute-BOW-prob sen15 moby-vocab moby-word-probabilities))
(println (compute-BOW-prob sen16 moby-vocab moby-word-probabilities))








