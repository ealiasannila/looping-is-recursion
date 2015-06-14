(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [exp acc]
                 (if (zero? exp)
                   acc
                   (recur (dec exp) (* base acc))))]
        (helper exp 1)))


(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                   acc
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))


(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
   :else false))

(defn find-first-index [pred a-seq]
  (loop [index 0
         part-of-a-seq a-seq]
    (cond
     (empty? part-of-a-seq) nil
     (pred (first part-of-a-seq)) index
     :else (recur (inc index) (rest part-of-a-seq)))))


(defn avg [a-seq]
  (loop [n 0
         sum 0
         rest-of-a-seq a-seq]
    (if (empty? rest-of-a-seq)
      (/ sum n)
      (recur (inc n) (+ sum (first rest-of-a-seq)) (rest rest-of-a-seq)))))



(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))


(defn parity [a-seq]
  (remove
  (loop [rest-of-a-seq a-seq
         even-elements (set a-seq)]
    (if (empty? rest-of-a-seq)
      even-elements
      (recur (rest rest-of-a-seq) (toggle even-elements (first rest-of-a-seq)))))
   (set a-seq)))


(defn fast-fibo [n]
  (cond
   (== n 0) 0
   (== n 1) 1
  :else (loop [k 1
               f 1
               f-1 0]
          (if (== n k)
            f
            (recur (inc k) (+ f f-1) f)))))





(defn cut-at-repetition [a-seq]
  (loop [rest-of-a-seq a-seq
         used-set #{}
         result []]
    (if (or (contains? used-set (first rest-of-a-seq)) (empty? rest-of-a-seq))
      result
      (recur (rest rest-of-a-seq) (conj used-set (first rest-of-a-seq)) (conj result (first rest-of-a-seq))))))



