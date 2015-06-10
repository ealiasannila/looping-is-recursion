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
    (helper 0 a-seq)))


(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
   :else false))


(defn find-first-index [pred a-seq]
  (loop [index 0
         part-of-a-seq a-seq]
    (if (pred (first a-seq))
      index
      (recur (inc index) (rest part-of-a-seq)))))

(find-first-index zero? [1 1 1 0 3 7 0 2])

(defn avg [a-seq]
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

