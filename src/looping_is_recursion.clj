(ns looping-is-recursion)

(defn power [base exp]
  (let [power-helper (fn [acc base po]
                 (cond
                   (== po 0) 1
                   (== po 1) acc
                   :else (recur (* acc base) base (dec po))))]
      (helper base base exp)))


(defn last-element [a-seq]
  (if (nil? (next a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (or (empty? seq1) (empty? seq2))  false
    (and (empty? seq1) (empty? seq2)) true
    (not= (first seq1) (first seq2))  false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [pred pred
         a-seq a-seq
         index 0]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) index
      :else (recur pred (rest a-seq) (inc index)))))

(defn avg [a-seq]
  (loop [acc 0
         nt 0
         a-seq a-seq]
    (if (empty? a-seq)
      (if (== nt 0)
        0
        (/ acc nt))
      (recur (+ acc (first a-seq)) (inc nt) (rest a-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [set elem]
                 (if(contains? set elem)
                   (disj set elem)
                   (conj set elem)))]
    (loop [a-seq a-seq
           emp #{}]
      (if (empty? a-seq)
          emp
        (recur (toggle emp (first a-seq)) (rest a-seq))))))

(defn fast-fibo [n]
  (loop [fn 0
         fn-1 1
         n n]
    (if (== n 0)
      fn
      (recur fn (+ fn fn-1) (dec n)))))


(defn cut-at-repetition [a-seq]
  (loop [emp #{}
         em []
         a-seq a-seq]
    (if (empty? a-seq)
        em
      ( (if (contains? emp elem)
          em
          (recur (conj em first a-seq) (conj emp first a-seq) (rest a-seq)))))))

