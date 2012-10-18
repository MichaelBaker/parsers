(ns parsers.partition)

(def partition-into-buckets)

(defn- partition-into-two-buckets
  ([items] (partition-into-two-buckets items 1))
  ([items size]
    (if (> size (count items))
      nil
      (lazy-seq
        (cons
          [(subvec items 0 size) (subvec items size)]
          (partition-into-two-buckets items (inc size)))))))

(defn- attach-head [buckets first-bucket remaining-items]
  (let [cons-head (partial cons first-bucket)]
    (map cons-head (partition-into-buckets buckets remaining-items))))

(defn- compiler
  "Accumulates the rows produced by partition-into-buckets into a single list."
  [buckets]
  (fn [rows [first-bucket remaining-items]]
    (concat rows (attach-head buckets first-bucket remaining-items))))

(defn print-rows
  "Prints each row of the partition on a separate line."
  [rows]
  (doseq [x rows]
    (println x)))

(defn partition-into-buckets
  "Returns all of the ways to partition the given items into the given number of buckets."
  [buckets items]
  (let [items (vec items)]
    (cond
      (< (count items) buckets)
        nil
      (= buckets 1)
        [[items]]
      :otherwise
        (let [partitions (partition-into-two-buckets items)]
          (reduce (compiler (dec buckets)) [] partitions)))))
