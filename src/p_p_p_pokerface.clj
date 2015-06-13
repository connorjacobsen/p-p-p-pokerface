(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacements r))))

(defn ranks [hand]
  (map rank hand))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn suits [hand]
  (map suit hand))

(defn rank-frequencies [hand]
  (vals (frequencies (ranks hand))))

(defn sorted-rank-frequencies [hand]
  (sort (rank-frequencies hand)))

(defn n-of-a-kind [hand]
  (apply max (rank-frequencies hand)))

(defn n-of-a-kind? [n hand]
  (if (>= (n-of-a-kind hand) n)
    true
    false))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (= 5 (first (vals (frequencies (suits hand))))))

(defn full-house? [hand]
  (if (= '(2 3) (sorted-rank-frequencies hand))
    true
    false))

(defn two-pairs? [hand]
  (= (sorted-rank-frequencies hand)
     '(1 2 2)))

(defn straight? [hand]
  (let [sorted-ranks (sort (ranks hand))
        n (first sorted-ranks)
        sorted-ranks-ace (sort (replace {14 1} (ranks hand)))
        straight (fn [first-rank] (range first-rank (+ first-rank 5)))]
    (or (= sorted-ranks (straight n)) (= sorted-ranks-ace (straight 1)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matching-checkers (filter #((first %1) hand) checkers)
        matching-values (map #(second %1) matching-checkers)]
    (apply max matching-values)))
