(ns p-p-p-pokerface)

(defn suit [card]
  (let
    [[fst snd] card]
    (str snd)))


(def rank-order {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let
    [[fst snd] card]
      (if
        (Character/isDigit fst)
          (Integer/valueOf (str fst))
          (get rank-order fst))))



(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])


(defn pair? [hand]
  (pos?
    (count
      (filter
        (fn [x] (= x 2))
        (vals
          (frequencies
            (map rank hand)))))))


(defn three-of-a-kind? [hand]
  (pos?
    (count
      (filter
        (fn [x] (= x 3))
        (vals
          (frequencies
            (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (pos?
    (count
      (filter
        (fn [x] (= x 4))
        (vals
          (frequencies
            (map rank hand)))))))


(defn flush? [hand]
  (=
    (seq [5]) ;need to make a seq for = eval
    (vals
      (frequencies
        (map suit hand)))))


(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or
    (>
      (count
        (filter
          (fn [x] (= x 2))
          (vals
            (frequencies
              (map rank hand)))))
      1)
    (pos?
      (count
        (filter
          (fn [x] (= x 4))
          (vals
            (frequencies
              (map rank hand))))))))


(defn straight? [hand]

  (let [rank-sorted  (sort (map rank hand))
        min-rank (apply min rank-sorted)
        max-rank (apply max rank-sorted)]
    (cond
      (= rank-sorted (range min-rank (+ min-rank 5))) true

      (= max-rank 14)
        (let
          [replaced-ace (sort (replace {14 1} rank-sorted))]
            (= replaced-ace (range 1 6)))

      :else false)))


;;
(defn straight? [hand]
  (let [high-ace-rks (sort (map rank hand))
        low-ace-rks (sort (replace {14 1} high-ace-rks))
        straight-seq? (fn [seq]
                        (= (range (apply min seq) (+ 5 (apply min seq))) seq))]
    (or (straight-seq? high-ace-rks) (straight-seq? low-ace-rks))))

;;

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))


(def pph-table
        [[pair? 1] [two-pairs? 2] [three-of-a-kind? 3]
          [straight? 4]   [flush? 5] [full-house? 6]
          [four-of-a-kind? 7] [straight-flush? 8]])

(defn value [hand]
  (let [pph
        (fn [p]
          (if
            ((first p) hand) (second p)
            0))]
    (apply
      max
       (map pph pph-table))))
