(ns advent-of-code-2021.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn read-file[file-name]
  (->> file-name
       io/resource
       slurp
       str/split-lines))

(comment
  (def s [199 200 208 210 200 207 240 269 260 263])
  (->> "01.txt"
       io/resource
       slurp
       str/split-lines
       (map #(Integer/parseInt %))
       (partition 2 1)
       (map (fn[[a b]] (< a b)))
       (filter true?)
       (count))

  (->>  "02.txt"
        io/resource
        slurp
        str/split-lines
        (map #(Integer/parseInt %))
        (partition 3 1)
        (map #(apply + %))
        (partition 2 1)
        (map (fn[[a b]] (< a b)))
        (filter true?)
        (count))
  )


(comment
  "Day 2"
  (->>
    (read-file "03.txt") 
    (map #(str/split % #" "))
    (map (fn[[a b]] [(keyword a) (Integer/parseInt b)]))
    (map (partial apply hash-map))
    (apply merge-with +)
    ((fn [{:keys [up down forward]}]
       (* forward (- down up))))
    )

  (->>
    (read-file "03.txt")
    (map #(str/split % #" "))
    (map (fn[[a b]] [(keyword a) (Integer/parseInt b)]))
    (reduce (fn[{:keys [depth hpos aim] :as accu} [direction x]]
              (case direction
                :forward {:depth (+ depth(* aim x)) :hpos (+ x hpos) :aim aim}
                :up      (update accu :aim #(- % x))
                :down    (update accu :aim #(+ % x))))
            {:depth 0 :hpos 0 :aim 0})
    ((fn [{:keys [depth hpos]}]
       (* depth hpos)))
    );; => 1685186100
  )


(comment
  "Day 3"

  (defn cpr [f row]
    (if (f (get row 0)
           (get row 1))
      0 1))

  (defn rate [f rows]
    (->> rows
         (map (partial cpr f))
         (apply str)
         (#(Integer/parseInt % 2))))

  (def ex-input ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"])

  (let [freqs
        (->>
          (read-file "04.txt")
          (map seq)
          (mapcat #(map-indexed (fn [idx itm] [idx (Integer/parseInt (str itm))]) %))
          (group-by first)
          (map (fn [[k v]] [k (map second v)]))
          (sort-by first)
          (map second)
          (map frequencies))
        gamma   (rate >= freqs)
        epsilon (rate < freqs)
        ]
    (* gamma epsilon)
    );; => 3901196


  (defn- int-at-pos [pos s]
    (Integer/parseInt(str(nth s pos))))

  (defn o2 [row]
    (cpr > row))
  (defn co2 [row]
    (cpr <= row))

  (o2 {0 4 1 5})
  (co2 {0 6 1 5})

  (defn diagnostic[nums f]
    (loop [nums nums pos 0]
      (let [filter-val    (-> (partial int-at-pos pos)
                              (map nums)
                              frequencies
                              f)
            filtered-nums (filter #(= filter-val (int-at-pos pos %)) nums)]

        (if (= 1(count filtered-nums))
          (-> filtered-nums
              first
              (Integer/parseInt 2))
          (recur
            (filter #(= filter-val (int-at-pos pos %)) nums)
            (inc pos))))))

  (* (diagnostic ex-input o2)
     (diagnostic ex-input co2));; => 230

  (def real-input (read-file "04.txt"))
  (* (diagnostic real-input o2)
     (diagnostic real-input co2));; => 4412188
  )

(comment "day 4"

         (defn nth-column [matrix n]
           (for [row matrix] (nth row n)))

         (defn transpose [matrix]
           (for [column (range (count (first matrix)))]
             (nth-column matrix column)))

         (defn remove-num [num-to-remove {:keys [rows cols]} ]
           {:rows (map (partial remove #{num-to-remove}) rows)
            :cols (map (partial remove #{num-to-remove}) cols)})

         (remove-num 5 {:rows [[ 1 2] [3 4] [5 6]]
                        :cols [[ 1 2] [3 4] [5 6]]})
         ;; => {:rows ((1 2) (3 4) (6)), :cols ((1 2) (3 4) (6))}

         (defn won? [{:keys [rows cols]}]
           (or (some empty? rows)
               (some empty? cols)))

         (defn winning-board? [board]
           (when (won? board) board))

         (defn field-sum [{rows :rows}]
           (apply + (flatten rows)))

         (defn nums [lines]
           (->> #","
                (str/split (first lines))
                (map #(Integer/parseInt %))))

         (defn boards [lines]
           (->> lines
                rest
                (partition-by #(= % ""))
                (remove #(= [""] %))
                (map (partial map
                              (comp
                                (partial map #(Integer/parseInt %))
                                (partial remove empty?)
                                #(str/split % #"\s+"))))
                (map (fn[rows] {:rows rows :cols (transpose rows)}))))

         (defn first-bingo-score [lines]

           (let [nums   (nums lines) 
                 boards (boards lines)]
             (loop [pos 0 boards boards]
               (let [num-to-remove (nth nums pos)
                     boards        (map (partial remove-num num-to-remove) boards)]
                 (if-let [winning-board (some winning-board? boards)]
                   (*(field-sum winning-board) num-to-remove)
                   (recur (inc pos) boards))))))

         (first-bingo-score (read-file "05-example.txt"));; => 4512
         (first-bingo-score (read-file "05.txt"));; => 38594

         (defn last-bingo-score[lines]
           (let [nums   (nums lines) 
                 boards (boards lines)]
             (loop [pos 0 boards boards]
               (let [num-to-remove (nth nums pos)
                     boards        (map (partial remove-num num-to-remove) boards)
                     non-won-boards (remove winning-board? boards)]
                 (if (empty? non-won-boards)
                   (* num-to-remove (field-sum (first boards)))
                   (recur (inc pos) non-won-boards))))))

         (last-bingo-score (read-file "05-example.txt"))
         (last-bingo-score (read-file "05.txt"));; => 21184
         )
