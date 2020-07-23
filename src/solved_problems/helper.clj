(ns solved-problems.helper
  (:gen-class))

(defn fact [number]
  (loop [n number factorial 1]
    (if (zero? n)
      factorial
      (recur (- n 1) (*' factorial n)))))

(defn slow-prime? [n]
  (if (> n 2)
    (zero? (mod (inc (fact (dec n))) n))
    false))

(defn- test-prime
  "Determine if a number is prime by looping through divisors"
  [x]
  (loop [iter 5 top (Math/sqrt x)]
    (cond
      (> iter top) true
      (or (zero? (mod x iter))
          (zero? (mod x (+ 2 iter)))) false
      :else (recur (+ 6 iter) top))))

(defn prime?
  "Determines if a given integer is prime."
  [x]
  (cond
    (<= x 3) (< 1 x)
    (or (zero? (mod x 2))
        (zero? (mod x 3))) false
    :else (test-prime x)))

(defn key-to-int [key]
  (bigint (subs (str key) 1)))

(defn next-prime
  [number]
  (loop [n number]
    (if (prime? (inc n))
      (inc n)
      (recur (inc n)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MATRIZ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn extract-diagonal
  [line column matriz baixo direita]
  (when (or (and (true? baixo) (<= line 16)
                 (or (and (false? direita) (>= column 4))
                     (and (true? direita) (<= column 16))))
            (and (false? baixo) (>= line 4)
                 (or (and (false? direita) (>= column 4))
                     (and (true? direita) (<= column 16)))))
    (loop [line line column column values []]
      (if (< (count values) 4)
        (recur (if baixo
                 (inc line)
                 (dec line))
               (if direita
                 (inc column)
                 (dec column))
               (conj values (nth (nth matriz line) column)))
        values))))

(defn extract-vertical
  [line column matriz]
  (when (and (<= line 19) (>= line 0) (<= column 19) (>= column 0))
    (map #(nth % column) (subvec matriz (- line 4) line))))

(defn extract-horizontal
  [line column matriz]
  (when (and (<= line 19) (>= line 0) (<= column 19) (>= column 0))
    (subvec (nth matriz line) column (+ column 4))))

(defn noroeste
  [line column matriz]
  (reduce * (extract-diagonal line column matriz false false)))

(defn nordeste
  [line column matriz]
  (reduce * (extract-diagonal line column matriz false true)))

(defn sudoeste
  [line column matriz]
  (reduce * (extract-diagonal line column matriz true false)))

(defn sudeste
  [line column matriz]
  (reduce * (extract-diagonal line column matriz true true)))

(defn norte
  [line column matriz]
  (when (>= line 4)
    (reduce * (extract-vertical line column matriz))))

(defn sul [line column matriz]
  (when (<= line 16)
    (reduce * (extract-vertical (+ line 4) column matriz))))

(defn leste [line column matriz]
  (when (<= column 16)
    (reduce * (extract-horizontal line column matriz))))

(defn oeste [line column matriz]
  (when (>= column 4)
    (reduce * (extract-horizontal line (- column 4) matriz))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;