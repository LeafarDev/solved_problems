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
