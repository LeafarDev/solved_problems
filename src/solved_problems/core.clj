(ns solved-problems.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(defn problem-one
  "If we list all the natural numbers below 10 that are multiples of 3 or 5
   we get 3, 5, 6 and 9. The sum of these multiples is 23.
   Find the sum of all the multiples of 3 or 5 below 1000."
  []
  (let [multiples (filter #(or (= 0 (mod % 3)) (= 0 (mod % 5)))
                          (range 1 1001))]
    (reduce #(+ %1 %2) multiples)))
(problem-one)

(defn problem-two
  "Each new term in the Fibonacci sequence is generated by adding the previous two terms.
  By starting with 1 and 2, the first 10 terms will be:
  1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
  By considering the terms in the Fibonacci sequence whose values do not exceed four
   million, find the sum of the even-valued terms.\n"
  []
  (loop [a 0 b 1 sum 0]
    (if (<= (+ a b) 4000000)
      (if (= (mod (+ a b) 2) 0)
        (recur b (+ a b) (+ sum (+ a b)))
        (recur b (+ a b) sum))
      sum)))
(problem-two)

(defn problem-three
  "The prime factors of 13195 are 5, 7, 13 and 29.
  What is the largest prime factor of the number 600851475143 ?"
  []
  1)

(problem-three)