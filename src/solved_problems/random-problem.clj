;
; Complete the 'fizzBuzz' function below.
;
; The function accepts INTEGER n as parameter.
;

(defn fizzBuzz [n]
  (loop [current-number 1]
    (when (<= current-number n)
      (cond (and (zero? (mod current-number 3)) (not= (mod current-number 5) 0))
            (println "Fizz")
            (and (zero? (mod current-number 5)) (not= (mod current-number 3) 0))
            (println "Buzz")
            (and (zero? (mod current-number 5)) (zero? (mod current-number 3)))
            (println "FizzBuzz")
            :else
            (println current-number)) (recur (inc current-number)))))

(defn roundAmount [originalAmount]
  (let [number-list (vec (str originalAmount))
        last-two-digits (Integer/parseInt (clojure.string/join (take-last 2 number-list)))
        list-size (count number-list)
        list-subvected (subvec number-list 0 (- list-size 2))]
    (if (>= originalAmount 50)
      (cond (and (>= last-two-digits 1) (<= last-two-digits 24))
            (Integer/parseInt (clojure.string/join (conj list-subvected "00")))
            (and (>= last-two-digits 25) (<= last-two-digits 49))
            (Integer/parseInt (clojure.string/join (conj list-subvected "50")))
            (and (>= last-two-digits 51) (<= last-two-digits 74))
            (Integer/parseInt (clojure.string/join (conj list-subvected "50")))
            (and (>= last-two-digits 75) (<= last-two-digits 99))
            (+ originalAmount (- 100 last-two-digits))
            :else
            originalAmount)
      50)))

1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz