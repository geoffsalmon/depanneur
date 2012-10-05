(ns depanneur.parse)

(defn- parse-block
  "Parses a given sequence of characters and returns a vector
  containing the characters ><+-., or sub vectors containing those
  characters. The sub vectors represent blocks of brainfuck commands
  between that are between []. They can be nested arbitrarily deep."
  [s depth]
  (loop [s s
         block []]
    (if (seq s)
      (case (first s)
        ;; recurse to parse the sub block
        \[
        (let [[rest-s sub-b] (parse-block (rest s) (inc depth))]
          (recur rest-s (conj block sub-b)))
        ;; return the parsed block
        \]
        (if (> depth 0)
          [(rest s) block]
          (throw (IllegalArgumentException. "Unmatched ]")))
        ;; add the cmd to the block and continue
        (recur (rest s) (conj block (first s))))
      (if (zero? depth)
        [nil block]
        (throw (IllegalArgumentException. "Unmatched ["))))))

(defn parse [s]
  (second (parse-block (filter #{\< \> \+ \- \. \, \[ \]} s) 0)))
