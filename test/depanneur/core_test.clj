(ns depanneur.core-test
  (:use clojure.test)
  (:require depanneur)
  (:import [java.io StringReader StringWriter]))

(defn run
  ([prog]
     (run prog ""))
  ([prog in]
     (let [out (StringWriter.)
           s (depanneur/interpret (depanneur/parse prog)
                                  (StringReader. (or in "")) out)]
       [(.toString out) s])))

(defn st [ptr & data]
  (apply hash-map :ptr ptr data))

(deftest simple
  (are [in out] (= (run in) out)
       "" ["" (st  0)]
       ">" ["" (st 1)]
       ">>" ["" (st 2)]
       ">><" ["" (st 1)]

       "+" ["" (st 0 0 1)]
       "++" ["" (st 0 0 2)]
       "+-+" ["" (st 0 0 1)]
       "+>>+" ["" (st 2 0 1 2 1)]))

(deftest simple-inout
  (are [prog in out] (= (run prog in) out)
       "," "a" ["" (st 0 0 (int \a))]
       ",>,>," "abc" ["" (st 2 0 (int \a) 1 (int \b) 2 (int \c))]
       ",>,>," "a" ["" (st 2 0 (int \a))]
       ",+." "a" ["b" (st 0 0 (int \b))]))

(deftest helloworld
  (is (= (first (run "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")) "Hello World!\n")))
