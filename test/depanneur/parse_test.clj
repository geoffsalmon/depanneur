(ns depanneur.parse-test
  (:use clojure.test)
  (:require [depanneur [parse :as parse]]))

(def p parse/parse)

(deftest parse
  (are [in out] (= (p in) out)
       "" []
       "notbf" []
       "<>+-,." [\< \> \+ \- \, \.]
       "mix<in>+  - other ,char2s.  " [\< \> \+ \- \, \.]
       "[]" [[]]
       "[[]]" [[[]]]
       "<<[..,[+]]" [\< \< [\. \. \, [\+]]]))

(deftest badp
  (is (thrown? IllegalArgumentException (p "[")))
  (is (thrown? IllegalArgumentException (p "]")))
  (is (thrown? IllegalArgumentException (p "[[[]")))
  (is (thrown? IllegalArgumentException (p "[]]]"))))
