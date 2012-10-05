(ns depanneur
  (:require [depanneur
             [core :as core]
             [parse :as parse]]))

(def parse parse/parse)
(def interpret core/interpret)
