;; Dane Johnson
;; March 14 2018
;; This program seeks to replicate the ID3 method
(ns weka-i3-clone.core
  (:require [clojure.string :refer [split]])
  (:gen-class))

;; An Attribute holds an id and values
(defrecord Attribute [id vals])
;; An Arff holds attributes and data
(defrecord Arff [attributes data])

(defn create-attribute
  "Creates an attribute from a string"
  [line]
  (->Attribute (second (re-seq #"\w+" line))
               (->> (re-find #"\{(.*)\}" line)
                    second
                    (re-seq #"[^\s,]+")
                    set)))

(defn create-data
  "Creates a data vector from a string"
  [line]
  (split line #","))

(defn read-arff-file
  "Reads a file into an Arff"
  [stream]
  (->Arff (map create-attribute (re-seq #"@attribute.*" stream))
          (->> (re-find #"@data\r?\n((.*\r?\n)+)" stream)
                second
                (re-seq #"([^%\s]*)\r?\n")
                (map second)
                (map create-data))))


(defn -main
  "Reads an arff file into an arff object"
  [& args]
  (-> (read-arff-file (slurp (first args)))
      (prn)))
