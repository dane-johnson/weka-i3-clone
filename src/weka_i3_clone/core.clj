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
  [attributes line]
  (into (hash-map) (map vector (map :id attributes) (split line #","))))

(defn read-arff-file
  "Reads a file into an Arff"
  [stream]
  (let [attributes (mapv create-attribute (re-seq #"@attribute.*" stream))]
    (->Arff attributes
            (->> (re-find #"@data\r?\n((.*\r?\n)+)" stream)
                 second
                 (re-seq #"([^%\s]*)\r?\n")
                 (map second)
                 (map #(create-data attributes %))))))
(defn nlgn
  "Given n, return n*lg(n)"
  [n]
  (* n (/ (Math/log n) (Math/log 2))))

(defn -main
  "Reads an arff file into an arff object"
  [& args]
  (-> (read-arff-file (slurp (first args)))
      (prn)))
