;; Dane Johnson
;; March 14 2018
;; This program seeks to replicate the ID3 method
(ns weka-i3-clone.core
  (:require [clojure.string :refer [split]])
  (:gen-class))

(defrecord Attribute [id vals])
(defrecord Arff [attributes data])

(defn create-attribute
  [line]
  (->Attribute (second (re-seq #"\w+" line))
               (->> (re-find #"\{(.*)\}" line)
                    second
                    (re-seq #"[^\s,]+"))))
(defn create-data
  [line]
  (split line #","))

(defn read-arff-file
  [stream]
  (->Arff (map create-attribute (re-seq #"@attribute.*" stream))
          (->> (re-find #"@data\r?\n((.*\r?\n)+)" stream)
                second
                (re-seq #"(\S*)\r?\n")
                (map second)
                (map create-data))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (-> (read-arff-file (slurp (first args)))
      (prn)))
