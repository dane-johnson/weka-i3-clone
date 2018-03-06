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
  (if-not (zero? n)
    (* n (/ (Math/log n) (Math/log 2)))
    0))

(defn test-attribute
  "The last attribute is the one we'd like to test for"
  [^Arff D]
  (:id (peek (:attributes D))))

(defn test-values
  "Gets the values of the attribute being tested for"
  [^Arff D]
  (:vals (peek (:attributes D))))

(defn info
  "Calculates the amount of information in the table"
  [^Arff D]
  (->> (:data D)
       (map #(get % (test-attribute D)))
       frequencies
       vals
       (map #(/ % (count (:data D))))
       (map nlgn)
       (apply +)
       -))

(defn d
  "Finds the number of attributes that hold a given value"
  [^Arff D attribute value]
  (->> (:data D)
       (filter #(= (get % attribute) value))
       count))

(defn dj
  "Finds the number of attributes that hold a given value while the test attribute
  holds a given value"
  [^Arff D test-value attribute value]
  (->> (:data D)
       (filter #(and (= (get % attribute) value)
                     (= (get % (test-attribute D)) test-value)))
       (count)))

(defn djoverd*infodj
  [^Arff D attribute value]
  (* (reduce + (map #(nlgn (/ (dj D % attribute value)
                              (d D attribute value))) (test-values D)))
     (/ (d D attribute value)
        (count (:data D)))
     -1))

;; (defn info-given
;;   "Calculates information requirement for a given attribute"
;;   [^Arff D attribute]
;;   (let [attrs (attribute (:attributes D))]
;;     (-> (for [attr attrs]))))

(defn -main
  "Reads an arff file into an arff object"
  [& args]
  (-> (read-arff-file (slurp (first args)))
      (prn)))
