;; Dane Johnson
;; March 14 2018
;; This program seeks to replicate the ID3 method
(ns weka-i3-clone.core
  (:require [clojure.string :refer [split]])
  (:gen-class))

;; An Arff holds attributes and data
(defrecord Arff [attributes test-attribute test-values data])
(defn arff? [x] (instance? Arff x))

(defn create-attribute
  "Creates an attribute from a string"
  [line]
  [(second (re-seq #"[\w-]+" line))
   (->> (re-find #"\{(.*)\}" line)
        second
        (re-seq #"[^\s,]+")
        set)])

(defn create-data
  "Creates a data vector from a string"
  [attributes line]
  (into (hash-map) (map vector (map first attributes) (split line #","))))

(defn read-arff-file
  "Reads a file into an Arff"
  [stream]
  (let [attributes (mapv create-attribute (re-seq #"@attribute.*" stream))]
    (->Arff (into {} (pop attributes))
            (first (peek attributes))
            (second (peek attributes))
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

(defn info
  "Calculates the amount of information in the table"
  [^Arff D]
  (->> (:data D)
       (map #(get % (:test-attribute D)))
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
                     (= (get % (:test-attribute D)) test-value)))
       (count)))

(defn djoverd*infodj
  [^Arff D attribute value]
  (if-not (zero? (d D attribute value))
    (* (reduce + (map #(nlgn (/ (dj D % attribute value)
                                (d D attribute value))) (:test-values D)))
       (/ (d D attribute value)
          (count (:data D)))
       -1)
    0))

(defn info-given
  "Calculates information requirement for a given attribute"
  [^Arff D attribute]
  (reduce + (map (partial djoverd*infodj D attribute) (get (:attributes D) attribute))))

(defn gain
  [^Arff D attribute]
  (- (info D) (info-given D attribute)))

(defn best-identifier
  [^Arff D]
  (apply max-key #(gain D (key %)) (:attributes D)))

(defn same-class?
  [^Arff D]
  (if (every? #{(get (first (:data D)) (:test-attribute D))}
              (map #(get % (:test-attribute D)) (:data D)))
    (get (first (:data D)) (:test-attribute D))
    false))

(defn partition-on-value
  [^Arff D attribute value]
  (-> D
      (update :attributes dissoc attribute)
      (update :data (fn [data] (filter #(= (get % attribute) value) data)))
      (update :data (fn [data] (map #(dissoc % attribute) data)))))

(defn i3
  [^Arff D]
  (cond
    (same-class? D) (same-class? D)
    :default (let [[best-attribute best-values] (best-identifier D)]
               (map #(vector best-attribute
                             %
                             (i3 (partition-on-value D best-attribute %))) best-values))))
(defn pprint-i3
  "Pretty-prints i3 output"
  ([i3-out] (pprint-i3 i3-out 0))
  ([i3-out level]
   (doseq [[attribute value rule] i3-out]
     (dotimes [n level]
       (print "|\t"))
     (print (str attribute " = " value))
     (if (string? rule)
       (println (str ": " rule))
       (do
         (println)
         (pprint-i3 rule (inc level)))))))

(defn -main
  "Reads an arff file into an arff object"
  [& args]
  (let [file (do (print "Please enter name of the data file:\t") (flush) (read-line))
        D (read-arff-file (slurp file))]
    (println (str (inc (count (:attributes D))) " attributes"))
    (println (str (count (:data D)) " examples"))
    (println) (println)
    (pprint-i3 (i3 D))
    (println) (println) (println)
    (print "Press ENTER to continue:") (flush) (read-line)))
