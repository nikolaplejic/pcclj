(ns pcclj.core
  (:require [clojure.spec.alpha :as s]
            [clojure.core.match :as m]))

(defrecord Parser   [pfn])
(defrecord PSuccess [chr rst])
(defrecord PError   [err])

(defn pchar*
  "Parses a single character in an input string."
  [chr input]
  (m/match (first input)
           chr   (PSuccess. (conj '() (subs input 0 1)) (subs input 1))
           :else (PError.   (str "Expecting " chr ", found " (first input)))))

;; ---

(defn pchar
  "Curried version of pchar-2"
  [chr]
  (Parser. (partial pchar* chr)))

(defn run-p
  [p input]
  ((:pfn p) input))

;; ---

(defn and-then
  [p1 p2]
  (Parser.
    (fn [input]
      (let [r1 (run-p p1 input)]
        (condp instance? r1
          PSuccess (let [r2 (run-p p2 (:rst r1))]
                     (condp instance? r2
                       PSuccess (PSuccess. (conj (:chr r2) (:chr r1)) (:rst r2))
                       PError   r2))
          PError   r1)))))

(defn or-else
  [p1 p2]
  (Parser.
   (fn [input]
     (let [r1 (run-p p1 input)]
       (condp instance? r1
         PSuccess r1
         PError   (run-p p2 input))))))

(defn choice [parsers] (reduce or-else parsers))

(defn any-of
  [chars]
  (let [parsers (map pchar chars)]
    (choice parsers)))

;; ---

(defn map-p
  [f parser]
  (Parser.
   (fn [input]
     (let [r (run-p parser input)]
       (condp instance? r
         PSuccess (PSuccess. (f (:chr r)) (:rst r))
         r)))))

;; ---

(defn return-p
  [val]
  (Parser. (fn [input] (PSuccess. val input))))
