(ns pcclj.core
  (:require [clojure.spec.alpha :as s]
            [clojure.core.match :as m]))

(defn aparser
  [input]
  (m/match (first input)
           \a [true (subs input 1)]
           :else [false input]))

;; ---



(defn pchar-2
  [chr input]
  (m/match (first input)
           chr   [true (subs input 0 1) (subs input 1)]
           :else [false input (str "Expecting " chr ", found " (first input))]))

;; ---

(defn pchar
  [chr]
  (partial pchar-2 chr))

;; ---

(defn and-then
  [p1 p2]
  (fn [input]
    (m/match (p1 input)
             [false _ _]  (p1 input)
             [true c1 r1] (m/match (p2 r1)
                                [true  c2 r2] [true (cons c1 c2) (last (p2 r1))]
                                [false _  r2] [false input r2]))))

(defn or-else
  [p1 p2]
  (fn [input]
    (let [r1 (p1 input)]
      (case (first r1)
        true  r1
        false (p2 input)))))

(defn choice [parsers] (reduce or-else parsers))

(defn any-of
  [chars]
  (let [parsers (map pchar chars)]
    (choice parsers)))

;; ---

(defn mapP
  [f parser]
  (fn [input]
    (let [parsed (parser input)]
      (if (true? (first parsed))
        [true (f (second parsed)) (last parsed)]
        [false input (last parsed)]))))

;; ---

(defn returnP
  [val]
  (fn [input] [true val input]))
