(ns pcclj.core)

(defrecord Parser   [pfn])
(defrecord PSuccess [res rst])
(defrecord PError   [err])

(defn pchar*
  "Parses a single character in an input string."
  [chr input]
  (if (= chr (first input))
    (PSuccess. (conj '() (subs input 0 1)) (subs input 1))
    (PError.   (str "Expecting " chr ", found " (first input)))))

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
                       PSuccess (PSuccess. (conj '() (:res r2) (:res r1)) (:rst r2))
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
         PSuccess (PSuccess. (f (:res r)) (:rst r))
         r)))))

;; ---

(defn return-p
  [val]
  (Parser. (fn [input] (PSuccess. val input))))

(defn apply-p
  [fp p]
  (map-p (fn [[f x & _]] (f x)) (and-then fp p)))

(defn lift2
  "'Lifts' a two-argument function into a parser.

  This assumes `f` is a two-argument function, and will fail badly if it is not.
  `x` and `y` are parsers that will be used to parse the arguments to the
  function.

  E.g. to lift `clojure.string/starts-with?`:
    `(def starts-with-p (partial lift2 clojure.string/starts-with?))`

  See `cons-p` for example usage."
  [f x y]
  (apply-p
   (apply-p
    (return-p (fn [arg] (partial f arg))) x)
   y))

;; ---

(def cons-p (partial lift2 cons))

(defn sequence-p
  "Turns a sequence of parsers into a parser."
  [[parser & rst]]
  (if (nil? parser) (return-p [])
    (cons-p parser (sequence-p rst))))

;; ---

(defn as-str-p
  "A helper function that flattens the result of the parser into a string
  instead of a sequence"
  [parser]
  (map-p (comp clojure.string/join flatten) parser))

(defn pstring
  "Turns a string into a parser for that string."
  [^String str]
  (as-str-p (sequence-p (map pchar (seq str)))))

(defn zero-or-more
  [parser input]
  (let [result (run-p parser input)]
    (condp instance? result
      PError   [[] input]
      PSuccess (let [inner-result (zero-or-more parser (:rst result))]
                 [(concat (:res result) (first inner-result)) (second inner-result)]))))

(defn many
  [parser]
  (Parser.
   (fn [input]
     (let [result (zero-or-more parser input)]
       (PSuccess. (first result) (second result))))))

(defn many1
  [parser]
  (Parser.
   (fn [input]
     (let [first-result (run-p parser input)]
       (condp instance? first-result
         PError   first-result
         PSuccess (let [second-result (zero-or-more parser (.rst first-result))
                        values (concat (.res first-result) (first second-result))]
                    (PSuccess. values (second second-result))))))))

(def digit (any-of (clojure.string/join (range 0 10))))
(def digits (many1 digit))

(def pint
  (let [to-int (comp (fn [s] (Integer/parseInt s)) clojure.string/join)]
    (map-p to-int digits)))
