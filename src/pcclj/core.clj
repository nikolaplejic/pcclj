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

;; ---

(defrecord PSome [val])
(defrecord PNone [])

(defn opt
  [p]
  (let [result (map-p (fn [x] (PSome. x)) p)
        none   (return-p (PNone.))]
    (or-else result none)))

;; ---

(def pint
  (let [to-int (fn [[sign nr]]
                 (let [nr (Integer/parseInt (clojure.string/join nr))]
                   (if (instance? PNone sign)
                     nr
                     (* -1 nr))))]
    (map-p to-int (and-then (opt (pchar \-)) digits))))

;; ---

(defn left-p
  "Executes two parsers, and keeps only the result of the first one."
  [p1 p2]
  (let [select-fn (fn [[l r]] l)]
    (map-p select-fn (and-then p1 p2))))

(defn right-p
  "Executes two parsers, and keeps only the result of the second one."
  [p1 p2]
  (let [select-fn (fn [[l r]] r)]
    (map-p select-fn (and-then p1 p2))))

(defn between
  [p1 p2 p3]
  (right-p p1 (left-p p2 p3)))

(defn bind-p
  "Combines a parser-producing function and a parser."
  [f p]
  (Parser.
   (fn [input]
     (let [r1 (run-p p input)]
       (condp instance? r1
         PError   r1
         PSuccess (let [p2 (f (:res r1))]
                    (run-p p2 (:rst r1))))))))

(defn map-p2
  "An alternative implementation of `map-p`."
  [f p]
  (bind-p (fn [input] (return-p (f input))) p))
