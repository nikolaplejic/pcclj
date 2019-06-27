(ns pcclj.core-test
  (:require [clojure.test :refer :all]
            [pcclj.core :refer :all])
  (:import [pcclj.core PSuccess PError]))

(deftest pchar-star-test
  (let [p (pchar* \a "asd")]
    (is (instance? PSuccess p)))
  (let [p (pchar* \a nil)]
    (is (instance? PError p)))
  (let [p (pchar* \a "Zsd")]
    (is (instance? PError p))))

(deftest pchar-test
  (let [parse-a (pchar \a)
        p (run-p parse-a "asd")]
    (is (instance? PSuccess p))))

(deftest and-then-test
  (let [parse-a (pchar \a)
        parse-b (pchar \b)
        parse-ab (and-then parse-a parse-b)
        s1 (run-p parse-ab "abc")
        s2 (run-p parse-ab "acc")
        s3 (run-p parse-ab "bcc")
        s4 (run-p parse-ab "")]
    (is (instance? PSuccess s1))
    (is (instance? PError s2))))

(deftest or-else-test
  (let [parse-a (pchar \a)
        parse-b (pchar \b)
        a-or-b (or-else parse-a parse-b)
        s1 (run-p a-or-b "azz")
        s2 (run-p a-or-b "bzz")
        s3 (run-p a-or-b "zzz")
        s4 (run-p a-or-b "")]
    (is (instance? PSuccess s1))
    (is (instance? PSuccess s2))
    (is (instance? PError s3))
    (is (instance? PError s4))))

(deftest choice-test
  (let [parsers [(pchar \a) (pchar \b) (pchar \c)]
        choice-parser (choice parsers)
        s1 (run-p choice-parser "azz")
        s2 (run-p choice-parser "bzz")
        s3 (run-p choice-parser "czz")
        s4 (run-p choice-parser "zzz")]
    (is (instance? PSuccess s1))
    (is (instance? PSuccess s2))
    (is (instance? PSuccess s3))
    (is (instance? PError s4))))

(deftest any-of-test
  (let [chars "abcdefg"
        parser (any-of (char-array chars))
        s1 (run-p parser "abcdefg")
        s2 (run-p parser "bcdefga")
        s3 (run-p parser "zabcdef")]
    (is (instance? PSuccess s1))
    (is (instance? PSuccess s2))
    (is (instance? PError s3))))

(deftest map-test
  (let [parse-digit (any-of "0123456789")
        four-digits (and-then parse-digit
                              (and-then parse-digit
                                        (and-then parse-digit parse-digit)))
        parser (map-p (fn [a] (clojure.string/join (flatten a))) four-digits)
        int-parser (map-p (fn [i] (Integer/parseInt i)) parser)
        s1 (run-p parser "012345")
        s2 (run-p parser "01a345")
        s3 (run-p int-parser "12345")]
    (is (instance? PSuccess s1))
    (is (= "0123" (:res s1)))
    (is (instance? PError s2))
    (is (= 1234 (:res s3)))))

(deftest apply-test
  (let [flatten-p (apply-p (return-p flatten)
                           (and-then (pchar \a) (pchar \b)))
        f-reverse-p (apply-p (return-p (comp flatten reverse))
                             (and-then (pchar \a) (pchar \b)))
        s1 (run-p flatten-p "abcd")
        s2 (run-p f-reverse-p "abcd")]
    (is (instance? PSuccess s1))
    (is (= '("a" "b") (:res s1)))
    (is (instance? PSuccess s2))
    (is (= '("b" "a") (:res s2)))))

(deftest sequence-test
  (let [ps [(pchar \a) (pchar \b) (pchar \c)]
        s1 (run-p (sequence-p ps) "abcde")
        s2 (run-p (sequence-p ps) "edcba")]
    (is (instance? PSuccess s1))
    (is (instance? PError s2))))

(deftest pstring-test
  (let [s "The quick brown fox"
        t1 (run-p (pstring s) "The quick brown fox jumps over the lazy dog")
        t2 (run-p (pstring s) "The quick brown dog jumps over the lazy fox")]
    (is (instance? PSuccess t1))
    (is (instance? PError t2))))

(deftest many-test
  (let [s "AAAAB"
        t1 (run-p (many (pchar \A)) s)
        t2 (run-p (many (pchar \B)) s)
        t3 (run-p (many (pstring "AA")) s)]
    (is (instance? PSuccess t1))
    (is (not= [] (:res t1)))
    (is (instance? PSuccess t2))
    (is (= [] (:res t2)))
    (is (instance? PSuccess t3))
    (is (not= [] (:res t3)))))

(deftest many1-test
  (let [s "AAAAAB"
        t1 (run-p (many1 (pchar \A)) s)
        t2 (run-p (many1 (pchar \B)) s)
        t3 (run-p (many1 (pstring "AA")) s)]
    (is (instance? PSuccess t1))
    (is (instance? PError t2))
    (is (instance? PSuccess t3))))

(deftest pint-test
  (let [s1 "123ABC"
        s2 "ABC123"
        s3 "-123c"
        t1 (run-p pint s1)
        t2 (run-p pint s2)
        t3 (run-p pint s3)]
    (is (instance? PSuccess t1))
    (is (= 123 (:res t1)))
    (is (instance? PError t2))
    (is (instance? PSuccess t3))
    (is (= -123 (:res t3)))))
