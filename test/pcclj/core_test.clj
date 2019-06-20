(ns pcclj.core-test
  (:require [clojure.test :refer :all]
            [pcclj.core :refer :all]))

(deftest aparser-test
  (let [p (aparser "")]
    (is (= false (first p))))
  (let [p (aparser "asd")]
    (is (= true (first p)))
    (is (= "sd" (second p)))))

(deftest pchar-2-test
  (let [p (pchar-2 \a "asd")]
    (is (= true (first p))))
  (let [p (pchar-2 \a nil)]
    (is (= false (first p))))
  (let [p (pchar-2 \a "Zsd")]
    (is (= false (first p)))))

(deftest pchar-test
  (let [parse-a (pchar \a)
        p (parse-a "asd")]
    (is (= true (first p)))))

(deftest and-then-test
  (let [parse-a (pchar \a)
        parse-b (pchar \b)
        parse-ab (and-then parse-a parse-b)
        s1 (parse-ab "abc")
        s2 (parse-ab "acc")
        s3 (parse-ab "bcc")
        s4 (parse-ab "")]
    (is (= true (first s1)))
    (is (= "c" (last s1)))
    (is (= false (first s2)))
    (is (= "acc" (second s2)))
    (is (= false (first s3)))
    (is (= "bcc" (second s3)))
    (is (= false (first s4)))))

(deftest or-else-test
  (let [parse-a (pchar \a)
        parse-b (pchar \b)
        a-or-b (or-else parse-a parse-b)
        s1 (a-or-b "azz")
        s2 (a-or-b "bzz")
        s3 (a-or-b "zzz")
        s4 (a-or-b "")]
    (is (= true (first s1)))
    (is (= true (first s2)))
    (is (= false (first s3)))
    (is (= false (first s4)))))

(deftest choice-test
  (let [parsers [(pchar \a) (pchar \b) (pchar \c)]
        choice-parser (choice parsers)
        s1 (choice-parser "azz")
        s2 (choice-parser "bzz")
        s3 (choice-parser "czz")
        s4 (choice-parser "zzz")]
    (is (= true (first s1)))
    (is (= true (first s2)))
    (is (= true (first s3)))
    (is (= false (first s4)))))

(deftest any-of-test
  (let [chars "abcdefg"
        parser (any-of (char-array chars))
        s1 (parser "abcdefg")
        s2 (parser "bcdefga")
        s3 (parser "zabcdef")]
    (is (= true (first s1)))
    (is (= true (first s2)))
    (is (= false (first s3)))))

(deftest map-test
  (let [parse-digit (any-of "0123456789")
        four-digits (and-then parse-digit
                              (and-then parse-digit
                                        (and-then parse-digit parse-digit)))
        parser (mapP clojure.string/join four-digits)
        int-parser (mapP (fn [i] (Integer/parseInt i)) parser)
        s1 (parser "012345")
        s2 (parser "01a345")
        s3 (int-parser "12345")]
    (is (= true (first s1)))
    (is (= "0123" (second s1)))
    (is (= false (first s2)))
    (is (= 1234 (second s3)))))
