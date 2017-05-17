(ns forclojure-solns.core-test
  (:require [clojure.test :refer :all]
            [forclojure-solns.core :refer :all]))

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))

(deftest test-rotate-seq
  (testing "rotate-seq"
    (is (= '(3 4 5 1 2) (rotate-seq 2 [1 2 3 4 5])))
    (is (= '(4 5 1 2 3) (rotate-seq -2 [1 2 3 4 5])))
    (is (= '(2 3 4 5 1) (rotate-seq 6 [1 2 3 4 5]))))
  )


(deftest test-rev-interleave
  (testing "rev-interleave"
    (is (= '((1 3 5) (2 4 6)) (rev-interleave [1 2 3 4 5 6] 2)))
    (is (= '((0 3 6) (1 4 7) (2 5 8)) (rev-interleave (range 9) 3)))
  ))

