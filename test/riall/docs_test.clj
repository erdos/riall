(ns riall.docs-test
  (:require [clojure.test :refer [deftest testing is are]]
            [riall.config]
            [riall.parse]))

(deftest test-readme-1
  (def lines (keep riall.parse/parse-config (clojure.string/split-lines (slurp "README.md"))))
  (println :! lines)
  (testing "README file mentions all configuration options"


  )
  
  (testing "All config options mentioned in README are valid"
    (doseq [line lines]
      (is (riall.config/config-exists? line)))))
