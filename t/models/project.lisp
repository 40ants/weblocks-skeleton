(defpackage #:app-test/models/project
  (:use #:cl
        #:rove)
  (:import-from #:hamcrest/rove
                #:assert-that
                #:contains
                #:has-type)
  (:import-from #:app-test/utils
                #:with-test-db
                #:with-metrics
                #:with-login))
(in-package app-test/models/project)


(deftest test-example
  (with-test-db
    (with-login ()
      (testing "This is a example of a testcase"
               (ok (= (length (list 1 2 3))
                      1))))))
