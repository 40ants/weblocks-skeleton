(defpackage #:app-test/db
  (:use #:cl
        #:rove)
  (:import-from #:app/db
                #:with-connection)
  (:import-from #:alexandria
                #:assoc-value)
  (:import-from #:app-test/utils
                #:with-test-db))
(in-package app-test/db)


(deftest test-with-connection
  (with-test-db ()
    (with-connection ()
      (ok (= (first
              (assoc-value (mito:retrieve-by-sql "select 1 as value")
                           :value))
             1)))
    (ok (= (first
            (assoc-value (mito:retrieve-by-sql "select 2 as value")
                         :value))
           2))))
