(defpackage #:app/models/migration
  (:use #:cl)
  (:import-from #:weblocks-auth/models)
  (:export
   #:migrate))
(in-package app/models/migration)


(defun migrate ()
  (app/db:with-connection ()
    (mito:migrate "./db/")))
