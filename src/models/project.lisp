(defpackage #:app/models/project
  (:use #:cl)
  (:import-from #:log4cl-json)
  (:import-from #:mito
                #:includes
                #:save-dao
                #:delete-dao
                #:select-dao
                #:create-dao)
  (:import-from #:jonathan
                #:to-json)
  (:import-from #:dexador)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:cl-arrows
                #:->)
  (:import-from #:cl-strings
                #:split)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:sxql
                #:order-by
                #:limit
                #:where)
  (:import-from #:weblocks-auth/models
                #:get-current-user)
  (:import-from #:app/db
                #:with-transaction)
  (:import-from #:app/utils
                #:update-plist
                #:make-update-diff)
  ;; (:import-from #:quickdist
  ;;               #:get-path
  ;;               #:get-filename
  ;;               #:get-dependencies
  ;;               #:get-system-files
  ;;               #:get-project-prefix
  ;;               #:get-content-sha1
  ;;               #:get-md5sum
  ;;               #:get-file-size
  ;;               #:get-archive-path
  ;;               #:get-project-url
  ;;               #:get-project-name)
  (:import-from #:app/stats
                #:increment-counter))
(in-package app/models/project)


(defclass project ()
  ((source :col-type (:text)
           :initarg :source
           :reader get-source
           :inflate (lambda (text)
                      (make-keyword (string-upcase text)))
           :deflate #'symbol-name)
   (name :col-type (:text)
         :initarg :name
         :accessor get-name))
  (:unique-keys name)
  (:metaclass mito:dao-table-class))

(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type t)
    (format stream
            "~A name=~A enabled=~A"
            (get-source project)
            (get-name project)
            (is-enabled-p project))))


(defun make-github-project (user-or-org project)
  (let ((name (concatenate 'string
                           user-or-org
                           "/"
                           project))
        (description (or (ignore-errors
                          ;; We ignore errors here, because
                          ;; description is not very important
                          ;; and can be updated later,
                          ;; but we definitely want to log these
                          ;; errors, to not miss some system problems.
                          (log4cl-json:with-log-unhandled ()
                            (%github-get-description user-or-org
                                                     project)))
                         "")))
    (create-dao 'project
                :source :github
                :name name)))


(defun get-all-projects (&key only-enabled)
  (if only-enabled
      (select-dao 'project
        (where :enabled))
      (select-dao 'project)))
