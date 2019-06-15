(defpackage #:app/cron
  (:use #:cl)
  (:import-from #:cl-cron)
  (:import-from #:log4cl)
  (:import-from #:slynk)
  (:import-from #:app/utils
                #:make-request-id)
  (:import-from #:log4cl-json
                #:with-log-unhandled
                #:with-fields)
  (:import-from #:app/db
                #:with-connection)
  (:import-from #:local-time
                #:now)
  (:import-from #:mito
                #:object-updated-at)
  (:import-from #:local-time-duration
                #:duration-maximum
                #:duration-as
                #:duration-
                #:timestamp-difference
                #:duration-minimum)
  (:export
   #:list-cron-jobs
   #:delete-all-cron-jobs
   #:setup
   #:stop
   #:start
   #:*cron-jobs-hash*
   #:get-time-of-the-next-check))
(in-package app/cron)


(defmacro deftask (name (&key (need-connection t)) &body body)
  "Defines a cron task function with following properties:

   * Each call has it's own unique id in log messages.
   * Unhandled exceptions will be logged along with their tracebacks.
   * A new database connection and trasaction will be started for each execution."
  
  (let ((body (if need-connection
                  `(with-connection ()
                     ,@body)
                  `(progn ,@body))))
    `(defun ,name ()
       (with-fields (:request-id (make-request-id))
         (log:debug "Running cron task" ',name)
         (handler-bind ((error (lambda (condition)
                                 (if slynk-api:*emacs-connection*
                                     (invoke-debugger condition)
                                     (return-from ,name nil)))))
           (with-log-unhandled ()
             ,body))
         (log:debug "Cron task is done" ',name)))))


(deftask do-nothing ()
  (log:info "Just example")
  (sleep 1))


(defun list-cron-jobs ()
  (loop for key being the hash-key of cl-cron::*cron-jobs-hash*
        collect key))


(defvar *stopped* nil
  "This flag will be set by `stop' function to true and used by `setup' to not recreate cron jobs on server restart if they were stopeed manually.")


(defun delete-all-cron-jobs ()
  (loop for key in (list-cron-jobs)
        do (cl-cron:delete-cron-job key)))


(defun setup ()
  "Creates all cron jobs. Does not start them. Call start for that."
  (log:debug "Creating cron jobs")
  ;; Run every 5 minutes
  (cl-cron:make-cron-job 'do-nothing
                         :hash-key 'do-nothing
                         :step-min 5))


(defun start (&key force)
  "Starts all created cron jobs."
  (cond
    ((and *stopped*
          (not force))
     (log:warn "Cron was stopped manually, add :force t to start it agaun"))
    (t 
     (log:debug "Starting cron thread")
     (cl-cron:start-cron)
     (setf *stopped* nil))))


(defun stop ()
  "Stops cron jobs execution."
  (log:debug "Stopping cron thread")
  (cl-cron:stop-cron)
  (setf *stopped* t))


;; Here we patch this function and replace it because
;; original tries to write into a file cl-cron.log
(defun cl-cron:log-cron-message (message &optional (type "error"))
  (if (string-equal type "error")
      (log:error message)
      (log:info message)))


