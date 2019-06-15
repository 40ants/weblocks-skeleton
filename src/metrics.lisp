(defpackage #:app/metrics
  (:use #:cl)
  (:import-from #:app/stats
                #:add-counter
                #:add-gauge)
  (:import-from #:mito
                #:count-dao)
  (:import-from #:weblocks-auth/models
                #:user)
  (:import-from #:function-cache
                #:defcached)
  (:export #:initialize))
(in-package app/metrics)


(defvar *ttl* (* 5 60)
  "To not load a database, we'll not count these metrics every time when Prometheus checks the values.")


(defcached (get-number-of-users :timeout *ttl*) ()
  (count-dao 'user))


(defun initialize ()
  (app/stats:initialize)
  
  (add-counter :checks-processed "A number of processed checks")
  
  (add-gauge :users-count "A number of projects"
             'get-number-of-users)
  (values))
