(defpackage #:app/widgets/main
  (:use #:cl)
  (:import-from #:weblocks-navigation-widget
                #:defwidget)
  (:import-from #:weblocks/html
                #:with-html-string)
  
  ;; Just depdendencies
  (:import-from #:log)
  (:import-from #:weblocks/app)
  (:import-from #:weblocks/widget)
  (:import-from #:weblocks/page)
  (:import-from #:app/widgets/landing
                #:make-landing-widget)
  (:import-from #:weblocks-auth/core
                #:make-logout-processor
                #:make-login-processor)
  (:import-from #:app/widgets/login-menu
                #:make-login-menu)
  (:import-from #:app/widgets/not-found
                #:page-not-found)
  (:export #:make-main-widget))
(in-package app/widgets/main)


(defwidget main-widget
    ("/"
     (make-landing-widget))
  ("/login"
   (make-login-processor))
  ("/logout"
   (make-logout-processor))
  (t
   (page-not-found)))


(defmethod weblocks/widget:render ((widget main-widget))
  (weblocks/widget:render
   (make-login-menu))

  (call-next-method))
