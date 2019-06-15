(defpackage #:app/widgets/spinner
  (:use #:cl)
  (:import-from #:weblocks/widget
                #:get-html-tag
                #:render
                #:defwidget)
  (:import-from #:weblocks/dependencies
                #:make-dependency
                #:get-dependencies)
  (:import-from #:weblocks/html
                #:with-html)
  (:export
   #:make-spinner
   #:spinner))
(in-package app/widgets/spinner)


(defwidget spinner ()
  ())


(defparameter *dependencies*
  nil)


(defmethod get-dependencies ((widget spinner))
  (append (call-next-method)
          *dependencies*))


(defmethod render ((widget spinner))
  (with-html
    (:img :src "/static/gear.gif")))


(defmethod get-html-tag ((widget spinner))
  :span)

(defun make-spinner ()
  (let ((path (asdf:system-relative-pathname :app #P"src/widgets/gear.gif")))

    (weblocks/server:serve-static-file "/static/gear.gif"
                                       path
                                       :content-type "image/gif")
    (make-instance 'spinner)))
