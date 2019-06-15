(defpackage #:app/widgets/landing
  (:use #:cl #:cl-syntax)
  (:import-from #:str
                #:concat)
  (:import-from #:weblocks/widget
                #:defwidget
                #:render)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks/page
                #:get-title)
  (:import-from #:metatilities
                #:format-date)
  (:import-from #:local-time
                #:timestamp-to-universal)
  (:import-from #:app/variables
                #:get-app-name
                #:get-app-motto)
  (:export
   #:make-landing-widget))
(in-package app/widgets/landing)

(use-syntax :interpol-syntax)

(defwidget landing-widget ()
  ())


(defun make-landing-widget ()
  (make-instance 'landing-widget))


(defmethod render ((widget landing-widget))
  (setf (get-title)
        #?"${(get-app-name)} - ${(get-app-motto)}")

  (let* ((name (get-app-name))
         (downcased-name (string-downcase name)))
    (with-html
      ;; Taken from https://simonwhitaker.github.io/github-fork-ribbon-css/
      (:a :class "github-fork-ribbon left-top"
          :href #?"https://github.com/${downcased-name}/${downcased-name}"
          :data-ribbon "Fork me on GitHub"
          :title "Fork me on GitHub"
          "Fork me on GitHub")

      (:p #?"${name} will help you to make more impact to Opensource.")

      (:h3 "PLACEHOLDER"))))


(defmethod weblocks/dependencies:get-dependencies ((widget landing-widget))
  (append
   (list
    (weblocks-lass:make-dependency
      `(.versions-list
        ((:or .version-cell .timestamp-cell)
         :vertical-align top
         :white-space nowrap
         :text-align left)
        (.timestamp-cell
         :width 100%)
        (.changelog-cell
         :padding-left 1.7em
         (.changelog
          :margin 0
          (p :margin 0)
          (.diff
           :margin 0
           (dt :margin 0
               :margin-right 0.6em
               :display inline-block)
           (dd :margin 0
               :display inline-block)))
         (.and-more :margin 0)))))
   (call-next-method)))
