(defpackage #:app/widgets/login-menu
  (:use #:cl #:cl-syntax)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/widget
                #:defwidget)
  (:import-from #:weblocks/html
                #:with-html)
  (:import-from #:weblocks-auth/models
                #:get-current-user
                #:get-nickname
                #:anonymous-p)
  (:import-from #:weblocks/response
                #:add-retpath-to)
  (:import-from #:app/variables
                #:get-app-name)

  (:export
   #:make-login-menu))
(in-package app/widgets/login-menu)


(use-syntax :interpol-syntax)


(defwidget login-menu ()
  ())


(defmethod weblocks/widget:render ((widget login-menu))
  (let* ((user (get-current-user))
        (name (get-app-name))
        (downcased-name (string-downcase name))
        (feedback-url #?"https://github.com/${downcased-name}/${downcased-name}/issues"))
    (if (anonymous-p user)
        (weblocks/html:with-html
          (:div :class "login-link"
                (:a :href feedback-url
                    "Leave feedback")
                (:a :href (add-retpath-to "/login") "Log In")))
        
        (with-html
          (:ul :class "dropdown menu"
               :data-dropdown-menu t
               (:li (:a :href "#"
                        (get-nickname user))
                    (:ul :class "menu"
                         (:li (:a :href feedback-url
                                  "Leave feedback"))
                         (:li (:a :href (add-retpath-to "/logout")
                                  "Logout")))))))))


(defun make-login-menu ()
  (make-instance 'login-menu))


(defparameter *dependencies*
  (list (weblocks-lass:make-dependency
          '(.login-menu
            :color red
            :position absolute
            :top 0
            :right 0
            (.login-link
             (a :margin-right 0.5rem))))))


(defmethod weblocks/dependencies:get-dependencies ((widget login-menu))
  (append *dependencies*
          (call-next-method)))
