(defpackage #:app/mail
  (:use #:cl)
  (:import-from #:mailgun)
  (:import-from #:weblocks/response
                #:make-uri)
  ;; (:import-from #:mito-email-auth/models
  ;;               #:get-code
  ;;               #:get-email)
  (:export
   #:send-login-code))
(in-package spp/mail)


;; TODO: remove this code, or move it to weblocks-auth
;; (defun send-login-code (code &key retpath)
;;   (let* ((retpath (when retpath
;;                     (quri:url-encode retpath)))
;;          (url (make-uri
;;                (format nil
;;                        "/login?code=~A~@[&retpath=~A~]"
;;                        (get-code code)
;;                        retpath)))
;;          (email (get-email code)))

;;     (cond
;;       ((and mailgun:*domain*
;;             mailgun:*api-key*)
;;        (log:debug "Sending login code to" email)
       
;;        (mailgun:send ("12Forks <noreply@12forks.com>"
;;                       email
;;                       "The code to log into 12Forks.com")
;;          (:p ("To log into [12Forks.com](~A), follow [this link](~A)."
;;               url
;;               url))
;;          (:p "Hurry up! This link will expire in one hour.")))

;;       (t (log:warn "You didn't set MAILGUN_DOMAIN and MAILGUN_API_KEY env variables. So I am unable to send auth code."
;;                    url)
;;          (weblocks/response:redirect url)))))
