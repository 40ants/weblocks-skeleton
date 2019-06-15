(defpackage #:app/server
  (:use #:cl #:cl-syntax)
  (:import-from #:app/metrics)
  (:import-from #:woo)
  (:import-from #:weblocks-auth/github)
  (:import-from #:spinneret/cl-markdown)
  (:import-from #:log4cl-json)
  (:import-from #:app/cron)
  (:import-from #:app/slynk)
  (:import-from #:mailgun)
  (:import-from #:slynk)
  (:import-from #:mito)
  (:import-from #:weblocks/debug)
  (:import-from #:weblocks/server)
  (:import-from #:weblocks-lass)
  (:import-from #:weblocks/session)
  (:import-from #:weblocks-ui
                #:*foundation-dependencies*)
  (:import-from #:weblocks/app
                #:defapp)
  (:import-from #:weblocks/page
                #:render-headers
                #:get-language)
  (:import-from #:weblocks/dependencies
                #:get-dependencies
                #:*cache-remote-dependencies-in*)
  (:import-from #:weblocks/html
                #:with-html
                #:with-html-string)
  (:import-from #:weblocks/response
                #:immediate-response)
  (:import-from #:uiop
                #:print-backtrace)
  (:import-from #:app/widgets/main
                #:make-main-widget)
  (:import-from #:app/utils
                #:getenv)
  (:import-from #:app/models/migration
                #:migrate)
  (:import-from #:app/analytics
                #:render-google-counter
                #:render-yandex-counter)
  (:import-from #:alexandria
                #:remove-from-plistf
                #:make-keyword)
  (:import-from #:weblocks/request-handler
                #:handle-request)
  (:import-from #:app/db
                #:with-connection)
  (:import-from #:defmain
                #:defmain)
  (:import-from #:app/variables
                #:get-app-motto
                #:get-app-domain
                #:get-app-name
                #:get-user-agent
                #:get-mailgun-domain
                #:get-mailgun-api-key
                #:get-github-client-id
                #:get-github-secret)
  (:import-from #:function-cache
                #:defcached)
  (:shadow #:restart)
  (:export
   #:main
   #:start
   #:restart
   #:stop))
(in-package app/server)

(use-syntax :interpol-syntax)


(defapp app
  :prefix "/"
  :description #?"The (get-app-domain) server."
  :autostart nil
  :debug t)


(defmethod weblocks/session:init ((app app))
  (make-main-widget))


(defparameter *app-dependencies*
  (list (weblocks-lass:make-dependency
          '(body
            :position absolute
            :height 100%
            :min-height 100%
            :width 100%
            :margin 0
            :padding 0

            (.motto
             :font-size 1.5em)

            (.num-projects
             :font-size 0.3em
             :top -1.75em)

            (*
             :box-sizing "border-box")
            (a
             ;; special color for links
             :color "#0071d8")

            (.page-header :border-bottom 1px solid "#add8e6"
                          :padding-bottom 0.5rem
                          :margin-bottom 1rem
             ((:and a :hover)
              ;; Don't want a site name change it's color because
              ;; SVG logo doesn't change it.
              :color "#0071d8")
             (.logo :width 1em
                    :position relative
                    :top -0.4em
                    :left 0.05em))
            
            (.page-footer :color "#AAA"
                          :margin-top 3em)))

        ;; (weblocks-lass:make-dependency
        ;;   '(.page-header
        ;;     :border-bottom 5px solid "#555"
        ;;     :padding-right 0.5rem
        ;;     :padding-left 0.5rem
        ;;     :padding-top 1rem
        ;;     :margin-bottom 2rem))
        
        (weblocks-lass:make-dependency
          '(:media "screen and (max-width: 40em)"
            (.latest-builds
             :display none)
            ((:or .motto .num-projects)
             :display none)
            ((:or .page-content .page-header .page-footer)
             :padding-left 1rem
             :padding-right 1rem)))
        
        ;; (weblocks-parenscript:make-dependency
        ;;   (defun reach-goal (name)
        ;;     "Регистрирует в Яндекс.Метрике достижение цели."

        ;;     (when (@ window ya-counter)
        ;;       (chain window ya-counter (reach-goal name)))

        ;;     (chain console (log (+ "Target " name " was reached")))))
        ))


(defmethod get-dependencies ((app app))
  (append (call-next-method)
          *foundation-dependencies*
          *app-dependencies*))


(defmethod render-headers ((app app))
  "Additional tags for head block."
  (call-next-method)
  
  (with-html
    (:link :rel "icon"
           :type "image/png"
           :href "/images/favicon.png")
    (:link :rel "apple-touch-icon"
           :type "image/png"
           :href "/images/apple-touch-favicon.png")
    ;; Taken from https://simonwhitaker.github.io/github-fork-ribbon-css/
    (:link :rel "stylesheet"
           :href "https://cdnjs.cloudflare.com/ajax/libs/github-fork-ribbon-css/0.2.2/gh-fork-ribbon.min.css")))


(defmethod weblocks/page:render-body ((app app) body-string)
  "Default page-body rendering method"
  (let ((spinneret::*pre* t))
    (render-yandex-counter)
    (render-google-counter)
  
    (with-html
      (:div :class "grid-x"
            (:div :class "cell small-12 medium-10 medium-offset-1 large-8 large-offset-2"
                  (:header :class "page-header"
                           (:h1 :class "site-name"
                                (:a :href "/" (get-app-name)))
                           (:h2 :class "motto"
                                (get-app-motto)))
                  (:div :class "page-content"
                        (let ((spinneret::*pre* t))
                          (with-html (:raw body-string))))


                  (:footer :class "page-footer"
                           (:p ("Proudly served by [Common Lisp](https://common-lisp.net) and [Weblocks](http://40ants.com/weblocks/)!"))))))))


(defmethod initialize-instance ((app app) &rest args)
  (declare (ignorable args))

  (app/metrics:initialize)
  
  ;; (serve-static-file
  ;;  "/favicon.png"
  ;;  (asdf:system-relative-pathname :app "second-favicon.png"))
  
  (call-next-method))


(defmethod on-error ((app app) condition)
  "Default implementation returns a plain text page and 500 status code."
  (declare (ignorable app))
  
  (when (weblocks/debug:status)
    (invoke-debugger condition))
  
  (setf (weblocks/page:get-title)
        #?"Some shit happened with ${(get-app-domain)}")

  (when condition
    (let ((traceback (print-backtrace :condition condition
                                      :stream nil)))
      (log:error "Returning 500 error to user" traceback)))

  (immediate-response
   ;; TODO: replace with weblocks/response:return-page
   (with-html-string
     (weblocks/page:render
      (weblocks/app:get-current)
      (with-html-string
        (:h3 "Some shit happened.")
        (:h4 "Don't panic."))))
   :code 500
   :content-type "text/html"))


(defmethod handle-request ((app app))
  "Here we create a new connection and start new transaction on each request."
  (with-connection ()
    (call-next-method)))


;; Top level start & stop scripts

(defvar *app* nil
  "App's instance.")


(defvar *previous-args* nil
  "Arguments of the previos `start' call. Used to restart
   server with same arguments.")


(defun start (&rest args)
  "Starts the application by calling 'weblocks/server:start' with appropriate
arguments."
  (log:info "Starting app" args)

  (setf *previous-args* args)
  
  (setf mailgun:*domain* (get-mailgun-domain))
  (unless mailgun:*domain*
    (log:error "Set MAILGUN_DOMAIN environment variable, otherwise login will not work"))
  
  (setf mailgun:*api-key* (get-mailgun-api-key))
  (unless mailgun:*api-key*
    (log:error "Set MAILGUN_API_KEY environment variable, otherwise login will not work"))

  (setf weblocks-auth/github:*client-id* (get-github-client-id))
  (unless weblocks-auth/github:*client-id*
    (log:error "Set GITHUB_CLIENT_ID environment variable, otherwise github integration will not work"))
  
  (setf weblocks-auth/github:*secret* (get-github-secret))
  (unless weblocks-auth/github:*secret*
    (log:error "Set GITHUB_SECRET environment variable, otherwise github integration will not work"))

  (setf mailgun:*user-agent* (get-user-agent))
  
  (setf *cache-remote-dependencies-in*
        ;; TODO: make configurable
        #?"/tmp/weblocks-cache/${(get-app-name)}/")
  (setf (get-language)
        "en")

  (log:info "Starting cron jobs")
  (app/cron:setup)
  (app/cron:start)

  (log:info "Starting server" args)
  (apply #'weblocks/server:start :server-type :woo args)

  (log:info "DONE")
  (setf *app*
        (weblocks/app:start 'app)))


(defun stop ()
  "Stops the application by calling 'stop-weblocks'."
  (weblocks/server:stop))


(defun restart ()
  (stop)
  (sleep 5)
  (apply #'start *previous-args*))


(defmain main ((workers "A comma-separated list of workers to connect to in form \"localhost:10100,localhost:10101\". If not given, then we'll not try to connect to any workers and version building will not be available.")
               (dont-start-server "Don't start HTTP server."
                                  :flag t))
  (log4cl-json:setup :level :debug)

  (let ((slynk-port 4005)
        (slynk-interface (getenv "SLYNK_INTERFACE" "0.0.0.0"))
        (interface (getenv "INTERFACE" "0.0.0.0"))
        (port (getenv "PORT" 80))
        (hostname (machine-instance))
        (debug (when (getenv "DEBUG")
                 t)))

    ;; To make it possible to connect to a remote SLYNK server where ports are closed
    ;; with firewall.
    (setf slynk:*use-dedicated-output-stream* nil)
    
    (format t "Starting slynk server on ~A:~A (dedicated-output: ~A)~%"
            slynk-interface
            slynk-port
            slynk:*use-dedicated-output-stream*)

    (app/slynk:setup)
    (slynk:create-server :dont-close t
                         :port slynk-port
                         :interface slynk-interface)

    ;; Now we'll ensure that tables are exists in the database
    ;; (migrate)

    (unless dont-start-server
      (format t "Starting HTTP server on ~A:~A~%"
              interface
              port)
      (start :port port
             :interface interface
             :debug debug))

    (format t "To start HTTP server:~%")
    (format t "Run ssh -6 -L ~A:localhost:4005 ~A~%"
            slynk-port
            hostname)
    (format t "Then open local Emacs and connect to the slynk on 4005 port~%")
    (format t "Evaluate:~%(server:stopserver)~%(server:runserver)~%~%in LISP repl and start hacking.~%"))

  ;; Now we'll wait forever for connections from SLY.
  (loop
    do (sleep 60))
  
  (format t "Exiting. Why? I don't know! This should never happen~%"))

