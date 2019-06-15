(defpackage #:app/variables
  (:use #:cl)
  (:import-from #:function-cache
                #:defcached)
  (:export
   #:get-app-name
   #:get-app-domain
   #:get-app-motto))
(in-package app/variables)


(defmacro def-env-var (getter var-name &optional default)
  `(progn
     (defcached ,getter ()
       "If `value' is not given, then tries to extract it from env variables or fall back to default."
       (or (uiop:getenv ,var-name)
           ,default))
     (export ',getter)))

(def-env-var get-app-name
  "APP_NAME"
  "12Forks")

(def-env-var get-app-motto
  "APP_MOTTO"
  "An Opensource Advisor.")

(def-env-var get-app-domain
  "APP_DOMAIN"
  "12forks.com")

(def-env-var get-yandex-counter-id
  "YANDEX_COUNTER_ID")

(def-env-var get-google-counter-id
  "GOOGLE_COUNTER_ID")

(def-env-var get-postgres-host
  "POSTGRES_HOST"
  "localhost")

(def-env-var get-postgres-dbname
  "POSTGRES_DBNAME"
  "lisp")

(def-env-var get-postgres-user
  "POSTGRES_USER"
  "lisp")

(def-env-var get-postgres-ro-user
  "POSTGRES_RO_USER"
  "lisp_ro")

(def-env-var get-postgres-pass
  "POSTGRES_PASS"
  "lisp")

(def-env-var get-postgres-ro-pass
  "POSTGRES_RO_PASS"
  "lisp_ro")

(def-env-var get-github-client-id
  "GITHUB_CLIENT_ID")

(def-env-var get-github-secret
  "GITHUB_SECRET")

(def-env-var get-mailgun-domain
  "MAILGUN_DOMAIN")

(def-env-var get-mailgun-api-key
  "MAILGUN_API_KEY")

(def-env-var get-user-agent
  "USER_AGENT"
  "12forks (https://12forks.com)")
