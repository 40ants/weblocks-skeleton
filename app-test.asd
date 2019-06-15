(defsystem "app-test"
  :class :package-inferred-system
  :pathname "t"
  :depends-on ("hamcrest"
               "app"
               "app-test/models/project")
  :perform (test-op :after (op c)
                    (symbol-call :log :config
                                 :sane2 :warn)
                    (symbol-call :rove :run c)))
