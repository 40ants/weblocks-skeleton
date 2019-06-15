(defun search-version-in-changelog (lines)
  (let* ((line (nth 4 lines))
         (space-pos (position #\Space line)))
    (when space-pos
      (subseq line 0 space-pos))))


(defsystem "app"
  :description "An opensource advisor."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :licence "BSD"
  :class :package-inferred-system
  :version (:read-file-line "ChangeLog.rst" :at search-version-in-changelog)
  :pathname "src"
  :depends-on ("cl-interpol"
               ;; We need this while will not support package inferred systems:
               ;; https://github.com/ultralisp/ultralisp/issues/3
               "weblocks-ui"
               "app/server"
               "app/widgets/landing")
  :in-order-to ((test-op (test-op ultralisp-test)))
  :perform (compile-op :before (o c)
                       #+ros.installing
                       (roswell:roswell '("install" "40ants/defmain"))))

(register-system-packages "prometheus.collectors.sbcl" '(#:prometheus.sbcl))
(register-system-packages "prometheus.collectors.process" '(#:prometheus.process))
