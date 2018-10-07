(defsystem "cl-solid-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Frederick C Gibson"
  :license ""
  :depends-on ("cl-solid"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-solid"))))
  :description "Test system for cl-solid"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
