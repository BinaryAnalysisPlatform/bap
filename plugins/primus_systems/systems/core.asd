(defsystem bap:binary-executor
  :description "Executes a binary program."
  :components (bap:load-binary
               bap:program-loader
               bap:x86-flag-initializer
               bap:powerpc-init))

(defsystem bap:base-lisp-machine
  :description "Executes Primus Lisp program."
  :components (bap:load-lisp-library
               bap:lisp-core
               bap:lisp-primitives
               bap:lisp-ieee754
               bap:lisp-type-error-printer
               bap:lisp-basic-io
               bap:lisp-dictionary
               bap:lisp-regions))

(defsystem bap:stubbed-executor
  :description "Executes a binary together with the Lisp Machine"
  :depends-on (bap:binary-executor
               bap:base-lisp-machine))

(defsystem bap:terminating-stubbed-executor
  :description "Executes a binary together with the Lisp Machine that
                is guaranteed to terminate."
  :depends-on (bap:binary-executor
               bap:base-lisp-machine)
  :components (bap:limit))

(defsystem bap:greedy-promiscuous-executor
  :description "Executes all linearly independent paths and never fails."
  :depends-on (bap:terminating-stubbed-executor)
  :components (bap:greedy-scheduler
               bap:promiscuous-mode
               bap:incident-location-recorder
               bap:lisp-incidents
               bap:mark-visited))


(defsystem bap:base-taint-analyzer
  :description "Uses greedy-promiscuous-executor for taint analysis.
                No policy is specified"
  :depends-on (bap:greedy-promiscuous-executor)
  :components (bap:taint-primitives
               bap:taint-signals))

(defsystem bap:taint-analyzer
  :description "Uses greedy-promiscuous-executor for taint analysis.
                Propagates taint by computation."
  :depends-on (bap:base-taint-analyzer)
  :components (bap:propagate-taint-by-computation))


(defsystem bap:reflective-taint-analyzer
  :description "A taint analyzer that reflects taints to/from BIR terms"
  :depends-on (bap:taint-analyzer)
  :components (bap:taint-intro
               bap:taint-marker
               bap:taint-mapper))


(defsystem bap:exact-taint-analyzer
  :description "Uses greedy-promiscuous-executor for taint analysis.
                Propagates taint exactly."
  :depends-on (bap:base-taint-analyzer)
  :components (bap:propagate-taint-exact))


(defsystem bap:constant-tracker
  :description "Uses greedy-promiscuous-executor for constant tracking."
  :depends-on (bap:greedy-promiscuous-executor)
  :components (bap:constant-tracker-primitives
               bap:constant-tracker))


(defsystem bap:multi-analyzer
  :description "Runs several analyses in parallel."
  :depends-on (bap:taint-analyzer
               bap:constant-tracker)
  :components (bap:conservative-garbage-collector))


(defsystem bap:string-deobfuscator
  :description "Uses greedy-promiscuous-executor to find obfuscated strings."
  :depends-on (bap:greedy-promiscuous-executor)
  :components (bap:beagle-hunter))
