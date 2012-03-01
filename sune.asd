(asdf:defsystem #:sune
  :serial t
  :depends-on (#:alexandria #:babel #:iolib #:yacc #:cffi #:split-sequence)
  :pathname "src/"
  :components
  ((:file "package")
   (:file "utils")
   (:file "parse")
   (:file "queue")
   (:file "connection")
   (:file "handlers")
   (:file "main")))
