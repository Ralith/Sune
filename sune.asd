(asdf:defsystem #:sune
  :serial t
  :depends-on (#:alexandria #:babel #:iolib #:yacc)
  :pathname "src/"
  :components
  ((:file "package")
   (:file "parse")
   (:file "queue")
   (:file "connection")
   (:file "handlers")
   (:file "main")))
