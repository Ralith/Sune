(asdf:defsystem #:sune
  :serial t
  :depends-on (#:alexandria #:babel #:iolib #:yacc #:cffi #:split-sequence
               #:cl-ppcre #:puri)
  :pathname "src/"
  :components
  ((:file "package")
   (:file "utils")
   (:file "parse")
   (:file "queue")
   (:file "buffer")
   (:file "connection")
   (:file "handlers")
   (:file "main")))
