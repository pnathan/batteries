(asdf:defsystem #:batteries
  :depends-on (#:alexandria #:closer-mop #:iolib.pathnames #:iolib.os #:babel #:iolib)
  :components ((:file "batteries")))