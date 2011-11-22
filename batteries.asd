(asdf:defsystem #:batteries
  :depends-on (#:alexandria #:closer-mop #:iolib.pathnames #:iolib.os)
  :components ((:file "batteries")))