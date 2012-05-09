(asdf:defsystem #:batteries
  :depends-on (#:alexandria
	       #:closer-mop
	       #:babel
	       #:iolib
	       #:iolib.os
	       #:iolib.pathnames)
  :components ((:file "batteries")))