;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Batteries system definition


(asdf:defsystem #:batteries
  :depends-on (#:alexandria
	       #:closer-mop
	       #:babel)
  :components ((:file "batteries")))

 (asdf:defsystem #:batteries-paths
   :depends-on (#:batteries
		#:iolib
		#:iolib.os
		#:iolib.pathnames)
   :components ((:file "batteries-paths")))
