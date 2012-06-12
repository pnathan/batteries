
(defpackage :batteries-paths

  (:use :common-lisp :batteries)
  (:export :getcwd
	   :join-paths))

(in-package :batteries-paths)

;; For manipulation of paths
(ql:quickload :iolib)
(ql:quickload :iolib.os)
(ql:quickload :iolib.pathnames)

(use-package :iolib.pathnames)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getcwd ()
  (iolib.pathnames:file-path-namestring
   (iolib.pathnames:file-path
    (iolib.os:current-directory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun join-paths (&rest paths)
  (let ((delimiter (string  iolib.pathnames:+directory-delimiter+ )))
    (join-string
     delimiter
     paths)))
