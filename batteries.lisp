;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; library.lisp
;; (c) Paul Nathan 2011, 2012
;; Agglutunation of useful Lisp routines.
;; Has dependancy on Quicklisp
;; General goal is to be some Batteries for Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :batteries
  (:use :common-lisp)
  (:export :expect
	   :*run-unit-tests*
	   :with-running-unit-tests

	   :uniqueize
	   :maxlist
	   :in
	   :every-other
	   :flatten
	   :join
	   :join-values
	   :upto
	   :after
	   :final
	   :heads
	   :tails
	   :zip
	   :firstn
	   :find-bag-if

	   :hashset
	   :hashget
	   :hashdel
	   :print-hash-table-1
	   :string-hash-table-1
	   :map-hash-to-hash
	   :filter-hash-to-hash
	   :mergable-hash-table-p
	   :merge-hash-table

	   :writeln
	   :emit
	   :concat-list
	   :strcat
	   :chomp
	   :join-string

	   :defobject
	   :class-slots-symbols
	   :object-to-hash
	   :print-generic-object

	   :sum
	   :range
	   :range-1
	   :neg
	   :true-p

	   :system

	   :write-file
	   :write-text-file
	   :write-to-file-as-variable
	   :read-file
	   :read-text-file

	   :pad-seq
	   :not-eql
	   :partition-by-index
	   :partition-padded

	   :getargs

	   :sliding-window-2-wide
	   :sliding-chunker
	   :take-predicate-generator
	   :gather-generator

	   :getcwd
	   :join-paths

	   :with-condition-retries
	   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO list
;; * Unit tests:
;;   ** map-hash-hash
;;   ** filter-hash-hash
;;   ** find-in-bag-if
;; * replace split-on-space with a split-sequence function?
;; * Consider moving the iolib routines into their own lib - they are
;;   slow to compile
;; * Add SPLITLINES for text files


(in-package :batteries)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ql:quickload :alexandria)
;; Check to see if the package 'clos' comes standard
(ql:quickload :closer-mop)
;;For file reading
(ql:quickload :babel)

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unit test stuff

;; So that this happens 'prior' to macro expansion.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *run-unit-tests* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun expect (expr1 expr2)
  (equal expr1 expr2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-running-unit-tests (&rest test-list)
  (when  *run-unit-tests*
    (labels ((sum (seq)  (reduce '+ seq)))
      (let ((test-successes
	     (sum (loop for test in test-list
		     do
		       (if (not (eval test))
			   (format t "Failure: ~a~%" test))
		     collect
		       (if (eval test) 1 0)))))
	(format t "~a out of ~a passed tests~%"
		test-successes
		(length test-list))
	(/ test-successes
	 (length test-list))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; List operations.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun maxlist (list)
  "Max of a list"
  (reduce #'max list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun in (thing seq &key (test 'eql))
  "Is thing in the sequence? Defaults to using EQL as the test"
  (find thing seq :test test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun every-other (seq &key (flipflop nil))
  "Returns every other element in `seq`.
Set :flipflop to T to take the even-indexed ones

Note: has nothing to do with flipflow circuit elements."
  (if seq
      (if flipflop
	  (every-other (cdr seq)
		       :flipflop (not flipflop))
	  (cons (car seq)
		(every-other (cdr seq)
			     :flipflop
			     (not flipflop))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun uniqueize (x &key (test #'eql))
  "Return a list that is unique. Recursive, O(n^2)"
  ;; Another implementation might be to develop a hash table-based
  ;; approach ala perl's uniquize approach
   (unless (endp x)
     (adjoin (car x)
	     (uniqueize (cdr x) :test test)
	     :test test)))

(with-running-unit-tests
    (expect nil (uniqueize niL))
  (expect '(1) (uniqueize '(1)))
  (expect '(1) (uniqueize '(1 1)))
  (expect '(1 2) (uniqueize '(1 2)))
  (expect '(1) (uniqueize '(1 1 1)))
  (expect '(1 2 3 4) (uniqueize '(1 2 2 3 4 ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten (x)
  "Descend into the supplied list until an atom is hit.
Append the atom to the flattened rest"
  (if (endp x)
      x
      (if (atom (car x ))
	  (append (list (car x))
		  (flatten (cdr x)))
	  (append (flatten (car x))
		  (flatten (cdr x ))))))

(with-running-unit-tests
    (expect nil (flatten nil))
    (expect '(1) (flatten '(1)))
    (expect '(1 2) (flatten '(1 2)))
    (expect '(1 2) (flatten '(1 (2))))
    (expect '(1 2 3) (flatten '(1 (2) 3)))
    (expect '(3 2 1 -1) (flatten '(((((3)) 2) 1) -1))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun join ( sep list)
  "Returns the seq interspersed with sep as a list"
   (butlast (mapcan #'(lambda (x) (list x sep))
	   list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequence functions - both array and lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun join-values (sep &rest things)
  (join sep things))

(with-running-unit-tests
    (expect nil (join nil nil))
  (expect '(1) (join nil '(1)))
  (expect '(1 3 2) (join 3 '(1 2)))
  (expect '(1 3 2 3 5) (join 3 '(1 2 5))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun upto (v l)
  "subsequence of l, up to first occurance of v;
 everything if no v"
  (subseq l 0 (position v l) ))

(with-running-unit-tests
    (expect nil (upto nil nil))
  (expect nil (upto 1 '(1 2 3 4 5)))
  (expect '(1 2 3 4 5)  (upto nil '(1 2 3 4 5)))
  (expect '(1 2 3)  (upto 4 '(1 2 3 4 5 6)))
  (expect '(1 2 3 4 5)  (upto 6 '(1 2 3 4 5))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun after (v l)
  "Returns everything after `v` in `l`
If `v` doesn't exist, return nil
Inverse of upto"
  (subseq l
	  (if (not (position v l))
	      (length l)
	    (+ 1 (position v l)))
	  (length l)))

(defun final (seq)
  "Returns the last element of `seq`"
  (elt (reverse seq) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; List in List routines

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun heads (lists)
  "gets the fronts of the lists in lists"
  (mapcar #'(lambda (x)
	      (car x))
	  lists))

(with-running-unit-tests
      (expect (heads '()) nil)
      (expect (heads '(()())) (list nil nil))
      (expect (heads '((10) (20))) '(10 20))
      (expect (heads '((10 30) (20 40))) '(10 20))
      (expect (heads '((10 30 50) (20 40 60))) '(10 20))
      (expect (heads '(((10) 30 50) ((20) 40 60))) '((10) (20))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tails (lists)
  "Gets the rests of the lists in lists"
  (mapcar #'(lambda (x) (subseq x 1))
	  lists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun interleave (lists)
  " (interleave '((1 2) (3 4))) => (1 3 2 4)"
  (when lists
      (apply #'mapcan #'list lists)))

(with-running-unit-tests
    (expect nil (interleave nil))
  (expect '(nil nil) (interleave '((nil) (nil))))
  (expect nil (interleave '(nil nil)))
  (expect (interleave '((1) (2))) '(1 2))
  ;; Raises condition
  ;;(expect (interleave '((1 2) (3))) '(1 3 2 nil))
  (expect (interleave '((1 2) (3 4))) '(1 3 2 4))

  (expect '(1 4 2 5 3 6)
	  (interleave '((1 2 3) (4 5 6))))

  (expect '(1 4 7 2 5 8 3 6 9)
	  (interleave '((1 2 3) (4 5 6) (7 8 9)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zip (lists)
  " (zip '((1 2) (3 4))) => ((1 3) (2 4))"
  (apply #'mapcar #'list lists))

(with-running-unit-tests
    (expect (zip '(nil nil nil)) nil)
    (expect (zip '((1 2) (3 4))) '((1 3) (2 4)))
    (expect (zip '((1 2) (3 4) (5 6))) '((1 3 5) (2 4 6))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun firstn (n l)
  "Returns the first `n` of `l`

Raises an error if n > length of l"
  (subseq l 0 n))

(with-running-unit-tests
    (expect nil (firstn 0 '(10 20 30 40)))
    (expect '(10)  (firstn 1 '(10 20 30 40)))
    (expect '(10 20 30 40)  (firstn 4 '(10 20 30 40))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-bag-if (predicate test-bag seq)
  "Is any of test-bag in seq, using predicate"
  (intersection test-bag seq :test predicate))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash functions

;; These provide a visual regularity in how hashes are accessed and
;; provide more visual meaning than using a 'get' for both setting and
;; getting.  However, they are not setf'able.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hashset (hash key val)
  (setf (gethash key hash) val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hashget(hash key)
  (gethash key hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hashdel(hash key)
  (remhash key hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mergable-hash-table-p (hash-a hash-b)
  "Are the two hash tables mergeable, that is, the keys do not
collide"
  (not (intersection (alexandria:hash-table-keys hash-a)
		     (alexandria:hash-table-keys hash-b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun merge-hash-table (hash-a hash-b)
  "Merge the two hash tables.

Returns a new hash table.

Does not respect key collisions"

  (let ((new-hash-table (make-hash-table)))
    (maphash #'(lambda (k v)
	       (hashset new-hash-table k v))
	     hash-a)

    (maphash #'(lambda (k v)
	       (hashset new-hash-table k v))
	     hash-b)
    new-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun string-hash-table-1 (hash)
  "Returns a string of the hash table, no recursion"
  (let ((accum
	 (loop for var in (alexandria:hash-table-keys hash) collect
	      (format nil "~a => ~a~&" var
		      (gethash var hash)))))
    (format nil "~{~a~}" accum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-hash-table-1 (hash)
  "Prints the hash table, no recursion"
  (format t  "~a" (string-hash-table-1 hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun map-hash-to-hash (fn hashtable)
  "Given a hash table, apply fn to all values and return a new hash
  table with the same keys mapping to (fn val)"
  (let* ((hash (make-hash-table))
	 (fn-wrapper
	  (lambda (k v)
	    (setf (gethash k hash) (apply fn (list v))))))
    (maphash fn-wrapper hashtable)
    hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun filter-hash-to-hash (predicate hashtable)
  "Given a predicate and a hash table, filter out all elements in the
  hashtable whose `values` do not return a non-nil value on (predicate
  value)"
  (let* ((hash (make-hash-table))
	 (fn-wrapper
	  (lambda (k v)
	    (if (apply predicate (list v))
		(setf (gethash k hash) v)))))
    (maphash fn-wrapper hashtable)
    hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STDOUT routines
(defun writeln (str)
  "Writes string as line"
  (format t "~a~%" str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun emit (str &rest stuff)
  "Quick dump to stdout"
  (apply #'format t str stuff))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string operations
(defun concat-list(seq)
  "Concatenates a list of strings"
  (apply 'concatenate 'string seq))

(with-running-unit-tests
    (expect "abc" (concat-list '("a" "b" "c")))
  (expect "abc" (concat-list '("ab" "c")))
  (expect "abc" (concat-list '("abc"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun strcat (&rest list)
  "Concatenates all the strings that are passed in"
  (concat-list list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun chomp (s)
  "Trims the right side off"
  (string-right-trim '(#\Space #\Tab #\Newline) s))

(with-running-unit-tests
    (expect "abc" (chomp "abc "))
    (expect "abc" (chomp "abc
 ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun join-string (sep seq)
  "Joins seq with sep.
Expects sep to be a string.
Expects seq to be a sequence of strings"
  (apply #'concatenate 'string (join sep seq)))

(with-running-unit-tests
    (expect "a-b-c" (join-string "-" '("a" "b" "c"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Object creation macros

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-var (classname var)
  (let ((variable-symbol (if (consp var)
			     (first var)
			     var))
	(variable-init-val (if (consp var)
			       (second var)
			       nil)))
  (list variable-symbol
        :initform variable-init-val
        :accessor (intern (concatenate 'string (string classname) "-"
                                       (string variable-symbol)))
        :initarg (intern (string variable-symbol) :keyword))))

(with-running-unit-tests
    (expect
     '(BAR :INITFORM NIL :ACCESSOR FOO-BAR :INITARG :BAR)
     (build-var 'foo 'bar))

  (expect
   '(BAR :INITFORM QUUX :ACCESSOR FOO-BAR :INITARG :BAR)
   (build-var 'foo '(bar quux))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-varlist (classname varlist)
   (loop for var in varlist
         collect (build-var classname var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-ez-class (name &optional varlist &key documentation superclasses)
  (let ((docpair nil)
	(vars (loop for var in varlist
		 collect (build-var name var))))

    (if documentation
	(setf docpair (list :documentation documentation))
	(setf docpair (list :documentation "")))

    `(if ,superclasses
	 ,(let ((classes (cadr superclasses)))
	   `(defclass ,name
	       ,classes
	     ,vars
	     ,docpair))
	 ;;else
	 (defclass ,name ()
	   ,vars
	   ,docpair))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-ez-class-ctor (name varlist)
  "
;(defun make-name (&key (bar nil))
;(make-instance 'name :bar bar))"
  (let ((initarg-list
	 (loop for var in varlist collect
	      (if (consp var)
		  (list (first var) (second var))
		  (list var nil))))
	(ctor-args
	 (flatten
	  (loop for var in varlist
	     collect
	       (let ((var-symbol (if (consp var)
				     (first var)
				     var)))
		     (list (intern (string var-symbol) :keyword) var-symbol))))))

    ;;Add the kwarg notator
    (if initarg-list
	(push '&key initarg-list))

     `(defun ,(intern (concatenate 'string "MAKE-" (string name)))
	  ,initarg-list
	(make-instance (quote ,name)
		       ,@ctor-args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: this macro makes for a good deal less typing for your
;; 'average' POD class structure. It's similar to DEFSTRUCT, but
;; instead is a "normal" CLOS object.
(defmacro defobject (name varlist &key (documentation nil) (superclasses nil))
  "Defines a class `name`

`name` will have its variables with these settings:
  initform as nil (or specified)
  accessor function as `name-var`
  initarg as :var

If a var is passed in as a pair (var val), val will become the
initform.

A make-`name` function definition will spring into existance

Example:
;(defobject world (population-normals population-wizards population-dragons)
  :documentation ''Fun place!'')"

  `(progn
     (def-ez-class ,name ,varlist :documentation ,documentation :superclasses ,superclasses)
     (def-ez-class-ctor ,name ,varlist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLOS manipulation routines

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun class-slots-symbols (class-instance)
  "Returns a list of the symbols used in the class slots"
  (mapcar 'closer-mop:slot-definition-name
	  (closer-mop:class-slots
	   (class-of class-instance))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun object-to-hash (obj)
  "Reflects over the slots of `obj`, and returns a hash table mapping
slots to their values"
  (let ((new-hash (make-hash-table))
	(slot-list (class-slots-symbols obj)))
    (loop for slot in slot-list do
	 (hashset new-hash (string slot)
		  (slot-value  obj slot)))
    new-hash))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-generic-object (obj)
  "Writes out the object as a generic slot => value list"
  (print-hash-table-1 (object-to-hash obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical routines

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sum (seq)
  (reduce '+ seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun range (bottom top)
  "Inclusive Range.
Generates a range from bottom to top on the integers"
  (loop for i from bottom to top collect i))

(defun range-1 (bottom top)
  "Slightly Exclusive Range.
Generates a range from bottom to top - 1 on the integers"
  (let ((top (1- top)))
    (loop for i from bottom to top collect i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun true-p (var)
  "When you want to test for truth, and not get the result back"
  (not (not var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun neg (num)
  (- num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell integration routines

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun system (cmd args  &key stdin)
  "system runs `cmd` with `args`

`args` is a list of 0 or more strings

If `stdin` is set, a string is expected

Output is returned as a triple (STDOUT, STDERR, RETURN-CODE) "

  #+clisp (with-open-stream (s1 (ext:run-shell-command cmd :output :stream))
	   (with-output-to-string (out)
	     (copy-stream s1 out)))
  #+sbcl
  (let ((stderr (make-string-output-stream))
	(stdout (make-string-output-stream))
	(stdin-stream (make-string-input-stream stdin)))
    (let ((code (sb-ext:process-exit-code
		 (sb-ext:run-program cmd args
				     :search t
				     :output stdout
				     :error stderr
				     :input stdin-stream))))
      (list (get-output-stream-string stdout)
	    (get-output-stream-string stderr)
	    code)))

  #-(or clisp sbcl)
  (error "Unsupported implementation"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file operations

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-stream (in out)
   (loop for line = (read-line in nil nil)
         while line
         do (write-line line out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-file (name content)
  "Writes an 8-bit byte file"
  (with-open-file (stream  name
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create
			   :element-type  '(unsigned-byte 8))
    (write-sequence content stream))
  name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-text-file (name text)
  "Writes a text file"
  (write-file name
	      (map 'vector #'char-int
		   (format nil "~a" text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun write-to-file-as-variable (filename variable-name data)
  "Writes var to file as a variable"
  (write-text-file filename
		   (with-output-to-string (stream)
		     (format stream "(setf ~a " variable-name)
		     ;;if a list, quote, otherwise don't quote
		     (cond ((consp data)
			    (format stream "'")
			    (print data stream))
			   (t
			    (print data stream)))
		     (format stream ")"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-file (filename)
  "Reads `filename` as a sequence of unsigned 8-bit bytes, no
encoding"
  (with-open-file (fin filename
		   :direction :input
		   :if-does-not-exist :error
		   :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length fin)
			   :element-type '(unsigned-byte 8)
			   :fill-pointer t)))
      (setf (fill-pointer seq)
	    (read-sequence seq fin))
      seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-text-file (filename)
  "Reads an ASCII file"
  (babel:octets-to-string  (read-file filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Partitioning routines

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pad-seq (seq n &optional fill-val)
  "Pads a sequence by a multiple of n
 fill-val is defined to be nil normally"
  (if (/= (mod (length seq) n) 0)
      (append seq
	      (make-list
	       (- n ( mod (length seq) n))
	       :initial-element fill-val))
    seq))
(with-running-unit-tests
      (expect (pad-seq '() 1) nil)
      (expect (pad-seq '(1) 1) '(1))
      (expect (pad-seq '(1 2) 1) '(1 2))
      (expect (pad-seq '(1 2) 2) '(1 2))
      (expect (pad-seq '(1 2) 3) '(1 2 nil))
      (expect (pad-seq '(1 2) 4) '(1 2 nil nil))
      (expect (pad-seq '(1 2 3 4 5) 2) '(1 2 3 4 5 nil))
      (expect (pad-seq '(1 2 3 4 5) 3) '(1 2 3 4 5 nil))
      (expect (pad-seq '(1 2 3 4 5) 4) '(1 2 3 4 5 nil nil nil))
      (expect (pad-seq '(1 2 3 4 5) 5) '(1 2 3 4 5)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: move this to a different section
(defun not-eql (a b)
  "Simplifies the task of testing not-equality."
  (not (eql a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (partition-by-index (1 2 3 4 5 6 7 8 9) 2)
;; => ((1 3 5 7 9) (2 4 6 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun partition-by-index (seq slice &key error-on-indivisible)
  "Partitions seq into slice pieces, walking by slice each part
If error-on-indivisible is T, then err when (not-eql (mod (length seq) slice) 0)
"
  (when (and error-on-indivisible
	     (not-eql (mod (length seq) slice) 0))
    (error "Not evenly partitionable"))

  (let (( list-of-lists (make-list slice)))
    ;;Basically walk up by index = slice*n
    (loop for var in seq
	  for n from 0
	  do
	  (setf (nth (mod n slice) list-of-lists)
		(append (nth (mod n slice) list-of-lists)
			(list var))))
    list-of-lists))

(with-running-unit-tests
      (expect (partition-by-index '() 1) '(nil))
      (expect (partition-by-index '(1) 1) '((1)))
      (expect (partition-by-index '(1 2) 1) '((1 2)))
      (expect (partition-by-index '(1 2) 2) '((1) (2)))
      (expect (partition-by-index '(1 2 3) 3) '((1) (2) (3)))
      (expect (partition-by-index '(1 2 3) 2) '((1 3) (2)))
      (expect (partition-by-index '(1 2 3 4 5 6 7 8 9) 2)
	      '((1 3 5 7 9)
		(2 4 6 8)))
      (expect (partition-by-index '(1 2 3 4 5 6 7 8 9) 5)
	      '((1 6)
		(2 7)
		(3 8)
		(4 9)
		(5))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun partition-padded (data parts &optional (pad-byte nil) )
  "Composing padding and partitioning, this partitions data into Parts number of parts."
  ;;Uses the global *pad-byte* to pad with.
  (partition-by-index  (pad-seq data parts pad-byte) parts))

(with-running-unit-tests
      (expect (partition-padded '() 1) '(nil))
      (expect (partition-padded '(1) 1) '((1)))
      (expect (partition-padded '(1) 2) (list '(1) (list nil)))
      (expect (partition-padded '(1) 3) (list '(1)
					      (list nil)
					      (list nil)))
      (expect (partition-padded '(8 9 10 4) 3)
	      (list '(8 4)
		    (list 9 nil)
		    (list 10 nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routines that havn't been appropriately textually munged into the right section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;todo: evaluate the split-sequence package
(defun split-on-space (str)
  (let
      ((s str)
       (lol)) ;list of lists
    (loop
     (push (upto #\space s) lol)
     (setf s (after #\space s));;
     (when (not s)
       (return lol))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment management
(defvar *argv*
  nil
  "Argv. Aliased to allow for multi-compiler use")

(defun load_argv ()
  "Loads the argument values"
  (setf *argv*
	#+SBCL
	sb-ext:*posix-argv*
	#+CLISP
	EXT:*ARGS*
	#+CCL
        *UNPROCESSED-COMMAND-LINE-ARGUMENTS*
	#-(or SBCL CLISP CCL) (error "Lisp system not yet supported")))

;; Known systems that I should support:
;; cmu allegro LispWorks abcl

(defun getargs ()
  (unless *argv*
    (load_argv))
  *argv*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sliding-window-2-wide (index data-read data-to-read)
  (> index 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sliding-chunker (sequence last-run index)
  (subseq sequence
	  (max 0 (1- last-run))
	  index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generator-y thing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun take-predicate-generator (sequence predicate chunker)
  "Generates a closure.

Said closure...

Reads `sequence` until predicate is T. At that point, the read values
are returned.  Not terribly fast.

The predicate must take index, data-read, and data-to-be-read

Next time it is called, the same thing happens.
"
  (let
      ((index 0)
       (last-run-pointer 0)
       (data-read nil)
       (data-to-read nil)
       (cached-length (length sequence)))
    (lambda ()
      (do
       ;; Variables are held in the let
       ()
       ;; Termination condition is below
       (nil)

	;; went too far.
	(when (> index cached-length) (return nil))

	;;Increment data
	(setf data-read (subseq sequence 0 index))
	(setf data-to-read (subseq sequence index))
	   (setf index (1+ index))


	;;Typical DO is before body operation: this makes an 'after' evaluation
	;;Termination conditions
	(cond
	  ;; went too far
	  ((> index cached-length)
	   (return nil))

	  ;;Predicate... or overrun.
	  ((or (funcall predicate index data-read data-to-read)
	       (= index cached-length))

	   (let
	       ;;What is returned
	       ((returned-sequence (funcall chunker sequence last-run-pointer index)))

	     (setf last-run-pointer index)

	     (return returned-sequence))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun gather-generator (sequence predicate chunker)
  "Runs the generator on sequence and collects the results"
  (let ((generator (take-predicate-generator sequence predicate chunker)))
    (labels ((collector ()
		;; run the generator
		(let ((data (funcall generator)))
		  ;; If we got something
		  (if data
		      (cons data (collector))
		      nil))))
       (collector))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-condition-retries(retries expected-errors fail-function &body body)
  "Attempts to execute `body` `retries` times, watching for
`expected-errors`.  Optionally, if `fail-function` is set, each time a
failure occurs, prior to retrying, `fail-function` is executed.

Other conditions beside `expected-errors` will exit out of this macro"
   (let ((counter (gensym))
	 (result (gensym))
	 (start-tag (gensym))
	 (error-handler
	  #'(lambda (c)
	    (let ((r (find-restart 'handler c)))
	      (when r
		(invoke-restart r c))))))

     `(let ((,counter 0)
	    (,result))
	    (tagbody
	       ,start-tag
	       (restart-case
		   (handler-bind
			,(mapcar #'(lambda (error-val)
				     (list error-val   error-handler))
				 expected-errors)
		     (setf ,result ,@body))
		 ;;the restart pointed at by the error-handler lambda
		 (handler (&rest args)
		   (declare (ignore args))
		   (incf ,counter)
		   (unless (> ,counter ,retries)
		     (when ,fail-function
		       (funcall ,fail-function))
		     (go ,start-tag)))))
	,result)))

