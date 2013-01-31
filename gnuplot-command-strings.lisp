;;;; gnuplot-command-strings.lisp

(in-package :gnuplot-command-strings)

(export '(insert-column-index print-inline-data))

(define-test insert-column-index
  (assert-equal "abc" (insert-column-index "abc" 1))
  (assert-equal "$1abc" (insert-column-index "$abc" 1))
  (assert-equal "a$1bc" (insert-column-index "a$bc" 1))
  (assert-equal "abc$1" (insert-column-index "abc$" 1)))
    
(defun insert-column-index (def index)
  "Append the integer `index' to the  `$' sign in `def'

Use: when constructing arguments to  `plot ... using'"
  (let* ((pos (position #\$ def)))
    (if pos
	(concatenate 'string
		     (subseq def 0 (1+ pos))
		     (prin1-to-string index)
		     (subseq def (1+ pos)))
	def)))

(export '(format-range))
(define-test format-range
  (assert-equal "[1:2] " (format-range (cons 1 2)))
  (assert-equal "[1:*] " (format-range (cons 1 nil)))
  (assert-equal "[*:2] " (format-range (cons nil 2)))
  (assert-equal "[*:*] " (format-range (cons nil nil)))
  (assert-equal "[*:*] " (format-range nil)))

(defun format-range (&optional car-cdr)
  "Using the car and cdr parts of `car-cdr' format the range in the form
`[X:Y] '

Non-nil parts of `car-cdr' are inserted using the ~a format directive.
Nil's are inserted as `*', i.e, interpreted for autoranging.

If `car-cdr' is nil, return [*:*] "
    (if car-cdr
	(format nil "[~a:~a] "
	      (aif (car car-cdr) it "*")
	      (aif (cdr car-cdr) it "*"))
	"[*:*] "))

(define-test print-inline-data
;; These tests are sensitive to blanks at the end of line.  The
;; functions should not produce any blanks at the end of line.
(assert-equal 
"1
2
e
"
(let (#+clisp(custom:*pprint-first-newline* nil))
  (with-output-to-string (stream)
    (print-inline-data stream (list 1 2)))))
(assert-equal 
"1
2
e
"
(let (#+clisp(custom:*pprint-first-newline* nil))
  (with-output-to-string (stream)
    (print-inline-data stream #(1 2)))))
(assert-equal 
"1 11 21 
2 12 22 
e
"
(let (#+clisp(custom:*pprint-first-newline* nil))
  (with-output-to-string (stream)
    (print-inline-data stream
		       (make-array '(2 3)
				   :initial-contents '((1 11 21)
						       (2 12 22))))))))

(export '(print-inline-data))

(let ((lf (string #\linefeed)))
  (defgeneric print-inline-data (stream data &rest more-data)
    (:documentation
"Print data in format for inline inclusion to the plot statement to
stream.

Data can be of type list, vector or matrix (2-d array)

The symbol `lf' is available to print the \#linefeed character(s)")
    (:method (stream (data list) &rest more-lists)
      (if more-lists
	  (let* ((list-count (1+ (length more-lists)))
		 (all-lists (make-array list-count
					:initial-contents
					(cons data more-lists))))
	    (do ()
		((null (aref all-lists 0)))
	      (dotimes (i list-count)
		(princ (car (aref all-lists i)) stream)
		(princ " " stream)
		(setf (aref all-lists i) (cdr (aref all-lists i))))
	      (princ lf stream)))
	  (dolist (value data)
	    (princ value stream)
	    (princ lf stream)))
      (princ #\e stream)
      (princ lf stream))
    (:method (stream (vector vector) &rest more-vectors)
      (if more-vectors
	  (let* ((vector-count (1+ (length more-vectors)))
		 (vector-length (length vector))
		 (all-vectors (make-array vector-count
					  :initial-contents
					  (cons vector more-vectors))))
	    (do ((i 0 (+ 1 i)))
		((= i vector-length))
	      (dotimes (j vector-count)
		(princ (aref (aref all-vectors j) i) stream)
		(princ " " stream))
	      (princ lf stream)))
	  (dotimes (i (length vector))
	    (princ (elt vector i) stream)
	    (princ lf stream)))
      #+skip(princ lf stream)
      (princ #\e stream)
      (princ lf stream))
    (:method (stream (data array) &rest ignorable)
      (if ignorable
	  (error "PRINT-INLINE-DATA can accept only one array"))
      (let ((dims (array-dimensions data)))
	(assert (= 2 (length dims))
		()
		"The data argument must be a matrix.  Instead it's dimensions is ~a"
		dims)
	(dotimes (i (first dims))
	  (dotimes (j (second dims))
	    (princ (aref data i j) stream)
	    (princ " " stream))
	  (princ lf stream))
	#+skip(princ lf stream)
	(princ #\e stream)
	(princ lf stream)))))
