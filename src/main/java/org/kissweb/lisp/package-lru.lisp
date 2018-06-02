
; Written by Blake McBride (blake@arahant.com)

(defpackage "PACKAGE-LRU" 
  (:use "COMMON-LISP" "THREADS")
  (:export "LOAD-PACKAGE" "PACKAGE-DONE" "PACKAGE-DONE-UNLOAD" "DELETE-OLD"))
(in-package "PACKAGE-LRU")

;; (defvar *count* 0)
;; (setq *count* (1+ *count*))
;; (format t "~%package-lru *count* = ~d~2%" *count*)

(defvar *package-list* (make-hash-table :test 'equal)
  "key is the package name.
   value is (usage-count time-since-last-unload)")

(defvar *package-list-lock* (gensym))

(defvar *max-wait* 240
  "Max number of seconds to hold on to a package after it has been loaded.")

(defmacro current-seconds ()
    '(truncate (/ (get-internal-real-time) internal-time-units-per-second)))

(defun load-package (package-name file-name)
  "Load the package if it isn't already loaded but assume it is running until it calls
   package-done to indicate that it is done running and ready for possible unloading.
   This function will throw an exception if it has a problem loading the file."
  (synchronized-on *package-list-lock*
		   (let ((val (gethash package-name *package-list*)))
		     (cond ((null val)
;		  	    (print "loading")
			    (load file-name)
			    (setf (gethash package-name *package-list*) '(1 0)))
			   (t (setf (gethash package-name *package-list*)
				    (list (1+ (car val)) 0))))))
  (delete-old))

(defun package-done (package-name)
  "Signal that the package is done running and can be deleted."
  (synchronized-on *package-list-lock*
		   (let ((val (gethash package-name *package-list*)))
		     (if (consp val)
			 (if (>= 1 (car val))
			     (setf (gethash package-name *package-list*)
				   (list 0 (current-seconds)))
			     (setf (gethash package-name *package-list*)
				   (list (1- (car val)) 0))))))
  (delete-old))

(defun package-done-unload (package-name)
  "Signal that the package is done running and should be deleted if it's the last one."
  (synchronized-on *package-list-lock*
		   (let ((val (gethash package-name *package-list*)))
		     (if (consp val)
			 (if (>= 1 (car val))
			     (setf (gethash package-name *package-list*)
				   '(0 -1))
			     (setf (gethash package-name *package-list*)
				   (list (1- (car val)) 0))))))
  (delete-old))

(defun delete-old ()
  "Delete packages that have use-count of zero and their hold time is up."
  (let ((late-time (- (current-seconds) *max-wait*)))
    (synchronized-on *package-list-lock*
		     (maphash #'(lambda (key val)
				  (cond ((and (zerop (car val))
					      (not (zerop (cadr val)))
					      (> late-time (cadr val)))
;					 (print (concatenate 'string "Deleting package " key))
					 (delete-package key)
					 (remhash key *package-list*))))
			      *package-list*))))


