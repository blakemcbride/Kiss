(defpackage "page1"
  (:use "COMMON-LISP" "MAPPINGS" "UTILS" "CLOS-UTILS" "JAVA")
  (:export "MAIN"))
(in-package "page1")

(defun main (injson outjson db servlet)
  (let* ((num1 (getInt injson "num1"))
	 (num2 (getInt injson "num2")))
    (putInt outjson "num3" (+ num1 num2))))

