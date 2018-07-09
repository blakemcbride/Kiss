(defpackage "services/MyLispService"
  (:use "COMMON-LISP" "MAPPINGS" "UTILS" "CLOS-UTILS" "JAVA")
  (:export "MAIN"))
(in-package "services/MyLispService")

(defun addNumbers (injson outjson db servlet)
  (let* ((num1 (getInt injson "num1"))
	 (num2 (getInt injson "num2")))
    (putInt outjson "num3" (+ num1 num2))))

