
; Written by Blake McBride (blake@arahant.com)


(defpackage "UTILS"
  (:use "COMMON-LISP" "CLOS-UTILS" "JAVA")
  (:export "DEFUN-INSTANCE-METHOD"
	   "MAP-ENTIRE-JAVA-CLASS"
	   "MAP-JAVA-CLASS"
	   "MAP-JAVA-CLASS-METHOD"
	   "MAP-JAVA-CONSTRUCTOR"
	   "MAP-JAVA-INSTANCE-METHOD-RETURN-SELF"
	   "MAP-JAVA-INSTANCE-METHOD"
	   "WRAP-JAVA"
	   "WRAP-JAVA-EVAL"
	   "MAP-JAVA-VAR"
	   "CLASS-NAME"
	   "JAVA-CLASS-NAME"
	   "CREATE-LISP-METHOD"
	   "JAVA-VALUE"
	   "CALL-JAVA"
	   "REC-LOOP"
	   "PRINTLN"
	   "ROUND-NUMBER"
	   "PRINT-STACK-TRACE"
	   "SELF"))
(in-package "UTILS")

;; (defvar *count* 0)
;; (setq *count* (1+ *count*))
;; (format t "~%utils *count* = ~d~2%" *count*)

;;  this should be defined early in the process!
(defun print-stack-trace (stack)
  (loop for elm in stack
       do (prin1 elm *error-output*)
       (terpri *error-output*)))

(defmacro defun-class-method (lfun cls-name meth-name &rest arg-types)
  "Maps a Java class method to a lisp function"
  (let ((arglist (mapcar (lambda (x) (gensym)) arg-types))
	(method (gensym))
	(cls (gensym)))
    `(let ((,method (jmethod ,cls-name ,meth-name ,@arg-types))
	   (,cls (jclass ,cls-name)))
       (defun ,lfun ,arglist
	 (jcall ,method ,cls ,@arglist)))))

(defmacro defun-instance-method (lfun cls-name meth-name &rest arg-types)
  "Maps a Java instance method to a lisp function"
  (let ((arglist (mapcar (lambda (x) (gensym)) (cons nil arg-types)))
	(method (gensym)))
    `(let ((,method (jmethod ,cls-name ,meth-name ,@arg-types)))
       (defun ,lfun ,arglist
	 (jcall ,method ,@arglist)))))

(defmacro defun-constructor (lfun cls-name &rest arg-types)
  "Maps a Java constructor to a lisp function"
  (let ((arglist (mapcar (lambda (x) (gensym)) arg-types))
	(method (gensym)))
    `(let ((,method (jconstructor ,cls-name ,@arg-types)))
       (defun ,lfun ,arglist
	 (jnew ,method ,@arglist)))))

(defun-class-method Class.forName "java.lang.Class" "forName" "java.lang.String")
(defun-instance-method Class.getDeclaredMethods "java.lang.Class" "getDeclaredMethods")
(defun-instance-method Class.getDeclaredFields "java.lang.Class" "getDeclaredFields")
(defun-instance-method Class.getName "java.lang.Class" "getName")
(defun-instance-method Class.getSuperclass "java.lang.Class" "getSuperclass")
(defun-instance-method Method.getName "java.lang.reflect.Method" "getName")
(defun-instance-method Method.getReturnType "java.lang.reflect.Method" "getReturnType")
(defun-instance-method Method.getParameterTypes "java.lang.reflect.Method" "getParameterTypes")
(defun-instance-method Method.getModifiers "java.lang.reflect.Method" "getModifiers")
(defun-instance-method Field.getName "java.lang.reflect.Field" "getName")
(defun-instance-method Field.getModifiers "java.lang.reflect.Field" "getModifiers")
(defun-class-method Modifier.isPublic "java.lang.reflect.Modifier" "isPublic" "int")
(defun-class-method Modifier.isStatic "java.lang.reflect.Modifier" "isStatic" "int")

(defmacro while (what &rest code)
  (let ((var (gensym)))
    `(do ()
	  ((not ,what))
	,@code)))

(defmacro while-more (s &rest code)
    `(while (next ,s) ,@code))

(defmacro cat (&rest str)
    `(concatenate 'string ,@str))

(defmacro println (&rest v)
    `(progn (princ (cat ,@v))
        (terpri)))

;(defmacro toString (num)
;    `(format nil "~A" ,num))

;  The following code is used because the Java obfuscator renames fields and changes protected fields to public
;  causing a lisp null pointer error
(defmacro defvar-if-not-null (var val)
  (let ((tvar (gensym)))
    `(let ((,tvar (ignore-errors ,val)))
       (if ,tvar (defvar ,var ,tvar)))))

(defun get-last-part (name)
  (let ((idx (position #\. name :from-end t)))
    (if  (numberp idx)
	 (subseq name (1+ idx))
	 name)))

(defmacro map-java-instance-methods (class-name)
  (let* ((cls (Class.forName class-name))
	 (cls-name (Class.getName cls))
	 (methods (Class.getDeclaredMethods cls))
	 (nmethods (array-total-size methods))
	 (package (get-last-part cls-name))
	 ret)
    `(progn
       ,@(do ((i 0 (1+ i)))
	     ((eql i nmethods) ret)
	     (let* ((meth (aref methods i))
		    (meth-name (Method.getName meth))
		    (params (Method.getParameterTypes meth))
		    (np (array-total-size params))
		    (arg-types (nreverse (do ((i 0 (1+ i))
					      (pl nil pl))
					     ((eql i np) pl)
					   (setq pl (cons (Class.getName (aref params i)) pl)))))
		    (lisp-func-name (intern meth-name))
		    (method (gensym))
		    (arglist (do ((i 0 (1+ i))
				  (al nil al))
				 ((> i np) al)
			       (setq al (cons (gensym) al)))))
	       (setq ret (cons `(let ((,method (jmethod ,cls-name ,meth-name ,@arg-types)))
				  (defun ,lisp-func-name ,arglist
				    (jcall ,method ,@arglist)))
			       ret)))))))

(defmacro java-value (ins)
	`(slot-value ,ins 'java-instance))

(defmacro map-entire-java-class (class-name)
  "Maps an entire Java class to a CLOS class that is created.  Does not map the constructors."
  (let* ((cls (Class.forName class-name))
	 (cls-name (Class.getName cls))
	 (methods (Class.getDeclaredMethods cls))
	 (nmethods (array-total-size methods))
	 (fields (Class.getDeclaredFields cls))
	 (nfields (array-total-size fields))
	 (short-name (get-last-part cls-name))
	 (class (intern short-name))
	 (super-class (Class.getSuperclass cls))
	 (super-class-name (Class.getName super-class))
	 (super-class-short-name (get-last-part super-class-name))
	 (super-symbol (intern super-class-short-name))
	 ret fret)
    `(progn

       ;; (defclass ,class
       ;; 	   ,(if (not (or (equal super-class-short-name "ArahantBean")
       ;; 			       (equal super-class-short-name "AuditedBean")
       ;; 			       (equal super-class-short-name "Filtered")))
       ;; 		      (list super-symbol))
       ;; 	 ((java-instance :initarg :java-object)
       ;; 			    (class-name :allocation :class :initform ,class-name)
       ;; 			    (class-object :allocation :class :initform (Class.forName ,class-name))))
       
       (defclass2 ,class
	   ,(if (not (or (equal super-class-short-name "Object")))
		(list super-symbol))
	 ((class-name :initform ,class-name)
	  (class-object :initform (Class.forName ,class-name)))
	 ((java-instance :initarg :java-object)))

       (export ',class)
       
       ,@(do ((i 0 (1+ i)))
	     ((eql i nfields) fret)
	     (let* ((fld (aref fields i))
		    (fld-name (Field.getName fld))
		    (mods (Field.getModifiers fld)))
	       (if (and (Modifier.isPublic mods)
			(Modifier.isStatic mods))
		   (let ((vname (intern (concatenate 'string short-name "." fld-name))))
		     (setq fret (cons `(defvar-if-not-null ,vname (jfield ,class-name ,fld-name))
				      fret))
		     (nconc fret (list `(export ',vname)))))))
       ,@(do ((i 0 (1+ i)))
	     ((eql i nmethods) ret)
	     (let* ((meth (aref methods i))
		    (meth-name (Method.getName meth))
		    (return-type (Method.getReturnType meth))
		    (return-type-short-name (get-last-part (Class.getName return-type)))
		    (mods (Method.getModifiers meth))
		    (params (Method.getParameterTypes meth))
		    (np (array-total-size params))
		    (arg-types (nreverse (do ((i 0 (1+ i))
					      (pl nil pl))
					     ((eql i np) pl)
					   (setq pl (cons (Class.getName (aref params i)) pl)))))
		    (lisp-func-name (intern meth-name))
		    (method (gensym))
		    (clsn (gensym))
		    (tmpvar (gensym))
		    (arglist (do ((i 0 (1+ i))
				  (al nil al))
				 ((eql i np) al)
			       (setq al (cons (gensym) al)))))
	       (if  (Modifier.isPublic mods)
		    (progn
		      (setq ret (cons (if (or (equal return-type-short-name "void")
					      (equal return-type-short-name "byte")
					      (equal return-type-short-name "char")
					      (equal return-type-short-name "short")
					      (equal return-type-short-name "int")
					      (equal return-type-short-name "long")
					      (equal return-type-short-name "float")
					      (equal return-type-short-name "double")
					      (equal return-type-short-name "String"))
					  (if (Modifier.isStatic mods)

					      `(let ((,method (jmethod ,cls-name ,meth-name ,@arg-types))
						     (,clsn (java-class-object ,class)))
						 (defmethod2 ,lisp-func-name (eql ,class) ,arglist
							     (jcall ,method ,clsn ,@arglist)))

					      `(let ((,method (jmethod ,cls-name ,meth-name ,@arg-types)))
						 (defmethod2 ,lisp-func-name ,class ,arglist
							     (jcall ,method (java-value self) ,@arglist)))

					      )
					  (if (Modifier.isStatic mods)
					      
					      `(let ((,method (jmethod ,cls-name ,meth-name ,@arg-types))
						     (,clsn (java-class-object ,class)))
						 (defmethod2 ,lisp-func-name (eql ,class) ,arglist
							     (let ((,tmpvar (jcall ,method  ,clsn ,@arglist)))
							       (if ,tmpvar
								   (make-instance ',(intern return-type-short-name) :java-object ,tmpvar)))))
					      
					      `(let ((,method (jmethod ,cls-name ,meth-name ,@arg-types)))
						 (defmethod2 ,lisp-func-name ,class ,arglist
							     (let ((,tmpvar (jcall ,method (java-value self) ,@arglist)))
							       (if ,tmpvar
								   (make-instance ',(intern return-type-short-name) :java-object ,tmpvar)))))

					      ))
				      ret))
		      (nconc ret (list `(export ',lisp-func-name))))))))))

;; (defmacro map-java-class (lisp-class java-class-name)
;;   `(defvar-if-not-null ,lisp-class
;;      (defclass ,lisp-class () ((java-instance :initarg :java-object)
;; 			       (class-name :allocation :class :initform ,java-class-name)
;; 			       (class-object :allocation :class :initform (jclass ,java-class-name))))))

;; (defmacro java-class-name (lisp-class)
;;   `(let ((i (make-instance ',lisp-class)))
;;      (slot-value i 'class-name)))

;; (defmacro java-class-object (lisp-class)
;;   `(let ((i (make-instance ',lisp-class)))
;;      (slot-value i 'class-object)))


(defmacro map-java-class (lisp-class java-class-name &optional super-class)
  "Maps a Java class to a CLOS class"
  `(progn
     (defclass2 ,lisp-class ,(if super-class (list super-class) nil)
       ((class-name :initform ,java-class-name)
	(class-object :initform (jclass ,java-class-name)))
       ((java-instance :initarg :java-object)))
     (export ',lisp-class)))

(defmacro java-class-name (lisp-class)
  `(get-slot ,lisp-class class-name))

(defmacro java-class-object (lisp-class)
  `(get-slot ,lisp-class class-object))

(defmacro java-instance-object (lisp-instance)
  `(get-slot ,lisp-instance java-instance))

(defmacro map-java-var (lispvar javaclass jfldname)
  "This macro creates a lisp global variable that has a copy of a java global variable."
  `(progn
     (defvar ,lispvar (jfield ,javaclass ,jfldname))
     (export ',lispvar)))

(defun jnew-array-from-x (element-type var)
  "Returns a new Java array of type ELEMENT-TYPE from a variety of lisp types"
  (cond ((typep var 'cons)
	 (jnew-array-from-list element-type var))
	((typep var 'array)
	 (jnew-array-from-array element-type var))
	(t
	 (jnew-array-from-list element-type (list var)))))

(defmacro call-java (&rest args)
  "Convenience macro to be used inside calls to create-lisp-method"
  `(jcall java-method232 (java-value self) ,@args))

(defmacro create-lisp-method (lisp-method lisp-class meth-arglist java-meth-name java-arg-types &rest code)
  "Creates a CLOS instance method that is able to execute a Java instance method"
  `(progn
     (let ((java-method232 (jmethod (java-class-name ,lisp-class) ,java-meth-name ,@java-arg-types)))
       (defmethod2 ,lisp-method ,lisp-class ,meth-arglist
		   ,@code))
     (export ',lisp-method)))

(defmacro map-java-instance-method (lisp-method lisp-class java-meth-name &rest java-arg-types)
  "Maps a Java instance method to a CLOS method returning a Java native object"
  (let ((arglist (mapcar (lambda (x) (gensym)) java-arg-types))
	(method (gensym)))
    `(progn
       (let ((,method (jmethod (java-class-name ,lisp-class) ,java-meth-name ,@java-arg-types)))
;        (defmethod ,lisp-method ((self ,lisp-class) ,@arglist)
	 (defmethod2 ,lisp-method ,lisp-class ,arglist
		     (jcall ,method (java-value self) ,@arglist)))
       (export ',lisp-method))))

(defmacro map-java-class-method-with-wrapper (lisp-method lisp-class java-meth-name other-class &rest java-arg-types)
  "Maps a Java class method with a CLOS method.  The \"-with-wrapper\" means that it returns in instance of another class"
  (let ((arglist (mapcar (lambda (x) (gensym)) java-arg-types))
	(method (gensym))
	(cls (gensym))
	(tmpvar (gensym)))
    `(progn
       (let ((,method (jmethod (java-class-name ,lisp-class) ,java-meth-name ,@java-arg-types))
	     (,cls (java-class-object ,lisp-class)))
;        (defmethod ,lisp-method ((self (eql ,lisp-class)) ,@arglist)
	 (defmethod2 ,lisp-method (eql ,lisp-class) ,arglist
		     (let ((,tmpvar (jcall ,method ,cls ,@arglist)))
		       (if ,tmpvar
			   (make-instance ',other-class :java-object ,tmpvar)))))
       (export ',lisp-method))))

(defmacro map-java-class-method (lisp-method lisp-class java-meth-name &rest java-arg-types)
  (let ((arglist (mapcar (lambda (x) (gensym)) java-arg-types))
	(method (gensym))
	(cls (gensym)))
    `(progn
       (let ((,method (jmethod (java-class-name ,lisp-class) ,java-meth-name ,@java-arg-types))
	     (,cls (java-class-object ,lisp-class)))
;        (defmethod ,lisp-method ((self (eql ,lisp-class)) ,@arglist)
	 (defmethod2 ,lisp-method (eql ,lisp-class) ,arglist
		     (jcall ,method ,cls ,@arglist)))
       (export ',lisp-method))))

(defmacro map-java-constructor (lisp-method lisp-class &rest java-arg-types)
  "Map a Java constructor to a CLOS method."
  (let ((arglist (mapcar (lambda (x) (gensym)) java-arg-types))
	(method (gensym)))
    `(progn
       (let ((,method (jconstructor (java-class-name ,lisp-class) ,@java-arg-types)))
;        (defmethod ,lisp-method ((self (eql ,lisp-class)) ,@arglist)
	 (defmethod2 ,lisp-method (eql ,lisp-class) ,arglist
		     (make-instance ',lisp-class :java-object (jnew ,method ,@arglist))))
       (export ',lisp-method))))

(defmacro map-java-instance-method-with-wrapper (lisp-method lisp-class java-meth-name other-class &rest java-arg-types)
  "
   lisp-method        the name of the lisp method we are creating
   lisp-class         in this lisp class
   java-method-name   the java method we are mapping to
   other-class        the java class the java method returns
   java-arg-types     the method signature
   "
  (let ((arglist (mapcar (lambda (x) (gensym)) java-arg-types))
	(method (gensym))
	(tmpvar (gensym)))
    `(progn
       (let ((,method (jmethod (java-class-name ,lisp-class) ,java-meth-name ,@java-arg-types)))
;        (defmethod ,lisp-method ((self ,lisp-class) ,@arglist)
	 (defmethod2 ,lisp-method ,lisp-class ,arglist
		     (let ((,tmpvar (jcall ,method (java-value self) ,@arglist)))
		       (if ,tmpvar
			   (make-instance ',other-class :java-object ,tmpvar)))))
       (export ',lisp-method))))

(defmacro map-java-instance-method-return-self (lisp-method lisp-class java-meth-name &rest java-arg-types)
  (let ((arglist (mapcar (lambda (x) (gensym)) java-arg-types))
	(method (gensym)))
    `(progn
       (let ((,method (jmethod (java-class-name ,lisp-class) ,java-meth-name ,@java-arg-types)))
;        (defmethod ,lisp-method ((self ,lisp-class) ,@arglist)
	 (defmethod2 ,lisp-method ,lisp-class ,arglist
		     (jcall ,method (java-value self) ,@arglist)
		     self))
       (export ',lisp-method))))

(defmacro list-loop (lst java-rec locals &body body)
  (let ((it (gensym)))
    `(let ((,it (iterator ,lst))
	   ,java-rec
	   ,@locals)
       (while (hasNext ,it)
	 (setq ,java-rec (next ,it))
	 ,@body))))

(defmacro wrap-java (java-obj clos-class)
  "Wrap a Java object into a CLOS object"
  `(make-instance ',clos-class :java-object ,java-obj))

(defmacro wrap-java-eval (java-obj clos-class)
  "Wrap a Java object into a CLOS object - evaluate the clos-class"
  `(make-instance ,clos-class :java-object ,java-obj))

(defmacro rec-loop (lst lisp-rec class locals &body body)
  (let ((jrec (gensym)))
    `(list-loop ,lst ,jrec ,locals
;		(wrap-java ,lisp-rec ,jrec ,class)
		(setq ,lisp-rec (make-instance ',class :java-object ,jrec))
		,@body)))


;; (defmacro createHCU (hsu cls)
;;   `(createCriteria ,hsu (let ((i (make-instance ',cls)))
;; 			  (slot-value i 'class-name))))

;; (defmacro createHCU (hsu cls)
;;   `(createCriteria ,hsu (get-slot ,cls class-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(map-java-class Statement "com.arahant.sql.Statement")
;(map-java-constructor new Database "java.sql.Connection")
;(map-java-instance-method release Database "release")
;(map-java-instance-method-with-wrapper createStatement Database "createStatement" Statement)
;(map-java-instance-method closeStatement Statement "close")
;(map-java-instance-method execute Statement "execute" "java.lang.String")
;(map-java-instance-method next Statement "next")
;(map-java-instance-method getString Statement "getString" "java.lang.String")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (map-java-class JClass "java.lang.Class")
;; (map-java-class JMethod "java.lang.reflect.Method")
;; (map-java-class JField "java.lang.reflect.Field")
;; (map-java-class JModifier "java.lang.reflect.Modifier")

;; (map-java-class-method-with-wrapper forName JClass "forName" JClass "java.lang.String")
;; (map-java-instance-method getDeclaredMethods JClass "getDeclaredMethods")
;; (map-java-instance-method getDeclaredFields JClass "getDeclaredFields")
;; (map-java-instance-method getName JClass "getName")
;; (map-java-instance-method getName JMethod "getName")
;; (map-java-instance-method getParameterTypes JMethod "getParameterTypes")
;; (map-java-instance-method getName JField "getName")
;; (map-java-instance-method getModifiers JField "getModifiers")
;; (map-java-instance-method isPublic JModifier "isPublic" "int")


(map-java-class iterator "java.util.Iterator")
(map-java-instance-method hasNext iterator "hasNext")
(map-java-instance-method next iterator "next")


(map-java-class LinkedList "java.util.LinkedList")
(map-java-constructor newLinkedList LinkedList)
(map-java-instance-method addLast LinkedList "addLast" "java.lang.Object")

(defun LispListToJavaList (lst)
  "Convert a Lisp list to a Java LinkedList."
  (let ((jll (newLinkedList LinkedList)))
    (map nil #'(lambda (x)
		 (cond ((consp x)
			(addLast jll (LispListToJavaList x)))
		       (t
			(addLast jll x))))
	 lst)
    (java-value jll)))


;(map-java-class ArahantSession "com.arahant.utils.ArahantSession")
;(map-java-class HibernateSessionUtil "com.arahant.utils.HibernateSessionUtil")
;(map-java-class HibernateCriteriaUtil "com.arahant.utils.HibernateCriteriaUtil")
(map-java-class jlist "java.util.List")
(map-java-instance-method-with-wrapper iterator jlist "iterator" iterator)
;(map-java-class HibernateScrollUtil "com.arahant.utils.HibernateScrollUtil")

;(map-java-instance-method-with-wrapper scroll HibernateCriteriaUtil "scroll" HibernateScrollUtil)

;(create-lisp-method in HibernateCriteriaUtil (prop lst) "in" ("java.lang.String" "java.util.Collection")
;		    (call-java prop (LispListToJavaList lst))
;		    self)

;(map-java-instance-method hasNext HibernateScrollUtil "next")
;(map-java-instance-method next HibernateScrollUtil "get")

; Unlike getting a list of records back that have to be enumerated, a
; HibernateScrollUtil already acts like an iterator so we don't need
; to get an actual iterator
;(defmethod2 iterator HibernateScrollUtil ()
;  self)

;(map-java-class-method-with-wrapper openHSU ArahantSession "openHSU" HibernateSessionUtil)
;(map-java-class-method-with-wrapper getHSU ArahantSession "getHSU" HibernateSessionUtil)
;(map-java-class-method closeHSU ArahantSession "closeHSU")

;(map-java-instance-method setCurrentPersonToArahant HibernateSessionUtil "setCurrentPersonToArahant")
;(map-java-instance-method commitTransaction HibernateSessionUtil "commitTransaction")
;(map-java-instance-method rollbackTransaction HibernateSessionUtil "rollbackTransaction")
;(map-java-instance-method-with-wrapper createCriteria HibernateSessionUtil "createCriteria" HibernateCriteriaUtil "java.lang.String")

;; (map-java-instance-method getRecordInternal HibernateSessionUtil "get" "java.lang.Class" "java.lang.String")
;; (defmacro getRecord (hsu class rec)
;;   `(wrap-java (getRecordInternal ,hsu (java-class-object ,class) ,rec) ,class))

;(progn
;  (let* ((class-name (java-class-name HibernateSessionUtil))
;	 (meth (jmethod class-name "get" "java.lang.Class" "java.lang.String")))
;    (defmethod2 getRecord HibernateSessionUtil (class rec)
;		(let ((ret (jcall meth (java-value self) (java-class-object class) rec)))
;		  (if ret
;		      (wrap-java-eval ret class)
;		      nil))))
;  (export 'getRecord))

;(progn
;  (let* ((class-name (java-class-name HibernateCriteriaUtil))
;	 (smeth (jmethod class-name "eq" "java.lang.String" "java.lang.String"))
;	 (imeth (jmethod class-name "eq" "java.lang.String" "int"))
;	 (cmeth (jmethod class-name "eq" "java.lang.String" "char")))
;    (defmethod2 eqVal HibernateCriteriaUtil (fld val)
;		(jcall (cond ((typep val 'string) smeth)
;			     ((typep val 'integer) imeth)
;			     ((typep val 'standard-char) cmeth))
;		       (java-value self)
;		       fld val)
;		self))
;  (export 'eqVal))
			      

;(map-java-instance-method-return-self orderBy HibernateCriteriaUtil "orderBy" "java.lang.String")
;(map-java-instance-method-return-self orderByDesc HibernateCriteriaUtil "orderByDesc" "java.lang.String")
;(map-java-instance-method-return-self eqString HibernateCriteriaUtil "eq" "java.lang.String" "java.lang.String")
;(map-java-instance-method-return-self geString HibernateCriteriaUtil "ge" "java.lang.String" "java.lang.String")
;(map-java-instance-method-return-self gtString HibernateCriteriaUtil "gt" "java.lang.String" "java.lang.String")
;(map-java-instance-method-return-self ltString HibernateCriteriaUtil "lt" "java.lang.String" "java.lang.String")
;(map-java-instance-method-return-self leString HibernateCriteriaUtil "le" "java.lang.String" "java.lang.String")
;(map-java-instance-method-return-self eqInt HibernateCriteriaUtil "eq" "java.lang.String" "int")
;(map-java-instance-method-return-self leInt HibernateCriteriaUtil "le" "java.lang.String" "int")
;(map-java-instance-method-return-self ltInt HibernateCriteriaUtil "lt" "java.lang.String" "int")
;(map-java-instance-method-return-self gtInt HibernateCriteriaUtil "gt" "java.lang.String" "int")
;(map-java-instance-method-return-self geInt HibernateCriteriaUtil "ge" "java.lang.String" "int")
;(map-java-instance-method-return-self isNull HibernateCriteriaUtil "isNull" "java.lang.String")
;(map-java-instance-method-return-self notNull HibernateCriteriaUtil "notNull" "java.lang.String")
;(map-java-instance-method-with-wrapper joinTo HibernateCriteriaUtil "joinTo" HibernateCriteriaUtil "java.lang.String")
;;(defun-instance-method getJavaList "com.arahant.utils.HibernateCriteriaUtil" "list")
;(map-java-instance-method-with-wrapper getList HibernateCriteriaUtil "list" jlist)
;(map-java-instance-method firstrec HibernateCriteriaUtil "first")
;(map-java-instance-method exists HibernateCriteriaUtil "exists")
;;(map-java-instance-method-return-self inCollection HibernateCriteriaUtil "in" "java.lang.String" "java.lang.String")

;(progn
;  (let* ((class-name (java-class-name HibernateCriteriaUtil))
;	 (meth (jmethod class-name "in" "java.lang.String" "java.util.Collection")))
;    (defmethod2 in-list HibernateCriteriaUtil (prop col)
;		(jcall meth
;		       (java-value self)
;		       prop (LispListToJavaList col))
;		self))
;  (export 'in-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *kiss-lisp-packages* (make-hash-table :test 'equal :size 100))

(defmacro fill-package-map (&rest vals)
  `(progn
     (clrhash *kiss-lisp-packages*)
     ,@(mapcar  (lambda (elm)
		  `(setf (gethash ,(car elm) *kiss-lisp-packages*)
			 ,(cadr elm)))
		vals)))

(defmacro append-package-map (&rest vals)
  `(progn
     ,@(mapcar  (lambda (elm)
		  `(setf (gethash ,(car elm) *kiss-lisp-packages*)
			 ,(cadr elm)))
		vals)))

(defun find-file (package)
  (gethash package *kiss-lisp-packages*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defclass2 Query () ()
;; 	   ((hsu :initform (getHSU ArahantSession))
;; 	    (bean-class :initarg :class)
;; 	    (crit :initform nil)))

;; (defmethod2 eqVal Query (fld val)
;; 	    (with-slots (hsu bean-class crit) self
;; 	      (if (null crit)
;; 		  (setq crit (createHCU hsu bean-class)))
;; 	      (eqVal crit fld val)))

;; (defmethod2 in Query (fld lst)
;; 	    (with-slots (hsu bean-class crit) self
;; 	      (if (null crit)
;; 		  (setq crit (createHCU hsu bean-class)))
;; 	      (in crit fld lst)))

;; (defmethod2 firstrec Query ()
;; 	    (with-slots (crit bean-class) self
;; 	      (let ((rec (firstrec crit)))
;; 		    (if rec
;; 			(wrap-java-eval rec bean-class)
;; 			nil))))

;; (defmethod2 joinTo Query (joinColName)
;; 	    (with-slots (hsu bean-class crit) self
;; 		 (if (null crit)
;; 		  (setq crit (createHCU hsu bean-class)))
;; 	      (setq crit (joinTo crit joinColName))
;; 	      crit))

;; (defmethod2 exists Query ()
;; 	    (with-slots (bean-class crit) self
;; 	      (exists crit)))

;; (defmethod2 in-list Query (prop col)
;; 	    (with-slots (crit bean-class) self
;; 	      (if (null crit)
;; 		  (setq crit (createHCU hsu bean-class)))
;; 	      (in-list crit prop col)))

;; (defmethod2 orderBy Query (col)
;; 	    (with-slots (crit bean-class) self
;; 	      (if (null crit)
;; 		  (setq crit (createHCU hsu bean-class)))
;; 	      (orderBy crit col)))

(defun round-number (num places)
  "Round number n to p places"
  (declare (real num)
	   (integer places))
  (let* ((d (expt 10.0d0 places))
	 (r (/ (floor (+ 0.5d0 (abs (* num d)))) d)))
    (declare (double-float d r))
    (if (< num 0.0)
	(- r)
	r)))


