
; Written by Blake McBride (blake@arahant.com)

(defpackage "MAPPINGS"
  (:use "COMMON-LISP" "UTILS" "CLOS-UTILS" "JAVA")
  (:export          ; the utilities perform an export
  ))
(in-package "MAPPINGS")


;; (format t "~%Loading mappings~2%")

;; (defvar *count* 0)
;; (setq *count* (1+ *count*))
;; (format t "~%mappings *count* = ~d~2%" *count*)

;(map-entire-java-class "com.arahant.beans.Holiday")
;(map-entire-java-class "com.arahant.beans.HrEeoCategory")
;(map-entire-java-class "com.arahant.beans.Person")
;(map-entire-java-class "com.arahant.beans.Employee")
;(map-entire-java-class "com.arahant.beans.HrBenefitJoin")
;(map-entire-java-class "com.arahant.beans.HrBenefitChangeReason")
;(map-entire-java-class "com.arahant.beans.HrBenefitConfig")
;(map-entire-java-class "com.arahant.beans.HrBenefit")
;(map-entire-java-class "com.arahant.beans.HrEmplStatusHistory")
;(map-entire-java-class "com.arahant.beans.SetupWithEndDate")
;(map-entire-java-class "com.arahant.beans.HrEmployeeStatus")
;(map-entire-java-class "com.arahant.business.BusinessLogicBase")
;(map-entire-java-class "com.arahant.business.BPerson")

(map-entire-java-class "org.kissweb.database.Connection")
(map-entire-java-class "org.kissweb.database.Command")
(map-entire-java-class "org.kissweb.database.Cursor")
(map-entire-java-class "org.kissweb.database.Record")

;; (map-java-constructor new-from-bean BPerson "com.arahant.beans.Person")
;; (progn
;;   (let ((method (jconstructor (java-class-name BPerson) "com.arahant.beans.Person")))
;;     (defmethod2 new-from-bean (eql BPerson) (bean)
;; 		(make-instance BPerson :java-object (jnew method (java-value bean)))))
;;   (export 'new-from-bean))
    


;(map-java-var HrBenefitJoin.PAYING_PERSON_ID "com.arahant.beans.IHrBenefitJoin" "PAYING_PERSON_ID")
;(map-java-var HrBenefitJoin.COVERED_PERSON_ID "com.arahant.beans.IHrBenefitJoin" "COVERED_PERSON_ID")
;(map-java-var HrBenefitJoin.HR_BENEFIT_CONFIG_ID "com.arahant.beans.IHrBenefitJoin" "HR_BENEFIT_CONFIG_ID")
;(map-java-var HrBenefitJoin.HR_BENEFIT_CONFIG "com.arahant.beans.IHrBenefitJoin" "HR_BENEFIT_CONFIG")
;(map-java-var HrBenefitConfig.HR_BENEFIT "com.arahant.beans.HrBenefitConfig" "HR_BENEFIT")
;(map-java-var HrBenefit.BENEFIT_CATEGORY_ID "com.arahant.beans.HrBenefit" "BENEFIT_CATEGORY_ID")
;(map-java-var HrBenefitChangeReason.NEW_HIRE "com.arahant.beans.HrBenefitChangeReason" "NEW_HIRE")
;(map-java-var HrBenefitChangeReason.QUALIFYING_EVENT "com.arahant.beans.HrBenefitChangeReason" "QUALIFYING_EVENT")
;(map-java-var HrBenefitChangeReason.OPEN_ENROLLMENT "com.arahant.beans.HrBenefitChangeReason" "OPEN_ENROLLMENT")
;(map-java-var HrBenefitCategory.VOLUNTARY "com.arahant.beans.HrBenefitCategory" "VOLUNTARY")
;(map-java-var HrBenefitCategory.VOLUNTARY "com.arahant.beans.HrBenefitCategory" "VOLUNTARY")

;; (map-java-class LispCall "com.arahant.lisp.LispCall")
;; (map-java-class-method myStaticMethod LispCall "myStaticMethod")
;; (map-java-instance-method-return-self myInstanceMethod LispCall "myInstanceMethod")
;; (map-java-constructor new LispCall)
;; (myStaticMethod LispCall)
;; (setq myInst (new LispCall))
;; (myInstanceMethod myInst)

;; (map-entire-java-class "com.arahant.lisp.LispCall")
;; (map-java-constructor new LispCall)
;; (myStaticMethod LispCall)
;; (setq myInst (new LispCall))
;; (myInstanceMethod myInst)


;(map-java-class DataObject     "com.arahant.utils.dynamicwebservices.DataObject")
;(map-java-class DataObjectList "com.arahant.utils.dynamicwebservices.DataObjectList")
;(map-java-class DataObjectMap  "com.arahant.utils.dynamicwebservices.DataObjectMap")

(map-java-class JSONObject     "org.kissweb.json.JSONObject")
(map-java-class JSONArray      "org.kissweb.json.JSONArray")

(map-java-class DateUtils "org.kissweb.DateUtils")
;(map-java-class-method getDateFormatted DateUtils "getDateFormatted" "int")
;(map-java-class-method addDays DateUtils "addDays" "int" "int")
;(map-java-class-method addMonths DateUtils "addMonths" "int" "int")

;; (map-java-class BProject "com.arahant.business.BProject")
;; (map-java-class-method makeProjectWithRoute BProject "makeProjectWithRoute" "java.lang.String" "java.lang.String" "java.lang.String" "java.lang.String" "java.lang.String")
;; (map-java-class-method makeRecent BProject "makeRecent" "java.lang.String" "java.lang.String" "java.lang.String" "java.lang.String" "java.lang.String")


;; (map-java-class BEmployee "com.arahant.business.BEmployee")
;; (map-java-class-method getBenefitsString BEmployee "getBenefitsString" "java.lang.String")

;(map-entire-java-class "com.arahant.business.BusinessLogicBase")
;(map-entire-java-class "com.arahant.business.SimpleBusinessObjectBase")
;(map-java-class BHRBenefitJoin "com.arahant.business.BHRBenefitJoin")
;(map-java-instance-method getChangeReasonType BHRBenefitJoin "getChangeReasonType")


;; (map-java-constructor new DataObjectMap)
;; (map-java-constructor new DataObjectList)
(map-java-constructor new JSONObject)
(map-java-constructor new JSONArray)

;; (map-java-instance-method-return-self putInt        DataObjectMap "put"   "java.lang.String" "int")
;; (map-java-instance-method-return-self putString     DataObjectMap "put"   "java.lang.String" "java.lang.String")
;; ;(map-java-instance-method-return-self putObjectList DataObjectMap "put"   "java.lang.String" "com.arahant.utils.dynamicwebservices.DataObjectList")
;; (map-java-instance-method-return-self printMap      DataObjectMap "print")
;; (map-java-instance-method-return-self addObject     DataObjectList "add"   "com.arahant.utils.dynamicwebservices.DataObject")
;; ;(map-java-instance-method-return-self addObjectMap  DataObjectList "add"   "com.arahant.utils.dynamicwebservices.DataObjectMap")


;; (create-lisp-method putIntArray DataObjectMap (name array) "put" ("java.lang.String" "[I")
;; 			   (call-java name (jnew-array-from-x "int" array))
;; 			   self)

;; (let ((method (jmethod (java-class-name DataObjectMap) "put" "java.lang.String" "[I")))
;;   (defmethod2 putIntArray DataObjectMap (name array)
;; 	      (jcall method (slot-value self 'java-instance) name (jnew-array-from-x "int" array))
;; 	      self))
;; (export 'putIntArray)

;; (create-lisp-method addObjectMap DataObjectList (dom) "add" ("com.arahant.utils.dynamicwebservices.DataObjectMap")
;; 			   (call-java (java-value dom))
;; 			   self)

;; (let ((method (jmethod (java-class-name DataObjectList) "add" "com.arahant.utils.dynamicwebservices.DataObjectMap")))
;;   (defmethod2 addObjectMap DataObjectList (dom)
;; 	      (jcall method (java-value self) (java-value dom))
;; 	      self))
;; (export 'addObjectMap)

;; (let ((method (jmethod (java-class-name DataObjectMap) "put" "java.lang.String" "com.arahant.utils.dynamicwebservices.DataObjectList")))
;;   (defmethod2 putObjectList DataObjectMap (name lst)
;; 	      (jcall method (java-value self) name (java-value lst))
;; 	      self))
;; (export 'putObjectList)

;; (create-lisp-method putObjectList DataObjectMap (name lst) "put" ("java.lang.String" "com.arahant.utils.dynamicwebservices.DataObjectList")
;; 			   (call-java name (java-value lst))
;; 			   self)

;; (defun make-web-service-args (in out hsu)
;;   (list (wrap-java in DataObjectMap)
;; 	(wrap-java out DataObjectMap)
;; 	(wrap-java hsu HibernateSessionUtil)))

(defun make-rest-service-args (in out db this)
  (list (wrap-java in JSONObject)
	(wrap-java out JSONObject)
	(wrap-java db Connection)
	this))

(map-java-instance-method getBoolean JSONObject "getBoolean" "java.lang.String")
(map-java-instance-method getInt JSONObject "getInt" "java.lang.String")
(map-java-instance-method getLong JSONObject "getLong" "java.lang.String")
(map-java-instance-method getFloat JSONObject "getFloat" "java.lang.String")
(map-java-instance-method getDouble JSONObject "getDouble" "java.lang.String")
(map-java-instance-method getString JSONObject "getString" "java.lang.String")


(map-java-instance-method-return-self putBoolean JSONObject "put" "java.lang.String" "boolean")
(map-java-instance-method-return-self putChar JSONObject "put" "java.lang.String" "char")
(map-java-instance-method-return-self putInt JSONObject "put" "java.lang.String" "int")
(map-java-instance-method-return-self putLong JSONObject "put" "java.lang.String" "long")
(map-java-instance-method-return-self putFloat JSONObject "put" "java.lang.String" "float")
(map-java-instance-method-return-self putDouble JSONObject "put" "java.lang.String" "double")
(map-java-instance-method-return-self putString JSONObject "put" "java.lang.String" "java.lang.Object")
(map-java-instance-method-return-self putArray JSONObject "put" "java.lang.String" "java.lang.Object")

(map-java-instance-method-return-self put JSONArray "put" "java.lang.Object")



