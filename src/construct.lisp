(in-package :json-schema-in-lisp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construct json object. Part of the api.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric make-json-instance (class json))


(defmethod make-json-instance (class (json string))
  (make-json-instance class
                      (with-input-from-string (s json)
                        (parse-json s))))


(defmethod make-json-instance (class (json list))
  (let ((class-obj (find-class class)))
    (unless (typep class-obj 'json-class)
      (error "~a is not json-class" class))
    (multiple-value-bind (present missing) (initlist-from-json-form class-obj
                                                                    json)
      (if missing
          (error "Missing values for slots with keys: ~A; in object of class: ~a" missing class)
          (apply #'make-instance (cons class present))))))


(defmethod make-json-instance (class (json stream))
  (make-json-instance class
                      (parse-json json)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle json arrays and unknown types, not part of the api.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-list-from-json-list (json)
  (unless (eq (car json) :array)
    (error "Json does not contains list as root element."))
  (mapcar (lambda (x)
            (extract-value-from-json (x)
                ()))
          (cdr json)))


(defun make-json-instance-or-map (json type)
  (unless (eq :object (car json))
    (error "Json does not contain object as root element"))
  (if (or (eq type t)
          (eq type 'json-map))
      (make-json-map (cdr json))
      (make-json-instance type (cdr json))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle data stored in the json lists.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
