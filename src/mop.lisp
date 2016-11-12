(in-package :json-schema-in-lisp)
(annot:enable-annot-syntax)


@export
(defclass json-class (standard-class) ()
  (:documentation "Metaclass of json serializable"))


(defmethod initialize-instance :after ((obj json-class) &rest initargs)
  (declare (ignore initargs)))


(defmethod closer-mop:validate-superclass ((class json-class)
                                           (superclass standard-class))
  t)


(defclass json-standard-slot-definition ()
  ((%required :initarg :optional
              :reader read-optional
              :type boolean
              :initform nil)
   (%map :type list
         :initarg :map
         :reader read-map)))


(defgeneric validate-slot-definition (slot))


(defmethod initialize-instance :after ((obj json-standard-slot-definition) &rest initargs)
  (declare (ignore initargs))
  (validate-slot-definition obj))



(defclass json-standard-direct-slot-definition (json-standard-slot-definition
                                                closer-mop:standard-direct-slot-definition)
  ())


(defclass json-standard-effective-slot-definition (json-standard-slot-definition
                                                   closer-mop:standard-effective-slot-definition)
  ())


(defun validate-map (slot)
  (let ((slot-name (closer-mop:slot-definition-name slot)))
    (when (slot-boundp slot '%map)
      (unless (eq t (closer-mop:slot-definition-type slot))
        (error "Incompatible map (~a) and type (~a) values in the slot ~a"
               (read-map slot)
               (closer-mop:slot-definition-type slot)
               slot-name))
      (let ((map-list (read-map slot)))
        (let ((min nil))
          (when-let ((minimal-number (getf map-list :minimum)))
            (setf min minimal-number)
            (when (> 0 minimal-number)
              (error "Minimal number of elements in schema map should be larger than 0.")))
          (when-let ((maximal-number (getf map-list :maximum)))
            (when (and min (> min maximal-number))
              (error "Maximum can't < to minimum"))
            (when (> 0 maximal-number)
              (error "Maximal number of elements in schema map should be larger than 0."))))))))


(defmethod validate-slot-definition ((slot json-standard-slot-definition))
  (validate-map slot))


(defmethod closer-mop:direct-slot-definition-class ((class json-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'json-standard-direct-slot-definition))


(defmethod closer-mop:effective-slot-definition-class ((class json-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'json-standard-effective-slot-definition))


(-> map-slots-with-initarg-key (list) hash-table)
(defun map-slots-with-initarg-key (slots)
  (let ((result (make-hash-table :test 'eq)))
    (iterate
      (for slot in slots)
      (for initarg = (car (closer-mop:slot-definition-initargs slot)))
      (when initarg
        (setf (gethash initarg result)
              slot)))
    result))


(-> initlist-from-json-form (json-class list) list)
(defun initlist-from-json-form (json-class json)
  (unless (eq :object (car json))
    (error "Json does not contain object as root element"))
  (let* ((slots (closer-mop:class-direct-slots json-class))
         (slots-map (map-slots-with-initarg-key slots))
         (current-slot nil)
         (init-map (make-hash-table :test 'eq))
         (initlist (iterate
                     (with skip = 0)
                     (for field in (cdr json))
                     (unless (zerop skip)
                       (decf skip)
                       (next-iteration))
                     (collect (or (when (eq (car field) :key)
                                    (destructuring-bind (key (string name)) field
                                      (assert (eq key :key))
                                      (assert (eq string :string))
                                      (let* ((symbol (find-symbol (string-upcase name) +keywords+))
                                             (slot (and symbol (gethash symbol slots-map))))
                                        (if slot
                                            (progn
                                              (setf current-slot slot)
                                              (setf (gethash symbol init-map)
                                                    current-slot)
                                              symbol)
                                            (progn (incf skip 1)
                                                   (next-iteration))))))
                                  (setf current-slot
                                        (extract-value-from-json (field)
                                            (:object
                                             (let ((type (closer-mop:slot-definition-type current-slot))
                                                   (map-args (and (slot-boundp current-slot
                                                                               '%map)
                                                                  (read-map current-slot))))
                                               (make-json-instance-or-map field
                                                                          type
                                                                          map-args))))))))))
    (values initlist
            (iterate
              (for (key slot) in-hashtable slots-map)
              (for init-value = (gethash key init-map))
              (unless (or init-value
                          (read-optional slot))
                (collect key at start))))))
