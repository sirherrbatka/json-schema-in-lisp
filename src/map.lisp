(in-package :json-schema-in-lisp)


(defclass json-map ()
  ((%content
    :type hash-table
    :initarg :content
    :accessor access-content)
   (%value-type
    :type symbol
    :initarg :value-type
    :reader read-value-type)))


(defmethod initialize-instance :after ((obj json-map) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-accessors ((content access-content)) obj
    (setf content (make-hash-table :test 'equal))))


(-> access-value (t json-map) (values t boolean))
(defun access-value (key map)
  (multiple-value-bind (result found) (gethash key
                                               (access-content map))
    (values result found)))


(defun (setf access-value) (value key map)
  (with-accessors ((key-type read-key-type)
                   (value-type read-value-type)
                   (content access-content)) map
    (unless (typep value value-type)
      (error "Invalid type of the value: ~a, allowed type is ~a"
             (type-of value)
             value-type))
    (setf (gethash key content)
          value)))


(defun make-json-map (data &key (value t) (minimum nil) (maximum nil))
  (unless (eq :object (car data))
    (error "Json does not contain object as root element"))
  (let ((result (make-instance 'json-map
                               :value-type value)))
    (iterate
      (with stack = nil)
      (for token-form in (cdr data))
      (for (token form) = token-form)
      (if (eq :key token)
          (let ((key (cadr form)))
            (setf (access-value key result)
                  nil)
            (push key stack))
          (setf (access-value (car stack) result)
                (extract-value-from-json (token-form) nil))))
    (let ((elements-count (hash-table-count (access-content result))))
      (when (or (and minimum (< elements-count minimum))
                (and maximum (> elements-count maximum)))
        (error "Number of elements in the map is = ~a but should be between ~a and ~a"
               elements-count
               minimum
               maximum)))
    result))

