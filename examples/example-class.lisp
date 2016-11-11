(defclass test-class ()
  ((slot
    :initarg :foo
    :optional t
    :type list)
   (bar
    :initarg :bar
    :type boolean)
   (baz
    :initarg :baz
    :type string)
   (map
    :initarg :map
    :map (:value boolean :minimum 2)))
  (:metaclass json-class))
