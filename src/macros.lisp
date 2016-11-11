(in-package :json-schema-in-lisp)


(defmacro extract-value-from-json ((form) (&key string object int real boolean array))
  (once-only (form)
    (unless string
      (setf string `(cadr ,form)))
    (unless object
      (setf object `(make-json-map (cadr ,form))))
    (unless int
      (setf int `(cadr ,form)))
    (unless real
      (setf real `(cadr ,form)))
    (unless boolean
      (setf boolean `(cadr ,form)))
    (unless array
      (setf array `(make-list-from-json-list ,form)))
    `(switch ((car ,form) :test 'eq :key identity)
       (:object ,object)
       (:int ,int)
       (:real ,real)
       (:bool ,boolean)
       (:string ,string)
       (:array ,array))))
