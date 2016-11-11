(in-package :json-schema-in-lisp)
(annot:enable-annot-syntax)


(-> parse-json (stream) list)
(defun parse-json (json)
  (let ((stack nil)
        (boolhandler cl-json:*boolean-handler*)
        (inthandler cl-json:*integer-handler*)
        (realhandler cl-json:*real-handler*))
    (labels ((collect-char (x)
               (format (car stack) "~a" x))
             (string-start ()
               (push (make-string-output-stream)
                     stack))
             (string-end ()
               (list :string (get-output-stream-string (pop stack))))
             (object-start ()
               (push (list :object) stack))
             (object-end ()
               (reverse (pop stack)))
             (object-key (x)
               (push (list :key x)
                     (car stack)))
             (object-value (x)
               (push x
                     (car stack)))
             (array-start ()
               (push (list :array) stack))
             (array-member (x)
               (push x (car stack)))
             (array-end ()
               (reverse (pop stack))))
      (json:bind-custom-vars
       (:beginning-of-array   #'array-start
        :integer              (lambda (x) (list :int (funcall inthandler x)))
        :real                 (lambda (x) (list :real (funcall realhandler x)))
        :boolean              (lambda (x) (list :bool (funcall boolhandler x)))
        :array-member         #'array-member
        :end-of-array         #'array-end
        :beginning-of-string  #'string-start
        :string-char          #'collect-char
        :end-of-string        #'string-end
        :beginning-of-object  #'object-start
        :object-key           #'object-key
        :object-value         #'object-value
        :end-of-object        #'object-end)
        (assure list (cl-json:decode-json json))))))



