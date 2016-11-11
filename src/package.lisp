(defpackage :json-schema-in-lisp
  (:use :common-lisp :iterate :alexandria :serapeum
        :cl-annot :annot.std :annot.eval-when :annot.slot)
  (:shadowing-import-from :iterate :collecting :summing :in))
