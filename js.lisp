(load (merge-pathnames "util.lisp" *load-truename*))

(category js-def)
(category js-act)

(setf (js-def 'fun)
      (lambda (name args &rest body)
          (format nil "function ~A(~A){~{~A;~}}"
                  name
                  (reduce (lambda (a v) (format nil "~A, ~A" a v))
                          args)
                  (mapcar (lambda (v)
                            (let ((f (and (listp v) (js-act (first v)))))
                              (if f (apply f (rest v)) v)))
                          body))))

(setf (js-act 'alert)
      (lambda (&rest args)
        (format nil "alert(~A)"
                (reduce (lambda (a v)
                          (format nil "~A + ~A" a v))
                        args))))

(let ((counter 0))
  (defun js-id (base)
    (incf counter)
    (format nil "~A_~X" base counter)))

(defun js (&rest definitions)
  (!+ 'tag := "script"
           :& '("type" "text/javascript")
           :< (loop for def in definitions
                    collect (apply (js-def (car def)) (cdr def)))))
