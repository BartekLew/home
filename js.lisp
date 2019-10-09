(load (merge-pathnames "util.lisp" *load-truename*))

(category js-def)

(defun js-name (name)
  (cond ((symbolp name)
          (reduce (lambda (a v)
                    (format nil "~A~A~A" a (char-upcase (char v 0))
                                       (subseq v 1)))
                  (split (format nil "~A" (string-downcase name)) #\-)))
        ((stringp name) (format nil "\"~A\"" name))
        (t name)))

(defun js-call (form)
  (format nil "~A(~A)"
          (if (listp (first form))
            (reduce (sep #\.)
                    (loop for x in (first form)
                          collect (js-eval x)))
            (js-name (first form)))
          (reduce (sep #\,) (mapcar #'js-eval (rest form)))))

(defun js-eval (form)
  (handler-case (apply (js-def (first form)) (rest form))
    (error () (if (listp form) (js-call form) (js-name form)))))

(setf (js-def 'fun)
      (lambda (name args &rest body)
          (format nil "function ~A(~A){~{~A;~}}"
                  name
                  (reduce (lambda (a v) (format nil "~A, ~A" a v))
                          args)
                  (mapcar #'js-eval body))))

(setf (js-def '+)
      (lambda (&rest args)
        (reduce (sep #\+) (mapcar #'js-eval args))))

(let ((counter 0))
  (defun js-id (base)
    (incf counter)
    (format nil "~A_~X" base counter)))

(defun js (&rest definitions)
  (!+ 'tag := "script"
           :& '("type" "text/javascript")
           :< (loop for def in definitions
                    collect (apply (js-def (car def)) (cdr def)))))
