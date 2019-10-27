(load (merge-pathnames "util.lisp" *load-truename*))

(category js-def)

(defun js-name (name)
  (cond ((symbolp name)
          (reduce (lambda (a v)
                    (format nil "~A~A~A" a (char-upcase (char v 0))
                                       (subseq v 1)))
                  (splitstr (format nil "~A" (string-downcase name)) #\-)))
        ((stringp name) (format nil "\"~A\"" name))
        (t name)))

(defun js-block (&rest forms)
  (reduce (sep #\Newline) (loop for x in forms collect (js-eval x :statement? T))))

(defun js-call (form &key statement?)
  (format nil "~A(~A)~A"
          (if (listp+ (first form))
            (reduce (sep #\.)
                    (loop for x in (first form)
                          collect (js-eval x)))
            (js-name (first form)))
          (if (rest form)
              (reduce (sep #\,) (mapcar #'js-eval (rest form)))
              "")
          (if statement? #\; "")))

(defun js-eval (form &key statement?)
  (if (not (listp form)) (return-from js-eval (js-name form)))
  (handler-case (apply (js-def (first form)) (rest form))
    (not-in-category () (js-call form :statement? statement?))))

(setf (js-def 'fun)
      (lambda (name args &rest body)
          (format nil "function ~A(~A){~{~A~}}"
                  name
                  (if args
                    (reduce (lambda (a v) (format nil "~A, ~A" a v))
                            args)
                    "")
                  (if body
                    (mapcar (lambda (x) (js-eval x :statement? T)) body)
                    ""))))

(setf (js-def 'if)
      (lambda (condition action &optional if-else)
        (format nil "if (~a) {~{~a~}}~a"
                (js-eval condition) (mapcar (lambda (x) (js-eval x :statement? T)) action)
                (if if-else (format nil "else {~a}" (js-eval if-else :statement? T)) ""))))

(setf (js-def '+)
      (lambda (&rest args)
        (format nil "(~A)"
            (reduce (sep #\+) (mapcar #'js-eval args)))))

(setf (js-def '-)
      (lambda (&rest args)
        (format nil "(~A)"
            (reduce (sep #\-) (mapcar #'js-eval args)))))

(setf (js-def '++)
      (lambda (arg)
        (format nil "~A++;" (js-name arg))))

(setf (js-def '--)
      (lambda (arg)
        (format nil "~A--;" (js-name arg))))

(setf (js-def '*)
      (lambda (&rest args)
        (format nil "(~A)"
            (reduce (sep #\*) (mapcar #'js-eval args)))))

(setf (js-def 'let)
      (lambda (name value)
        (format nil "var ~A = ~A;" (js-name name) (js-eval value))))

(setf (js-def 'prop)
      (lambda (obj p)
        (format nil "~A.~A" (js-eval obj) (js-name p))))

(setf (js-def '@)
      (lambda (arr idx)
        (format nil "~A[~A]" (js-eval arr) (js-eval idx))))

(setf (js-def '=)
      (lambda (a b)
        (format nil "~A = ~A;" (js-eval a) (js-eval b))))

(setf (js-def ':=)
      (lambda (a b)
        (format nil "~A = ~A" (js-eval a) (js-eval b))))

(setf (js-def '==)
      (lambda (a b)
        (format nil "~A == ~A" (js-eval a) (js-eval b))))

(setf (js-def '<)
      (lambda (a b)
        (format nil "~A < ~A" (js-eval a) (js-eval b))))

(setf (js-def 'return)
      (lambda (a)
        (format nil "return ~A;" (js-eval a))))

(setf (js-def 'by-id)
      (lambda (id)
        (format nil "document.getElementById(~A)" (js-eval id))))

(setf (js-def 'for)
      (lambda (setup &rest commands)
        (format nil "for(var ~A; ~A; ~A) { ~{~A~} }"
                (js-eval (first setup)) (js-eval (second setup)) (js-eval (third setup))
                (loop for x in commands
                      collect (js-eval x :statement? T)))))
(setf (js-def 'json)
      (lambda (obj)
        (format nil "JSON.stringify(~A)" (js-eval obj))))

(setf (js-def 'parse)
      (lambda (obj)
        (format nil "JSON.parse(~A)" (js-eval obj))))

(setf (js-def 'try)
      (lambda (forms backup)
        (format nil "try { ~{~A~} } catch(exception) { ~{~A~} }"
                (mapcar (lambda (x) (js-eval x :statement? T)) forms)
                (mapcar (lambda (x) (js-eval x :statement? T)) backup))))

(setf (js-def '@[])
      (lambda (tab index)
        (format nil "~A[~A]" (js-eval tab) (js-eval index))))

(setf (js-def 'on-pixels)
      (lambda (canvas &rest transforms)
        (apply #'js-block
               (append `((let canvas ,canvas)
                         (let ctx ((canvas get-context) "2d"))
                         (let pixel-data (ctx.get-image-data 0 0 canvas.width canvas.height))
                         (let pixels (prop pixel-data data)))
                       (reduce (lambda (acc tfm)
                                  (let ((x (@ tfm '(0 0)))
                                        (y (@ tfm '(0 1)))
                                        (r (@ tfm '(1 0)))
                                        (g (@ tfm '(1 1)))
                                        (b (@ tfm '(1 2)))
                                        (a (or (@ tfm '(1 3)) 255)))
                                    (append acc
                                            `((= (@ pixels    (* 4 (+ (* ,y canvas.width) ,x)))    ,r)
                                            (= (@ pixels (+ (* 4 (+ (* ,y canvas.width) ,x)) 1)) ,g)
                                            (= (@ pixels (+ (* 4 (+ (* ,y canvas.width) ,x)) 2)) ,b)
                                            (= (@ pixels (+ (* 4 (+ (* ,y canvas.width) ,x)) 3)) ,a)))))
                               transforms :initial-value nil)

                       '((ctx.put-image-data pixel-data 0 0))))))

(setf (js-def '[])
      (lambda (&rest elements)
        (format nil "[~A]"
                (if elements (reduce (sep #\,) (mapcar #'js-eval elements)) ""))))

(setf (js-def 'draw-line)
      (lambda (canvas width &rest points)
        (apply #'js-block
               (append 
                   `((let ctx ((,canvas get-context) "2d"))
                     (ctx.begin-path)
                     (= ctx.line-width ,width)
                     (ctx.move-to ,(@ points '(0 0)) ,(@ points '(0 1))))
                   (loop for x in (rest points)
                         collect `(ctx.line-to ,(first x) ,(second x)))
                   `((ctx.stroke))))))

(let ((counter 0))
  (defun js-id (base)
    (incf counter)
    (format nil "~A_~X" base counter)))

(defun js (&rest definitions)
  (!+ 'tag := "script"
           :& '("type" "text/javascript")
           :< (loop for def in definitions
                    collect (apply (js-def (car def)) (cdr def)))))
