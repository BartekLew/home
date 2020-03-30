(load (merge-pathnames "util.lisp" *load-truename*))

(category js-def)

(defun splitstr (str delim &optional acc)
	(if (> (length str) 0) (let ((p (position delim str)))
			(if p (splitstr (subseq str (+ p 1)) delim (append acc (list (subseq str 0 p))))
				(append acc (list str))))
		acc))

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

(defun js-name! (x)
  (if (stringp x) x (js-name x)))

(setf (js-def 'fun)
      (lambda (name args &rest body)
          (format nil "function ~A(~A){~{~A~}}~%~%"
                  (if name (js-name! name) "")
                  (if args
                    (reduce (lambda (a v) (format nil "~A, ~A" a v))
                            (mapcar #'js-name! args))
                    "")
                  (if body
                    (mapcar (lambda (x) (js-eval x :statement? T)) body)
                    ""))))

(setf (js-def 'call)
    (lambda (&rest form)
      (js-call form)))

(setf (js-def '->)
    (lambda (&rest args)
      (reduce (lambda (a v)
                (format nil "~A.~A" a v))
              (mapcar #'js-eval args))))

(setf (js-def 'symbol)
      (lambda (x)
        (format nil "~A" x)))

(setf (js-def 'not)
      (lambda (x) (format nil "!(~A)" (js-eval x))))

(setf (js-def 'if)
      (lambda (condition action &optional if-else)
        (format nil "if (~a) {~{~a~}}~a~%"
                (js-eval condition) (mapcar (lambda (x) (js-eval x :statement? T)) action)
                (if if-else (format nil "else {~{~a~}}"
                                    (mapcar (lambda (x) (js-eval x :statement? T)) if-else)) ""))))

(setf (js-def '+)
      (lambda (&rest args)
        (format nil "(~A)"
            (reduce (sep #\+) (mapcar #'js-eval args)))))

(setf (js-def '+=)
      (lambda (first &rest args)
        (format nil "~A += ~A;" (js-eval first)
            (reduce (sep #\+) (mapcar #'js-eval args)))))

(setf (js-def 'and)
      (lambda (&rest args)
        (format nil "(~A)"
            (reduce (lambda (a v) (format nil "~A && ~A" a v))
                    (mapcar #'js-eval args)))))

(setf (js-def 'or)
      (lambda (&rest args)
        (format nil "(~A)"
            (reduce (lambda (a v) (format nil "~A || ~A" a v))
                    (mapcar #'js-eval args)))))

(setf (js-def '-)
      (lambda (&rest args)
        (format nil "(~A)"
            (reduce (sep #\-) (mapcar #'js-eval args)))))

(setf (js-def '^)
      (lambda (a b)
        (format nil "Math.pow(~A,~A)" (js-eval a) (js-eval b))))

(setf (js-def 'floor)
      (lambda (x)
        (format nil "Math.floor(~A)" (js-eval x))))

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
      (lambda (head &rest tail)
        (reduce (lambda (a v)
                  (format nil "~A.~A" a (if (stringp v) v (js-name v))))
                (cons (js-eval head) tail))))

(setf (js-def '@)
      (lambda (arr idx)
        (format nil "~A[~A]" (js-eval arr) (js-eval idx))))

(setf (js-def '=)
      (lambda (a b)
        (format nil "~A = ~A;" (js-eval a) (js-eval b))))

(setf (js-def '=.)
      (lambda (a b)
        (format nil "~A = ~A" (js-eval a) (js-eval b))))

(setf (js-def ':=)
      (lambda (a b)
        (format nil "~A = ~A" (js-eval a) (js-eval b))))

(setf (js-def '==)
      (lambda (a b)
        (format nil "~A == ~A" (js-eval a) (js-eval b))))

(setf (js-def '!=)
      (lambda (a b)
        (format nil "~A != ~A" (js-eval a) (js-eval b))))

(setf (js-def '<)
      (lambda (a b)
        (format nil "~A < ~A" (js-eval a) (js-eval b))))

(setf (js-def '<=)
      (lambda (a b)
        (format nil "~A <= ~A" (js-eval a) (js-eval b))))

(setf (js-def '>)
      (lambda (a b)
        (format nil "~A > ~A" (js-eval a) (js-eval b))))

(setf (js-def '>=)
      (lambda (a b)
        (format nil "~A >= ~A" (js-eval a) (js-eval b))))

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

(setf (js-def 'schedule)
        (lambda (function)
          (js-eval `(fun "" ()
                        (set-timeout ,function 0)))))

(setf (js-def 'foreach)
      (lambda (setup &rest commands)
        (format nil "for(var ~A in ~A) { ~{~A~} }"
                (js-eval (first setup)) (js-eval (second setup))
                (loop for x in commands
                      collect (js-eval x :statement? T)))))

(setf (js-def 'while)
      (lambda (cond &rest body)
        (format nil "while (~A) {~{~A~}}"
                (js-eval cond)
                (mapcar (lambda (x) (js-eval x :statement? T)) body))))

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

(setf (js-def 'throw)
      (lambda (what)
        (format nil "throw ~A;" (js-eval what))))

(setf (js-def 'continue)
      (lambda () "continue;"))

(setf (js-def 'break)
      (lambda () "break;"))

(setf (js-def '@[])
      (lambda (tab index)
        (format nil "~A[~A]" (js-eval tab) (js-eval index))))

(setf (js-def 'list)
      (lambda (&rest args)
        (format nil "[~A]"
                (reduce (curry #'format nil "~A,~A") (mapcar #'js-eval args)))))

(setf (js-def 'hash)
      (lambda (&rest args)
        (format nil "{~A}"
                (reduce (curry #'format nil "~A,~A")
                        (mapcar (lambda (x) (format nil "~A: ~S" 
                                                    (js-eval (first x)) 
                                                    (js-eval (second x)))) args)))))

(setf (js-def 'just) (lambda (x) (format nil "~A;" (js-eval x))))

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

(setf (js-def 'array?)
      (lambda (x)
        (format nil "Array.isArray(~A)" (js-eval x))))

(setf (js-def 'typeof)
      (lambda (x)
        (format nil "typeof ~A" (js-eval x))))

(setf (js-def 'number?)
      (lambda (x) (js-eval `(== (typeof ,x) "number"))))

(setf (js-def 'object?)
      (lambda (x) (js-eval `(== (typeof ,x) "object"))))

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

(setf (js-def 'dom-node)
   (lambda (type &rest props)
     (format nil "(~A)()"
       (js-eval `(fun nil ()
            (let nod ((document create-element) ,type))
            ,@(loop for prop in props
                    collect `(= (prop nod ,(first prop)) ,(second prop)))
            (return nod))))))

(let ((counter 0))
  (defun js-id (base)
    (incf counter)
    (format nil "~A_~X" base counter)))

(defun js (&rest definitions)
  (!+ 'tag := "script"
           :& '("type" "text/javascript")
           :< (loop for def in definitions
                    collect (let ((ans (apply (js-def (car def)) (cdr def))))
                              (if (eql (car def) 'fun) ans (format nil "~A;" ans))))))
