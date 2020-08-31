(category txt)

(defvar *lang* (mapcar (lambda (x) (format nil "~a" x))
                    (list! (splitstr (or (sb-unix::posix-getenv "HOME_LANG")
                                          "") #\ ))))

(defun read-from-file (file)
    (handler-case
        (read-from-string (join (sep #\ ) (<f file)))
        (error (e) (format T "Error reading language file: ~A~%"
                           file))))

(let ((defaults '((no-canvas "Error: no canvas support!")
                  (wrong-list-len "Wrong list length:")
                  (arg-not-list "Argument is not list: ")
                  (arg-not-number "Argument is not number: ")
                  (not-int-list "Wrong list element type (only numbers allowed): ")
                  (arg-not-fun "Argument is not function: ")
                  (wrong-argc "Wrong arguments count: ")
                  (more-arts "More articles")
                  (who-when "Who are you? Look at lang.cl"))))
    (loop for text in (if *lang*
                         (append defaults
                                 (apply #'append
                                        (mapcar #'read-from-file *lang*)))
                         defaults)
          do (setf (txt (first text)) (second text))))
