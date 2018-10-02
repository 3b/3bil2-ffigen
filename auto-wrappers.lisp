(in-package 3bil2-ffigen)

;;; loads android.jar from android SDK
;; fixme: make recompilation depend on timestamp of android.jar
(symbol-macrolet ((data #.(let* ((c (load-android.jar))
                                 (p (make-packages c)))
                            (list c p))))
  (macrolet ((a1 (&environment env)
               (destructuring-bind (*classes* p) (macroexpand 'data env)
                 (expand-packages p)))
             (b1 (&environment env)
               (destructuring-bind (*classes* p) (macroexpand 'data env)
                 (expand-ffi-defs p))))
    (progn
      (a1)
      (b1))))


