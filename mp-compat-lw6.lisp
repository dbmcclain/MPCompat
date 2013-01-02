;; mp-compat-lw.lisp
;; --------------------------------------------------------------------------------------
;; Compatibility layer for Lispworks, Allegro, OS X, and Win32, Mulit-Processing Primitives
;;
;; Copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; --------------------------------------------------
(in-package #:mp-compatibility)
;; --------------------------------------------------
;; Compatibility Layer

;; ------------------------------------------------
;; Spin-locks

(defun do-with-spinlock (lock fn &aux ans)
  (loop until (mp:with-lock (lock nil 0)
                (setf ans (multiple-value-list (funcall fn)))
                t))
  (values-list ans))

(defmacro with-spinlock ((lock) &body body)
  `(do-with-spinlock ,lock (lambda () ,@body)))

#|(defmacro xwith-spinlock ((lock) &body body)
  `(mp:with-lock (,lock) ,@body))|#

(editor:setup-indent "with-spinlock" 1)

