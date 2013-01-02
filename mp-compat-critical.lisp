
;; --------------------------------------------------
(in-package #:mp-compatibility)
;; --------------------------------------------------

(um:defmacro! critical (&body body)
  `(let ((,g!lock #.(make-lock :name "CriticalSection")))
     (with-lock (,g!lock)
       ,@body)))

(um:defmacro! spin-critical (&body body)
  `(let ((,g!lock #.(make-lock :name "SpinCriticalSection")))
     (with-spin-lock (,g!lock)
       ,@body)))
