(define-library (xlang)
  (import
    scheme
    r7rs
    utf8
    srfi-127
    (xlang token))
  (begin

    (display
      ((!float)
       (generator->lseq read-char)))

    ))
