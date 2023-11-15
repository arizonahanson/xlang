(define-library (xlang)
  (import
    scheme
    r7rs
    utf8
    srfi-127
    (xlang sigil)
    (xlang stream))
  (begin

    (display
      (%value ((!float)
       (generator->lseq read-char))))
    (newline)

    ))
