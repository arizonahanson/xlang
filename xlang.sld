(define-library (xlang)
  (import
    scheme
    r7rs
    utf8
    srfi-127
    (xlang monad)
    (xlang glyph)
    (xlang sigil))
  (begin

    ((%bind
       (%string (*integer))
       (lambda (value)
         (display value)
         (newline)
         display))
     (generator->lseq read-char))

    ))
