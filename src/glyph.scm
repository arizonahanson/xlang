(define-library
  (xlang glyph)
  (import
    scheme
    r7rs
    srfi-14
    utf8
    (xlang monad))
  (export
    *whitespace
    *digit
    *nonzero
    *letter)
  (begin

    (define (*char-set-is cs)
      (*is (lambda (value)
        (char-set-contains? cs value))))

    (define (*whitespace)
      (*char-set-is char-set:whitespace))

    (define (*digit)
      (*char-set-is char-set:digit))

    (define (*nonzero)
      (*is (lambda (value)
        (not (equal? value #\0)))))

    (define (*letter)
      (*char-set-is char-set:letter))

    ))
