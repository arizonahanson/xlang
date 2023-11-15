(define-library
  (xlang glyph)
  (import
    scheme
    r7rs
    srfi-14
    utf8
    (xlang stream))
  (export
    %digit
    %nonzero)
  (begin

    (define char-set:nonzero
      (char-set-delete
        char-set:digit #\0))

    (define (%char-set cs)
      (%is (lambda (value)
        (char-set-contains? cs value))))

    (define (%digit)
      (%char-set
        char-set:digit))

    (define (%nonzero)
      (%char-set
        char-set:nonzero))

    ))
