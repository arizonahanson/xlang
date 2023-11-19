(define-library
  (xlang sigil)
  (import
    scheme
    r7rs
    utf8
    (chicken base)
    (xlang monad)
    (xlang glyph))
  (export
    *flatten
    *string
    *integer
    *float)
  (begin

    (define (*flatten . parsers)
      (*bind
        (apply *each-of parsers)
        (lambda (value)
          (*return (flatten value)))))

    (define (*string . operators)
      (*bind
        (apply *flatten operators)
        (lambda (value)
          (*return (apply string value)))))

    (define (*integer)
      (*any-of
        (*is equal? #\0)
        (*each-of
          (*nonzero)
          (*zero-or-more (*digit)))))

    (define (*float)
      (*string
        (*integer)
        (*maybe (*each-of
          (*is equal? #\.)
          (*one-or-more (*digit))))
        (*maybe
          (*each-of
            (*any-of
              (*is equal? #\e)
              (*is equal? #\E))
            (*any-of
              (*is equal? #\+)
              (*is equal? #\-))
            (*integer)))))
    ))




