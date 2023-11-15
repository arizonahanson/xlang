(define-library
  (xlang sigil)
  (import
    scheme
    r7rs
    utf8
    (xlang stream)
    (xlang glyph))
  (export
    !integer
    !float)
  (begin

    (define (!string . operators)
      (%bind
        (apply %flatten operators)
        (lambda (value)
          (%return (apply string value)))))

    (define (!integer)
      (%any-of
        (%is equal? #\0)
        (%each-of
          (%nonzero)
          (%zero-or-more (%digit)))))

    (define (!float)
      (!string
        (!integer)
        (%each-of
          (%is equal? #\.)
          (%one-or-more (%digit)))
        (%maybe
          (%each-of
            (%any-of
              (%is equal? #\e)
              (%is equal? #\E))
            (%any-of
              (%is equal? #\+)
              (%is equal? #\-))
            (!integer)))))
    ))




