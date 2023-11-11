(define-library (xlang token)
  (import
    scheme
    r7rs
    utf8
    (xlang operator)
    (xlang chars))
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
        (%each-of
          (%nonzero)
          (%zero-or-more (%digit)))
        (%is equal? #\0)))

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




