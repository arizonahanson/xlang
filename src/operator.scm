(define-library (xlang operator)
  (import
    scheme
    r7rs
    utf8
    srfi-2
    srfi-127
    (chicken base))
  (export
    %identity
    %return
    %fail
    %bind
    %any-of
    %foldr
    %each-of
    %is
    %none
    %maybe
    %zero-or-more
    %one-or-more
    %flatten)
  (begin

    (define (%fail)
      (lambda (input) #f))

    (define (%identity)
      (lambda (input)
        (if (null? input) #f input)))

    (define (%return value)
      (lambda (input)
        (cons value input)))

    (define (%bind operator proc)
      (lambda (input)
        (and-let*
          ((stream (operator input)))
          ((proc (car stream)) (lseq-rest stream)))))

    (define (%any-of operator . operators)
      (lambda (input)
        (or (operator input)
          (if (null? operators) #f
            ((apply %any-of operators) input)))))

    (define (%foldr proc init operators)
      (%bind
        (car operators)
        (lambda (value)
          (%bind
            (let ((rest (cdr operators)))
              (if (null? rest) (%return init)
                (%foldr proc init rest)))
            (lambda (next)
              (%return (proc value next)))))))

    (define (%each-of . operators)
      (%foldr cons '() operators))

    (define (%is predicate . args)
      (%bind
        (%identity)
        (lambda (value)
          (if (apply predicate (cons value args))
            (%return value)
            (%fail)))))

    (define (%none)
      (%return '()))

    (define (%maybe operator)
      (%any-of operator (%none)))

    (define (%zero-or-more operator)
      (%maybe (%one-or-more operator)))

    (define (%one-or-more operator)
      (%bind
        operator
        (lambda (value)
          (%bind
            (%zero-or-more operator)
            (lambda (next)
              (%return (cons value next)))))))

    (define (%flatten . operators)
      (%bind
        (apply %each-of operators)
        (lambda (value)
          (%return (flatten value)))))

    ))
