(define-library
  (xlang monad)
  (import
    scheme
    r7rs
    utf8
    (only
      srfi-2
      and-let*)
    (only
      srfi-127
      lseq-rest))
  (export
    %result
    %value
    %input
    %fail
    %identity
    %return
    %bind
    %any-of
    %foldr
    %each-of
    %none
    %not
    %maybe
    %zero-or-more
    %one-or-more
    %is)
  (begin

    (define-record-type result%
      (%result value input)
      %result?
      (value %value)
      (input %input))

    (define ((%fail) input) #f)

    (define ((%identity) input)
      (if (null? input) #f
        (%result
          (car input)
          (lseq-rest input))))

    (define ((%return value) input)
      (%result value input))

    (define ((%bind parser combinator) input)
      (and-let*
        ((result (parser input)))
        ((combinator (%value result)) (%input result))))

    (define ((%any-of parser . parsers) input)
      (or (parser input)
        (if (null? parsers) #f
          ((apply %any-of parsers) input))))

    (define (%foldr proc init parsers)
      (%bind
        (car parsers)
        (lambda (value)
          (%bind
            (let ((rest (cdr parsers)))
              (if (null? rest) (%return init)
                (%foldr proc init rest)))
            (lambda (next)
              (%return (proc value next)))))))

    (define (%each-of . parsers)
      (%foldr cons '() parsers))

    (define (%none)
      (%return '()))

    (define ((%not parser) input)
      (if (parser input) #f
        ((%none) input)))

    (define (%maybe parser)
      (%any-of parser (%none)))

    (define (%zero-or-more parser)
      (%maybe (%one-or-more parser)))

    (define (%one-or-more parser)
      (%bind
        parser
        (lambda (value)
          (%bind
            (%zero-or-more parser)
            (lambda (next)
              (%return (cons value next)))))))

    (define (%is predicate . args)
      (%bind
        (%identity)
        (lambda (value)
          (if (apply predicate (cons value args))
            (%return value)
            (%fail)))))

    ))
