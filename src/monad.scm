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
      generator->lseq
      lseq-rest)
    (only
      srfi-158
      make-coroutine-generator))
  (export
    %result
    %value
    %stream
    *fail
    *identity
    *return
    *bind
    *chain
    *foldl
    *foldr
    *any-of
    *each-of
    *none
    *not
    *maybe
    *zero-or-more
    *one-or-more
    *is)
  (begin

    (define-record-type result%
      (%result value stream)
      %result?
      (value %value)
      (stream %stream))

    (define ((*fail) stream) #f)

    (define ((*identity) stream)
      (if (null? stream) #f
        (%result
          (car stream)
          (lseq-rest stream))))

    (define ((*return value) stream)
      (%result value stream))

    (define ((*bind parser combinator) stream)
      (and-let*
        ((result (parser stream)))
        ((combinator (%value result)) (%stream result))))

    (define ((*chain parser . parsers) stream)
      (if (null? parsers)
        (parser stream)
        ((apply *chain parsers)
         (generator->lseq
           (make-coroutine-generator
             (lambda (yield)
               (let loop ((input stream))
                 (and-let*
                   ((result (parser input)))
                   (yield (%value result))
                   (loop (%stream result))))))))))

    (define (*foldl proc init parsers)
      (*bind
        (car parsers)
        (lambda (value)
          (let
            ((rest (cdr parsers))
             (next (proc init value)))
            (if (null? rest)
              (*return next)
              (*foldl proc next rest))))))

    (define (*foldr proc init parsers)
      (*bind
        (car parsers)
        (lambda (value)
          (*bind
            (let ((rest (cdr parsers)))
              (if (null? rest)
                (*return init)
                (*foldr proc init rest)))
            (lambda (next)
              (*return (proc value next)))))))

    (define ((*any-of parser . parsers) stream)
      (or (parser stream)
        (if (null? parsers) #f
          ((apply *any-of parsers) stream))))

    (define (*each-of . parsers)
      (*foldr cons '() parsers))

    (define (*none)
      (*return '()))

    (define ((*not parser) stream)
      (if (parser stream) #f
        ((*none) stream)))

    (define (*maybe parser)
      (*any-of parser (*none)))

    (define (*zero-or-more parser)
      (*maybe (*one-or-more parser)))

    (define (*one-or-more parser)
      (*bind
        parser
        (lambda (value)
          (*bind
            (*zero-or-more parser)
            (lambda (next)
              (*return (cons value next)))))))

    (define (*is predicate . args)
      (*bind
        (*identity)
        (lambda (value)
          (if (apply predicate (cons value args))
            (*return value)
            (*fail)))))

    ))
