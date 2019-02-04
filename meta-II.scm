;; Monadic parsing in Scheme, and the META-II compiler.

(use-modules (ice-9 match)
             (ice-9 binary-ports)
             (rnrs bytevectors)
             (ice-9 textual-ports)
             (srfi srfi-9))

(define *gensym-count* 1)

(define (gensym)
  (let ((res (string->symbol (format #f "L~a" *gensym-count*))))
    (set! *gensym-count* (1+ *gensym-count*))
    res))

(define (reset-gensym-counter!)
  (set! *gensym-count* 1))

;; Helper procedures `compile-port' and `emit' for output.
(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format #t "Not an output port ~s." p)))
     p)))

(define (emit . args)
  (apply format (compile-port) args)
  (format (compile-port) "")
  (newline (compile-port)))

;; Scheme doesn't treat strings like lists, but we can!
(define (string-car s) (string-ref s 0))
(define (string-cdr s) (substring s 1))

;; Scheme doesn't have a `show' procedure like in Haskell, so make our
;; own.
(define (to-string o)
  (if (char? o) (string o) (object->string o)))

;; Strings in Scheme aren't lists like in Haskell.  Unfortunately this
;; causes problems later, so we fix it with cons-string.

;; (cons-string #\h "ello") => "hello".
(define (cons-string c s)
  (if (null? s)
      (to-string c)
      (string-concatenate/shared `(,(to-string c) ,s))))

;; Monadic gensym
(define m-gensym
  (lambda (s)
    (cons (gensym) s)))

;; Parse a single character. 
;; Parser Char
(define item
  (lambda (s)
    (if (string-null? s)
        '()
        (cons (string-car s) (string-cdr s)))))

(define (return a)
  (lambda (state)
    (cons a state)))

(define (out . args)
  (lambda (state)
    (format #t "\t")
    (apply format #t args)
    (newline)
    (cons args state)))

(define (label . args)
  (lambda (state)
    (apply format #t args)
    (newline)
    (cons args state)))

;; Parser a -> (a -> Parser b) -> Parser b
(define (>>= m k)
  (lambda (x)
    (let* ((ay (m x)))
      (if (null? ay)
          '()
          (let* ((a (car ay))
                 (y (cdr ay)))
            ((k a) y))))))

;; Failure.
;; Parser m -> ()
(define fail
  (lambda (s)
    '()))

;;; letM*
;; This allows for a similar notation to "do" in Haskell.
;; do a <- b
;;    c <- d
;;    return a : c

;; Is the same as

;; (letM* ((a b)
;;         (c d))
;;        (return (cons a c)))

(define-syntax letM*
  (syntax-rules ()
    ((_ () expr) expr)
    ((_ ((name val) (name2 val2) ...) expr)
     (>>= val
          (lambda (name)
            (letM* ((name2 val2) ...)
                   expr))))))


(define-syntax doM*
  (syntax-rules (let let* letrec letrec* <-)
    ((doM* s)                         s)
    ((doM* (x <- s) ss ...)           (>>= s (lambda (x) (doM* ss ...))))
    ((doM* (let bs) ss ...)           (let bs (doM* ss ...)))
    ((doM* (let* bs) ss ...)          (let* bs (doM* ss ...)))
    ((doM* (letrec bs) ss ...)        (letrec bs (doM* ss ...)))
    ((doM* (letrec* bs) ss ...)       (letrec* bs (doM* ss ...)))
    ((doM* s ss ...)                  (>>= s (lambda (_) (doM* ss ...))))))


;; Given two parsers p and q, try p then if that fails try q.
(define +++
  (lambda (p q)
    (lambda (string)
      (let ((res (p string)))
        (if (null? res)
            (q string)
            res)))))

;; Choice operator
(define-syntax <:>
  (syntax-rules ()
    ((_ a)
     a)
    ((_ a b ...)
     (+++ a (<:> b ...)))))

;; Lift a predicate into a parser.
;; (Char -> Bool) -> Parser Char
(define sat
  (lambda (p)
    (doM* (c <- item)
          (if (p c)
              (return c)
              fail))))

;; Make a parser that only accepts a certain character.
;; Char -> Parser Char
(define (char c)
  (sat (lambda (t)
         (eq? t c))))

;; Allows a parser p to be repeated fail or more times.
(define (many p)
  (<:> (many1 p) (return '())))

;; Allows a parser p to be repeated one or more times.
;; many and many1 are mutually recursive.
(define (many1 p)
  (doM* (a <- p)
        (as <- (many p))
        ;; We use cons-string here because we want to possibly
        ;; collect individual characters into strings.
        (return (cons-string a as))))

;; Allows a parser p to be repeated fail or more times.
(define (many-n p)
  (<:> (many1-n p) (return '())))

;; Allows a parser p to be repeated one or more times.
;; many-n and many1-n are mutually recursive.
(define (many1-n p)
  (doM* (a <- p)
        (as <- (many-n p))
        ;; We use cons here because we want collect whatever the
        ;; parser returned into a list.  This is a limitation of
        ;; using Scheme, as strings aren't lists of characters.
        (return (cons a as))))

;; Given a string, treat it like a character set and create a parser
;; that only accepts characters pertaining to that character set.
(define (oneof string)
  (sat (lambda (x) (char-set-contains? (string->char-set string) x))))

(define (noneof string)
  (sat (lambda (x) (not (char-set-contains? (string->char-set string) x)))))

;; Eat whitespace.
(define space
  (many (oneof " \n\t")))

;; Turn a parser p into a "token" parser, i.e. one that also eats up
;; whitespace following the parse.
(define (token p)
  (doM* (a <- p)
        space
        (return a)))

;; Make a parser that only accepts a certain string s.
(define (str s)
  (if (string-null? s)
      (return '())
      (let ((c (string-car s))
            (cs (string-cdr s)))
        (doM* (char c)
              (str cs)
              ;; Use string-concatenate/shared for possible speedup,
              ;; also because no mutation is performed.
              (return (string-concatenate/shared `(,(string c) ,cs)))))))

;; Tokenize a string.
(define (symb cs)
  (token (str cs)))

;; Before applying parser p, eat up leading whitespace.
(define (apply-p p)
  (doM* space
        p))

;; Haven't found a good use for chainl and chainl1, not sure if it
;; works as expected.  Taken from Hutton's paper on monadic parsing.
(define (chainl p op a)
  (<:> (chainl1 p op)
       (return a)))

(define (chainl1 p op)
  (define (rest a)
    (<:> (letM* ((f op)
                 (b p))
                (rest (f a b)))
         (return a)))
  (letM* ((a p))
         (rest a)))

;; Given a parser m and a predicate p, apply the parser and check the
;; result against the predicate, then succeed or fail based on that.
(define (:> m p)
  (letM* ((a m))
         (if (p a)
             (return a)
             fail)))

;; Parse a single numeric character.
(define digit
  (doM* (a <- (:> item char-numeric?))
        (return a)))

;; Parse a natural number.
(define nat
  (doM* (xs <- (many1 digit))
        (return (string->number xs))))

;; A natural number, with whitespace following.
(define natural
  (token nat))

;; Get all the words in a sentence, space separated.
(define words
  (doM* space
        (w <- (many-n (apply-p (many1 (noneof " ")))))
        (return w)))


;; From a paper, forgot which one.
(define (sepby p sep)
  (<:> (sepby1 p sep)
       (return '())))

(define (sepby1 p sep)
  (doM* (a <- p)
        (as <- (many (letM* ((_ sep) (a p)) (return a))))
        (return (cons a as))))

(define (sepby-n p sep)
  (<:> (sepby1-n p sep)
       (return '())))

(define (sepby1-n p sep)
  (doM* (a <- p)
        (as <- (many-n (letM* ((_ sep) (a p)) (return a))))
        (return (cons a as))))

;; Parse an alphabetic character.
(define alpha
  (:> item char-alphabetic?))

;; Parse an alphanumeric character.
(define alpha-num
  (:> item (lambda (x)
             (or (char-alphabetic? x)
                 (char-numeric? x)))))

(define word (apply-p (many1 alpha-num)))

;; Consume a string up to a given character.
(define (up-to c)
  (letM* ((a (many (sat (lambda (x) (not (eq? x c)))))))
         (return a)))

(define (parse p s)
  (let ((a (p s)))
    (if (null? a)
        (emit "Parsing failed.")
        (if (not (string-null? (cdr a)))
            (begin (emit "Warning: Unconsumed input from position ~a, \"~a\""
                         (- (string-length s) (string-length (cdr a)))
                         (cdr a))
                   (car a))
            (car a)))))


(define (read-file-string filename)
  (let* ((port (open-input-file filename))
         (data (get-string-all port)))
    (close-port port)
    data))

(define parse-string
  (doM* (char #\')
        (x <- (many (noneof "'")))
        (char #\')
        (return x)))

(define mstring (token parse-string))

(define id (token word))

;; Here's a fun challenge: is it possible to automatically generate
;; this code through a META II program that can convert META II
;; grammars into Scheme code?  If so, what does such a program look
;; like?  If not, why not?

(define ex3
  (<:> (doM* (x <- id)
             (out "LD ~a" x))
       (doM* (symb "(")
             ex1
             (symb ")"))))

(define ex2
  (doM* ex3
        (many (doM* (symb "*")
                    ex3
                    (out "MLT")))))

(define ex1
  (doM* ex2
        (many (doM* (symb "+")
                    ex2
                    (out "ADD")))))

(define out1
  (<:> (doM* (symb "*1")
             (out "GN1"))
       (doM* (symb "*2")
             (out "GN2"))
       (doM* (symb "*")
             (out "CI"))
       (doM* (x <- mstring)
             (out "CL '~a'" x))))

(define output
  (doM* (<:> (doM* (symb ".OUT")
                   (symb "(")
                   (many out1)
                   (symb ")"))
             (doM* (symb ".LABEL")
                   (out "LB")
                   out1))
        (out "OUT")))

(define ex3
  (<:> (doM* (x <- id)
             (out "CLL ~a" x))
       (doM* (x <- mstring)
             (out "TST '~a'" x))
       (doM* (symb ".ID")
             (out "ID"))
       (doM* (symb ".NUMBER")
             (out "NUM"))
       (doM* (symb ".STRING")
             (out "SR"))
       (doM* (symb "(")
             ex1
             (symb ")"))
       (doM* (symb ".EMPTY")
             (out "SET"))
       (doM* (symb "$")
             (*1 <- m-gensym)
             (label "~a" *1)
             ex3
             (out "BT ~a" *1)
             (out "SET"))))

(define ex2
  (doM* (*1 <- m-gensym)
        (<:> (doM* ex3
                   (out "BF ~a" *1))
             output)
        (many (<:> (doM* ex3
                         (out "BE"))
                   output))
        (label "~a" *1)))

(define ex1
  (doM* ex2
        (*1 <- m-gensym)
        (many (doM* (symb "/")
                    (out "BT ~a" *1)
                    ex2))
        (label "~a" *1)))

(define st
  (doM* (* <- id)
        (label "~a" *)
        (symb "=")
        ex1
        (symb ".,")
        (out "R")))

(define program
  (doM* (symb ".SYNTAX")
        (* <- id)
        (out "ADR ~a" *)
        (many st)
        (symb ".END")
        (out "END")))

(define (read-file-string filename)
  (let* ((port (open-input-file filename))
         (data (get-string-all port)))
    (close-port port)
    data))

(define (parse-meta-II-file filename)
  (reset-gensym-counter!)
  (let ((msg (read-file-string filename)))
    (program msg)))

