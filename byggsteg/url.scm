

(define-module (byggsteg-url)
  #:use-module (byggsteg-process)
  #:use-module (web request)             
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (sxml simple)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  )


(define (at-eq x) (string-split x #\=))
(define (unsafe-mk-alist x) (cons (car x) (cdr x)))

(define-public (read-url-encoded-body body)
  (let* ((str (bytevector->string body "utf-8"))
         (raw-kv-pairs (string-split str #\&))
         (xs (map at-eq raw-kv-pairs))
         (xss (map unsafe-mk-alist xs))
         )    
    (display xss)
    xss
    )
  )

(define-public url-decode-alist
  '(("%20" . " ")
    ("%21" . "!")
    ("%22" . "\"")
    ("%23" . "#")
    ("%24" . "$")
    ("%25" . "%")
    ("%26" . "&")
    ("%27" . "'")
    ("%28" . "(")
    ("%29" . ")")
    ("%2A" . "*")
    ("%2B" . "+")
    ("%2C" . ",")
    ("%2F" . "/")
    ("%3A" . ":")
    ("%3B" . ";")
    ("%3C" . "<")
    ("%3D" . "=")
    ("%3E" . ">")
    ("%3F" . "?")
    ("%40" . "@")
    ("%25" . "%")))


(define-public (url-decode str)
  (for-each (lambda (kv)
              (set! str (string-replace-substring str (car kv) (cdr kv)))
              ) url-decode-alist)
  str
  )
