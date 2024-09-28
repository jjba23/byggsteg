(define-module (byggsteg-job)
  #:use-module (byggsteg-base16)
  #:use-module (byggsteg-preferences)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 futures)
  )

(define-public (read-job-success log-filename)
  (cond
   ((file-exists? (string-append job-success-location log-filename)) #t)
   (else #f)))

(define-public (read-job-failure log-filename)
  (cond
   ((file-exists? (string-append job-failure-location log-filename)) #t)
   (else #f)))

(define-public (get-file-list dir)  
  (string-split
   (run-system (format #f "ls -1 --sort=time ~a" dir)) #\newline ))
