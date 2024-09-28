

(define-module (byggsteg-log)
  #:use-module (byggsteg-preferences)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  )

(define (new-project-log-filename project)
  "Create a new name for a log file, based on the project and current timestamp."
  (let ((timestamp (get-current-date-time)))
    (string-append project "__" timestamp ".byggsteg.log")))


