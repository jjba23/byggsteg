

(define-module (byggsteg-log)
  #:use-module (byggsteg-preferences)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  )

(define (get-log-current-date-time)
  "Get current timestamp string formatted to contain only dashes."
  (let ((now (localtime (current-time))))
    (strftime "%Y-%m-%d__%H:%M:%S" now)))


(define-public (new-project-log-filename project)
  "Create a new name for a log file, based on the project and current timestamp."
  (string-append project "__" (get-log-current-date-time) ".byggsteg.log"))


