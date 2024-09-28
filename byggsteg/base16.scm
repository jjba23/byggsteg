(define-module (byggsteg-base16)
  #:use-module (byggsteg-process)
  #:use-module (byggsteg-preferences)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 format))



(define-public (base-16-encode str)
  (string-replace-substring
   (run-system (format #f "echo \"~a\" | xxd -p" str)) "\n" ""))

(define-public (base-16-decode str)
  (string-replace-substring
   (run-system (format #f "echo \"~a\" | xxd -p -r" str)) "\n" ""))
