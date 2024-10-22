;;; server.scm

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; byggsteg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; byggsteg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

(define-module (byggsteg-server)
  #:export (not-found
            respond-json
            request-path-components
            respond-static-file
            get-file-list)
  #:use-module (byggsteg-preferences)
  #:use-module (byggsteg-process)
  #:use-module (web server)
  #:use-module (web request)             
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 futures)
  )

(define-public (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define* (respond-json json #:optional body #:key
                       (status 200)
                       (title "Hello hello!")

                       (content-type-params '((charset . "utf-8")))
                       (content-type 'application/json)
                       (extra-headers '()))
  (values (build-response
           #:code status
           #:headers `((content-type
                        . (,content-type ,@content-type-params))
                       ,@extra-headers))
          (lambda (port) (display json port))))


(define (get-file-list dir)  
  (string-split
   (syscall (format #f "ls -1 --sort=time ~a" dir)) #\newline ))



(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))


(define* (respond-static-file path content-type #:key
                              (status 200)                              
                              (content-type-params '((charset . "utf-8")))
                              (extra-headers '()))
  (values (build-response
           #:code status
           #:headers `((content-type
                        . (,content-type ,@content-type-params))
                       ,@extra-headers))
          (lambda (port)
            (display (get-string-all (open-input-file path)) port)
            )))
