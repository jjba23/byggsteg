;;; main.scm

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

(define-module (byggsteg-main)
  #:use-module (byggsteg-process)
  #:use-module (byggsteg-rest)
  #:use-module (byggsteg-log)
  #:use-module (byggsteg-html)
  #:use-module (byggsteg-preferences)
  #:use-module (byggsteg-base16)
  #:use-module (byggsteg-server)
  #:use-module (web server)
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
  #:use-module (ice-9 futures)
  )


(define-public (byggsteg-http-server request body)
  (let ((path (request-path-components request)))
    (cond
     ((and (equal? path '()) (equal? (request-method request) 'GET))
      (welcome-page))
     ((and (equal? path '("jobs" "request")) (equal? (request-method request) 'GET))
      (job-request-form-page))
     ((and (equal? path '("jobs" "submit")) (equal? (request-method request) 'POST))
      (job-submit-endpoint request body))
     ((and (equal? path '("jobs" "delete")) (equal? (request-method request) 'POST))
      (job-delete-endpoint request body))
     ((and (equal? (car path) "logs") (equal? (request-method request) 'GET))
      (log-page path ))
     ((and
       (and (equal? (car path) "api") (equal? (car (cdr path)) "logs") )
       (equal? (request-method request) 'GET))
      (log-api-page path ))
     ((and (equal? path '("resources" "js" "tailwind.config.js")) (equal? (request-method request) 'GET))
      (respond-static-file "./resources/js/tailwind.config.js" 'text/javascript))
     (else (not-found request)))))



