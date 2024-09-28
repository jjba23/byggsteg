;;; url.scm

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
  #:use-module (ice-9 iconv))


(define (at-eq x) (string-split x #\=))
(define (unsafe-mk-alist x) (cons (car x) (cdr x)))

(define-public (read-url-encoded-body body)
  (let* ((str (bytevector->string body "utf-8"))
         (raw-kv-pairs (string-split str #\&))
         (xss (map unsafe-mk-alist (map at-eq raw-kv-pairs))))    
    xss))

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
  (for-each
   (lambda (kv)
     (set! str (string-replace-substring str (car kv) (cdr kv))))
   url-decode-alist)  str)
