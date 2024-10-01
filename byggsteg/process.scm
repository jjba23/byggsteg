;;; process.scm

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

(define-module (byggsteg-process)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (byggsteg-preferences)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 threads)
  )

(define-public (syscall cmd)
  (let* ((process (open-input-pipe cmd))
         (process-output (get-string-all process)))
    (close-pipe process)
    (display process-output)
    process-output))

(define-public (syscall-silent cmd)
  (let* ((process (open-input-pipe cmd))
         (process-output (get-string-all process)))
    (close-pipe process)
    process-output))


(define-public (syscall-to-log-file log-filename cmd)
  (syscall-to-file log-filename cmd job-log-location))

(define-public (syscall-to-detail-file log-filename cmd)
  (syscall-to-file log-filename cmd job-detail-location))

(define-public (syscall-to-file log-filename cmd location)
  (let*
      ((log-file-port
        (open-file (string-append location log-filename) "a")))
    (with-output-to-port log-file-port
      (lambda () (syscall cmd)))
    (close log-file-port)))
