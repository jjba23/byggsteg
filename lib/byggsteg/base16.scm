;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © 2024 "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © 2024 "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>

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


(define-module (byggsteg base16)
  #:use-module (byggsteg process)
  #:use-module (byggsteg preferences)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 format))

(define-public (base-16-encode str)
  (string-replace-substring
   (syscall-silent (format #f "echo \"~a\" | xxd -p" str)) "\n" ""))

(define-public (base-16-decode str)
  (string-replace-substring
   (syscall-silent (format #f "echo \"~a\" | xxd -p -r" str)) "\n" ""))
