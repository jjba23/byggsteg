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

(define-module (byggsteg tailwind)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 futures))

;;
;; Buttons
;;
(define base-button
  `("rounded-xl" "p-2" "w-fit" "text-sm" "font-mono" "font-bold"))

(define-public button-face
  (string-join (append base-button
                       '("bg-teal-700" "hover:bg-teal-800"
                         "active:bg-teal-800" "text-stone-300")) " "))

(define-public danger-button-face
  (string-join (append base-button
                       '("bg-red-700" "hover:bg-red-800" "active:bg-red-800"
                         "text-stone-300")) " "))

(define-public nav-button-face
  (string-join '("text-teal-600" "hover:border-b-teal-400"
                 "hover:text-teal-400"
                 "cursor-pointer"
                 "p-1"
                 "border-b-2"
                 "font-mono"
                 "border-b-teal-600"
                 "text-md"
                 "font-bold") " "))
;;
;; Forms
;;
(define-public input-face
  (string-join '("rounded-xl" "border"
                 "border-stone-600"
                 "p-2"
                 "text-md"
                 "bg-stone-800"
                 "text-stone-300"
                 "font-mono") " "))

(define-public textarea-face
  (string-join '("rounded-xl" "border"
                 "border-stone-600"
                 "p-2"
                 "w-full"
                 "text-md"
                 "bg-stone-800"
                 "font-mono"
                 "text-stone-300"
                 "min-h-64") " "))

(define-public label-face
  (string-join '("text-stone-300" "text-sm" "font-bold") " "))

(define-public hr-face
  (string-join '("my-6" "border-t-2" "border-t-stone-600") " "))

;;
;; Headers
;;
(define-public h2-face
  (string-join '("text-2xl" "text-stone-300" "font-bold" "font-mono") " "))

(define-public h4-face
  (string-join '("text-lg" "text-stone-400" "font-bold" "font-mono") " "))

(define-public brand-title-face
  (string-join '("text-2xl" "text-teal-500" "hover:border-b-teal-400"
                 "hover:text-teal-400" "font-bold" "font-mono") " "))

;;
;; Links
;;
(define-public dash-link-face
  (string-join '("text-stone-300" "hover:text-stone-100" "font-bold"
                 "cursor-pointer" "text-md" "font-mono") " "))

(define-public link-face
  (string-join '("text-teal-500" "hover:text-teal-400" "font-bold"
                 "cursor-pointer" "text-sm") " "))
