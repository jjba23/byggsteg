;;; tailwind.scm

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

(define-module (byggsteg-tailwind)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 futures))

(define-public button-class
  (string-join
   '("bg-orange-500/75"
     "text-stone-200"
     "rounded-xl"
     "cursor-pointer"
     "p-2"
     "border"
     "w-fit"
     "m-2"
     "text-lg"
     "font-bold") " "))

(define-public nav-button-class
  (string-join
   '(
     "text-orange-500/75"
     "cursor-pointer"
     "p-2"
     "m-2"
     "border-b-2"
     "border-b-orange-500"
     "text-xl"
     "font-bold") " "))

(define-public danger-button-class
  (string-join
   '("rounded-xl"
     "bg-red-700"
     "text-stone-200"
     "font-bold"
     "w-fit"
     "border"
     "text-lg"
     "cursor-pointer"
     "p-2"
     "m-2") " "))

(define-public input-class
  (string-join
   '("rounded-xl"
     "border"
     "border-stone-500"
     "p-2"
     "ml-6"
     "text-lg"
     "bg-stone-800"
     "text-stone-200"
     ) " "))

(define-public hr-class
  (string-join
   '("my-6" "border-t-2" "border-t-stone-600") " "))

(define-public h2-class
  (string-join
   '("text-3xl" "text-stone-200" "my-4" "font-bold") " "))



(define-public textarea-class
  (string-join
   '("rounded-xl"
     "border"
     "border-stone-500"
     "p-2"
     "ml-6"
     "text-lg"
     "bg-stone-800"
     "text-stone-200"
     "min-h-64"
     ) " "))
