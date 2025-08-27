;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

;; byggsteg is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; byggsteg is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

(define-module (byggsteg tailwind)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (hygguile hygguile)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 futures))

;;
;; Buttons
;;
(define base-button
  `(rounded-xl p-2 w-fit text-sm font-sans font-bold))

(define-public button-face
  (mk-class (append base-button
                    '(bg-orange-400 hover:bg-orange-500 active:bg-orange-500
                                    text-stone-900 shadow-md))))

(define-public danger-button-face
  (mk-class (append base-button
                    '(bg-red-700 hover:bg-red-800 active:bg-red-800
                                 text-stone-300 shadow-md))))

;;
;; Forms
;;
(define-public input-face
  (mk-class '(rounded-xl border
                         border-stone-600
                         p-3
                         text-md
                         shadow-md
                         bg-stone-900
                         text-stone-300
                         font-mono)))

(define-public textarea-face
  (mk-class '(rounded-xl border
                         border-stone-600
                         p-2
                         w-full
                         text-md
                         shadow-md
                         bg-stone-900
                         font-mono
                         text-stone-300
                         min-h-64)))

(define-public label-face
  (mk-class '(text-stone-300 text-sm font-bold py-2 md:px-2)))

(define-public hr-face
  (mk-class '(mx-8 my-5 border-t-2 border-t-stone-600)))

;;
;; Headers
;;

(define-public brand-title-face
  (mk-class '(text-2xl text-orange-400 hover:border-b-orange-400
                       hover:text-orange-500 font-bold font-sans)))

;;
;; Links
;;
(define-public dash-link-face
  (mk-class '(text-stone-700 hover:text-orange-600
                             dark:text-stone-300
                             hover:dark:text-orange-300
                             font-bold
                             cursor-pointer
                             text-md
                             font-sans)))

(define-public link-face
  (mk-class '(text-orange-400 hover:text-orange-300 font-bold cursor-pointer
                              text-sm)))

;;
;; Pre
;;

;;
;; DL DD DT
;;
(define-public dt-face
  (mk-class '(dark:text-stone-300 text-stone-700 text-sm py-4)))
(define-public dd-face
  (mk-class '(dark:text-stone-300 text-stone-700 text-sm mb-2 sm:ml-4)))

