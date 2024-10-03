;;; preferences.scm

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

(define-module (byggsteg-preferences)
  #:use-module (ice-9 time)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  )

(define-public job-failure-location "/var/log/byggsteg/job-failure/")
(define-public job-success-location "/var/log/byggsteg/job-success/")
(define-public job-clone-location "/var/log/byggsteg/job-clone/")
(define-public job-log-location "/var/log/byggsteg/job-log/")
(define-public job-detail-location "/var/log/byggsteg/job-detail/")

(define-public profile-location "/var/log/byggsteg/profile/")
