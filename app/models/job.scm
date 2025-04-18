;;; byggsteg - the CI/CD orchestrator written in Guile Scheme

;; Copyright © "Josep Bigorra" known as "jjba23" <jjbigorra@gmail.com>
;; Copyright © "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>

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

(import (artanis mvc model))

(create-artanis-model job
                      (:deps)
                      (id char-field
                          (#:primary-key #:maxlen 36))
                      (human_id char-field
                                (#:not-null #:unique #:maxlen 1024))
                      (job_data char-field
                                (#:not-null #:maxlen 8192))
                      (job_success tiny-integer
                                   (#:not-null))
                      (job_in_progress tiny-integer
                                       (#:not-null))
                      (job_failed tiny-integer
                                  (#:not-null))
                      (created_at date-field
                                  (#:not-null))
                      (updated_at date-field))
