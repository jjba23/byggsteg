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

(import (artanis mvc model))

(create-artanis-model user
                      (:deps)
                      (id char-field
                          (#:primary-key #:maxlen 36))
                      (username char-field
                                (#:not-null #:maxlen 256))
                      (email char-field
                             (#:not-null #:unique #:maxlen 256))
                      (password char-field
                                (#:not-null #:maxlen 512))
                      (salt char-field
                            (#:not-null #:maxlen 36))
                      (permissions char-field
                                   (#:maxlen 4096)))
