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

;; The clock a recurring pass runs on.
;;
;; The queue worker is the one process that outlives a request, so the work that
;; has no request to hang off -- sweeping the artifact directory, pruning runs
;; that have outlived their retention -- is done there, on every trip round its
;; loop.  How often such a pass actually runs is decided by a clock rather than
;; by the trip, so a queue with nothing in it and a queue under load do their
;; upkeep equally often, and a visit that has nothing to do costs one comparison.

(define-module (byggsteg upkeep)
  #:use-module (ice-9 atomic)
  #:export (make-upkeep due? mark-done! when-due))

(define (make-upkeep)
  "A clock for one recurring pass, remembering when it last ran.

It starts at the epoch, so the first visit after a deployment starts is due:
picking up whatever the last process did not get to is what a restart is for."
  (make-atomic-box 0))

(define (due? upkeep every)
  "Whether a pass on UPKEEP is due, EVERY seconds after the last one it ran."
  (>= (- (current-time) (atomic-box-ref upkeep)) every))

(define (mark-done! upkeep)
  "Record that the pass on UPKEEP is running now, and hand UPKEEP back."
  (atomic-box-set! upkeep (current-time))
  upkeep)

(define* (when-due upkeep every thunk)
  "THUNK when a pass on UPKEEP is due, and the empty list when it is not.

The clock is marked before THUNK runs rather than after, so a caller arriving
while a pass is under way cannot start a second one; a pass that raises leaves
the clock marked all the same, and the next attempt is a whole interval away."
  (if (due? upkeep every)
      (begin
        (mark-done! upkeep)
        (thunk))
      '()))
