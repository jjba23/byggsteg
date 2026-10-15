/* byggsteg - the CI/CD orchestrator written in Guile Scheme

   Copyright © Josep Bigorra known as "jjba23" <jjbigorra@gmail.com>
   Copyright © Mu Lei known as "NalaGinrut" <NalaGinrut@gmail.com>

   byggsteg is free software: you can redistribute it and/or modify
   it under the terms of the GNU Affero General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   byggsteg is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Affero General Public License for more details.

   You should have received a copy of the GNU Affero General Public License
   along with byggsteg.  If not, see <https://www.gnu.org/licenses/>.

   What a form says while it is being sent.

   Every control of this console is a form: running a job, saving a profile,
   retrying a run, deleting one, signing in.  Each of them leaves the page and
   comes back with an answer, and between those two moments -- a queued job
   waiting on the deployment's own work, a search over a table of logs -- the
   page a reader is looking at says nothing at all.  A reader who pressed a
   button and sees nothing happen presses it again, and the deployment runs the
   same job twice.

   So the control that was pressed carries the console's mark of work in
   progress while it waits: the framework's own ring (`.hg-spinner'), which is
   the mark a running job wears on the page that reads it (see `pending-mark' in
   `(byggsteg faces)'), with the word it stands for in a layer a screen reader
   reads.  The control is disabled for as long as the page is going away, so a
   second press lands on nothing.

   The ring is the framework's and the turning is Olive's `animate-spin'; what
   this file adds is the one class a reader who has asked for less motion does
   not get -- a ring that is simply there, which is what it says either way.

   Nothing here knows anything on its own: a form is marked when it is sent, and
   a page whose script did not run is a page with forms that work (and no mark),
   which is what a browser has always done. */

(function () {
  var reduced = window.matchMedia
    && window.matchMedia('(prefers-reduced-motion: reduce)').matches

  /* The mark, as the framework draws it: a ring, and the sentence it stands for
     in a box the page does not show. */
  function mark(button) {
    if (!button || button.getAttribute('data-busy') === 'true') { return }
    button.setAttribute('data-busy', 'true')
    button.setAttribute('aria-busy', 'true')
    button.disabled = true

    var ring = document.createElement('span')
    ring.className = 'hg-spinner inline-block align-middle mr-2 w-4 h-4'
    if (!reduced) { ring.className += ' animate-spin' }
    ring.setAttribute('aria-hidden', 'true')

    button.insertBefore(ring, button.firstChild)
  }

  /* The button a form was sent with: the one that was pressed where the browser
     says which that was, and the form's own submit control otherwise (a reader
     who sent it with the Enter key). */
  function submitterOf(form, event) {
    if (event.submitter) { return event.submitter }
    return form.querySelector('button[type="submit"]')
      || form.querySelector('button:not([type])')
  }

  /* Read on the way down -- a listener on the document, in the capturing phase
     -- so the mark is in the page before the browser starts to leave it. */
  document.addEventListener('submit', function (event) {
    var form = event.target
    if (!form || form.nodeName !== 'FORM' || !form.checkValidity
        || !form.checkValidity()) {
      return
    }
    mark(submitterOf(form, event))
  }, true)
})()
