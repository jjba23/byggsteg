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

   A job's log page, followed.

   The page is answered once with the output the job had printed by then, and
   this is what happens after that.  A job that is still writing its output has
   the pane it is read in asked for again every few seconds and drawn again with
   what came back, so a build's output arrives while the build prints it, drawn
   the way a terminal draws it.  The page used to ask for itself again instead,
   which reloaded the whole document every three seconds: the reader lost their
   place in a long log at every poll, and every reading of a line they had
   unfolded was folded again.

   Two readings of the output are on the page and this chooses between them: the
   console the job printed, and the bytes the command sent with the control
   characters spelled out.  The bytes are not in the page until they are asked
   for -- a reader who wants the console should not download the log twice --
   and are fetched the first time that reading is chosen.

   What a reader follows is the output, not the document.  The pane the log is
   read in has a height of its own and scrolls inside the page, so "the end" is
   the end of the pane's own scroll -- the page around it stays where it is, and
   the run's name, its facts and its actions stay in front of the reader while
   the output moves under them.  A page whose pane is shorter than its content
   is the only case where the page itself has to move, which is why each of
   those two readings is taken from the box it is a reading of.

   A poll of the pane is answered with the run's state as well: its status, the
   mark of work in progress, its facts and its actions (see `log-poll' in
   `(app views logs)').  Those are drawn where the page draws them, so a build
   that ends while a reader is watching it says `job succeeded' -- and offers the
   retry it now allows -- without the page being asked for again.

   A poll that is not answered is asked again rather than given up on.  The job
   is still writing whether or not this page can reach the server for a moment,
   and a page that stopped following it at the first refused request left the
   reader with a log that stops mid-line and a status that says `in progress'
   for a run that will never say anything again on that page.  So a failed ask
   is made again, each one after a longer wait -- the page backs off to the half
   minute a reader will wait before deciding the deployment is down rather than
   busy, and comes back to its own pace the moment an ask is answered.

   Nothing here knows anything on its own: the address the pane is fetched from,
   whether the job is still writing, how long the log row was when the reader was
   last sent it, and the line each folded reading belongs to, all travel in the
   markup (see `log-page' in `(app views logs)').  Everything in this file is
   read as a hint rather than a truth: a page whose script did not run is still a
   page with the job's output on it. */
(function () {
  var section = document.getElementById('job-log')
  if (!section || !window.fetch) { return }

  var url = section.getAttribute('data-log-url')
  var view = 'output'
  var read = { output: true, raw: false }
  var timer = null
  var busy = false
  /* The pace a page that is being written is read at, and the longest this
     waits after asks that were not answered. */
  var seconds = 3
  var longest = 30
  var wait = seconds

  function pane(name) {
    return section.querySelector('[data-log-pane="' + name + '"]')
  }

  /* Whether the run has more to say, which is the section's own reading of it:
     the pane is not asked, because a pane is what is being drawn and the state
     of the run is what says whether to draw it again. */
  function running() {
    return section.getAttribute('data-log-running') === 'true'
  }

  /* Whether the reader is at the end of what they are reading, which is where a
     reader following a build is: what arrives is drawn where they are looking,
     and a reader who has scrolled back to read a line keeps where they were.

     The reading is taken of the pane rather than of the page, because the pane
     is what the reader follows: the page around it stays where it is, and a
     reader who stretches the window while a build runs is still at the end of
     the output underneath it.  A page with no pane in it -- one whose script
     could not find the section it is following -- is read as a reader who is
     at the end, so that a poll still draws what it was answered with. */
  function atTheEnd() {
    var box = pane(view)
    if (!box) { return true }
    return box.scrollTop + box.clientHeight >= box.scrollHeight - 24
  }

  /* Put the reader at the end of the pane, which is what a page opened on a job
     that is still writing does: the last line is the one they came for, and a
     build that has printed a thousand lines should open on the thousandth.  The
     page itself is left where it is -- the run's name, its facts and its
     actions are above the pane, and a reader who opened the page to read them
     is not sent past them. */
  function followTheEnd() {
    var box = pane(view)
    if (box && box.scrollHeight > box.clientHeight) {
      box.scrollTop = box.scrollHeight
    }
  }

  /* The readings the reader has unfolded, by the line they belong to.  The pane
     that replaces this one is drawn from the log again, so the folded readings
     are folded again with it; these are the ones to unfold once it is drawn. */
  function unfolded() {
    var indexes = []
    var open = section.querySelectorAll('[data-frames-toggle][aria-expanded="true"]')
    for (var i = 0; i < open.length; i++) {
      indexes.push(open[i].getAttribute('data-frames-index'))
    }
    return indexes
  }

  function unfold(button, open) {
    var group = button.parentNode
    var frames = group ? group.querySelector('[data-frames]') : null
    var caret = button.querySelector('[data-frames-caret]')
    if (frames) { frames.hidden = !open }
    if (caret) { caret.textContent = open ? '\u25be' : '\u25b8' }
    button.setAttribute('aria-expanded', open ? 'true' : 'false')
  }

  function unfoldAgain(indexes) {
    for (var i = 0; i < indexes.length; i++) {
      var button = section.querySelector('[data-frames-toggle][data-frames-index="'
                                        + indexes[i] + '"]')
      if (button) { unfold(button, true) }
    }
  }

  /* Draw the run's state from the answer over the place the page draws it: the
     status in the heading, the mark of work in progress beside the tabs, the
     facts in the column and the actions that become possible once the job has
     stopped writing.

     What is drawn is the server's markup for it and not a reading of the answer
     here, because the readings are the server's: only it knows how a job went,
     what it took, or what its reader may do next.  A place that is answered
     empty -- the mark of a job that is over, the actions of a reader who is
     offered none -- is hidden rather than left as a box in the layout.

     The places are looked for in the document rather than in the section the
     output is read in: the status is in the page's heading and the run's facts
     and actions are in the column beside the pane, which is the whole point of
     drawing them -- they stay in front of a reader while the output moves. */
  function redraw(state) {
    var places = ['data-log-status', 'data-log-live', 'data-log-facts',
                  'data-log-actions']
    for (var i = 0; i < places.length; i++) {
      var here = document.querySelector('[' + places[i] + ']')
      var there = state.querySelector('[' + places[i] + ']')
      if (!here || !there) { continue }
      if (here.innerHTML !== there.innerHTML) {
        here.innerHTML = there.innerHTML
        here.hidden = !there.firstElementChild
      }
    }
  }

  /* The tab showing the reading the reader is looking at is filled and the other
     is paper.  What those two look like is the console's -- the page is drawn
     with the two faces on each tab, and this script takes whichever of the two
     it means from the tab itself, so the theme is stated once, where the rest of
     the console's faces are, and not again here. */
  function choose(name) {
    view = name
    var tabs = section.querySelectorAll('[data-log-tab]')
    for (var i = 0; i < tabs.length; i++) {
      var chosen = tabs[i].getAttribute('data-log-tab') === name
      tabs[i].setAttribute('aria-pressed', chosen ? 'true' : 'false')
      var face = tabs[i].getAttribute(chosen ? 'data-log-tab-chosen'
                                             : 'data-log-tab-rest')
      if (face) { tabs[i].className = face }
    }
    pane('output').hidden = name !== 'output'
    pane('raw').hidden = name !== 'raw'
    try { localStorage.setItem('log-view', name) } catch (error) { }
    if (!read[name]) { follow(name) }
  }

  function stop() {
    if (timer) { clearTimeout(timer); timer = null }
  }

  /* When the pane is asked for again, which is after an answer and after a
     failure alike: what decides whether to ask at all is the job, not this. */
  function schedule(delay) {
    stop()
    timer = setTimeout(function () { follow(view) }, delay * 1000)
  }

  function follow(name) {
    if (busy) { return }
    busy = true
    var target = pane(name)
    var have = target.getAttribute('data-log-have')
    var opened = unfolded()
    var stick = atTheEnd()
    /* `at' is what keeps a poll from being answered out of a cache: every ask
       is a page of a log that is being written, and the one before it is not
       an answer to it. */
    var query = url + '?view=' + name + '&at=' + new Date().getTime()
              + (have ? '&have=' + have : '')
    fetch(query, { credentials: 'same-origin' })
      .then(function (response) {
        /* A refusal or an error is an answer that says nothing about the job,
           which is the kind of ask this retries rather than trusts. */
        if (!response.ok) { throw new Error('the log was not answered') }
        return response.text()
      })
      .then(function (html) {
        busy = false
        /* The server is answering again, so the page goes back to the pace of
           a log that is being written. */
        wait = seconds
        var holder = document.createElement('div')
        holder.innerHTML = html
        var state = holder.firstElementChild
        /* An answer that is not the state of a run -- an error page served
           with a 200, or a page from something else in front of the
           deployment -- leaves the reader with what they have and asks again. */
        if (!state || !state.hasAttribute('data-log-state')) {
          schedule(wait)
          return
        }
        /* The run's own readings are drawn first, and then the output: a page
           that is told the job is over draws it where the reader is looking
           before the last line arrives under it. */
        redraw(state)
        /* What the page believes about the job is what the answer says, so that
           the section is read the same way by whoever reads it next -- the
           retry after a failed ask included. */
        section.setAttribute('data-log-running',
                             state.getAttribute('data-log-running') || 'false')
        /* A pane the reader already has is answered with the state and no
           output at all, which is what a poll with nothing new to draw is: the
           job has printed no more, and the document is not drawn again. */
        var content = state.querySelector('[data-log-out]')
        if (content) {
          /* The output is moved out of the answer into the pane rather than the
             answer being put in the pane, because the answer also carries the
             run's state, which belongs outside it. */
          while (target.firstChild) { target.removeChild(target.firstChild) }
          while (content.firstChild) { target.appendChild(content.firstChild) }
          unfoldAgain(opened)
        }
        target.setAttribute('data-log-have', state.getAttribute('data-log-length') || '')
        read[name] = true
        if (stick) { followTheEnd() }
        /* A job that has stopped writing has said everything it will say: the
           page draws what it was answered with and stops asking.  One that is
           still writing is asked for again. */
        if (running()) { schedule(wait) }
        else { stop() }
      })
      .catch(function () {
        busy = false
        /* A page that cannot ask once is not a page that cannot ask: the job is
           still writing, and the pane it writes into is what the reader came
           for.  So the ask is made again, each failure after a longer wait, so
           that a deployment that is busy -- or a reader whose connection has
           gone -- is not hammered by every tab that is following a build.  What
           the page says meanwhile is the output it was last answered with, and
           the mark of work in progress beside it, which is still true. */
        wait = Math.min(wait * 2, longest)
        if (running()) { schedule(wait) } else { stop() }
      })
  }

  document.addEventListener('click', function (event) {
    var node = event.target
    while (node && node !== document.body) {
      if (node.hasAttribute) {
        if (node.hasAttribute('data-frames-toggle')) {
          unfold(node, node.getAttribute('aria-expanded') !== 'true')
          return
        }
        if (node.hasAttribute('data-log-tab')) {
          choose(node.getAttribute('data-log-tab'))
          return
        }
      }
      node = node.parentNode
    }
  })

  var drawn = section.getAttribute('data-log-length')
  if (drawn) { pane('output').setAttribute('data-log-have', drawn) }

  var remembered = null
  try { remembered = localStorage.getItem('log-view') } catch (error) { remembered = null }
  choose(remembered === 'raw' ? 'raw' : 'output')

  if (running()) {
    /* A job that is still writing is opened at the end of what it has written
       so far, and then asked for again: the reader is following the build, and
       the pane is where it is being written. */
    followTheEnd()
    schedule(seconds)
  }
})()
