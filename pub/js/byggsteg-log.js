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

   Nothing here knows anything on its own: the address the pane is fetched from,
   whether the job is still writing, how long the log row was when the reader was
   last sent it, and the line each folded reading belongs to, all travel in the
   markup (see `log-output' in `(app views logs)').  Everything in this file is
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
  var seconds = 3

  function pane(name) {
    return section.querySelector('[data-log-pane="' + name + '"]')
  }

  function running() {
    var writer = section.querySelector('[data-log-running]')
    return writer ? writer.getAttribute('data-log-running') === 'true' : false
  }

  /* Whether the reader is at the end of the page, which is where a reader
     following a build is: what arrives is drawn where they are looking, and a
     reader who has scrolled back to read a line keeps where they were. */
  function atTheEnd() {
    return window.pageYOffset + window.innerHeight >= document.body.scrollHeight - 48
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

  function choose(name) {
    view = name
    var tabs = section.querySelectorAll('[data-log-tab]')
    for (var i = 0; i < tabs.length; i++) {
      var chosen = tabs[i].getAttribute('data-log-tab') === name
      tabs[i].setAttribute('aria-pressed', chosen ? 'true' : 'false')
      tabs[i].style.opacity = chosen ? '1' : '0.6'
    }
    pane('output').hidden = name !== 'output'
    pane('raw').hidden = name !== 'raw'
    try { localStorage.setItem('log-view', name) } catch (error) { }
    if (!read[name]) { follow(name) }
  }

  function stop() {
    if (timer) { clearInterval(timer); timer = null }
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
      .then(function (response) { return response.text() })
      .then(function (html) {
        busy = false
        var holder = document.createElement('div')
        holder.innerHTML = html
        var state = holder.firstElementChild
        if (!state || !state.hasAttribute('data-log-running')) { return }
        /* A pane the reader already has is answered with the state and no
           output at all, which is what a poll with nothing new to draw is: the
           job has printed no more, and the document is not drawn again. */
        if (state.firstElementChild) {
          /* The answer is the pane as the server draws it -- the state of the
             job around the output -- so it is drawn where the pane's own state
             was rather than in place of it: what the page was answered with
             first is what it is answered with again. */
          while (target.firstChild) { target.removeChild(target.firstChild) }
          target.appendChild(state)
          unfoldAgain(opened)
        }
        target.setAttribute('data-log-have', state.getAttribute('data-log-length') || '')
        read[name] = true
        if (stick) { window.scrollTo(0, document.body.scrollHeight) }
        if (state.getAttribute('data-log-running') !== 'true') { stop() }
      })
      .catch(function () {
        busy = false
        /* A page that cannot ask any more is a page with the output it was
           answered with, which is what a reader has if they never open it. */
        stop()
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

  var drawn = section.querySelector('[data-log-length]')
  if (drawn) {
    pane('output').setAttribute('data-log-have', drawn.getAttribute('data-log-length'))
  }

  var remembered = null
  try { remembered = localStorage.getItem('log-view') } catch (error) { remembered = null }
  choose(remembered === 'raw' ? 'raw' : 'output')

  if (running()) { timer = setInterval(function () { follow(view) }, seconds * 1000) }
})()
