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

   The list of runs, kept current.

   A list of runs is a page about runs that are still going: one of them starts
   while a reader is looking at it, another prints, stops, and says how it went.
   The page used to answer that by asking for itself again every twelve seconds,
   which is a page that takes the reader's place on the list with it every time
   -- and does it whether or not a single row can have changed.

   So the rows themselves say which of them can still change (`data-log-running',
   drawn by `make-job-link' in `(byggsteg html)'), and this asks for those rows
   alone, every few seconds, at `data-logs-status-url' on the list.  A page of
   runs that are all over asks for nothing at all.

   What comes back is a row per job -- its status, with the mark of work in
   progress inside it, and the controls the run is offered now (see `logs-status'
   in `(app views logs)') -- and the row is drawn over where it stands rather
   than the list being drawn again: the answer names the job each row is about,
   and only the places inside that row are replaced.  A reader keeps their place
   on the list, their scroll and their row.

   A run can also be taken while the reader is reading: one that was not on the
   list when it was drawn is a row the page has never had, and no amount of
   redrawing the rows it does have will produce it.  So the first page of the
   list also asks for the newest runs (`data-logs-head'), and the answer carries
   those rows whole; the ones the list does not show are put in at the top, in
   the order the list keeps.  A list with nothing left to come still asks, more
   slowly, so that it can be told about a run that starts while it is open.

   Nothing here knows anything on its own: the address, the ids of the rows still
   to come, how many newest runs to ask for and whether each row is still to come
   all travel in the markup.  Everything in this file is read as a hint rather
   than a truth: a page whose script did not run is a page of runs as they were
   when it was answered, which is what a list has always been. */

(function () {
  var list = document.getElementById('logs-list')
  if (!list || !window.fetch) { return }

  var url = list.getAttribute('data-logs-status-url') || '/logs-status'
  /* How many newest runs the answer also carries, or null on a page that is not
     the newest page of the list: an older page has no top for a run just taken
     to arrive at. */
  var head = list.getAttribute('data-logs-head')
  var seconds = 3
  /* A list with nothing left to come is not a list with nothing to say: a run
     can be taken at any moment, and this is how often the page looks for one.
     It is read more slowly than a run that is still writing, because a run
     appearing is not a reading that moves under the reader's eye. */
  var idle = 15
  var longest = 30
  var wait = seconds
  var timer = null

  function rows() {
    return list.querySelectorAll('[data-log-row]')
  }

  /* The ids of the rows that can still change, which is what the answer is
     asked for.  A row that has stopped is not asked for again: its status is
     the reading the row was last drawn with, and there is nothing more to say
     about it. */
  function stillToCome() {
    var nodes = rows()
    var ids = []
    for (var i = 0; i < nodes.length; i++) {
      if (nodes[i].getAttribute('data-log-running') === 'true') {
        ids.push(nodes[i].getAttribute('data-log-row'))
      }
    }
    return ids
  }

  function place(root, name) {
    return root.querySelector('[' + name + ']')
  }

  /* Draw one place of a row from the place the answer draws it in.  What is
     drawn is the server's markup and not a reading of it here, because the
     readings are the server's: only it knows how a run went, or what its reader
     may do next.  A place the answer leaves empty -- the actions of a run that
     is going -- is hidden rather than left as a box in the row. */
  function apply(row, answered, name) {
    var here = place(row, name)
    var there = place(answered, name)
    if (!here || !there) { return }
    here.innerHTML = there.innerHTML
    here.hidden = !there.firstElementChild
  }

  /* Draw the answer over the rows it is about, put in the runs that appeared
     while the reader was reading, and leave the rest of the list alone.

     A row of the answer the list does not show is one of two things: a run that
     was taken since the list was drawn, which is a row to add, or a row of the
     list that is no longer there, which is a row this has nothing to draw on.
     The answer only carries rows the list asks for and rows of the newest page
     of it, so a row named and not shown is a run that has just started. */
  function draw(html) {
    var holder = document.createElement('div')
    holder.innerHTML = html
    var answered = holder.querySelectorAll('[data-log-row]')
    var fresh = []
    for (var i = 0; i < answered.length; i++) {
      var id = answered[i].getAttribute('data-log-row')
      var row = list.querySelector('[data-log-row="' + id + '"]')
      if (!row) { fresh.push(answered[i]); continue }
      row.setAttribute('data-log-running',
                       answered[i].getAttribute('data-log-running') || 'false')
      apply(row, answered[i], 'data-log-status')
      apply(row, answered[i], 'data-log-actions')
    }
    /* The answer names the newest runs newest first, and each one put in at the
       top pushes the one before it down, so they go in from the oldest of them
       to leave the list in the order it is read in. */
    for (var j = fresh.length - 1; j >= 0; j--) {
      list.insertBefore(fresh[j], list.firstChild)
    }
  }

  function stop() {
    if (timer) { clearTimeout(timer); timer = null }
  }

  function schedule(delay) {
    stop()
    timer = setTimeout(ask, delay * 1000)
  }

  /* How long the list waits before asking again: the pace of a run that is
     still writing while there is one, the pace of a list that is only waiting
     for a run to be taken otherwise, and nothing at all when neither is true. */
  function pace() {
    if (stillToCome().length) { return seconds }
    if (head) { return idle }
    return null
  }

  function ask() {
    var ids = stillToCome()
    var next = pace()
    /* A list of finished runs on a page that is told about new ones still asks,
       more slowly: it is the page that says a run has started.  One that is
       told nothing new stops entirely. */
    if (!next) { stop(); return }
    /* `at' keeps a poll from being answered out of a cache: every ask is about
       rows that are changing, and the answer before it is not an answer to it. */
    var query = url + '?ids=' + ids.join(',')
              + (head ? '&head=' + head : '') + '&at=' + new Date().getTime()
    fetch(query, { credentials: 'same-origin' })
      .then(function (response) {
        if (!response.ok) { throw new Error('the rows were not answered') }
        return response.text()
      })
      .then(function (html) {
        wait = seconds
        draw(html)
        /* What decides the next ask is the list as the answer just left it: a
           run that arrived with it is one more row that is still to come. */
        var after = pace()
        if (after) { schedule(after) } else { stop() }
      })
      .catch(function () {
        /* A list that cannot ask once is not a list that cannot ask: the runs
           on it are still going, so it asks again, and each failure makes the
           next ask wait longer -- up to the half minute a reader will wait
           before deciding the deployment is down rather than busy. */
        wait = Math.min(wait * 2, longest)
        if (head || stillToCome().length) { schedule(wait) } else { stop() }
      })
  }

  if (pace()) { schedule(seconds) }
})()
