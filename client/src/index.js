import lo from "lodash";
import $ from "jquery";

//--------------------
// INITIALIZATION
//--------------------

const initState = {
  timerFormOpen: false,
  noteFormOpen: false,
  currentTimers: {}
};

function init() {
  console.log('initializing...');

  const uid = currentUserID();
  const state = lo.merge(lo.cloneDeep(initState), { userID: uid });
    // We just pass the state around by reference and mutate it,
    // because we're lazy programmers.

  if (uid === null) {
    console.log("could not get user ID from URL. are you on the right page?");
    return;
  }

  // Set up a constant tick to update all the timers that
  // have been triggered. Every half second seems like a
  // reasonable tick, since timers will lose seconds
  // at different times.
  setInterval(() => updateTimers(state.currentTimers), 500);

  $.get(`/users/${uid}/elements`)
    .done((data) => initUI(data, state))
    .fail(failInit);
}

function updateTimers(timers) {
  for (const timerID in timers) {
    updateTimer(timers[timerID]);
  }
}

function updateTimer(timer) {
  const sinceTriggered = Math.trunc((Date.now() - timer.triggeredTime) / 1000);
  const remaining = timer.duration - sinceTriggered;

  if (remaining < 0) {
    timer.durationDOM.text(`${timer.duration}s`);
  } else {
    timer.durationDOM.text(`${remaining}s`);
  }
}

function failInit(error) {
  console.log(`failed to initialize application: ${error}`);
}

function initUI(initData, state) {
  console.log(`initializing UI...`);
  console.log(initData);

  addElementButtons(state);

  initData.data.elements.forEach((element) => {
    if (element.element_type === 'timer') {
      $('#content').append(timerElement(
        state,
        element.element_id,
        element.timer_name,
        element.timer_duration,
        element.timer_triggered_time
      ));
    } else if (element.element_type === 'note') {
      $('#content').append(noteElement(
        state,
        element.element_id,
        element.note_text
      ));
    }
  });
}

function addElementButtons(state) {
  const addTimer =
    $('<button id="add-timer" type="button">+ Timer</button>');
  const addNote =
    $('<button id="add-note" type="button">+ Note</button>');
  const addTimerForm =
    $(`
      <form id="add-timer-form">
        <input type="text" name="timer_name" placeholder="Timer name" required/>
        <input type="text" name="timer_duration" placeholder="Timer duration" required/>
        <button type="submit">Create timer</button>
      </form>
    `);
  const addNoteForm =
    $(`
      <form id="add-note-form">
        <input type="text" name="note_text" placeholder="Note text" required/>
        <button type="submit">Create note</button>
      </form>
    `);

  addTimer.click(onClickAddTimer(state, addTimerForm, addNoteForm));
  addNote.click(onClickAddNote(state, addTimerForm, addNoteForm));

  const buttons =
    $('<div id="add-buttons"></div>');

  buttons.append(addTimer);
  buttons.append(addNote);

  buttons.append(addTimerForm);
  buttons.append(addNoteForm);

  addTimerForm.hide();
  addNoteForm.hide();

  addTimerForm.submit((event) => {
    onSubmitTimerForm(state, addTimerForm, addNoteForm);
    event.preventDefault();
  });
  addNoteForm.submit((event) => {
    onSubmitNoteForm(state, addTimerForm, addNoteForm);
    event.preventDefault();
  });

  $('#content').append(buttons);
}

//--------------------
// UTILITIES
//--------------------

// Create a new timer DOM node and register it with the application state
// so it can be updated.
function timerElement(state, elementID, name, duration, triggeredTime) {
  const timer = $(`
    <div id="element-${elementID}" class="timer">
      <span class="timer-name">${name}</span>
    </div>
  `);
  const remainingDuration = $(`<span class="timer-duration"></span>`);
  const triggerButton = $(`<button type="button">Start timer</button>`);
  const deleteButton = $(`<button type="button">X</button>`);

  timer.append(remainingDuration);
  timer.append(triggerButton);
  timer.append(deleteButton);

  triggerButton.click((event) => {
    onTriggerTimer(state, elementID);
    event.preventDefault();
  });

  deleteButton.click((event) => {
    onClickDelete(state, timer, elementID);
    event.preventDefault();
  });

  state.currentTimers[elementID] = {
    durationDOM: remainingDuration,
    duration: Number(duration),
    triggeredTime: triggeredTime && Date.parse(triggeredTime)
  };

  return timer;
}

// Create a new note DOM node.
function noteElement(state, elementID, text) {
  const note = $(`
    <div id="element-${elementID}" class="note">
      <span class="note-text">${text}</span>
    </div>
  `);
  const deleteButton = $(`<button type="button">X</button>`);

  note.append(deleteButton);

  deleteButton.click((event) => {
    onClickDelete(state, note, elementID);
    event.preventDefault();
  });

  return note;
}

// We assume that we're on the user's dashboard.
function currentUserID() {
  const pattern = /\/dashboard\/([0-9]+)$/;
  const match = pattern.exec(window.location.toString()) || [];

  return Number(match[1]);
}

//--------------------
// EVENT HANDLERS
//--------------------

function onClickAddTimer(state, addTimerForm, addNoteForm) {
  return () => {
    if (state.timerFormOpen) return;
    state.timerFormOpen = true;
    state.noteFormOpen = false;

    console.log("opening the timer form...");

    addTimerForm.show();
    addNoteForm.hide();
  };
}

function onClickAddNote(state, addTimerForm, addNoteForm) {
  return () => {
    if (state.noteFormOpen) return;
    state.noteFormOpen = true;
    state.timerFormOpen = false;

    console.log("opening the note form...");

    addNoteForm.show();
    addTimerForm.hide();
  };
}

function onClickDelete(state, elementDOM, elementID) {
  console.log(`deleting element ${elementID}...`);

  $.ajax({
    method: 'DELETE',
    url: `/users/${state.userID}/elements/${elementID}`
  })
    .done((result) => {
      if (!result.data.ok) {
        console.log(`failed to delete element ${elementID}`);
        return;
      }

      elementDOM.remove();
    })
    .fail(console.log);
}

function onTriggerTimer(state, elementID) {
  console.log(`triggering timer ${elementID}...`);

  $.post(`/users/${state.userID}/elements/${elementID}/trigger`)
    .done((result) => {
      if (!result.data.ok) {
        console.log(`failed to trigger timer ${elementID}`);
        return;
      }

      state.currentTimers[elementID].triggeredTime =
        Date.parse(result.data.timer_triggered_time);
    })
    .fail(console.log);
}

function onSubmitTimerForm(state, timerForm, noteForm) {
  console.log("creating new timer...");

  const values = {};
  timerForm.children('input').each(function(_idx, input) {
    values[input.name] = input.value;
  });

  const postData = {
    element: lo.merge(
      lo.update(values, 'timer_duration', Number),
      {
        element_type: 'timer',
        timer_triggered_time: null
      }
    )
  };

  $.post({
    url: `/users/${state.userID}/elements`,
    data: JSON.stringify(postData),
    contentType: 'application/json'
  })
    .done((result) => {
      if (!result.data.ok) {
        console.log(`error from server: ${result.data.error}`);
        return;
      }

      $('#content').append(timerElement(
        state,
        result.data.element_id,
        values.timer_name,
        values.timer_duration,
        null
      ));

      state.timerFormOpen = false;
      state.noteFormOpen = false;

      timerForm.hide();
      noteForm.hide();

      timerForm.children('input').val('');
    })
    .fail(console.log);
}

function onSubmitNoteForm(state, timerForm, noteForm) {
  console.log("creating new note...");

  const values = {};
  noteForm.children('input').each((_idx, input) => {
    values[input.name] = input.value;
  });

  const postData = {
    element: lo.merge(
      values,
      { element_type: 'note' }
    )
  };

  $.post({
    url: `/users/${state.userID}/elements`,
    data: JSON.stringify(postData),
    contentType: 'application/json'
  })
    .done((result) => {
      if (!result.data.ok) {
        console.log(`error from server: ${result.data.error}`);
        return;
      }

      $('#content').append(noteElement(
        state,
        result.data.element_id,
        values.note_text
      ));

      state.timerFormOpen = false;
      state.noteFormOpen = false;

      timerForm.hide();
      noteForm.hide();

      noteForm.children('input').val('');
    })
    .fail(console.log);
}

init();
