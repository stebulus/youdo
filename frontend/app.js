'use strict';

var document = require('global/document');
var hg = require('mercury');
var h = require('mercury').h;

/**
 * Regex to capture the date
 */
var dateRegex = /([a-z]{3}[0-9]{1,2}|tomorrow|today|nextweek|soon|whenever)/i;

/**
 * initialization state for the reducer responsible for parsing
 * text and finding mentions and tags.
 */
var initYouDoReducerState = function() {
  return {
    mentions: [],
    tags: [],
    buffer: []
  };
};

/**
 * initialization state for the reducer responsible for styling
 * YouDo text
 */
var initYouDoReducerStylerState = function() {
  return {
    styledText: [],
    buffer: []
  };
};

/**
 * Reducer method responsible for parsing assignees, dates, and 
 * tags from the text.
 *
 * State looks like:
 * acc = { mentions: []
 *       , tags: []
 *       , buffer: [] // array of characters
 *       }
 */
var youDoReducer = function(acc, value) {

  if (value == '@' ) {
    acc.mentions.push(acc.buffer.join(""));
    acc.buffer = [];
  } else if (value == '#') {
    acc.tags.push(acc.buffer.join(""));
    acc.buffer = [];
  } else if (value == ' ') {
    acc.buffer = [];
  } else {
    /**
     * prepend to buffer
     * (we're folding right)
     */
    acc.buffer.unshift(value);
  }

  return acc;
}

/**
 * Reducer responsible for stylizing the text. uses mercury's
 * weird styling `h` function.
 *
 * State looks like:
 * acc = { styledText: [] // array text or h('',...)
 *       , buffer: [] // array of characters
 *       }
 */
var youDoReducerStyler = function(acc, value, index) {

  if(value == '@') {
    acc.styledText.unshift(h('span.red', '@' + acc.buffer.join("")));
    acc.buffer = [];
  } else if (value == '#') {
    acc.styledText.unshift(h('span.green', '#' + acc.buffer.join("")));
    acc.buffer = [];
  } else if (value == ' ') {
    acc.styledText.unshift(' ' + acc.buffer.join(""));
    acc.buffer = [];
  } else if (index == 0) {
    acc.styledText.unshift(value + acc.buffer.join(""));
  } else {
    acc.buffer.unshift(value);
  }

  return acc;
}


/**
 * YouDo state
 */
function YouDo() {
  return hg.state({
    text: hg.value(''),
    who: hg.value(''),
    when: hg.value(''),
    tags: hg.value([]),
    txId: hg.value(-1),
    handles: {
      change: setText,
      finishEdit: save,
    }
  });
}

/**
 * Helper function to create an input element
 */
function inputBox(value, sink, finish) {
  return h('form', {role: "form"}, h('div.form-group', h('input.form-control', {
    value: value,
    name: 'text',
    type: 'text',
    'ev-event': hg.changeEvent(sink),
    'ev-blur': hg.valueEvent(finish)
  })));
}

/**
 * After receiving form input, parse it and
 * set the state
 */
function setText(state, data) {

  var parsedResults = data.text.split('').reduceRight(youDoReducer, initYouDoReducerState());

  state.text.set(data.text);

  if( parsedResults.mentions.length > 0) {
    state.who.set(parsedResults.mentions[0]);
  }

  /**
   * Fold over the tags and catch which ones match the date regex.
   * TODO: this accepts only the last date to match.
   */
  var tagSplit = parsedResults.tags.reduce(function(acc,tag) {
      if(dateRegex.test(tag)) {
        acc.when = tag;
      } else {
        acc.tags.push(tag);
      }

      return acc;
    }, { when: "", tags: [] });

  state.when.set(tagSplit.when);
  state.tags.set(tagSplit.tags);

}

/**
 * Save to a database
 */
function save(state, data) {
  console.log('saving...');
  console.log('who: ' + state.who());
  console.log('when: ' + state.when());
  console.log('text: ' + state.text());
  console.log('tags: ' + state.tags());
  console.log('last transaction: ' + state.txId());
  state.txId.set(state.txId() + 1);
  console.log('new transaction: ' + state.txId());
}


/**
 * Render a YouDo
 */
YouDo.render = function render(state) {

  return h('div', [
      inputBox(state.text, state.handles.change, state.handles.finishEdit),
      h('p.content',
        state
          .text
          .split('')
          .reduceRight(youDoReducerStyler, initYouDoReducerStylerState())
          .styledText
       )
  ]);
};

/**
 * Main entry point
 */
hg.app(document.getElementById('youdo-app'), YouDo(), YouDo.render);
