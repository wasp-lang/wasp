/**
 * Medium Editor Helpers
 *
 * Browser-side functions for pasting content into Medium's editor via
 * Chrome DevTools `evaluate_script`. Each exported function is a standalone
 * snippet — pass it as the `function` parameter to `evaluate_script`.
 *
 * IMPORTANT: evaluate_script `args` passes DOM elements, not strings, so
 * string values (title text, HTML chunks) must be inlined into the function
 * body before calling evaluate_script. The wrapper functions below accept
 * the value as a regular parameter; when you call evaluate_script, replace
 * the parameter reference with a string literal.
 *
 * These use synthetic ClipboardEvent paste — do NOT replace with
 * document.execCommand or the fill tool; both bypass Medium's internal
 * state and cause save errors.
 */

/**
 * Sets the article title in Medium's new-story editor.
 *
 * Usage with evaluate_script — inline the title directly:
 *   evaluate_script({ function: setMediumTitle.toString().replace('TITLE_PLACEHOLDER', escapedTitle) })
 *
 * Or copy the function body below and replace TITLE_PLACEHOLDER with the
 * actual title string.
 */
function setMediumTitle() {
  const title = `TITLE_PLACEHOLDER`;
  const editor = document.querySelector("[contenteditable]");
  const titleEl = editor.querySelector("h3");
  titleEl.focus();
  const range = document.createRange();
  range.selectNodeContents(titleEl);
  const sel = window.getSelection();
  sel.removeAllRanges();
  sel.addRange(range);
  const dt = new DataTransfer();
  dt.setData("text/plain", title);
  const pasteEvent = new ClipboardEvent("paste", {
    bubbles: true,
    cancelable: true,
    clipboardData: dt,
  });
  titleEl.dispatchEvent(pasteEvent);
  return "title set";
}

/**
 * Pastes an HTML chunk into the body of Medium's editor at the end of the
 * current content.
 *
 * Usage with evaluate_script — inline the HTML chunk directly:
 *   evaluate_script({ function: pasteMediumChunk.toString().replace('HTML_CHUNK_PLACEHOLDER', escapedHtml) })
 *
 * Or copy the function body below and replace HTML_CHUNK_PLACEHOLDER with
 * the actual HTML string. Escape backticks and ${} sequences first.
 */
function pasteMediumChunk() {
  const htmlChunk = `HTML_CHUNK_PLACEHOLDER`;
  const editor = document.querySelector("[contenteditable]");
  const range = document.createRange();
  range.selectNodeContents(editor);
  range.collapse(false);
  const sel = window.getSelection();
  sel.removeAllRanges();
  sel.addRange(range);
  const dt = new DataTransfer();
  dt.setData("text/html", htmlChunk);
  dt.setData("text/plain", "");
  const pasteEvent = new ClipboardEvent("paste", {
    bubbles: true,
    cancelable: true,
    clipboardData: dt,
  });
  editor.dispatchEvent(pasteEvent);
  return "pasted " + htmlChunk.length + " chars";
}
