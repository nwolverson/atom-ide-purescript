// module IdePurescript.Atom.PromptPanel

exports.getEditorModel = function (editor) {
  return function () { return editor.getModel(); };
}

exports.focus = function (e) {
  return function () {
    e.focus();
  }
}
