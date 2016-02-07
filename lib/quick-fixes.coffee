{ QuickFixView } = require('./views/select-views')

fix = (name, action) =>
  title: name
  action: () =>
    action?()
    console.debug("Applied fix: " + name)

replaceRange = (editor, range, text) =>
  editor.setTextInBufferRange(range, text)

removeRange = (editor, range) =>
  replaceRange(editor, range, "")

getFix = (editor, msg, range) =>
  debugger
  if msg?.suggestion?.hasSuggestion
    title = switch msg.errorCode
      when "UnusedImport", "RedundantEmptyHidingImport", "DuplicateImport", "RedundantUnqualifiedImport" then "Remove import"
      when "DeprecatedQualifiedSyntax" then "Qualify import"
      when "ImplicitImport" then "Make import explicit"
      when "UnusedExplicitImport" then "Remove unused references"
      else "Apply suggestion"
    fix(title, () => replaceRange(editor, range, msg.suggestion.replacement))

module.exports.showQuickFixes = (editor, linterMain, messages) =>
  debugger
  cursor = editor.getCursorBufferPosition()
  editorLinter = linterMain.getEditorLinter(editor)
  messages = Array.from(editorLinter.getMessages())

  getRange = (msg) =>
    if msg.range
      marker = editorLinter.markers.get(msg)
      if marker?.isValid()
        marker.getBufferRange()

  matches = messages.filter (msg) =>
    range = getRange(msg)
    range && range.containsPoint(cursor)

  fixes = matches.map (match) => getFix(editor, match, getRange(match))
    .filter (msg) => !!msg

  if fixes.length > 0
    qfv = new QuickFixView(editor, fixes)
    qfv.show()
