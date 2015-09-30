{ BufferedProcess } = require 'atom'
{ getProjectRoot } = require './utils'

class Psci
  activate: ->
    atom.commands.add("atom-workspace", "psci:open", @openPsciCommand)
    atom.commands.add("atom-text-editor", "psci:send-line", @sendLineCommand)
    atom.commands.add("atom-text-editor", "psci:send-selection", @sendSelectionCommand)
    atom.commands.add("atom-text-editor", "psci:reset", @resetPsciCommand)
  deactivate: ->

  resetPsciCommand: =>
    if @proc
      @proc.kill()
    if @editor
      @editor.buffer.reload()
    @startPsci()

  openPsciCommand: =>
    @startRepl()
    @startPsci()

  sendLineCommand: =>
    editor = atom.workspace.getActiveTextEditor()
    @sendText editor.lineTextForBufferRow(editor.getCursorBufferPosition().row)
    editor.moveDown 1
    editor.moveToBeginningOfLine()

  sendSelectionCommand: =>
    editor = atom.workspace.getActiveTextEditor()
    @sendLineCommand editor.getSelectedText()
    editor.moveDown 1
    editor.moveToBeginningOfLine()

  sendText: (text) =>
    @addText(text + "\n")
    @proc.process.stdin.write(text + "\n")

  startRepl: ->
    atom.workspace.open("PSCI", { split: "right" })
      .done (ed) =>
        @editor = ed
        view = atom.views.getView ed
        view.component.setInputEnabled false

  addText: (text) ->
    if @editor
      @editor.moveToBottom()
      @editor.insertText text

  startPsci: ->
    wholeCommand = atom.config.get("ide-purescript.psciCommand").split(/\s+/)
    command = wholeCommand[0]
    args = wholeCommand.slice(1)
    stdout = (output) =>
      @addText output
    stderr = (error) =>
      @addText error
      atom.notifications.addError error
    exit = (code) =>
      if code is 0
        atom.notifications.addInfo("psci exited happily")
      else
        atom.notifications.addError("psci exited with code #{code}")
    options =
      cwd: getProjectRoot()
    @proc = new BufferedProcess({command,args,stdout,exit,options})

module.exports = Psci
