{ BufferedProcess } = require 'atom'
{ getProjectRoot } = require './utils'
ChildProcess = require 'child_process'

class PsciProcess
  @killed = false

  sendText: (text) ->
    return if @killed
    @proc.stdin.write text

  kill: ->
    @killed = true
    @proc.stdin.end()
    @proc.kill()

  start: (textCallback) ->
    wholeCommand = atom.config.get("ide-purescript.psciCommand").split(/\s+/)
    command = wholeCommand[0]
    args = wholeCommand.slice(1)
    options =
      cwd: getProjectRoot()
    proc = ChildProcess.spawn(command, args, options)
    proc.stdout.on('data', (data) =>
      return if @killed
      textCallback data.toString()
      )
    proc.stderr.on('data', (data) =>
      return if @killed
      textCallback data.toString()
      atom.notifications.addError data.toString()
      )
    proc.on('close', (code) =>
      debugger
      return if @killed
      if code is 0
        console.info("psci exited happily")
      else
        atom.notifications.addError("psci exited with code #{code}")
      )
    proc.on('error', (err) => atom.notifications.addError("failed to spawn psci"))
    @proc = proc


class Psci
  activate: ->
    atom.commands.add("atom-workspace", "psci:open", @openPsciCommand)
    atom.commands.add("atom-text-editor", "psci:send-line", @sendLineCommand)
    atom.commands.add("atom-text-editor", "psci:send-selection", @sendSelectionCommand)
    atom.commands.add("atom-text-editor", "psci:reset", @resetPsciCommand)
  deactivate: ->
    @killProc()

  killProc: ->
    @proc.kill() if @proc
    @proc = null

  resetPsciCommand: =>
    @killProc()
    if @editor
      @editor.buffer.reload()
    @startPsci()

  openPsciCommand: =>
    @startRepl()
    @startPsci()

  startPsci: =>
    @proc = new PsciProcess()
    @proc.start @addText

  sendLineCommand: =>
    editor = atom.workspace.getActiveTextEditor()
    @sendText editor.lineTextForBufferRow(editor.getCursorBufferPosition().row)
    editor.moveDown 1
    editor.moveToBeginningOfLine()

  sendSelectionCommand: =>
    editor = atom.workspace.getActiveTextEditor()
    @sendText editor.getSelectedText()
    editor.moveDown 1
    editor.moveToBeginningOfLine()

  sendText: (text) =>
    if @proc
      text = text.trim() + "\n"
      @addText text
      @proc.sendText text

  startRepl: =>
    atom.workspace.open("PSCI", { split: "right" })
      .done (ed) =>
        @editor = ed
        view = atom.views.getView ed
        view.component.setInputEnabled false

  addText: (text) =>
    if @editor
      @editor.moveToBottom()
      @editor.insertText text

module.exports = Psci
