{BufferedProcess,Point} = require 'atom'
{XRegExp} = require 'xregexp'
fs = require 'fs'

{ getProjectRoot } = require './utils'

pscIde = require './psjs/IdePurescript.PscIde'
psAtomCompletion = require './psjs/IdePurescript.Atom.Completion'

class PscIde
  editors: null
  error: false

  activate: (editors) ->
    @editors = editors
    @startServer()
      .then () =>
        setTimeout(=>
          @editors.activate(this)
          100)

  startServer: ->
    # should watch these and restart
    @pscIdePort = atom.config.get("ide-purescript.pscIdePort")
    pscIdeServer = atom.config.get("ide-purescript.pscIdeServerExe")
    path = getProjectRoot()

    exit = (code) =>
      if code isnt 0
        atom.notifications.addError "Could not start psc-ide-server process. Check the configured port number is valid."

    # Check if there is an existing server to use first, try to provide some useful diagnostics.
    # If the configured path is incorrect an error from exec should show this.
    @getWorkingDir()
      .then (output) =>
        output = output.trim()
        if output is path
          atom.notifications.addInfo "Found existing psc-ide-server with correct path"
        else
          atom.notifications.addError "Found existing psc-ide-server with wrong path: #{output}. Correct, kill or configure a different port, and restart."
      .catch (err) =>
        atom.notifications.addInfo "Starting psc-ide-server"
        @serverProcess = new BufferedProcess
          command: pscIdeServer
          args: ['-p', @pscIdePort]
          exit: exit
          options:
            cwd: path
        @serverProcess

  dispose: ->
    @serverProcess.kill()

  getWorkingDir: ->
    pscIde.cwd()

module.exports = PscIde
