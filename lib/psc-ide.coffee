{BufferedProcess} = require 'atom'
{XRegExp} = require 'xregexp'

class PscIde
  editors: null
  error: false

  constructor: ->
    @startServer()

  runCmd: (cmd) ->
    return new Promise (resolve,reject) =>
      command = @pscIde
      args = ['-p', @pscIdePort]
      result = undefined
      stdout = (output) =>
        try
          output = JSON.parse output
        console.debug "psc-ide", cmd, "-->", output
        result = output
      exit = (code) =>
        console.debug "exited with code #{code}"
        if code is 0
          resolve result
        else
          reject { code, result }
      bp = new BufferedProcess({command,args,stdout,exit})
      bp.process.stdin.write JSON.stringify(cmd) + '\n'

  startServer: ->
    # should watch these and restart
    @pscIde = atom.config.get("ide-purescript.pscIdeExe")
    @pscIdePort = atom.config.get("ide-purescript.pscIdePort")
    pscIdeServer = atom.config.get("ide-purescript.pscIdeServerExe")
    dirs = atom.project.rootDirectories
    if dirs.length > 1
      atom.notifications.addWarning "Multiple project roots - using #{dir}"
    path = dirs[0].path

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
          atom.notifications.addError "Found existing psc-ide-server with wrong path. Correct, kill or configure a different port, and restart."
      .catch (err) =>
        atom.notifications.addInfo "Starting psc-ide-server"
        @serverProcess = new BufferedProcess
          command: pscIdeServer
          args: ['-p', @pscIdePort]
          exit: exit
          options:
            cwd: path

  deactivate: ->
    @serverProcess.kill()

  getList: (text) ->
    text.split ","
      .map (s) -> s.trim()
      .filter (s) -> s.length > 0
  trimQuote: (text) ->
    withQuotes = /"(.*)"/.exec(text)
    if withQuotes then withQuotes[1] else text

  getLoadedModules: ->
    @runCmd { command: "list" }
      .then (output) =>
        @getList output

  getWorkingDir: ->
    @runCmd { command: "cwd" }

  getCompletion: (text, modules) ->
    @runCmd
      command: "complete"
      params:
        filters: [
          {
            filter: "prefix"
            params:
              search: text
          }
          {
            filter: "modules"
            params:
              modules: modules
          }
        ]

  getType: (text) ->
    @runCmd
      command: "type"
      params:
        search: text
        filters: []
    .then (result) =>
      if result.length > 0
        @abbrevType result[0].type
      else
        ""

  abbrevType: (type) ->
    type.replace(/(?:\w+\.)+(\w+)/g, "$1")

  loadDeps: (editor) ->
    res = XRegExp.exec(editor.getText(), /^module (\S+) where/)
    if res
      @runCmd
        command: "load"
        params:
          dependencies: [res[1]]
    else
      Promise.resolve()

  getSuggestions: ({editor, bufferPosition, scopeDescriptor, prefix}) =>
    new Promise (resolve) =>
      prefix = prefix.trim()
      if prefix.length > 0
        @getCompletion(prefix,@editors.activeModules)
          .then (completions) =>
            resolve completions.map (c) =>
              text: c.identifier
              displayText: c.identifier + ": " + @abbrevType c.type
              description: c.type
              type: if /->/.test(c.type) then "function" else "value"
          .catch (err) =>
            console.warn "Suggestion error: " + err

module.exports = PscIde
