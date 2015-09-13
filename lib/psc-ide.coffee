{BufferedProcess} = require 'atom'
{XRegExp} = require 'xregexp'

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


  runCmd: (cmd) ->
    return new Promise (resolve,reject) =>
      command = @pscIde
      args = ['-p', @pscIdePort]
      response = undefined
      stdout = (output) =>
        try
          response = JSON.parse output
        console.debug "psc-ide", cmd, "-->", response
      exit = (code) =>
        console.debug "exited with code #{code}"
        if code is 0
          if response.resultType is "success"
            resolve response.result
          else
            reject { code, result: response.result }
        else
          result = if response && response.result then response.result else response
          reject { code, result }
      bp = new BufferedProcess({command,args,stdout,exit})
      bp.process.stdin.write JSON.stringify(cmd) + '\n'

  startServer: ->
    # should watch these and restart
    @pscIde = atom.config.get("ide-purescript.pscIdeExe")
    @pscIdePort = atom.config.get("ide-purescript.pscIdePort")
    pscIdeServer = atom.config.get("ide-purescript.pscIdeServerExe")
    dirs = atom.project.rootDirectories
    path = dirs[0].path
    if dirs.length > 1
      atom.notifications.addWarning "Multiple project roots - using #{path}"

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
        @serverProcess

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

  modulesFilter: (mods) ->
    filter: "modules"
    params:
      modules: if mods then mods else @editors.getUnqualActiveModules()

  getCompletion: (text, modulePrefix) ->
    if modulePrefix
      if modulePrefix.indexOf(".") isnt -1
        mods = [modulePrefix]
      else
        mods = @editors.getQualModule(modulePrefix)
        if mods.length is 0
          mods = [modulePrefix]
    @runCmd
      command: "complete"
      params:
        filters: [
          {
            filter: "prefix"
            params:
              search: text
          }
          @modulesFilter(mods)
        ]

  getType: (text) ->
    @runCmd
      command: "type"
      params:
        search: text
        filters: [ @modulesFilter() ]
    .then (result) =>
      if result.length > 0
        @abbrevType result[0].type
      else
        ""

  abbrevType: (type) ->
    type.replace(/(?:\w+\.)+(\w+)/g, "$1")

  loadDeps: (editor) ->
    main = @editors.getMainModuleForEditor editor
    if main
      @runCmd
        command: "load"
        params:
          dependencies: [main]
    else
      Promise.resolve()

  getSuggestions: ({editor, bufferPosition, scopeDescriptor, prefix}) =>
    new Promise (resolve) =>
      prefix = prefix.trim()

      moduleRegex = /(?:^|[^A-Za-z_.])((?:[A-Z][A-Za-z]*\.)*(?:[A-Z][A-Za-z]*))\.$/
      textBefore = editor.getTextInRange([[bufferPosition.row, 0], [bufferPosition.row, bufferPosition.column - prefix.length]])
      modulePrefix = textBefore.match(moduleRegex)?[1]

      if prefix.length > 0
        @getCompletion(prefix,modulePrefix)
          .then (completions) =>
            resolve completions.map (c) =>
              text: c.identifier
              displayText: c.identifier + ": " + @abbrevType c.type
              description: c.type
              type: if /->/.test(c.type) then "function" else "value"
          .catch (err) =>
            console.warn "Suggestion error: " + err

module.exports = PscIde
