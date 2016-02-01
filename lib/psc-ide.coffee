{BufferedProcess,Point} = require 'atom'
{XRegExp} = require 'xregexp'
fs = require 'fs'

{ getModulePrefix, getProjectRoot } = require './utils'

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
        console.debug "psc-ide", JSON.stringify(cmd), "-->", JSON.stringify(response)
      exit = (code) =>
        console.debug "exited with code #{code}"
        if code is 0
          if response.resultType is "success"
            resolve response.result
          else
            reject { code, result: response.result, command: cmd }
        else
          result = if response && response.result then response.result else response
          reject { code, result, cmd }
      bp = new BufferedProcess({command,args,stdout,exit})
      bp.process.stdin.write JSON.stringify(cmd) + '\n'

  startServer: ->
    # should watch these and restart
    @pscIde = atom.config.get("ide-purescript.pscIdeExe")
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

  getLoadedModules: ->
    @runCmd { command: "list", params: { type: "module" } }

  getImports: (file) ->
    @runCmd { command: "list", params: { type: "import", file: file } }

  getWorkingDir: ->
    @runCmd { command: "cwd" }

  modulesFilter: (modulePrefix) ->
    if modulePrefix
      if modulePrefix.indexOf(".") isnt -1
        mods = [modulePrefix]
      else
        mods = @editors.getQualModule(modulePrefix)
        if mods.length is 0
          mods = [modulePrefix]
    else
      mods = @editors.getUnqualActiveModules()
    filter: "modules"
    params:
      modules: mods

  doPursuitQuery: (text, queryType) =>
    @runCmd
      command: "pursuit"
      params:
        query: text
        type: queryType
  getPursuitModuleCompletion: (text) => @doPursuitQuery(text, "package")
  getPursuitCompletion: (text) => @doPursuitQuery(text, "completion")

  getCompletion: (text, modulePrefix, moduleCompletion) ->
    filters = [
      {
        filter: "prefix"
        params:
          search: text
      }
    ]
    filters.push @modulesFilter(modulePrefix) if !moduleCompletion
    @runCmd
      command: "complete"
      params: { filters }

  getType: (text, modulePrefix) ->
    @runCmd
      command: "type"
      params:
        search: text
        filters: [ @modulesFilter(modulePrefix) ]
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
      originalPrefix = prefix
      prefix = "" if prefix is "." # shift right

      line = editor.getTextInRange([[bufferPosition.row, 0], bufferPosition])
      modulePrefix = getModulePrefix(editor, bufferPosition.translate([0, -prefix.length]))

      moduleCompletion = /^import/.test(line)
      # Module completion
      if moduleCompletion && modulePrefix
        prefix = modulePrefix + "." + prefix
        originalPrefix = prefix
        modulePrefix = undefined

      if prefix.length > 0 || originalPrefix is "."
        @getCompletion(prefix,modulePrefix,moduleCompletion)
          .then (completions) =>
            result =
              completions
              .filter (c) => !moduleCompletion || c.type is "module"
              .map (c) =>
                type =
                  if c.type is "module"
                    "import"
                  else if /^[A-Z]/.test(c.identifier)
                    "type"
                  else if /->/.test(c.type)
                    "function"
                  else
                    "value"
                {
                  replacementPrefix: # Keep the module prefix for values but not modules
                    if c.type is "module"
                      (modulePrefix||"") + originalPrefix
                    else
                      prefix
                  text: c.identifier
                  displayText:
                    if c.type is "module"
                      c.identifier
                    else if type == "type"
                      c.identifier + " " + @abbrevType c.type
                    else
                      c.identifier + ": " + @abbrevType c.type
                  description: c.type
                  type: type
                }
            resolve result
          .catch (err) =>
            console.warn "Suggestion error: " + err

module.exports = PscIde
