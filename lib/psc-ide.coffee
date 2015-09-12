{BufferedProcess} = require 'atom'
{XRegExp} = require 'xregexp'

class PscIde
  editors: null

  constructor: ->
    @startServer()

  runCmd: (cmd) ->
    return new Promise (resolve,reject) =>
      command = @pscIde
      args = ['-p', @pscIdePort]
      stdout = (output) =>
        try
          output = JSON.parse output
        console.debug "psc-ide", cmd, "-->", output
        resolve output
      exit = (code) =>
        console.debug "exited with code #{code}"
        reject code if code is not 0
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
    @serverProcess = new BufferedProcess
      command: pscIdeServer
      args: ['-p', @pscIdePort]
      options:
        cwd: dirs[0].path
      exit = (code) =>
        atom.notifications.addWarning "psc-ide process exited with code #{code}"

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
      if prefix.length is 0
        null
      else
        @getCompletion(prefix,@editors.activeModules)
          .then (completions) =>
            resolve completions.map (c) =>
              text: c.identifier
              displayText: c.identifier + ": " + @abbrevType c.type
              description: c.type
              type: if /->/.test(c.type) then "function" else "value"

module.exports = PscIde
