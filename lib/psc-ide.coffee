{BufferedProcess} = require 'atom'
{XRegExp} = require 'xregexp'

class PscIde
  editors: null

  constructor: ->
    @pscIde = atom.config.get("ide-purescript.pscIdeExe")
    @pscIdePort = atom.config.get("ide-purescript.pscIdePort")

  runCmd: (str) ->
    return new Promise (resolve,reject) =>
      command = @pscIde
      args = ['-p', @pscIdePort]
      stdout = (output) =>
        console.debug "psc-ide", str, "-->", output
        resolve (@trimQuote output).trim()
      exit = (code) =>
        console.debug "exited with code #{code}"
        reject code if code is not 0
      bp = new BufferedProcess({command,args,stdout,exit})
      bp.process.stdin.write str + '\n'

  getList: (text) ->
    text.split ","
      .map (s) -> s.trim()
      .filter (s) -> s.length > 0
  trimQuote: (text) ->
    withQuotes = /"(.*)"/.exec(text)
    if withQuotes then withQuotes[1] else text

  getLoadedModules: ->
    @runCmd "print"
      .then (output) =>
        @getList output

  getCompletion: (text, modules) ->
    @runCmd "complete #{text} Project using #{modules.join(', ')}"
      .then (output) =>
        regex = /\(([^,]+), ([^,]+), ([^,]+)\)(,|$)/g
        results = []
        XRegExp.forEach(output, regex, (match) ->
          results.push
            module: match[1]
            ident: match[2]
            type: match[3]
        )
        results

  getType: (text) ->
    @runCmd "typeLookup #{text}"
      .then (result) =>
        result = result.trim()
        if result.indexOf("not found") != -1 then "" else result

  abbrevType: (type) ->
    type.replace(/(?:\w+\.)+(\w+)/g, "$1")

  loadDeps: (editor) ->
    res = XRegExp.exec(editor.getText(), /^module (\S+) where/)
    if res
      @runCmd "dependencies #{res[1]}"
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
              text: c.ident
              displayText: c.ident + ": " + @abbrevType c.type
              description: c.type
              type: if /->/.test(c.type) then "function" else "value"

module.exports = PscIde
