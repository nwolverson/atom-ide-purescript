{BufferedProcess} = require 'atom'

class PscIde
  constructor: ->
    @pscIde = atom.config.get("ide-purescript.pscIdeExe")
    @pscIdePort = atom.config.get("ide-purescript.pscIdePort")
    @getModules()
      .then (output) ->
        console.log output

  runCmd: (str) ->
    return new Promise (resolve,reject) =>
      command = @pscIde
      args = ['-p', @pscIdePort]
      stdout = (output) =>
        console.log "psc-ide", str, "-->", output
        resolve (@trimQuote output)
      exit = (code) =>
        console.log "exited with code #{code}"
        reject code if code is not 0
      bp = new BufferedProcess({command,args,stdout,exit})
      bp.process.stdin.write str + '\n'

  getList: (text) ->
    text.split(",").map (s) -> s.trim()
  trimQuote: (text) ->
    withQuotes = /"(.*)"/.exec(text)
    if withQuotes then withQuotes[1] else text

  getModules: ->
    @runCmd "print"
      .then (output) =>
        @getList output

  getCompletion: (text) ->
    @runCmd "complete #{text} Project"
      .then (output) =>
        (@getList output).map @trimQuote

  getType: (text) ->
    @runCmd "typeLookup #{text}"

  getSuggestions: ({editor, bufferPosition, scopeDescriptor, prefix}) =>
    new Promise (resolve) =>
      prefix = prefix.trim()
      if prefix.length is 0
        null
      else
        @getCompletion prefix
          .then (completions) =>
            typeProms = []
            completions.forEach (c) =>
              promise = @getType c
                .then (type) =>
                  unqualType: type.replace(/(?:\w+\.)+(\w+)/g, "$1")
                  text: c
                  type: type
              typeProms.push promise
            Promise.all(typeProms).then (types) =>
              resolve types.map (x) =>
                text: x.text
                displayText: x.text + ": " + x.unqualType
                description: x.type
                type: if /->/.test(x.type) then "function" else "value"

module.exports = PscIde
