path = require 'path'
{XRegExp} = require 'xregexp'
helpers = require 'atom-linter'
{Range} = require 'atom'

parseTextErrors = (result) ->
  mkResult = (match) ->
    lineEnd = match.lineEnd || match.lineStart
    colEnd = match.colEnd || match.colStart
    return {
      type: match.type || "Error",
      text: match.message.trimRight(),
      filePath: match.file,
      range: [[match.lineStart-1, match.colStart-1], [lineEnd-1, colEnd-1]]
      multiline: /\n/.test(match.message)
    }

  matches = []

  regexes = [
    '^(?<type>Error|Warning)[^\\n]+:\\n+(\\s*in module [^\\n]+\\n)?(\\s*at (?<file>[^\\n]*) line (?<lineStart>[0-9]+), column (?<colStart>[0-9]+) - line (?<lineEnd>[0-9]+), column (?<colEnd>[0-9]+)\\n)?\\n*(?<message>.*?)^[^\\n]*?See'
  ]

  regexes.forEach (regex) ->
    XRegExp.forEach result, XRegExp(regex, "sm"), (match) ->
      res = mkResult(match)
      # Previously removed overlapping warnings but just go ahead, there really are distinct errors:
      # if !matches.some((existing) -> Range.fromObject(existing.range).intersectsWith(Range.fromObject(res.range), true))
      matches.push(mkResult(match))

  matches

parseJsonErrors = (result) ->
  mkResult = (err, errorType) =>
    type: errorType,
    text: err.message,
    filePath: err.filename,
    range: [[err.position.startLine-1, err.position.startColumn-1], [err.position.endLine-1, err.position.endColumn-1]] if err.position
    multiline: /\n/.test(err.message)

  res = []
  result.split '\n'
    .forEach (line) =>
      if line.startsWith '{"warnings":'
        debugger
        out = JSON.parse line
        res = out.errors.map (e) => mkResult(e, "Error")
          .concat(out.warnings.map (e) => mkResult(e, "Warning"))
      else
        []
  console.log(JSON.stringify(res))
  res

class LinterPurescript
  lintProcess: null

  constructor: (@editors) ->

  lint: (textEditor) ->


    return new Promise (resolve, reject) =>
      if !atom.config.get("ide-purescript.enableAtomLinter")
        resolve([])
        return

      buildCommand = atom.config.get("ide-purescript.buildCommand").split(/\s+/)
      command = buildCommand[0]
      args = buildCommand.slice(1)

      filePath = textEditor.getPath()
      dirs = (dir for dir in atom.project.rootDirectories when dir.contains(filePath))
      projDir = if dirs.length == 1 then dirs[0].path else filePath.replace(/src\/.*/, "")

      options = { cwd: projDir, stream: "stderr" }

      atom.notifications.addInfo "linter: compiling PureScript"
      helpers.exec(command, args, options)
        .then (result) =>
          matches = parseTextErrors result
            .concat(parseJsonErrors result)

          @editors.onCompiled()

          atom.notifications.addSuccess "linter: compiled PureScript"

          resolve(matches)
        .then null, (err) ->
          reject(err)


module.exports = LinterPurescript
