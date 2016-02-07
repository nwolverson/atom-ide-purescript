path = require 'path'
helpers = require 'atom-linter'
{Range, BufferedProcess} = require 'atom'

{ getProjectRoot } = require './utils'

psBuild = require './psjs/IdePurescript.Atom.Build'

class LinterPurescript
  lintProcess: null

  constructor: (@editors, @linter) ->

  lintOnSave: (textEditor) =>
    if !atom.config.get("ide-purescript.buildOnSave")
      return Promise.resolve([])

    filePath = textEditor.getPath()
    dirs = (dir for dir in atom.project.rootDirectories when dir.contains(filePath))
    projDir = if dirs.length == 1 then dirs[0].path else filePath.replace(/src\/.*/, "")
    @lint projDir

  lintOnBuild: () =>
    projDir = getProjectRoot()
    @lint projDir

  lint: (projDir) ->
    return new Promise (resolve, reject) =>
      atom.notifications.addInfo "Compiling PureScript"

      buildCommand = atom.config.get("ide-purescript.buildCommand").trim().split(/\s+/)
      command = buildCommand[0]
      args = buildCommand.slice(1)

      psBuild.linterBuild({ command, args, directory: projDir })()
        .then ({messages, result}) =>
          @linter.deleteMessages()
          @linter.setMessages messages
          @editors.onCompiled messages
          if result is "success"
            atom.notifications.addSuccess("Built PureScript")
          else
            atom.notifications.addWarning "PureScript build completed with errors"
          resolve messages
        .catch (err) =>
          console.error(err)
          atom.notifications.addError "Error running build command '#{command}'. Check configuration.\n" + err

module.exports = LinterPurescript
