LinterPurescript = require './linter-purescript'
PscIde = require './psc-ide'

glob = require("glob")

module.exports =
  config:
    pscIdeExe:
      title: "psc-ide executable location"
      type: 'string'
      default: process.env.HOME + '/.local/bin/psc-ide'
    pscIdePort:
      title: "psc-ide port number"
      type: 'integer'
      default: 4242
    buildCommand:
      title: "build command"
      type: 'string'
      default: "pulp build"

  activate: (state) ->
    console.log "Activated ide-purescript"
    @pscide = new PscIde()

  provideAutocomplete: ->
    selector: '.source.purescript'
    disableForSelector: '.source.purescript .comment, .source.purescript .string'
    inclusionPriority: 1
    excludeLowerPriority: true
    getSuggestions: @pscide.getSuggestions
    # onDidInsertSuggestion: ({editor, triggerPosition, suggestion}) ->
    # dispose: ->


  provideLinter: ->
    linter = new LinterPurescript()
    return {
      grammarScopes: ['source.purescript']
      scope: 'package'
      lint: (f) -> linter.lint(f)
      lintOnFly: false
    }
  provideBuildConfig: ->
    return {
      niceName: 'PureScript',
      isEligable: (path) ->
        files = glob.sync("src/**/*.purs", {cwd: path})
        files && files.length > 0
      settings: (path) ->
        buildCommand = atom.config.get("ide-purescript.buildCommand").split(/\s+/)
        cmd = buildCommand[0]
        args = buildCommand.slice 1
        return new Promise (resolve,reject) =>
          resolve {
            cmd: cmd
            exec: cmd
            sh: true
            args: args
            errorMatch: [
              '(?<type>Error|Warning) at (?<file>[^\n]*?) line (?<line>[0-9]+), column (?<col>[0-9]+)',
              'Unable to parse module:\n[^"]*"(?<file>[^"]+)" \\(line (?<line>[0-9]+), column (?<col>[0-9]+)\\):'
            ]
          }
    }
