LinterPurescript = require './linter-purescript'

glob = require("glob")

module.exports = AtomLinterPurescript =
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
        return new Promise (resolve,reject) =>
          resolve {
            cmd: 'pulp'
            exec: 'pulp'
            sh: true
            args: ['build']
            errorMatch: [
              '(?<type>Error|Warning) at (?<file>[^\n]*?) line (?<line>[0-9]+), column (?<col>[0-9]+)',
              'Unable to parse module:\n[^"]*"(?<file>[^"]+)" \\(line (?<line>[0-9]+), column (?<col>[0-9]+)\\):'
            ]
          }
    }
