LinterPurescript = require './linter-purescript'

module.exports = AtomLinterPurescript =
  provideLinter: ->
    linter = new LinterPurescript()
    return {
      grammarScopes: ['source.purescript']
      scope: 'file'
      lint: (f) -> linter.lint(f)
      lintOnFly: false
    }
