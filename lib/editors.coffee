{CompositeDisposable, Disposable, TextEditor } = require 'atom'
{XRegExp} = require 'xregexp'

class Editors extends Disposable
  activeModules: []
  constructor: (@pscide) ->
    super =>
      @subscriptions.dispose()
    @subscriptions = new CompositeDisposable()
    @subscriptions.add atom.workspace.onDidChangeActivePaneItem (item) =>
      if item instanceof TextEditor
        @useEditor item

    @subscriptions.add atom.workspace.observeTextEditors (editor) =>
      @subscriptions.add editor.onDidSave (event) =>
        @useEditor editor
    @pscide.editors = this

  useEditor: (editor) ->
    @pscide.loadDeps editor
    @activeModules = @getModulesForEditor editor
    console.debug "Active modules: " + @activeModules

  getModulesForEditor: (editor) ->
    regex = /^import (?:qualified )?([a-zA-Z.]+)/mg
    modules = []
    XRegExp.forEach(editor.getText(), regex, (match) ->
      modules.push(match[1])
    )
    modules

module.exports = Editors
