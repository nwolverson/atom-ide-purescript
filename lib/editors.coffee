{CompositeDisposable, Disposable, TextEditor } = require 'atom'
{XRegExp} = require 'xregexp'

class Editors extends Disposable
  activeModules: []
  constructor: ->
    super =>
      @subscriptions.dispose()
    @subscriptions = new CompositeDisposable()

  activate: (pscide) ->
    @pscide = pscide
    @subscriptions.add atom.workspace.onDidChangeActivePaneItem (item) =>
      if item instanceof TextEditor
        @useEditor item

    @subscriptions.add atom.workspace.observeTextEditors (editor) =>
      @subscriptions.add editor.onDidSave (event) =>
        @useEditor editor

    editor = atom.workspace.getActiveTextEditor()
    @useEditor editor if editor


  useEditor: (editor) ->
    @pscide.loadDeps editor
      .catch (err) =>
        console.error (err)
    @activeModules = @getModulesForEditor editor
    console.debug "Active modules: " + @activeModules

  getMainModuleForEditor: (editor) ->
    res = XRegExp.exec(editor.getText(), /^module (\S+) where/)
    res[1] if res

  getModulesForEditor: (editor) ->
    regex = /^import (?:qualified )?([a-zA-Z.]+)/mg
    modules = []
    XRegExp.forEach(editor.getText(), regex, (match) ->
      modules.push(match[1])
    )
    main = @getMainModuleForEditor editor
    modules.push(main) if main
    modules

module.exports = Editors
