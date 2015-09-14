{CompositeDisposable, Disposable, TextEditor } = require 'atom'
{XRegExp} = require 'xregexp'

class Editors extends Disposable
  activeModules: null
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

  getUnqualActiveModules: () ->
    if @activeModules.main
      @activeModules.modules.concat(@activeModules.main)
    else
      @activeModules.modules

  getQualModule: (qualifier) ->
    @activeModules.qmodules
      .filter (m) -> m.qualifier is qualifier
      .map (m) -> m.module

  getMainModuleForEditor: (editor) ->
    res = XRegExp.exec(editor.getText(), /^module (\S+) where/)
    res[1] if res

  getModulesForEditor: (editor) ->
    buffer = editor.buffer
    lines = buffer.getLines()

    # lax module parsing
    regex = /^import\s+(?:qualified\s+)?([a-zA-Z0-9.]+)\s*(?:hiding\s*)?(?:\([^)]*\))?\s*(?:as\s+([a-zA-Z0-9.]+))?/
    qmodules = []
    modules = []
    lines.forEach (line) =>
      match = line.match regex
      if match
        if match[2]
          qmodules.push
            module: match[1]
            qualifier: match[2]
        else
          modules.push(match[1])
      # TODO - bail early
    {
      main: @getMainModuleForEditor editor
      modules: modules
      qmodules: qmodules
    }

module.exports = Editors
