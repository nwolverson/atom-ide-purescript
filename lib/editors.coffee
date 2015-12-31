{CompositeDisposable, Disposable, TextEditor, Range } = require 'atom'
{XRegExp} = require 'xregexp'
{ showQuickFixes } = require './quick-fixes'

class Editors extends Disposable
  activeModules: { main: null, modules: [], qmodules: [] }
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
        if @linter
          @linter.lintOnSave editor
            .then => @useEditor editor
        else
          @useEditor editor

    editor = atom.workspace.getActiveTextEditor()
    @useEditor editor if editor

  onCompiled: (messages) ->
    editor = atom.workspace.getActiveTextEditor()
    @messages = messages
    @useEditor editor if editor

  useEditor: (editor) ->
    @pscide.loadDeps editor
      .catch (err) =>
        console.error (err)
    @getModulesForEditor editor

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
    res = XRegExp.exec(editor.getText(), /^module\s+([\w.]+)/m)
    res[1] if res

  # only distinguishing qualified imports and other - explicit and hiding parts are not respected
  getModulesForEditor: (editor) ->
    imports = @pscide.getImports editor.getPath()
      .then (imports) =>
        @activeModules =
          main: @getMainModuleForEditor editor
          modules: (imports
            .filter((imp) => !imp.qualifier)
            .map((imp) => imp.module))
          qmodules: (imports
            .filter((imp) => imp.qualifier))
        console.debug "Active modules: " + JSON.stringify(@activeModules)

  addImport: (module) ->
    return if @activeModules.modules.concat(@activeModules.qmodules.map((x) -> x.module)).indexOf(module) isnt -1

    buffer = atom.workspace.getActiveTextEditor().getBuffer()
    lastImport = 0
    buffer.getLines().forEach (l,i) =>
      if /^module|^import/.test(l)
        lastImport = i
    buffer.insert([lastImport+1, 0], "import #{module}\n")

  showQuickFixes: () =>
    editor = atom.workspace.getActiveTextEditor()
    return if !editor or !@messages

    showQuickFixes(editor, @linterMain, @messages)


module.exports = Editors
