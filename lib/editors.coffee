{CompositeDisposable, Disposable, TextEditor, Range } = require 'atom'
{XRegExp} = require 'xregexp'
{ showQuickFixes } = require './quick-fixes'

psModules = require './psjs/IdePurescript.Modules'

class Editors extends Disposable
  constructor: ->
    super =>
      @subscriptions.dispose()
    @subscriptions = new CompositeDisposable()
    @psmodules = psModules.modulesState()

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

  getUnqualActiveModules: () => @psmodules.getUnqualActiveModules()

  getQualModule: (qualifier) => @psmodules.getQualModule(qualifier)()

  getMainModuleForEditor: (editor) =>
    @psmodules.getMainModuleForFile(editor.getText())

  getModulesForEditor: (editor) =>
    @psmodules.loadModules(editor.getPath())(editor.getText())()
      .then () =>
        console.debug "Active modules: " + JSON.stringify( @psmodules.getUnqualActiveModules() )

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
