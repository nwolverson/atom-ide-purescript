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

  addImport: (module) ->
    return if @activeModules.modules.concat(@activeModules.qmodules.map((x) -> x.module)).indexOf(module) isnt -1

    buffer = atom.workspace.getActiveTextEditor().getBuffer()
    lastImport = 0
    buffer.getLines().forEach (l,i) =>
      if /^module|^import/.test(l)
        lastImport = i
    buffer.insert([lastImport+1, 0], "import #{module}\n")

module.exports = Editors
