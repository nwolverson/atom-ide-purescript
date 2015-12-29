{SelectListView} = require 'atom-space-pen-views'

class PursuitSelectListView extends SelectListView
  constructor: (@getCompletions) ->
    super

  initialize: () =>
    super
    @panel ?= atom.workspace.addModalPanel(item: this, visible: false)
    editor = @[0].firstChild.getModel()
    buffer = editor.getBuffer()
    buffer.stoppedChangingDelay = 1000
    buffer.onDidStopChanging (text) =>
      @getCompletions buffer.getText()

  show: ->
    @storeFocusedElement()
    @panel.show()
    @focusFilterEditor()

  hide: ->
    @panel.hide()

  confirmed: (item) =>
    @cancel()

  cancelled: =>
    @hide()

class PursuitIdentifierListView extends PursuitSelectListView
  initialize: () ->
    super

  getFilterKey: -> "ident"

  getFilterQuery: -> ""

  viewForItem: (item) ->
    "<li class='two-lines'>
      <div class='primary-line'>#{item.ident}: <span class='text-info'>#{item.type}</span></div>
      <div class='secondary-line'>#{item.module} (#{item.package})</div>
    </li>"

  confirmed: (item) =>
    super()
    console.log("#{item.identifier} was selected")

class PursuitModuleListView extends PursuitSelectListView
  constructor: (@getCompletions, @editors) ->
    super(@getCompletions)

  initialize: () ->
    super

  getFilterKey: -> "module"

  viewForItem: (item) ->
    "<li class='two-lines'>
      <div class='primary-line'>#{item.module}</div>
      <div class='secondary-line'>#{item.package}</div>
    </li>"

  confirmed: (item) =>
    view = new PursuitModuleActionView(@editors, item.module)
    view.show()
    super()

class PursuitModuleActionView extends SelectListView
  constructor: (@editors, @moduleName) ->
    super

  initialize: () =>
    super
    @panel ?= atom.workspace.addModalPanel(item: this, visible: false)
    @setItems ["Import module", "Cancel"]

  viewForItem: (item) ->
    "<li>#{item}</li>"

  show: ->
    @storeFocusedElement()
    @panel.show()
    @focusFilterEditor()

  hide: ->
    @panel.hide()

  confirmed: (item) =>
    @cancel()
    if item is "Import module"
      @editors.addImport @moduleName

  cancelled: =>
    @hide()

class Pursuit
  constructor: (@pscide, @editors) ->

  getModuleCompletions: (text) =>
    @pscide.getPursuitModuleCompletion text
      .then (items) => @selectModuleView.setItems items

  getCompletions: (text) =>
    @pscide.getPursuitCompletion text
      .then (items) => @selectView.setItems items

  searchModule: () =>
    if not @selectModuleView
      @selectModuleView = new PursuitModuleListView(@getModuleCompletions, @editors)
    @selectModuleView.show()

  search: () =>
    if not @selectView
      @selectView = new PursuitIdentifierListView(@getCompletions)
    @selectView.setMaxItems(30)
    @selectView.show()

module.exports = Pursuit
