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
      .then (items) => @setItems items

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

  getFilterKey: -> "identifier"

  getFilterQuery: -> ""

  viewForItem: (item) ->
    "<li class='two-lines'>
      <div class='primary-line'>#{item.identifier}: <span class='text-info'>#{item.type}</span></div>
      <div class='secondary-line'>#{item.module} (#{item.package})</div>
    </li>"

  confirmed: (item) =>
    super()
    console.log("#{item.identifier} was selected")

class PursuitModuleListView extends PursuitSelectListView
  constructor: (@getCompletions, @addImport) ->
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
    view = new PursuitModuleActionView(@addImport, item.module)
    view.show()
    super()

class PursuitModuleActionView extends SelectListView
  constructor: (@addImport, @moduleName) ->
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
      @addImport @moduleName

  cancelled: =>
    @hide()

class Pursuit
  searchModule: (getModuleCompletions, addImport) =>
    if not @selectModuleView
      @selectModuleView = new PursuitModuleListView(getModuleCompletions, addImport)
    @selectModuleView.show()

  search: (getCompletions) =>
    if not @selectView
      @selectView = new PursuitIdentifierListView(getCompletions)
    @selectView.setMaxItems(30)
    @selectView.show()

module.exports = Pursuit
