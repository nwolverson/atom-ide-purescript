{SelectListView} = require 'atom-space-pen-views'

class PursuitSelectListView extends SelectListView
  constructor: (@getCompletions) ->
    super

  initialize: () =>
    super
    # @addClass('overlay from-top')
    @panel ?= atom.workspace.addModalPanel(item: this, visible: false)
    editor = @[0].firstChild.getModel()
    buffer = editor.getBuffer()
    buffer.stoppedChangingDelay = 200
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
    console.log("#{item.identifier} was selected")

  cancelled: =>
    @hide()

class PursuitIdentifierListView extends PursuitSelectListView
  initialize: () ->
    super

  getFilterKey: -> "ident"

  viewForItem: (item) ->
    "<li class='two-lines'>
      <div class='primary-line'>#{item.ident}: <span class='text-info'>#{item.type}</span></div>
      <div class='secondary-line'>#{item.module} (#{item.package})</div>
    </li>"

class PursuitModuleListView extends PursuitSelectListView
  initialize: () ->
    super

  getFilterKey: -> "module"

  viewForItem: (item) ->
    "<li class='two-lines'>
      <div class='primary-line'>#{item.module}</div>
      <div class='secondary-line'>#{item.package}</div>
    </li>"


class Pursuit
  constructor: (@pscide) ->

  getModuleCompletions: (text) =>
    @pscide.getPursuitModuleCompletion text
      .then (items) => @selectModuleView.setItems items

  getCompletions: (text) =>
    @pscide.getPursuitCompletion text
      .then (items) => @selectView.setItems items

  searchModule: () =>
    if not @selectModuleView
      @selectModuleView = new PursuitModuleListView(@getModuleCompletions)
      @selectModuleView.initialize()
    @selectModuleView.show()

  search: () =>
    if not @selectView
      @selectView = new PursuitIdentifierListView(@getCompletions)
      @selectView.initialize()
    @selectView.show()

module.exports = Pursuit
