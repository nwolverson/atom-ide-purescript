{SelectListView} = require 'atom-space-pen-views'

class ModuleSelectListView extends SelectListView
  constructor: (@editors) ->
    super

  initialize: () =>
    super
    modules = getAvailableModules()
    @setItems modules
    @panel ?= atom.workspace.addModalPanel(item: this, visible: false)

  show: ->
    @storeFocusedElement()
    @panel.show()
    @focusFilterEditor()

  hide: ->
    @panel.hide()

  confirmed: (item) =>
    @cancel()
    @editors.addImport item

  cancelled: =>
    @hide()

  viewForItem: (item) ->
    "<li>#{item}</li>"

class QuickFixView extends SelectListView
  constructor: (@editor, @items) ->
    super

  initialize: () =>
    super
    @setItems @items

    @addClass 'overlay'
    @addClass 'ps-inline-overlay'
    @filterEditorView.model.placeholderText = "Filter list"

  show: ->
    @storeFocusedElement()
    @overlayDecoration = @editor.decorateMarker(@editor.getLastCursor().getMarker(),
            { type: "overlay", position: "tail", item: this });
    # @panel.show()
    setTimeout @onShown, 20

  onShown: =>
    @focusFilterEditor()

  hide: ->
    @restoreFocus()
    @overlayDecoration.destroy()

  confirmed: (item) =>
    item.action()
    @cancel()

  cancelled: =>
    @hide()
  getFilterKey: -> "title"
  viewForItem: (item) ->
    "<li>#{item.title}</li>"

module.exports.ModuleSelectListView = ModuleSelectListView
module.exports.QuickFixView = QuickFixView
