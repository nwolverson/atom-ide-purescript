{SelectListView} = require 'atom-space-pen-views'

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

module.exports.QuickFixView = QuickFixView
