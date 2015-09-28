{SelectListView} = require 'atom-space-pen-views'

class ModuleSelectListView extends SelectListView
  constructor: (@editors) ->
    super

  initialize: () =>
    super
    modules = require('./utils').getAvailableModules()
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

module.exports = ModuleSelectListView
