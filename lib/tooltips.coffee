
{ HoverTooltips } = require 'hover-tooltips';
{ getModulePrefix } = require './utils';
{ Point } = require 'atom'
class PsTooltips extends HoverTooltips
  constructor: (@pscIde) ->
    super()
    @syntax = 'source.purescript'
    @provider = (pos) => new Promise (resolve) =>
      p = [pos.line-1, pos.column-1]
      editor = atom.workspace.getActivePaneItem()
      buffer = editor.buffer

      regex = /[a-zA-Z_0-9']*/
      match = ""
      initialLength = 0
      buffer.backwardsScanInRange(regex, [[p[0], 0], p], (it) ->
        match = it.matchText
        initialLength = match.length
      )
      buffer.scanInRange(regex, [p, [p[0], Infinity]], (it) ->
        match += it.matchText
      )

      prefix = getModulePrefix(editor, Point.fromObject(p).translate([0, -initialLength]))

      @pscIde.getType(match, prefix)
        .then (result) =>
          resolve { valid: result.length > 0, info: result }
        .catch (err) =>
          resolve { valid: false }

  dispose: =>
    @deactivate()

module.exports = PsTooltips
