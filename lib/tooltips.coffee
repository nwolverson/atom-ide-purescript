
{ HoverTooltips } = require 'hover-tooltips';

class PsTooltips extends HoverTooltips
  constructor: (@pscIde) ->
    super()
    @syntax = 'source.purescript'
    @provider = (pos) => new Promise (resolve) =>
      p = [pos.line-1, pos.column-1]
      buffer = atom.workspace.getActivePaneItem().buffer

      regex = /[a-zA-Z_']*/
      match = ""
      buffer.backwardsScanInRange(regex, [[p[0], 0], p], (it) ->
        match = it.matchText
      )
      buffer.scanInRange(regex, [p, [p[0], Infinity]], (it) ->
        match += it.matchText
      )

      @pscIde.getType match
        .then (result) =>
          result = @pscIde.abbrevType result
          resolve { valid: result.length > 0, info: result }
        .catch (err) =>
          resolve { valid: false }

module.exports = PsTooltips
