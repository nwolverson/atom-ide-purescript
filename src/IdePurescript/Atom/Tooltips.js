// module IdePurescript.Atom.Tooltips

exports.mkTooltipProvider = function(provide) {
    var HoverTooltips = require('hover-tooltips').HoverTooltips;
    function ToolTipProvider() {
      HoverTooltips.call(this);
      this.syntax = 'source.purescript';
      this.provider = provide;
      this.activate();
    }
    ToolTipProvider.prototype.dispose = function () { this.deactivate(); };
    ToolTipProvider.prototype = Object.create(HoverTooltips.prototype);

    return new ToolTipProvider();
};
