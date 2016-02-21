// module IdePurescript.Atom.Tooltips

exports.mkTooltipProvider = function(f) {
    var ToolTipProvider = require('./tooltips');
    return new ToolTipProvider(f);
};
