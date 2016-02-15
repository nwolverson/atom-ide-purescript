// module IdePurescript.Atom.QuickFixes

exports.showQuickFixesImpl = function(e,l,m) {
  var qf = require('./quick-fixes');
  qf.showQuickFixes(e,l,m);
  return {};
}
