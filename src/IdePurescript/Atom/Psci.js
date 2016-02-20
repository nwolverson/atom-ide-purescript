// module IdePurescript.Atom.Psci

exports.init = function() {
  var Psci = require('./psci');
  new Psci().activate();
  return {};
}
