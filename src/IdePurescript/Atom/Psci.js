// module IdePurescript.Atom.Psci

exports.init = function() {
  var Psci = require('./psci');
  new Psci().activate();
  return {};
}

exports.getModel = function(element) {
  return function() {
    if (typeof element.getModel === "function") {
      return element.getModel();
    }
    return null;
  }
}
