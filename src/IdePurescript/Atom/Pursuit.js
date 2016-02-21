// module IdePurescript.Atom.Pursuit

var Pursuit = require('./views/pursuit');

exports.pursuitSearchImpl = function(f) {
  var pursuit = new Pursuit();
  pursuit.search(f);
};

exports.pursuitSearchModulesImpl = function(f, addF) {
  var pursuit = new Pursuit();
  pursuit.searchModule(f, addF);
};
