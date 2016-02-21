// module IdePurescript.Atom.Imports

exports.showAddImportsViewImpl = function(getModules, addImport) {
  var ModuleSelectListView = require('./views/select-views').ModuleSelectListView;
  var view = new ModuleSelectListView(getModules, addImport);
  view.show();
};
