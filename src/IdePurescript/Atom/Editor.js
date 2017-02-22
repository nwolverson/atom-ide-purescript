// module IdePurescript.Atom.Editor

exports._addPurescriptClass = function(className) {
  return function(editor) {
    return function() {
      var view = atom.views.getView(editor);

      if (view.classList.contains(className))
        return {};

      view.classList.add(className);
      editor.onDidDestroy(function() {
        view.classList.remove(className);
      });

      return {};
    };
  };
};
