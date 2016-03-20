// module IdePurescript.Atom.Hooks.Dependencies

exports.installDependencies = function() {
  return require('atom-package-deps')
    .install('ide-purescript');
}
