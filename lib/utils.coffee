module.exports.getModulePrefix = (editor, bufferPosition) ->
  moduleRegex = /(?:^|[^A-Za-z_.])((?:[A-Z][A-Za-z0-9]*\.)*(?:[A-Z][A-Za-z0-9]*))\.$/
  textBefore = editor.getTextInRange([[bufferPosition.row, 0], bufferPosition])
  modulePrefix = textBefore.match(moduleRegex)?[1]

module.exports.getProjectRoot = ->
  dirs = atom.project.rootDirectories
  path = dirs[0].path
