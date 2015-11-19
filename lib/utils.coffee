module.exports.getModulePrefix = (editor, bufferPosition) ->
  moduleRegex = /(?:^|[^A-Za-z_.])((?:[A-Z][A-Za-z0-9]*\.)*(?:[A-Z][A-Za-z0-9]*))\.$/
  textBefore = editor.getTextInRange([[bufferPosition.row, 0], bufferPosition])
  modulePrefix = textBefore.match(moduleRegex)?[1]

getRoot = (path) ->
    if (!path || path.isRoot())
      null
    else if (!path.getSubdirectory("src").existsSync() )
      getRoot(path.getParent())
    else
      path

module.exports.getProjectRoot = ->
  dirs = (getRoot(dir) for dir in atom.project.rootDirectories)
  validDirs = (dir for dir in dirs when dir)

  if (validDirs.length is 0)
      atom.notifications.addWarning "Doesn't look like a purescript project - didn't find any src dir"
      return null

  dir = validDirs[0]
  path = dir.path
  if validDirs.length > 1
    atom.notifications.addWarning "Multiple project roots, using first - #{path}"
  else if dirs.length > 1 && validDirs.length == 1
    atom.notifications.addWarning "Multiple project roots but only 1 looks valid - #{path}"

  if (! dir.getSubdirectory("output").existsSync() )
    atom.notifications.addWarning "Doesn't look like a project has been built - didn't find #{path}/output"

  return path


module.exports.getAvailableModules = () ->
  fs = require('fs')
  paths = fs.readdirSync(atom.project.rootDirectories[0].path + "/output")
  paths
