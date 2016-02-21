glob = require 'glob'

# still used by psci
module.exports.getProjectRoot = ->
  getRoot = (path) ->
      if (!path || path.isRoot())
        null
      else if (!path.getSubdirectory("src").existsSync() )
        getRoot(path.getParent())
      else
        path
  validDir = (d) ->
    if d
      files = glob.sync("src/**/*.purs", {cwd: d.path})
      files && files.length > 0
    else
      false
  dirs = atom.project.rootDirectories.map getRoot
  validDirs = dirs.filter validDir

  if (validDirs.length is 0)
      atom.notifications.addWarning "Doesn't look like a purescript project - didn't find any src dir"
      return null

  dir = validDirs[0]
  if validDirs.length > 1
    atom.notifications.addWarning "Multiple project roots, using first - #{dir.path}"
  else if dirs.length > 1 && validDirs.length == 1
    atom.notifications.addWarning "Multiple project roots but only 1 looks valid - #{dir.path}"

  if (! dir.getSubdirectory("output").existsSync() )
    atom.notifications.addWarning "Doesn't look like a project has been built - didn't find #{dir.path}/output"

  return dir.path
