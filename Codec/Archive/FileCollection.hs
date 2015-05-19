{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
-- This module represents an abstraction of a file tree. The motivating use case
-- is abstracting the difference between a real file system tree, rooted in some
-- directory, and the contents of a zip archive. Where possible, the methods
-- associated with a FileCollection mimic those provided by
-- directory:System.Directory, but take an extra leading parameter denoting
-- the root to work from

module Codec.Archive.FileCollection
       (
       ) where

import qualified System.Directory as SD
import Codec.Archive.Zip
import Data.Monoid((<>))
import qualified Data.List as L
import Data.Maybe(mapMaybe,listToMaybe)

class FileCollection d where
  createDirectory :: d → FilePath → IO d
  createDirectoryIfMissing :: d → FilePath → IO d
  removeDirectory :: d → FilePath → IO d
  removeDirectoryRecursive :: d → FilePath → IO d
  renameDirectory :: d → FilePath → FilePath → IO d
  getDirectoryContents :: d → FilePath → IO [FilePath]
  removeFile :: d → FilePath → IO d
  renameFile :: d → FilePath → FilePath → IO d
  copyFile :: d → FilePath → FilePath → IO d
  -- These don't work recursively
  findFile :: d → [FilePath] → String → IO (Maybe FilePath)
  findFiles :: d → [FilePath] → String → IO [FilePath]
  doesFileExist :: d → FilePath → IO Bool
  doesDirectoryExist :: d → FilePath → IO Bool


combine :: FilePath → FilePath → FilePath
combine root relative = root <> ('/':relative)

combineRunReturn action root rel = do
  let full = combine root rel
  _ ← action full
  return full
combineRunReturn2 action root old new = do
    let fullOld = combine root old
        fullNew = combine root new
    _ ← action fullOld fullNew
    return fullNew

-- This represents native files
instance FileCollection [Char] where
  createDirectory = combineRunReturn SD.createDirectory
  createDirectoryIfMissing = combineRunReturn (SD.createDirectoryIfMissing True)
  removeDirectory = combineRunReturn SD.removeDirectory
  removeDirectoryRecursive = combineRunReturn SD.removeDirectoryRecursive
  renameDirectory = combineRunReturn2 SD.renameDirectory
  getDirectoryContents root rel = SD.getDirectoryContents $ combine root rel
  removeFile = combineRunReturn SD.removeFile
  renameFile = combineRunReturn2 SD.renameFile
  copyFile = combineRunReturn2 SD.copyFile
  findFile root subs = SD.findFile (map (combine root) subs)
  findFiles root subs = SD.findFiles (map (combine root) subs)
  doesFileExist root rel = SD.doesFileExist $ combine root rel
  doesDirectoryExist root rel = SD.doesDirectoryExist $ combine root rel

-- Returns all paths located beneath root in the heirarchy
subFiles arch path = filter (L.isPrefixOf path) $ map eRelativePath $ zEntries arch
getDirectoryContents' arch path = map (drop l) $ subFiles arch path
  where l = length path

instance FileCollection Archive where
  -- Creating a directory doesn't really mean anything in a zip archive, since
  -- every entry has its own path fully specified
  createDirectory a = return . const a
  createDirectoryIfMissing a = return . const a
  removeDirectory a = return . const a
  removeDirectoryRecursive arch path = return $ foldr deleteEntryFromArchive arch $ subFiles arch path
  renameDirectory arch oldPath newPath =
    return $ foldr addEntryToArchive (foldr deleteEntryFromArchive arch oldNames) newEntries
    where oldNames = subFiles arch oldPath
          oldEntries = mapMaybe (`findEntryByPath` arch) oldNames
          newEntries = map rename oldEntries
          rename e@Entry{ eRelativePath = rp } =
            e { eRelativePath = newPath <> drop l rp }
          l = length oldPath
  getDirectoryContents arch path = return $ getDirectoryContents' arch path
  removeFile arch path = return $ deleteEntryFromArchive path arch
  renameFile arch old new
    | old == new = return arch
    | otherwise = copyFile arch old new >>= flip removeFile old
  copyFile arch old new
    | old == new = return arch
    | otherwise = case newEntry of
        (Just e) → return $ addEntryToArchive e arch
        Nothing → return arch
    where oldEntry = findEntryByPath old arch
          newEntry = changeName new <$> oldEntry
          changeName new e = e { eRelativePath = new }
  findFile arch subs target = listToMaybe <$> findFiles arch subs target
  findFiles arch subs target =
     return $ map (<>target)
     $ filter (elem target . getDirectoryContents' arch) subs
  doesFileExist arch path= case findEntryByPath path arch of
    Just e → if null $ subFiles arch $ eRelativePath e
             then return True
             else return False
    Nothing → return False
  doesDirectoryExist arch path= case findEntryByPath path arch of
    Just e → if null $ subFiles arch $ eRelativePath e
             then return False
             else return True
    Nothing → return False

          
