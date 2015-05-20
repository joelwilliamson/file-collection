{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

-------------------------------------------------------------------------------
-- |
-- Module       :       Codec.Archive.FileCollection
-- Copyright    :       (c) Joel Williamson 2015
-- License      :       BSD 3-clause
-- Maintainer   :       joel.s.williamson@gmail.com
-- Stability    :       Testing
-- 
-- A uniform interface over file archives and directories
--
-------------------------------------------------------------------------------
module Codec.Archive.FileCollection
       (
         FileCollection(..),
         File(..)
       ) where

import qualified System.Directory as SD
import Codec.Archive.Zip
import Data.Monoid((<>))
import qualified Data.List as L
import Data.Maybe(mapMaybe,listToMaybe)
import qualified Data.ByteString.Lazy as BS(ByteString,appendFile,readFile,writeFile)
import System.Clock(Clock(Realtime),getTime,sec)

import Prelude hiding (readFile,writeFile)
{- $intro
This module represents an abstraction of a file tree. The motivating use case
is abstracting the difference between a real file system tree, rooted in some
directory, and the contents of a zip archive. Where possible, the methods
associated with a FileCollection mimic those provided by
directory:System.Directory, but take an extra leading parameter denoting
the root to work from
-}

class File f where
  fileName :: f → String
  readFile :: f → IO BS.ByteString
  writeFile :: f → BS.ByteString → IO f
  appendFile :: f → BS.ByteString → IO f

instance File FilePath where
  fileName = id
  readFile f = BS.readFile f
  writeFile f c = BS.writeFile f c >> return f
  appendFile f c = BS.appendFile f c >> return f

instance File Entry where
  fileName = eRelativePath
  readFile = return . fromEntry
  writeFile e newContents = do
    now <- fromIntegral <$> sec <$> getTime Realtime
    return $ toEntry (eRelativePath e) now newContents
  appendFile e addContents = do
    oldContents <- readFile e
    writeFile e $ oldContents <> addContents
    
    

-- | @class FileCollection d where@ An object that is a heirarchical arrangement
-- | of files. This could be a tree in the file system, or a file archive.
class File (AssocFile d) ⇒ FileCollection d where
  -- | An element in the collection, capable of being read from.
  type AssocFile d :: *
  -- | @'createDirectory' root path@ creates a new directory /root\/path/ which
  -- | is initially empty. The path to the new directory is returned.
  createDirectory :: d → FilePath → IO d
  -- | @'createDirectoryIfMissing' root path@ creates a new directory /root\/path/
  -- | if it doesn't exist. It also creates any missing ancestors of @path@
  createDirectoryIfMissing :: d → FilePath → IO d
  -- | @'removeDirectory' root dir@ removes an existing directory /dir/.
  removeDirectory :: d → FilePath → IO d
  -- | @'removeDirectoryRecursive' root dir@ removes a directory /dir/ and all
  -- | its contents and subdirectories.
  removeDirectoryRecursive :: d → FilePath → IO d
  -- | @'renameDirectory' root source target@ changes the name of an existing
  -- | directory from /source/ to /target/. If the /target/ directory already
  -- | exists, it can be removed or merged with the /source/ directory.
  renameDirectory :: d → FilePath → FilePath → IO d
  -- | @'getDirectoryContents' root dir@ returns a list of all entries
  -- | immediately contained in /dir/.
  getDirectoryContents :: d → FilePath → IO [FilePath]
  -- | @'getFile' filePath@ returns a `AssocFile d` with the given path.
  -- | /Warning: Error checking is not guaranteed. This should only be used with
  -- | the output from `getDirectoryContents`
  getFile :: d → FilePath → AssocFile d
  -- | @'removeFile' root file@ removes the directory entry for an existing file,
  -- | where /file/ is not a directory.
  removeFile :: d → FilePath → IO d
  -- | @'renameFile' root old new@ changes the name of an existing file from /old/
  -- | to /new/. If the /new/ object already exists, it is replaced by /old/.
  renameFile :: d → FilePath → FilePath → IO d
  -- | @'copyFile' root old new@ creates a duplicate of /old/ with the name /new/.
  copyFile :: d → FilePath → FilePath → IO d
  -- | @'addFile' root file@ adds the file to the collection if it isn't already
  -- | present.
  addFile :: d → AssocFile d → IO d
  -- These don't work recursively
  -- | @'findFile' root dirs file@ returns the path of /file/ if it can be found
  -- | in any of /dirs/.
  findFile :: d → [FilePath] → String → IO (Maybe (AssocFile d))
  findFiles :: d → [FilePath] → String → IO [AssocFile d]
  -- | @'doesFileExist' root path@ returns True if /path/ exists and is not a
  -- | directory.
  doesFileExist :: d → FilePath → IO Bool
  -- | @'doesDirectoryExist' root path@ returns True if /path exists and is a
  -- | directory.
  doesDirectoryExist :: d → FilePath → IO Bool


combine :: FilePath → FilePath → FilePath
combine root relative = root <> ('/':relative)

combineRunReturn :: (FilePath → IO a) → FilePath → FilePath → IO FilePath
combineRunReturn action root rel = do
  let full = combine root rel
  _ ← action full
  return full
combineRunReturn2 :: (FilePath → FilePath → IO a) → FilePath → FilePath → FilePath → IO FilePath
combineRunReturn2 action root old new = do
    let fullOld = combine root old
        fullNew = combine root new
    _ ← action fullOld fullNew
    return fullNew

-- This represents native files
instance FileCollection [Char] where
  type AssocFile [Char] = FilePath
  createDirectory = combineRunReturn SD.createDirectory
  createDirectoryIfMissing = combineRunReturn (SD.createDirectoryIfMissing True)
  removeDirectory = combineRunReturn SD.removeDirectory
  removeDirectoryRecursive = combineRunReturn SD.removeDirectoryRecursive
  renameDirectory = combineRunReturn2 SD.renameDirectory
  getDirectoryContents root rel = SD.getDirectoryContents $ combine root rel
  getFile _ f = f
  removeFile = combineRunReturn SD.removeFile
  renameFile = combineRunReturn2 SD.renameFile
  copyFile = combineRunReturn2 SD.copyFile
  addFile dir = return . const dir
  findFile root subs = SD.findFile (map (combine root) subs)
  findFiles root subs = SD.findFiles (map (combine root) subs)
  doesFileExist root rel = SD.doesFileExist $ combine root rel
  doesDirectoryExist root rel = SD.doesDirectoryExist $ combine root rel

-- Returns all paths located beneath root in the heirarchy
subFiles :: Archive → FilePath → [FilePath]
subFiles arch path = filter (L.isPrefixOf path) $ map eRelativePath $ zEntries arch
getDirectoryContents' :: Archive → FilePath → [FilePath]
getDirectoryContents' arch path = map (drop l) $ subFiles arch path
  where l = length path

instance FileCollection Archive where
  type AssocFile Archive = Entry
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
  getFile arch path = case findEntryByPath path arch of
    Nothing → error "Called Archive:getFile on a non-existant path"
    Just e → e
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
          newEntry = changeName <$> oldEntry
          changeName e = e { eRelativePath = new }
  addFile arch entry = return $ addEntryToArchive entry arch
  findFile arch subs target = listToMaybe <$> findFiles arch subs target
  findFiles arch subs target =
     return $ map (getFile arch) $ map (<>target)
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

          
