{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}

{-# OPTIONS_GHC -Wall #-}

module Composable.Path
    ( Path
    -- * Construction
    , root
    , dir
    , file
    -- * Operations
    , stripProperPrefix
    , replaceProperPrefix
    , isProperPrefixOf
    , parent
    , filename
    , dirname
    , (</>)

    -- * Exceptions
    , InvalidPrefix(..)
    , InvalidParent(..)
    , InvalidFilename(..)

    -- * Re-exports
    , Category(..)
    , (>>>)
    , (<<<)
    ) where

import Control.Category (Category(..), (<<<), (>>>))
import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow(..))
import Data.String.Interpolate (__i)
import Prelude hiding ((.), id)
import System.OsPath (OsPath)

import qualified Control.Exception as Exception
import qualified Control.Monad.Catch as Catch
import qualified System.OsPath as OsPath

data Node = Root | Dir | File

data Path (a :: Node) (b :: Node) where
    PathId :: Path a a
    PathRoot :: Path 'Root 'Dir
    PathDir :: Path a 'Dir -> OsPath -> Path a 'Dir
    PathFile :: Path a 'Dir -> OsPath -> Path a 'File

instance Category Path where
    id = PathId

    PathDir parent_ component . path =
        PathDir (parent_ . path) component

    PathFile parent_ component . path =
        PathFile (parent_ . path) component

    PathId . path = path

    path . PathId = path

instance Show (Path a b) where
    show path = show (unsafeToFilePath path)

(</>) :: Path a b -> Path b c -> Path a c
(</>) = (>>>)

root :: Path 'Root 'Dir
root = PathRoot

dir :: OsPath -> Path Dir Dir
dir component = PathDir PathId component

file :: OsPath -> Path Dir File
file component = PathFile PathId component

toOsPath :: MonadThrow m => Path a b -> m OsPath
toOsPath PathId = OsPath.encodeUtf ""
toOsPath PathRoot = OsPath.encodeUtf "/"
toOsPath (PathDir parent_ component) = do
    newParent <- toOsPath parent_
    pure (newParent OsPath.</> component)
toOsPath (PathFile parent_ component) = do
    newParent <- toOsPath parent_
    pure (newParent OsPath.</> component)

toFilePath :: MonadThrow m => Path a b -> m FilePath
toFilePath path = do
    ospath <- toOsPath path
    OsPath.decodeUtf ospath

unsafeToFilePath :: Path a b -> FilePath
unsafeToFilePath path = case toFilePath path of
    Left exception -> Exception.throw exception
    Right filepath -> filepath

-- toFilePath (pathL </> pathR) = toFilePath pathL </> toFilePath pathR
-- toFilePath id = theIdentityFilePath
--
-- theIdentityFilePath </> (path :: FilePath) = path
-- theIdentityFilePath = ""

-- TODO: Use Path here
data InvalidPrefix = InvalidPrefix
    { prefix :: FilePath
    , pathToStrip :: FilePath
    } deriving stock (Show)

instance Exception InvalidPrefix where
    displayException InvalidPrefix{..} =
        [__i|
        stripProperPrefix: invalid prefix

        prefix       : #{prefix}
        path to strip: #{pathToStrip}
        |]

stripProperPrefix :: MonadThrow m => Path a b -> Path a c -> m (Path b c)
stripProperPrefix prefix pathToStrip = case prefix of
    PathId ->
        pure pathToStrip
    PathRoot ->
        case pathToStrip of
            PathId ->
                Catch.throwM invalidPrefix
            PathRoot -> do
                pure PathId
            PathDir parent_ component -> do
                newParent <- stripProperPrefix PathRoot parent_
                pure (PathDir newParent component)
            PathFile parent_ component -> do
                newParent <- stripProperPrefix PathRoot parent_
                pure (PathFile newParent component)
    PathDir parentL componentL ->
        case pathToStrip of
            PathDir parentR componentR
                | componentL == componentR -> do
                    _ <- stripProperPrefix parentL parentR
                    return PathId
            _ -> do
                Catch.throwM invalidPrefix
    PathFile parentL componentL ->
        case pathToStrip of
            PathFile parentR componentR
                | componentL == componentR -> do
                    _ <- stripProperPrefix parentL parentR
                    return PathId
            _ -> do
                Catch.throwM invalidPrefix
  where
    invalidPrefix = InvalidPrefix
        { prefix = unsafeToFilePath prefix
        , pathToStrip = unsafeToFilePath pathToStrip
        }

isProperPrefixOf :: Path a b -> Path a c -> Bool
isProperPrefixOf prefix pathToStrip =
    case stripProperPrefix prefix pathToStrip of
        Nothing -> False
        Just _  -> True

replaceProperPrefix
    :: MonadThrow m => Path a b -> Path a b -> Path a c -> m (Path a c)
replaceProperPrefix oldPrefix newPrefix pathToStrip = do
    suffix <- stripProperPrefix oldPrefix pathToStrip
    pure (newPrefix </> suffix)

data InvalidParent = InvalidParent
    { path :: FilePath
    } deriving (Show)

instance Exception InvalidParent where
    displayException InvalidParent{..} =
        [__i|
        parent: invalid parent

        path: #{path}
        |]

parent :: MonadThrow m => Path a b -> m (Path a Dir)
parent path = case path of
    PathId -> Catch.throwM invalidParent
    PathRoot -> Catch.throwM invalidParent
    PathDir parent_ _ -> pure parent_
    PathFile parent_ _ -> pure parent_
  where
    invalidParent = InvalidParent{ path = unsafeToFilePath path }

data InvalidFilename = InvalidFilename
    { path :: FilePath
    } deriving (Show)

instance Exception InvalidFilename where
    displayException InvalidFilename{..} =
        [__i|
        filename: invalid filename

        path: #{path}
        |]

filename :: MonadThrow m => Path a File -> m (Path Dir File)
filename path = case path of
    PathId -> Catch.throwM invalidFilename
    PathFile _ component -> pure (file component)
  where
    invalidFilename = InvalidFilename{ path = unsafeToFilePath path }

data InvalidDirname = InvalidDirname
    { path :: FilePath
    } deriving (Show)

instance Exception InvalidDirname where
    displayException InvalidDirname{..} =
        [__i|
        filename: invalid filename

        path: #{path}
        |]

dirname :: MonadThrow m => Path a Dir -> m (Path Dir Dir)
dirname path = case path of
    PathId -> Catch.throwM invalidDirname
    PathRoot -> Catch.throwM invalidDirname
    PathDir _ component -> pure (dir component)
  where
    invalidDirname = InvalidDirname{ path = unsafeToFilePath path }
