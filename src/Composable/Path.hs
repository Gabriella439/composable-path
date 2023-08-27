{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}

{-| This module provides a `Path` type that is well-typed, meaning that you
    can tell from the type of the `Path` whether it is a file or directory
    and also whether it is a relative path or absolute path.

    Additionally, this `Path` type is composable, meaning that you can combine
    `Path`s using the `Category` instance for `Path`.
-}

module Composable.Path
    (
    -- * Types
      Path
    , Node(..)

    -- * Construction
    , root
    , dir
    , file
    , (</>)

    -- * Splitting
    , split
    , parent
    , basename

    -- * Prefixes
    , stripPrefix
    , isPrefixOf

    -- * Extensions
    , splitExtensions
    , extensions
    , dropExtensions

    -- * Exceptions
    , EmptyPath(..)
    , InvalidPrefix(..)

    -- * Re-exports
    , Category(..)
    , (>>>)
    , (<<<)
    ) where

import Control.Applicative (Alternative(..))
import Control.Category (Category(..), (<<<), (>>>))
import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow(..))
import Data.Maybe (isJust)
import Data.String.Interpolate (__i)
import Prelude hiding ((.), id)

import qualified Control.Monad.Catch as Catch
import qualified Data.List as List
import qualified System.FilePath as FilePath

{-| A well-typed path whose type parameters indicate what type of path it is:

@
'Path' 'Root' 'Dir'   -- The type of an absolute path to a directory
'Path' 'Root' 'File'  -- The type of an absolute path to a file
'Path' 'Dir'  'Dir'   -- The type of a relative path to a directory
'Path' 'Dir'  'File'  -- The type of a relative path to a file
@

    You can build a `Path` using the following primitive operations:

    - `id` creates an empty `Path` (with 0 path components)
    - `root` create an empty `Path` (with 0 path components) anchored at the
      root of the filesystem
    - `dir` creates a `Path` with one path component representing a directory
    - `file` creates a `Path` with one path component representing a file

    … and you can combine those primitive `Path`s using (`</>`) to create
    longer `Path`s, like this:

    >>> root </> dir "foo" </> file "bar"
    root </> dir "foo" </> file "bar"
    >>> :type it
    it :: Path 'Root 'File
    >>> toFilePath it
    "/foo/bar"

    >>> dir "foo" </> dir "bar" </> dir "baz"
    dir "foo" </> dir "bar" </> dir "baz"
    >>> :type it
    it :: Path 'Dir 'Dir
    >>> toFilePath it
    "foo/bar/baz"

    As the above examples show, you can use `toFilePath` to convert a `Path`
    back into a `FilePath`.
-}
data Path (a :: Node) (b :: Node) where
    PathId :: Path a a
    PathRoot :: Path 'Root 'Dir
    PathDir :: Path a 'Dir -> FilePath -> Path a 'Dir
    PathFile :: Path a 'Dir -> FilePath -> Path a 'File

{-| All path components have two type parameters, both of which are `Node`s

    These `Node` type parameters represent where a path component \"begins\"
    and \"ends\".
-}
data Node
    = Root
    -- ^ The first type parameter for an absolute path
    | Dir
    -- ^ The first type parameter for a relative path and the second type
    --   parameter for a directory
    | File
    -- ^ The second type parameter for a file

instance Category Path where
    id = PathId

    PathDir parent_ component . path =
        PathDir (parent_ . path) component

    PathFile parent_ component . path =
        PathFile (parent_ . path) component

    PathId . path = path

    path . PathId = path

instance Show (Path a b) where
    showsPrec _ PathId = showString "id"
    showsPrec _ PathRoot = showString "root"
    showsPrec precedence (PathDir PathId component) =
        showParen (precedence > 10) (showString "dir " . showsPrec 10 component)
    showsPrec precedence (PathDir parent_ component) =
        showParen (precedence > 5)
            ( showsPrec 5 parent_
            . showString " </> dir "
            . showsPrec 10 component
            )
    showsPrec precedence (PathFile PathId component) =
        showParen (precedence > 10) (showString "file " . showsPrec 10 component)
    showsPrec precedence (PathFile parent_ component) =
        showParen (precedence > 5)
            ( showsPrec 5 parent_
            . showString " </> file "
            . showsPrec 10 component
            )

-- | Combine two paths
(</>) :: Path a b -> Path b c -> Path a c
(</>) = (>>>)

infixr 5 </>

-- | The root of the filesystem
root :: Path 'Root 'Dir
root = PathRoot

-- | A path component that is a directory
dir :: FilePath -> Path 'Dir 'Dir
dir component = PathDir PathId component

-- | A path component that is a file
file :: FilePath -> Path 'Dir 'File
file component = PathFile PathId component

{-| Convert a `Path` to a `FilePath`

>>> toFilePath (file "foo")
"foo"
>>> toFilePath (dir "foo")
"foo"
>>> toFilePath root
"/"
>>> toFilePath id
""

>>> toFilePath (root </> dir "foo" </> file "bar")
"/foo/bar"
>>> toFilePath (dir "foo" </> dir "bar" </> dir "baz")
"foo/bar/baz"
-}
toFilePath :: Path a b -> FilePath
toFilePath PathId = ""
toFilePath PathRoot = "/"
toFilePath (PathDir parent_ component) =
    toFilePath parent_ FilePath.</> component
toFilePath (PathFile parent_ component) =
    toFilePath parent_ FilePath.</> component

{-| This exception is thrown by operations that only supports non-empty
    paths when given an empty path
-}
data EmptyPath = EmptyPath
    { path :: FilePath
    } deriving (Show)

instance Exception EmptyPath where
    displayException EmptyPath{..} =
        [__i|
        Empty path

        path: #{path}
        |]

{-| `split` splits a non-empty `Path` into its `parent` path and its
    `basename`s

    The first part of the result is everything except the last path component
    and the second part of the result is the last path component.

    This throws an `EmptyPath` exception if the path has no path components.

>>> split (file "foo")
(id,file "foo")
>>> split (dir "foo")
(id,dir "foo")
>>> split id
*** Exception: EmptyPath {path = ""}
>>> split root
*** Exception: EmptyPath {path = "/"}

>>> split (root </> dir "foo" </> file "bar")
(root </> dir "foo",file "bar")
>>> split (dir "foo" </> dir "bar" </> dir "baz")
(dir "foo" </> dir "bar",dir "baz")
-}
split :: MonadThrow m => Path a c -> m (Path a 'Dir, Path 'Dir c)
split path = case path of
    PathId -> Catch.throwM emptyPath
    PathRoot -> Catch.throwM emptyPath
    PathDir parent_ component -> pure (parent_, dir component)
    PathFile parent_ component -> pure (parent_, file component)
  where
    emptyPath = EmptyPath{ path = toFilePath path }

{-| `parent` drops the last path component of a `Path`

@
'parent' path = 'fmap' 'fst' ('split' path)
@

    This throws an `EmptyPath` exception if the path has no path components.

>>> parent (file "foo")
id
>>> parent (dir "foo")
id
>>> parent id
*** Exception: EmptyPath {path = ""}
>>> parent root
*** Exception: EmptyPath {path = "/"}

>>> parent (root </> dir "foo" </> file "bar")
root </> dir "foo"
>>> parent (dir "foo" </> dir "bar" </> dir "baz") 
dir "foo" </> dir "bar"
-}
parent :: MonadThrow m => Path a c -> m (Path a 'Dir)
parent path = fmap fst (split path)

{-| `basename` returns the last path component of a `Path`

@
'basename' path = 'fmap' 'snd' ('split' path)
@

    This throws an `EmptyPath` exception if the path has no path components.

>>> basename (file "foo")
file "foo"
>>> basename (dir "foo")
dir "foo"
>>> basename id
*** Exception: EmptyPath {path = ""}
>>> basename root
*** Exception: EmptyPath {path = "/"}

>>> basename (root </> dir "foo" </> file "bar")
file "bar"
>>> basename (dir "foo" </> dir "bar" </> dir "baz")
dir "baz"
-}
basename :: MonadThrow m => Path a b -> m (Path 'Dir b)
basename path = fmap snd (split path)

{-| This exception is thrown when a `Path` is not a valid prefix of another
    `Path`
-}
data InvalidPrefix = InvalidPrefix
    { prefix :: FilePath
    , pathToStrip :: FilePath
    } deriving stock (Show)

instance Exception InvalidPrefix where
    displayException InvalidPrefix{..} =
        [__i|
        Invalid prefix

        prefix       : #{prefix}
        path to strip: #{pathToStrip}
        |]

{-| Strip a `Path` prefix from another `Path`

@
`stripPrefix` `id` = `pure` `id`
`stripPrefix` (f `.` g) = liftA2 (.) (`stripPrefix` f) (`stripPrefix` g)
@

>>> stripPrefix root root
id
>>> stripPrefix (dir "foo") (dir "foo")
id
>>> stripPrefix (file "foo") (file "foo")
id

>>> stripPrefix root (root </> dir "foo")
dir "foo"
>>> stripPrefix (root </> dir "foo") (root </> dir "foo" </> file "bar")
file "bar"
>>> stripPrefix (dir "foo") (dir "foo" </> dir "bar" </> dir "baz")
dir "bar" </> dir "baz"

>>> stripPrefix (dir "foo") (file "foo")
*** Exception: InvalidPrefix {prefix = "foo", pathToStrip = "foo"}

ghci> stripPrefix (dir "foo") (dir "bar")
*** Exception: InvalidPrefix {prefix = "foo", pathToStrip = "bar"}
ghci> stripPrefix (dir "foo" </> dir "bar") (dir "foo" </> dir "baz")
*** Exception: InvalidPrefix {prefix = "foo/bar", pathToStrip = "foo/baz"}
-}
stripPrefix :: MonadThrow m => Path a b -> Path a c -> m (Path b c)
stripPrefix prefix pathToStrip = case prefix of
    PathId ->
        pure pathToStrip
    PathRoot ->
        case pathToStrip of
            PathId ->
                Catch.throwM invalidPrefix
            PathRoot -> do
                pure PathId
            PathDir parent_ component -> do
                newParent <- stripPrefix prefix parent_
                pure (PathDir newParent component)
            PathFile parent_ component -> do
                newParent <- stripPrefix prefix parent_
                pure (PathFile newParent component)
    PathDir parentL componentL ->
        case alternative0 <|> alternative1 of
            Just suffix -> pure suffix
            Nothing -> Catch.throwM invalidPrefix
      where
        alternative0 =
            case pathToStrip of
                PathDir parentR componentR
                    | componentL == componentR -> do
                        _ <- stripPrefix parentL parentR
                        pure PathId
                _ ->
                    empty

        alternative1 = do
            case pathToStrip of
                PathId -> do
                    empty
                PathRoot -> do
                    empty
                PathDir parentR componentR -> do
                    newParent <- stripPrefix prefix parentR
                    pure (PathDir newParent componentR)
                PathFile parentR componentR -> do
                    newParent <- stripPrefix prefix parentR
                    pure (PathFile newParent componentR)
    PathFile parentL componentL ->
        case alternative0 <|> alternative1 of
            Just suffix -> pure suffix
            Nothing -> Catch.throwM invalidPrefix
      where
        alternative0 =
            case pathToStrip of
                PathFile parentR componentR
                    | componentL == componentR -> do
                        _ <- stripPrefix parentL parentR
                        pure PathId
                _ ->
                    empty

        alternative1 = do
            case pathToStrip of
                PathId -> do
                    empty
                PathRoot -> do
                    empty
                PathDir parentR componentR -> do
                    newParent <- stripPrefix prefix parentR
                    pure (PathDir newParent componentR)
                PathFile parentR componentR -> do
                    newParent <- stripPrefix prefix parentR
                    pure (PathFile newParent componentR)
  where
    invalidPrefix = InvalidPrefix
        { prefix = toFilePath prefix
        , pathToStrip = toFilePath pathToStrip
        }

{-| Check to see if one `Path` is a prefix of another `Path`

@
'isPrefixOf' prefix path = 'isJust' ('stripPrefix' prefix path)
@
-}
isPrefixOf :: Path a b -> Path a c -> Bool
isPrefixOf prefix path = isJust (stripPrefix prefix path)

{-| Separate out the extensions from a `Path`, returning the original
    path minus extensions and then list of extensions

>>> splitExtensions (file "foo.tar.gz")
(file "foo",["tar","gz"])
>>> splitExtensions id
(id,[])
>>> splitExtensions (root </> dir "foo" </> file "bar.tar.gz")
(root </> dir "foo" </> file "bar",["tar","gz"])
-}
splitExtensions :: Path a 'File -> (Path a 'File, [String])
splitExtensions PathId = (PathId, [])
splitExtensions (PathFile parent_ component) =
    (PathFile parent_ prefix0, extensions0)
  where
    (prefix0, suffix0) = List.break (== '.') component

    extensions0 = case suffix0 of
        ""         -> [ ]
        _ : suffix -> loop suffix

    loop suffix1 = prefix : extensions_
      where
        (prefix, suffix2) = List.break (== '.') suffix1

        extensions_ = case suffix2 of
            ""          -> []
            _ : suffix3 -> loop suffix3

{-| Return the list of extensions for a `Path`

@
'extensions' path = 'snd' ('splitExtensions' path)
@
-}
extensions :: Path a 'File -> [String]
extensions path = snd (splitExtensions path)

{-| Strip the extensions from a `Path`

@
'dropExtensions' path = 'fst' ('dropExtensions' path)
@
-}
dropExtensions :: Path a 'File -> Path a 'File
dropExtensions path = fst (splitExtensions path)
