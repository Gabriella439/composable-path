{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

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
    , APathFrom(..)
    , APathTo(..)
    , APath(..)

    -- * Construction
    , root
    , dir
    , file
    , (</>)
    , ParsePath(..)

    -- * Elimination
    , toFilePath

    -- * Splitting
    , split
    , dirname
    , basename

    -- * Prefixes and Suffixes
    , stripPrefix
    , isPrefixOf
    , replacePrefix
    , stripSuffix
    , isSuffixOf
    , replaceSuffix

    -- * Extensions
    , splitExtension
    , splitExtensions
    , extension
    , extensions
    , dropExtension
    , dropExtensions
    , addExtension
    , addExtensions
    , hasExtension

    -- * Exceptions
    , EmptyPath(..)
    , InvalidPrefix(..)
    , InvalidSuffix(..)

    -- * Re-exports
    , Category(..)
    , (>>>)
    , (<<<)
    ) where

import Control.Applicative (Alternative(..))
import Control.Category (Category(..), (<<<), (>>>))
import Control.Exception (Exception(..))
import Control.Monad.Catch (MonadThrow(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isJust)
import Data.String.Interpolate (__i)
import Prelude hiding ((.), id)

import qualified Control.Monad.Catch as Catch
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified System.FilePath as FilePath

{- $setup

>>> :set -XDataKinds
-}

{-| A well-typed path whose type parameters indicate what type of path it is:

@
'Path' 'Root' 'Dir'   -- The type of an absolute path to a directory
'Path' 'Root' 'File'  -- The type of an absolute path to a file
'Path' 'Dir'  'Dir'   -- The type of a relative path to a directory
'Path' 'Dir'  'File'  -- The type of a relative path to a file
@

    You can build a `Path` using `parse`:

>>> parse @(Path Root File) "/foo/bar"
root </> dir "foo" </> file "bar"

    … or by using the following primitive operations:

    - `id` creates an empty `Path` (with zero path components)
    - `root` create an empty `Path` (with zero path components) anchored at the
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
"foo/bar/baz/"

    As the above examples show, you can use `toFilePath` to convert a `Path`
    back into a `FilePath`.
-}
data Path (a :: Node) (b :: Node) where
    PathId :: Path a a
    PathRoot :: Path 'Root 'Dir
    PathDir :: Path a 'Dir -> FilePath -> Path a 'Dir
    PathFile :: Path a 'Dir -> FilePath -> Path a 'File

instance Eq (Path a b) where
    x == y = toFilePath x == toFilePath y

instance Ord (Path a b) where
    compare x y = compare (toFilePath x) (toFilePath y)

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

    PathDir parent component . path =
        PathDir (parent . path) component

    PathFile parent component . path =
        PathFile (parent . path) component

    PathId . path = path

    path . PathId = path

instance Show (Path a b) where
    showsPrec _ PathId = showString "id"
    showsPrec _ PathRoot = showString "root"
    showsPrec precedence (PathDir PathId component) =
        showParen (precedence > 10) (showString "dir " . showsPrec 10 component)
    showsPrec precedence (PathDir parent component) =
        showParen (precedence > 5)
            ( showsPrec 5 parent
            . showString " </> dir "
            . showsPrec 10 component
            )
    showsPrec precedence (PathFile PathId component) =
        showParen (precedence > 10) (showString "file " . showsPrec 10 component)
    showsPrec precedence (PathFile parent component) =
        showParen (precedence > 5)
            ( showsPrec 5 parent
            . showString " </> file "
            . showsPrec 10 component
            )

-- | A path which can be either a directory or a file
data APathFrom a
    = ADir (Path a 'Dir)
    -- ^ A directory
    | AFile (Path a 'File)
    -- ^ A file

-- | A path which can be either an absolute path or relative path
data APathTo b
    = Absolute (Path 'Root b)
    -- ^ An absolute path
    | Relative (Path 'Dir b)
    -- ^ A relative path

-- | A directory or file which can be either an absolute path or relative path
data APath
    = AbsoluteDir (Path 'Root 'Dir)
    -- ^ An absolute path to a directory
    | AbsoluteFile (Path 'Root 'File)
    -- ^ An absolute path to a file
    | RelativeDir (Path 'Dir 'Dir)
    -- ^ A relative path to a directory
    | RelativeFile (Path 'Dir 'File)
    -- ^ A relative path to a file

-- | The root of the filesystem
root :: Path 'Root 'Dir
root = PathRoot

-- | A path component that is a directory
dir :: FilePath -> Path 'Dir 'Dir
dir component = PathDir PathId component

-- | A path component that is a file
file :: FilePath -> Path 'Dir 'File
file component = PathFile PathId component

-- | Combine two paths
(</>) :: Path a b -> Path b c -> Path a c
(</>) = (>>>)

infixl 5 </>

-- | This exception is thrown when failing to parse a `Path`
data ParseFailure = ParseFailure
    { reason :: String
    } deriving stock (Show)

instance Exception ParseFailure where
    displayException ParseFailure{..} =
        [__i|
        Parse failure

        reason: #{reason}
        |]

{-| Class for parsing `Path`s from `FilePath`s

    The parsers are strict about what `FilePath`s they accept because they
    enforce the following conventions:

    - Absolute paths must begin with a trailing separator
    - Relative paths must not begin with a trailing separator
    - Directories must end with a trailing separator
    - Files must not end with a trailing separator

    This means that any given `FilePath` will only parse as one of the
    following types:

    - `Path Root Dir` - an absolute path to a directory
    - `Path Root File` - an absolute path to a file
    - `Path Dir Dir` - a relative path to a directory
    - `Path Dir File` - a relative path to a file

    The following examples spell out all of the possible cases:

>>> parse @(Path 'Root 'Dir) "foo"
*** Exception: ParseFailure {reason = "Absolute path does not begin with a leading /"}
>>> parse @(Path 'Root 'File) "foo"
*** Exception: ParseFailure {reason = "Absolute path does not begin with a leading /"}
>>> parse @(Path 'Dir 'Dir) "foo"
*** Exception: ParseFailure {reason = "Directory does not end with trailing /"}
>>> parse @(Path 'Dir 'File) "foo"
file "foo"

>>> parse @(Path 'Root 'Dir) "/foo"
*** Exception: ParseFailure {reason = "Directory does not end with trailing /"}
>>> parse @(Path 'Root 'File) "/foo"
root </> file "foo"
>>> parse @(Path 'Dir 'Dir) "/foo"
*** Exception: ParseFailure {reason = "Relative path begins with a leading /"}
>>> parse @(Path 'Dir 'File) "/foo"
*** Exception: ParseFailure {reason = "Relative path begins with a leading /"}

>>> parse @(Path 'Root 'Dir) "foo/"
*** Exception: ParseFailure {reason = "Absolute path does not begin with a leading /"}
>>> parse @(Path 'Root 'File) "foo/"
*** Exception: ParseFailure {reason = "Absolute path does not begin with a leading /"}
>>> parse @(Path 'Dir 'Dir) "foo/"
dir "foo"
>>> parse @(Path 'Dir 'File) "foo/"
*** Exception: ParseFailure {reason = "File ends with trailing /"}

>>> parse @(Path 'Root 'Dir) "/foo/"
root </> dir "foo"
>>> parse @(Path 'Root 'File) "/foo/"
*** Exception: ParseFailure {reason = "File ends with trailing /"}
>>> parse @(Path 'Dir 'Dir) "/foo/"
*** Exception: ParseFailure {reason = "Relative path begins with a leading /"}
>>> parse @(Path 'Dir 'File) "/foo/"
*** Exception: ParseFailure {reason = "Relative path begins with a leading /"}

>>> parse @(Path 'Root 'Dir) "/"
root
>>> parse @(Path 'Root 'File) "/"
*** Exception: ParseFailure {reason = "File ends with trailing /"}
>>> parse @(Path 'Dir 'Dir) "/"
*** Exception: ParseFailure {reason = "Relative path begins with a leading /"}
>>> parse @(Path 'Dir 'File) "/"
*** Exception: ParseFailure {reason = "Relative path begins with a leading /"}
-}
class ParsePath path where
    parse :: MonadThrow m => FilePath -> m path

instance ParsePath APath  where
    parse filepath = do
        let (absolute, nonEmpty) =
                case filepathComponents filepath of
                    "" :| component : components ->
                        (True, component :| components)
                    components ->
                        (False, components)

        let directories = NonEmpty.init nonEmpty

        let file_ = NonEmpty.last nonEmpty

        if absolute then
            if file_ == "" then do
                pure (AbsoluteDir (root </> foldr (</>) id (map dir directories)))
            else do
                pure (AbsoluteFile (root </> foldr (</>) (file file_) (map dir directories)))
        else
            if file_ == "" then do
                pure (RelativeDir (foldr (</>) id (map dir directories)))
            else do
                pure (RelativeFile (foldr (</>) (file file_) (fmap dir directories)))

instance ParsePath (APathFrom 'Root) where
    parse filepath = do
        aPath <- parse filepath

        case aPath of
            AbsoluteDir  path -> pure (ADir path)
            AbsoluteFile path -> pure (AFile path)
            _                   -> Catch.throwM noLeadingSeparator

instance ParsePath (APathFrom 'Dir) where
    parse filepath = do
        aPath <- parse filepath

        case aPath of
            RelativeDir  path -> pure (ADir path)
            RelativeFile path -> pure (AFile path)
            _                 -> Catch.throwM unexpectedLeadingSeparator

instance ParsePath (APathTo 'Dir) where
    parse filepath = do
        aPath <- parse filepath

        case aPath of
            AbsoluteDir path -> pure (Absolute path)
            RelativeDir path -> pure (Relative path)
            _                -> Catch.throwM noTrailingSeparator

instance ParsePath (APathTo 'File) where
    parse filepath = do
        aPath <- parse filepath

        case aPath of
            AbsoluteFile path -> pure (Absolute path)
            RelativeFile path -> pure (Relative path)
            _                 -> Catch.throwM unexpectedTrailingSeparator

instance ParsePath (Path 'Root 'Dir) where
    parse filepath = do
        aPath <- parse filepath

        case aPath of
            AbsoluteDir  path -> pure path
            AbsoluteFile _    -> Catch.throwM noTrailingSeparator
            RelativeDir  _    -> Catch.throwM noLeadingSeparator
            RelativeFile _    -> Catch.throwM noLeadingSeparator

instance ParsePath (Path 'Root 'File) where
    parse filepath = do
        aPath <- parse filepath

        case aPath of
            AbsoluteDir  _    -> Catch.throwM unexpectedTrailingSeparator
            AbsoluteFile path -> pure path
            RelativeDir  _    -> Catch.throwM noLeadingSeparator
            RelativeFile _    -> Catch.throwM noLeadingSeparator

instance ParsePath (Path 'Dir 'Dir) where
    parse filepath = do
        aPath <- parse filepath

        case aPath of
            AbsoluteDir  _    -> Catch.throwM unexpectedLeadingSeparator
            AbsoluteFile _    -> Catch.throwM unexpectedLeadingSeparator
            RelativeDir  path -> pure path
            RelativeFile _    -> Catch.throwM noTrailingSeparator

instance ParsePath (Path 'Dir 'File) where
    parse filepath = do
        aPath <- parse filepath

        case aPath of
            AbsoluteDir  _    -> Catch.throwM unexpectedLeadingSeparator
            AbsoluteFile _    -> Catch.throwM unexpectedLeadingSeparator
            RelativeDir   _    -> Catch.throwM unexpectedTrailingSeparator
            RelativeFile  path -> pure path

instance ParsePath (Path 'File 'File) where
    parse "" = pure PathId
    parse _  = Catch.throwM ParseFailure{ reason = "Path must be empty " }

instance ParsePath (Path 'Root 'Root) where
    parse "" = pure PathId
    parse _  = Catch.throwM ParseFailure{ reason = "Path must be empty " }

noTrailingSeparator :: ParseFailure
noTrailingSeparator = ParseFailure{..}
  where
    reason =
        "Directory does not end with trailing " <> [ FilePath.pathSeparator ]

unexpectedTrailingSeparator :: ParseFailure
unexpectedTrailingSeparator = ParseFailure{..}
  where
    reason = "File ends with trailing " <> [ FilePath.pathSeparator ]

noLeadingSeparator :: ParseFailure
noLeadingSeparator = ParseFailure{..}
  where
    reason = "Absolute path does not begin with a leading " <> [ FilePath.pathSeparator ]

unexpectedLeadingSeparator :: ParseFailure
unexpectedLeadingSeparator = ParseFailure{..}
  where
    reason = "Relative path begins with a leading " <> [ FilePath.pathSeparator ]

filepathComponents :: FilePath -> NonEmpty String
filepathComponents filepath =
    case suffix of
        [ ] -> pure filepath
        _ : uffix -> NonEmpty.cons prefix (filepathComponents uffix)
  where
    (prefix, suffix) = List.break FilePath.isPathSeparator filepath

{-| `toFilePath` converts a `Path` to a `FilePath`.

@
'fmap' 'toFilePath' ('parse' path) = 'pure' path  -- If parsing succeeds
@

    __Carefully note:__ The following law is __NOT__ always true:

@
'parse' ('toFilePath' path) = 'pure' path
@

    … if any of the path components are empty.  For example, this can happen if
    you create a `Path` using the `dir` or `file` utilities directly (e.g.
    @`dir` ""@).

>>> toFilePath (file "foo")
"foo"
>>> toFilePath (dir "foo")
"foo/"
>>> toFilePath root
"/"
>>> toFilePath id
""

>>> toFilePath (root </> dir "foo" </> file "bar")
"/foo/bar"
>>> toFilePath (dir "foo" </> dir "bar" </> dir "baz")
"foo/bar/baz/"
-}
toFilePath :: Path a b -> FilePath
toFilePath PathId =
    ""
toFilePath PathRoot =
    [ FilePath.pathSeparator ]
toFilePath (PathDir parent component) =
    toFilePath parent FilePath.</> (component <> [ FilePath.pathSeparator ])
toFilePath (PathFile parent component) =
    toFilePath parent FilePath.</> component

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

{-| `split` splits a non-empty `Path` into its `dirname` and `basename`.

    This is analogous to @"System.FilePath".`FilePath.splitFileName`@.

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
    PathDir parent component -> pure (parent, dir component)
    PathFile parent component -> pure (parent, file component)
  where
    emptyPath = EmptyPath{ path = toFilePath path }

{-| `dirname` drops the last path component of a `Path`

    This is analogous to @"System.FilePath".`FilePath.takeDirectory`@.

@
'dirname' path = 'fmap' 'fst' ('split' path)
@

    This throws an `EmptyPath` exception if the path has no path components.

>>> dirname (file "foo")
id
>>> dirname (dir "foo")
id
>>> dirname id
*** Exception: EmptyPath {path = ""}
>>> dirname root
*** Exception: EmptyPath {path = "/"}

>>> dirname (root </> dir "foo" </> file "bar")
root </> dir "foo"
>>> dirname (dir "foo" </> dir "bar" </> dir "baz") 
dir "foo" </> dir "bar"
-}
dirname :: MonadThrow m => Path a c -> m (Path a 'Dir)
dirname path = fmap fst (split path)

{-| `basename` returns the last path component of a `Path`

    __Carefully note:__ This does not behave the same as
    @"System.FilePath".`FilePath.takeBaseName`@, which returns the last path
    component minus any extensions.  This `basename` utility preserves all
    extensions and corresponds more closely to
    @"System.FilePath".`FilePath.takeFileName`@.  The reason for not following
    the same naming convention is because:

    - This does not always return a file (the last path component can be a
      directory)
    - This adheres more closely to Unix naming conventions (where "basename"
      typically refers to the entirety of the last path component)

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

{-| `stripPrefix` strips a `Path` prefix from another `Path`.

    This is analogous to @"System.FilePath".`FilePath.makeRelative`@.

    This throws `InvalidPrefix` if the given `Path` prefix is not a valid
    prefix of the `Path` to strip.

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
*** Exception: InvalidPrefix {prefix = "foo/", pathToStrip = "foo"}
>>> stripPrefix (dir "foo") (dir "bar")
*** Exception: InvalidPrefix {prefix = "foo/", pathToStrip = "bar/"}
>>> stripPrefix (dir "foo" </> dir "bar") (dir "foo" </> dir "baz")
*** Exception: InvalidPrefix {prefix = "foo/bar/", pathToStrip = "foo/baz/"}
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
            PathDir parent component -> do
                newParent <- stripPrefix prefix parent
                pure (PathDir newParent component)
            PathFile parent component -> do
                newParent <- stripPrefix prefix parent
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

{-| Replace a `Path` prefix with a new one

@
'replacePrefix' oldPrefix newPrefix path =
    'fmap' (newPrefix '</>') ('stripPrefix' oldPrefix path)
@

@
'replacePrefix' id prefix path = 'pure' (prefix '</>' path)

'replacePrefix' prefix id path = 'stripPrefix' prefix path
@

>>> replacePrefix (dir "foo") (dir "bar") (dir "foo" </> file "baz")
dir "bar" </> file "baz"
>>> replacePrefix (dir "foo") (dir "bar") (dir "fob" </> file "baz")
*** Exception: InvalidPrefix {prefix = "foo/", pathToStrip = "fob/baz"}

>>> replacePrefix (root </> dir "foo") (dir "..") (root </> dir "foo" </> dir "bar")
dir ".." </> dir "bar"
-}
replacePrefix
    :: MonadThrow m => Path a c -> Path b c -> Path a d -> m (Path b d)
replacePrefix oldPrefix newPrefix path =
    fmap (newPrefix </>) (stripPrefix oldPrefix path)

{-| This exception is thrown when a `Path` is not a valid suffix of another
    `Path`
-}
data InvalidSuffix = InvalidSuffix
    { suffix :: FilePath
    , pathToStrip :: FilePath
    } deriving stock (Show)

instance Exception InvalidSuffix where
    displayException InvalidSuffix{..} =
        [__i|
        Invalid suffix

        suffix       : #{suffix}
        path to strip: #{pathToStrip}
        |]

{-| `stripSuffix` strips a `Path` suffix from another `Path`.

    This throws `InvalidSuffix` if the given `Path` suffix is not a valid
    suffix of the `Path` to strip.

@
`stripSuffix` `id` = `pure` `id`

`stripSuffix` (f `.` g) = liftA2 (.) (`stripSuffix` f) (`stripSuffix` g)
@

>>> stripSuffix root root
id
>>> stripSuffix (dir "foo") (dir "foo")
id
>>> stripSuffix (file "foo") (file "foo")
id

>>> stripSuffix (dir "foo") (root </> dir "foo")
root
>>> stripSuffix (file "bar") (root </> dir "foo" </> file "bar")
root </> dir "foo"
>>> stripSuffix (dir "bar" </> dir "baz") (dir "foo" </> dir "bar" </> dir "baz")
dir "foo"

>>> stripSuffix (dir "bar" </> file "foo") (file "foo")
*** Exception: InvalidSuffix {suffix = "bar/", pathToStrip = ""}
>>> stripSuffix (dir "foo") (dir "bar")
*** Exception: InvalidSuffix {suffix = "foo/", pathToStrip = "bar/"}
>>> stripSuffix (dir "foo" </> dir "bar") (dir "foo" </> dir "baz")
*** Exception: InvalidSuffix {suffix = "foo/bar/", pathToStrip = "foo/baz/"}
-}
stripSuffix :: MonadThrow m => Path b c -> Path a c -> m (Path a b)
stripSuffix suffix pathToStrip =
    case suffix of
        PathId ->
            pure pathToStrip
        PathRoot ->
            case pathToStrip of
                PathRoot -> pure PathId
                _        -> Catch.throwM invalidSuffix
        PathDir parentL componentL ->
            case pathToStrip of
                PathDir parentR componentR
                    | componentL == componentR ->
                        stripSuffix parentL parentR
                _ ->
                    Catch.throwM invalidSuffix
        PathFile parentL componentL ->
            case pathToStrip of
                PathFile parentR componentR
                    | componentL == componentR ->
                        stripSuffix parentL parentR
                _ ->
                    Catch.throwM invalidSuffix
  where
    invalidSuffix = InvalidSuffix
        { suffix = toFilePath suffix
        , pathToStrip = toFilePath pathToStrip
        }

{-| Check to see if one `Path` is a suffix of another `Path`

@
'isSuffixOf' suffix path = 'isJust' ('stripSuffix' suffix path)
@
-}
isSuffixOf :: Path b c -> Path a c -> Bool
isSuffixOf suffix path = isJust (stripSuffix suffix path)

{-| Replace a `Path` suffix with a new one

@
'replaceSuffix' oldSuffix newSuffix path =
    'fmap' ('</>' newSuffix) ('stripSuffix' oldSuffix path)
@

@
'replaceSuffix' id suffix path = 'pure' (path '</>' suffix)

'replaceSuffix' suffix id path = 'stripSuffix' suffix path
@

>>> replaceSuffix (file "bar") (file "baz") (dir "foo" </> file "bar")
dir "foo" </> file "baz"
>>> replaceSuffix (file "bar") (dir "baz") (dir "foo" </> file "bat")
*** Exception: InvalidSuffix {suffix = "bar", pathToStrip = "foo/bat"}

>>> replaceSuffix (dir "foo" </> file "bar") (dir "baz") (root </> dir "foo" </> file "bar")
root </> dir "baz"
-}
replaceSuffix
    :: MonadThrow m => Path b c -> Path b d -> Path a c -> m (Path a d)
replaceSuffix oldSuffix newSuffix path =
    fmap (</> newSuffix) (stripSuffix oldSuffix path)

{-| Separate out the extension from a `Path`, returning the original
    path minus the extension and the extension (if present)

    This is analogous to @"System.FilePath".`FilePath.splitExtension`@.

>>> splitExtension (file "foo.tar.gz")
(file "foo.tar",Just "gz")
>>> splitExtension id
(id,Nothing)
>>> splitExtension (root </> dir "foo" </> file "bar.tar.gz")
(root </> dir "foo" </> file "bar.tar",Just "gz")
-}
splitExtension :: Path a 'File -> (Path a 'File, Maybe String)
splitExtension PathId = (PathId, Nothing)
splitExtension (PathFile parent component) =
    (PathFile parent prefix, extension_)
  where
    (prefix, suffix) = FilePath.splitExtension component

    extension_ = case suffix of
        [ ]       -> Nothing
        _ : uffix -> Just uffix

{-| Separate out the extensions from a `Path`, returning the original
    path minus extensions and list of extensions

    This is analogous to @"System.FilePath".`FilePath.splitExtensions`@.

>>> splitExtensions (file "foo.tar.gz")
(file "foo",["tar","gz"])
>>> splitExtensions id
(id,[])
>>> splitExtensions (root </> dir "foo" </> file "bar.tar.gz")
(root </> dir "foo" </> file "bar",["tar","gz"])
-}
splitExtensions :: Path a 'File -> (Path a 'File, [String])
splitExtensions PathId = (PathId, [ ])
splitExtensions (PathFile parent component0) =
    (PathFile parent newComponent0, diffs0 [])
  where
    ~(newComponent0, diffs0) = loop component0

    loop component = case suffix of
        [ ]       -> (prefix, id)
        _ : uffix -> (newComponent, diffs . (uffix :))
      where
        (prefix, suffix) = FilePath.splitExtension component

        ~(newComponent, diffs) = loop prefix

{-| Return the extension (if present) for a `Path`

    This is analogous to @"System.FilePath".`FilePath.takeExtension`@.

@
'extension' path = 'snd' ('splitExtension' path)
@
-}
extension :: Path a 'File -> Maybe String
extension path = snd (splitExtension path)

{-| Return the list of extensions for a `Path`

    This is analogous to @"System.FilePath".`FilePath.takeExtensions`@.

@
'extensions' path = 'snd' ('splitExtensions' path)
@
-}
extensions :: Path a 'File -> [String]
extensions path = snd (splitExtensions path)

{-| Strip the extension (if present) from a `Path`

    This is analogous to @"System.FilePath".`FilePath.dropExtension`@.

@
'dropExtension' path = 'fst' ('splitExtension' path)
@
-}
dropExtension :: Path a 'File -> Path a 'File
dropExtension path = fst (splitExtension path)

{-| Strip the extensions from a `Path`

    This is analogous to @"System.FilePath".`FilePath.dropExtensions`@.

@
'dropExtensions' path = 'fst' ('splitExtensions' path)
@
-}
dropExtensions :: Path a 'File -> Path a 'File
dropExtensions path = fst (splitExtensions path)

{-| Add an extension to the end of a `Path`

    This is analogous to @"System.FilePath".`FilePath.addExtension`@.

>>> addExtension (dir "foo" </> file "bar") "zip"
dir "foo" </> file "bar.zip"
>>> addExtension id "zip"
*** Exception: EmptyPath {path = ""}

-}
addExtension :: MonadThrow m => Path a 'File -> String -> m (Path a 'File)
addExtension PathId _ =
    Catch.throwM EmptyPath{ path = toFilePath PathId }
addExtension (PathFile parent component) extension_ = do
    pure (PathFile parent (FilePath.addExtension component extension_))

{-| Add extensions to the end of a `Path`

@
'uncurry' 'addExtensions' ('splitExtensions' path) = path

'addExtensions' path [] = []
@

>>> addExtensions (dir "foo" </> file "bar") [ "tar", "gz" ]
dir "foo" </> file "bar.tar.gz"
>>> addExtensions id [ "tar", "gz" ]
*** Exception: EmptyPath {path = ""}
>>> addExtensions id []
id

-}
addExtensions :: MonadThrow m => Path a 'File -> [String] -> m (Path a 'File)
addExtensions PathId [ ] =
    pure PathId
addExtensions PathId _ =
    Catch.throwM EmptyPath{ path = toFilePath PathId }
addExtensions (PathFile parent component) extensions_ = do
    let newComponent = foldl FilePath.addExtension component extensions_
    pure (PathFile parent newComponent)

{-| Check to see if a file has any extensions

    This is analogous to @"System.FilePath".`FilePath.hasExtension`@.

@
'hasExtension' path = 'not' ('null' ('extensions' path))
@
-}
hasExtension :: Path a 'File -> Bool
hasExtension path = not (null (extensions path))
