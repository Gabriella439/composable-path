# `composable-path`

This package provides a more type-safe `Path` type that reduces the likelihood
of creating ill-formed `FilePath`s.  This `Path` type takes two type parameters
that indicate what type of path it represents:

```haskell
data Node = Root | Dir | File

data Path (a :: Node) (b :: Node) where …

absolutePathToFile :: Path 'Root 'File
absolutePathToDir  :: Path 'Root 'Dir
relativePathToFile :: Path 'Dir  'File
relativePathToDir  :: Path 'Dir  'Dir
```

In other words, a `Path` specifies what what type of `Node` it begins and ends
on.  An absolute path is one that begins on a `Root` node and a relative path is
one that begins on a `Dir` node.  A directory is a path that ends on a `Dir`
node and a file is a path that ends on a `File` node.

There are two ways you can create a `Path`:

- You can create a `Path` component-by-component

  The `Path` type provides the following utilities:

  ```haskell
  root :: Path Root Dir

  dir :: String -> Path Dir Dir

  file :: String -> Path Dir File

  (</>) :: Path a b -> Path b c -> Path b c
  ```

  … so that you can write something like this:

  ```haskell
  root </> dir "foo" </> file "bar" :: Path Root File
  ```

  … and the result is still well-formed because the compiler checks that the
  `Node` types match whenever we combine two `Path`s.  For example, you can't
  create an ill-formed `Path` like this one:

  ```haskell
  file "foo" </> dir "bar"
  ```

  … because then you get a type error:

  ```
    • Couldn't match type ‘'Dir’ with ‘'File’
      Expected: Path 'File 'Dir
        Actual: Path 'Dir 'Dir
    • In the second argument of ‘(</>)’, namely ‘dir "bar"’
      In the expression: file "foo" </> dir "bar"
  ```

- You can parse a `Path` into the desired type

  … and the string you parse will be checked against that type:

  ```haskell
  >>> parse "/foo/bar/" :: IO (Path 'Root 'Dir)
  root </> dir "foo" </> dir "bar"
  ```

  This package also provides support for parsing more than one type of `Path`
  (e.g. parsing any type of absolute path or parsing any type of file) while
  still preserving the safety guarantees.

  ```haskell
  >>> parse "/foo/bar/" :: IO (APathFrom 'Root)
  ADir (root </> dir "foo" </> dir "bar")
  >>> parse "foo/bar/" :: IO (APathFrom 'Root)
  *** Exception: ParseFailure {reason = "Absolute path does not begin with a leading /"}
  >>> parse "foo/bar/" :: IO (APathTo 'Dir)
  Relative (dir "foo" </> dir "bar")
  >>> parse "foo/bar" :: IO (APathTo 'Dir)
  *** Exception: ParseFailure {reason = "Directory does not end with trailing /"}
  ```

If you're familiar with the
[`path` package](https://hackage.haskell.org/package/path), then this package
is similar in scope.  The main difference between the two packages is how they
use the two type parameters for the `Path` type.  The approach used in this
package supports a `Category` instance for `Path`, which is nice on its own, but
also means that operations from this package work in (slightly) more cases.

For example, the `path` package only supports stripping a "proper" prefix from a
`Path` and will fail if the prefix exactly matches the path:

```haskell
>>> foo <- parseAbsDir "/foo/"
>>> stripProperPrefix foo foo
*** Exception: NotAProperPrefix "/foo/" "/foo/"
```

In contrast, this package provides a `stripPrefix` utility that works for all
prefixes, even exact prefixes:

```haskell
>>> foo <- parse @(Path Root Dir) "/foo/"
>>> stripPrefix foo foo
id
```

… because this package is capable of modeling an "empty" `Path` (`id`) with
zero path components.

This package also improves upon the `path` package in a few smaller ways:

- The `Path` type has a valid `Show` instance

  Unlike the `path` package, this package's `Path` type provides a `Show`
  instance that generates valid Haskell code for creating a `Path`.

- Support for parsing `Path`s with unknown anchors

  The `path` package provides a `SomeBase` type for parsing paths where you
  don't know ahead of time if the `Path` is a file or directory.  This package
  expands upon that idiom by also providing support for parsing `Path`s where
  you don't know in advance if the path is relative or absolute.

- Separate exception types

  Every function that can fail with an exception will only fail with an
  exception relevant to that operation.  In contrast, the `path` package
  combines all of the exception types into a single type, which means that you
  have to handle all of them (even the ones that are unrelated to the operation
  at hand).

- The internal representation is structured

  In other words the `Path` type is not just a `newtype` around a `FilePath`.
  This structured representation helps improve the consistency of various
  operations on `FilePath`s and makes it easier to reason about their behavior.
