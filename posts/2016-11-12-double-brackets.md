---
layout: post
title: Double brackets
---

```
writeStuff :: [Char] -> IO ()
writeStuff filename =
  bracket openFiles closeFiles doStuff
  where
    openFiles :: IO (Handle, Handle)
    openFiles = do
      f0 <- openFile (filename ++ "_first")  WriteMode
      f1 <- openFile (filename ++ "_second") WriteMode
      return (f0, f1)

    closeFiles :: (Handle, Handle) -> IO ()
    closeFiles (f0, f1) = do
      hClose f0
      hClose f1

    doStuff :: (Handle, Handle) -> IO ()
    doStuff = undefined -- etc
```

Bad! don't duplicate your brackets! Notice in `openFiles`:

```
openFiles :: IO (Handle, Handle)
openFiles = do
  f0 <- openFile (filename ++ "_first")  WriteMode -- what happens if this breaks?
  f1 <- openFile (filename ++ "_second") WriteMode
  return (f0, f1)
```

See that question? Well, what happens is that you be able to properly close both
of those files. The correct way of going about this is to nest your brackets:

```
writeStuff :: [Char] -> IO ()
writeStuff filename =
  bracket openOne hClose (\hdl1 ->
    bracket openTwo hClose (\hdl2 ->
      dooStuff hdl1 hdl2 ) )
  where
    openOne :: IO Handle
    openOne = openFile (filename ++ "_first")  WriteMode

    openTwo :: IO Handle
    openTwo = openFile (filename ++ "_second") WriteMode

    doStuff :: Handle -> Handle -> IO ()
    doStuff = undefined -- etc
```




