# Relude Best Practices

When using relude, follow these idiomatic substitutions (from [relude's HLint rules](https://github.com/kowainik/relude/blob/main/.hlint.yaml)):

## Basic Idioms

- `pass` instead of `pure ()` or `return ()`
- `one` instead of `(: [])`, `(:| [])`, or singleton functions
- `<<$>>` for double fmap: `f <<$>> x` instead of `fmap (fmap f) x`
- `??` (flap) operator: `ff ?? x` instead of `fmap ($ x) ff`

## File I/O

- `readFileText`, `writeFileText`, `appendFileText` for Text
- `readFileLText`, `writeFileLText`, `appendFileLText` for lazy Text
- `readFileBS`, `writeFileBS`, `appendFileBS` for ByteString
- `readFileLBS`, `writeFileLBS`, `appendFileLBS` for lazy ByteString

## Console Output

- `putText`, `putTextLn` for Text
- `putLText`, `putLTextLn` for lazy Text
- `putBS`, `putBSLn` for ByteString
- `putLBS`, `putLBSLn` for lazy ByteString

## Maybe/Either Helpers

- `whenJust m f` instead of `maybe pass f m`
- `whenJustM m f` for monadic versions
- `whenNothing_ m x` / `whenNothingM_ m x` for Nothing cases
- `whenLeft_ m f`, `whenRight_ m f` for Either
- `whenLeftM_ m f`, `whenRightM_ m f` for monadic Either
- `leftToMaybe`, `rightToMaybe` for conversions
- `maybeToRight l`, `maybeToLeft r` for conversions

## List Operations

- `ordNub` instead of `nub` (O(n log n) vs O(n²))
- `sortNub` instead of `Data.Set.toList . Data.Set.fromList`
- `sortWith f` instead of `sortBy (comparing f)`
- `viaNonEmpty f x` instead of `fmap f (nonEmpty x)`
- `asumMap f xs` instead of `asum (map f xs)`
- `toList` instead of `foldr (:) []`

## Monadic Operations

- `andM s` instead of `and <$> sequence s`
- `orM s` instead of `or <$> sequence s`
- `allM f s` instead of `and <$> mapM f s`
- `anyM f s` instead of `or <$> mapM f s`
- `guardM f` instead of `f >>= guard`
- `infinitely` instead of `forever`
- `unlessM (not <$> x)` → use `whenM x`
- `whenM (not <$> x)` → use `unlessM x`

## State/Reader Operations

- `usingReaderT` instead of `flip runReaderT`
- `usingStateT` instead of `flip runStateT`
- `evaluatingStateT s st` instead of `fst <$> usingStateT s st`
- `executingStateT s st` instead of `snd <$> usingStateT s st`

## Transformer Lifting

- `hoistMaybe m` instead of `MaybeT (pure m)`
- `hoistEither m` instead of `ExceptT (pure m)`

## List Pattern Matching

- `whenNotNull m f` for `case m of [] -> pass; (x:xs) -> f (x :| xs)`
- `whenNotNullM m f` for monadic version

## Text/ByteString Conversions

- `toText`, `toString`, `toLText` instead of pack/unpack
- `encodeUtf8`, `decodeUtf8` for UTF-8 encoding
- `fromStrict`, `toStrict` for lazy/strict conversions
