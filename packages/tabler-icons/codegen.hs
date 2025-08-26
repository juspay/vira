#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

-- \|
-- Simple Tabler Icons Code Generator
--
-- This script generates Haskell modules from Tabler SVG files.
-- Usage: runhaskell codegen.hs <tabler-path> <output-dir>

import Control.Monad (forM_, when)
import Data.ByteString qualified as BS
import Data.List (sort)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeExtension, (</>))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [tablerPath, outputDir] -> generateBindings tablerPath outputDir
    _ -> do
      putStrLn "Usage: runhaskell codegen.hs <tabler-path> <output-dir>"
      putStrLn "Example: runhaskell codegen.hs /nix/store/...-tabler-icons src/Web/TablerIcons"

generateBindings :: FilePath -> FilePath -> IO ()
generateBindings tablerPath outputDir = do
  putStrLn $ "Generating Tabler Icons bindings from: " <> tablerPath

  -- Check if tabler path exists
  tablerExists <- doesDirectoryExist tablerPath
  if not tablerExists
    then putStrLn $ "Error: Tabler path does not exist: " <> tablerPath
    else do
      -- Generate bindings for outline icons
      let outlinePath = tablerPath </> "icons" </> "outline"
      generateIconSet outlinePath (outputDir </> "Outline.hs") "Web.TablerIcons.Outline"

      -- Generate bindings for filled icons
      let filledPath = tablerPath </> "icons" </> "filled"
      generateIconSet filledPath (outputDir </> "Filled.hs") "Web.TablerIcons.Filled"

generateIconSet :: FilePath -> FilePath -> String -> IO ()
generateIconSet iconsDir outputFile moduleName = do
  exists <- doesDirectoryExist iconsDir
  if not exists
    then putStrLn $ "Warning: Icons directory does not exist: " <> iconsDir
    else do
      putStrLn $ "Processing icons from: " <> iconsDir

      -- Get all SVG files
      files <- listDirectory iconsDir
      let svgFiles = sort $ filter (\f -> takeExtension f == ".svg") files

      putStrLn $ "Found " <> show (length svgFiles) <> " SVG icons"

      -- Create output directory
      createDirectoryIfMissing True (takeDirectory outputFile)

      -- Generate module content
      content <- generateModuleContent moduleName iconsDir svgFiles
      writeFile outputFile content

      putStrLn $ "Generated: " <> outputFile

takeDirectory :: FilePath -> FilePath
takeDirectory = reverse . dropWhile (/= '/') . reverse

generateModuleContent :: String -> FilePath -> [FilePath] -> IO String
generateModuleContent moduleName iconsDir svgFiles = do
  iconData <- mapM processIcon svgFiles

  let bindings =
        map
          ( \(name, content, original) ->
              "-- | "
                <> original
                <> "\n"
                <> name
                <> " :: ByteString\n"
                <> name
                <> " = "
                <> show content
                <> "\n"
          )
          iconData

  pure $
    unlines $
      [ "{-# LANGUAGE NoImplicitPrelude #-}"
      , "{-# LANGUAGE OverloadedStrings #-}"
      , ""
      , "{- |"
      , "Tabler " <> extractStyle moduleName <> " Icons"
      , ""
      , "Auto-generated Haskell bindings for Tabler " <> extractStyle moduleName <> " icons."
      , "Each icon is exported as a ByteString containing the raw SVG content."
      , ""
      , "Source: https://github.com/tabler/tabler-icons"
      , "Generated icons: " <> show (length svgFiles)
      , "-}"
      , "module " <> moduleName <> " where"
      , ""
      , "import Prelude ()"
      , "import Data.ByteString (ByteString)"
      , ""
      ]
        <> bindings
  where
    processIcon svgFile = do
      let iconName = svgFileToHaskellName (takeBaseName svgFile)
      svgContent <- BS.readFile (iconsDir </> svgFile)
      pure (iconName, svgContent, takeBaseName svgFile)

    extractStyle = reverse . takeWhile (/= '.') . reverse

-- Convert SVG filename to valid Haskell identifier
svgFileToHaskellName :: String -> String
svgFileToHaskellName name =
  let cleaned = map (\c -> if c == '-' then '_' else c) name
      -- Handle names that start with numbers by prefixing with 'icon_'
      withPrefix = case cleaned of
        [] -> "icon_"
        (c : _) | not (isAlpha c) -> "icon_" <> cleaned
        _ -> cleaned
      -- Only handle Haskell keywords, not Prelude conflicts since we hide Prelude
      final =
        if withPrefix `elem` haskellKeywords
          then withPrefix <> "_"
          else withPrefix
   in final
  where
    isAlpha c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

-- Haskell reserved keywords to avoid
haskellKeywords :: [String]
haskellKeywords =
  [ "case"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "else"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "then"
  , "type"
  , "where"
  , "as"
  , "qualified"
  , "hiding"
  ]
