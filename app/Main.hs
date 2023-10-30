module Main where

import CV.Latex
import CV.LuaLatex
import Data.Yaml qualified as Yaml
import Optics
import Options.Applicative
import Relude
import Text.LaTeX

optionsParser :: Parser RenderOptions
optionsParser =
    RenderOptions
        <$> includeOption "about" 'a'
        <*> includeOption "projects" 'p'
        <*> pure []
        <*> minPriority
        <*> orientation
  where
    includeOptionReadM = includeOptionFromText =<< str
    includeOption longArg shortArg =
        option includeOptionReadM $
            long longArg
                <> short shortArg
                <> value (defaultOptions ^. #includeAbout)
                <> showDefault
    orientationReadM = orientationFromText =<< str
    orientation =
        option orientationReadM $
            long "orientation"
                <> short 'o'
                <> value (defaultOptions ^. #orientation)
                <> showDefault
    minPriority = option auto $ long "priority" <> showDefault <> value 1

fullParser :: Parser (String, RenderOptions)
fullParser = (,) <$> argument str (metavar "FILE") <*> optionsParser

main :: IO ()
main = do
    (fn, opts) <- execParser $ info (fullParser <**> helper) fullDesc
    res <- Yaml.decodeFileThrow fn
    output <- runLuaLatex "./tmp" fn $ encodeUtf8 $ render $ renderCV opts res
    print output
