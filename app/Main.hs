module Main where

import CV.Latex
import CV.LuaLatex
import Data.Text
import Data.Yaml qualified as Yaml
import GHC.TypeLits
import Iris qualified
import Optics
import Options.Applicative
import Relude
import Text.LaTeX

optFromParser ::
    forall n a k.
    ( Show a
    , k ~ A_Lens
    , KnownSymbol n
    , LabelOptic n k RenderOptions RenderOptions a a
    ) =>
    Proxy n ->
    Char ->
    (forall m. (MonadFail m) => Text -> m a) ->
    Parser a
optFromParser lbl shortArg parser = do
    option (parser =<< str)
        $ long (symbolVal lbl)
        <> short shortArg
        <> value (defaultOptions ^. fromLabel @n)
        <> showDefaultWith (toString . toLower . show)

optionsParser :: Parser RenderOptions
optionsParser =
    RenderOptions
        <$> aboutOption
        <*> projectsOption
        <*> minPriority
        <*> projectSectionOption
  where
    aboutOption =
        optFromParser (Proxy @"aboutLength") 'a' (fmap coerce . includeOptionFromText)
    projectsOption =
        optFromParser (Proxy @"projectLength") 'p' (fmap coerce . includeOptionFromText)
    minPriority = option auto $ long "priority" <> showDefault <> value 1
    projectSectionOption =
        optFromParser (Proxy @"projectSection") 'l' projectSectionFromText

fullParser :: Parser (String, RenderOptions)
fullParser = (,) <$> argument str (metavar "FILE") <*> optionsParser

app :: Iris.CliApp (String, RenderOptions) () ()
app = do
    (fn, opts) <- asks Iris.cliEnvCmd
    res <- Yaml.decodeFileThrow fn
    output <- liftIO $ runLuaLatex "./tmp" fn $ encodeUtf8 $ render $ renderCV opts res
    print output

main :: IO ()
main = do
    (fn, opts) <- execParser $ info (fullParser <**> helper) fullDesc
    res <- Yaml.decodeFileThrow fn
    output <- runLuaLatex "./tmp" fn $ encodeUtf8 $ render $ renderCV opts res
    print output
