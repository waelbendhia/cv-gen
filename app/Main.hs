module Main where

import CV.InteractiveApp
import CV.Latex
import CV.LuaLatex
import Data.Text
import Data.Yaml qualified as Yaml
import GHC.TypeLits hiding (Mod)
import Optics
import Options.Applicative
import Relude
import Text.LaTeX hiding ((&))

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
    Mod OptionFields a ->
    Parser a
optFromParser lbl shortArg parser opts = do
    option (parser =<< str)
        $ long (symbolVal lbl)
        <> short shortArg
        <> value (defaultOptions ^. fromLabel @n)
        <> showDefaultWith (toString . toLower . show)
        <> opts

renderOptions :: Parser RenderOptions
renderOptions =
    RenderOptions
        <$> summaryOption
        <*> projectsOption
        <*> minPriority
        <*> projectSectionOption
        <*> pure []
  where
    summaryOption =
        optFromParser
            (Proxy @"summaryLength")
            's'
            (fmap coerce . includeOptionFromText)
            (help "Include long, short or no summary section.")
    projectsOption =
        optFromParser
            (Proxy @"projectLength")
            'p'
            (fmap coerce . includeOptionFromText)
            (help "Include long (with technologies), short (no technologies) or omit projects entirely.")
    minPriority =
        option auto
            $ long "priority"
            <> showDefault
            <> value 1
            <> help "Minimum priority to have project included."
    projectSectionOption =
        optFromParser
            (Proxy @"projectSection")
            'l'
            projectSectionFromText
            (help "Include projects in work experience or render in seperate section.")

data AppSettings = Interactive String | NonInteractive String RenderOptions

appSettings :: Parser AppSettings
appSettings = do
    (interactive *> (Interactive <$> file))
        <|> (NonInteractive <$> file <*> renderOptions)
  where
    file = argument str (metavar "FILE" <> help "YAML file containing CV data.")
    interactive = flag' () $ help "run in interactive mode" <> short 'i' <> long "interactive"

main :: IO ()
main = do
    appSettings' <- execParser $ info (appSettings <**> helper) fullDesc
    (cv, fn, mOpts) <- case appSettings' of
        Interactive fn -> do
            cv <- Yaml.decodeFileThrow fn
            (cv,fn,) <$> getRenderOptions cv
        NonInteractive fn opts -> do
            cv <- Yaml.decodeFileThrow fn
            pure (cv, fn, Just opts)
    mOpts & maybe (print @Text "aborted") \opts -> do
        output <- runLuaLatex "./tmp" fn $ encodeUtf8 $ render $ renderCV opts cv
        print output
