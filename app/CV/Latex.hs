module CV.Latex (
    renderCVToLatex,
    renderCV,
    RenderOptions (..),
    IncludeOption (..),
    includeOptionFromText,
    orientationFromText,
    defaultOptions,
) where

import CV.Types
import Data.Set hiding (filter, fromList, toList)
import Data.Text as Text hiding (any, elem, filter, head, intersperse, lines, span, takeWhile, unlines)
import Optics
import Relude hiding (intercalate)
import Text.LaTeX hiding (lines, unlines, (&))
import Text.LaTeX.Base.Class

data IncludeOption = None | Short | Long
    deriving (Show, Eq)

includeOptionFromText :: (MonadFail f) => Text -> f IncludeOption
includeOptionFromText text =
    case toLower text of
        "short" -> pure Short
        "long" -> pure Long
        "none" -> pure None
        t -> fail $ "Unknown IncludeOption value " <> toString t

data Orientation = Either | FrontEnd | BackEnd
    deriving (Show, Eq)

orientationFromText :: (MonadFail m) => Text -> m Orientation
orientationFromText text =
    case toLower text of
        "either" -> pure Either
        "front" -> pure FrontEnd
        "back" -> pure BackEnd
        t -> fail $ "Unknown Orientation value " <> toString t

isWithinOrientation :: Orientation -> Technology -> Bool
isWithinOrientation orientation tech =
    case orientation of
        Either -> True
        FrontEnd -> tech `member` frontEndTech
        BackEnd -> not $ tech `member` frontEndTech
  where
    frontEndTech =
        fromList
            [Angular, AntDesign, React, ReactQuery, Redux, ReduxSaga, Remix, Typescript]

data RenderOptions = RenderOptions
    { includeAbout :: IncludeOption
    , includeProjects :: IncludeOption
    , excludedProjects :: [Text]
    , minimumPriority :: Int
    , orientation :: Orientation
    }
    deriving (Show)

makeFieldLabelsNoPrefix ''RenderOptions

defaultOptions :: RenderOptions
defaultOptions =
    RenderOptions
        { includeAbout = Short
        , includeProjects = Short
        , excludedProjects = []
        , minimumPriority = 2
        , orientation = Either
        }

textT :: (LaTeXC l) => Text -> l
textT = fromString . toString

summary :: (Monad m) => LaTeXT_ m -> LaTeXT_ m
summary content = do
    comm1 "begin" "summary"
    content
    comm1 "end" "summary"

cvSection :: (Monad m) => Text -> LaTeXT_ m -> LaTeXT_ m
cvSection name content = do
    comm2 "begin" "cvsection" (textT name)
    content
    comm1 "end" "cvsection"

cvSubSection :: (Monad m) => Text -> Text -> Text -> LaTeXT_ m -> LaTeXT_ m
cvSubSection position company duration content = do
    comm4 "begin" "cvsubsection" (textT position) (textT company) (textT duration)
    content
    comm1 "end" "cvsubsection"

renderBlock :: forall m. (Monad m) => Text -> LaTeXT_ m
renderBlock t = lByl $ lines t
  where
    startsWithDash x = viaNonEmpty head (toString x) == Just '-'
    lByl :: [Text] -> LaTeXT m ()
    lByl [x] = textT x
    lByl (x : xs)
        | startsWithDash x = do
            let (items, rest) = span startsWithDash xs
            itemize $ forM_ (x : items) \i -> do
                comm0 "item"
                textT $ Text.drop 2 i
            lByl rest
        | otherwise = do
            textT x
            newline
            lByl xs
    lByl [] = pass

renderAboutSection :: forall m. (MonadReader RenderOptions m) => About -> LaTeXT_ m
renderAboutSection about = do
    options <- lift $ ask @RenderOptions
    mapM_ (summary . renderBlock) case options ^. #includeAbout of
        None -> Nothing
        Short -> about ^. #short
        Long -> about ^. #long

itemized :: (Monad m) => [LaTeXT_ m] -> LaTeXT_ m
itemized = itemize . mapM_ (comm0 "item" *>)

renderSkill :: forall m. (Monad m) => Text -> [Text] -> LaTeXT_ m
renderSkill name = comm2 "rowlist" (textT name) . renderList
  where
    renderList :: [Text] -> LaTeXT_ m
    renderList l = sequence_ $ intersperse ", " $ textT <$> l

renderSkillsSection :: forall m. (Monad m) => Skills -> LaTeXT_ m
renderSkillsSection skills =
    -- TODO: render proficiency seperately
    cvSection "Languages and Technologies" $
        itemized
            [ renderSkill "Programming Languages" $ skills ^. #programmingLanguages
            , renderSkill "Tools" $ skills ^. #tools
            , renderSkill "Languages" $ skills ^. #languages
            ]

renderHeader :: (MonadReader RenderOptions m) => GeneralInfo -> LaTeXT_ m
renderHeader gi = do
    headerC "phonenumber" $ gi ^. #phonenumber
    headerC "address" $ gi ^. #address
    headerC "name" $ gi ^. #fullname
    headerC "email" $ gi ^. #email
    headerC "github" $ gi ^. #github
    headerC "linkedin" $ gi ^. #linkedIn
  where
    headerC n = comm1 n . textT

renderGeneral :: (MonadReader RenderOptions m) => GeneralInfo -> LaTeXT_ m
renderGeneral gi = do
    renderAboutSection $ gi ^. #about

renderProject :: forall m. (MonadReader RenderOptions m) => Project -> LaTeXT_ m
renderProject p = do
    orientation <- lift $ asks (view #orientation)
    shouldIncludeProjects <- lift $ asks (view #includeProjects)
    minPriority <- lift $ asks (view #minimumPriority)
    excluded <- lift $ asks (view #excludedProjects)
    let projectPriority =
            p ^. #priority
                + case orientation of
                    Either -> 0
                    _ ->
                        if any (isWithinOrientation orientation) (p ^. #technologies)
                            then 1
                            else -1
        shouldSkip =
            (p ^. #title) `elem` excluded
                || projectPriority < minPriority
                || shouldIncludeProjects == None
    unless shouldSkip $ project do
        renderBlock (p ^. #description)
        mapM_ (\o -> newline *> renderBlock o) (p ^. #outcome)
        when (shouldIncludeProjects == Long) do
            newline
            renderSkill "Technologies" (toText <$> p ^. #technologies)
  where
    renderedLinks :: LaTeXT_ m
    renderedLinks =
        fromMaybe pass $
            (p ^. #links)
                & viaNonEmpty
                    ( sequence_
                        . intersperse ", "
                        . toList
                        . fmap (\l -> (comm2 "href" `on` textT) (l ^. #url) (l ^. #title))
                    )
    project :: LaTeXT_ m -> LaTeXT_ m
    project content = do
        comm3 "begin" "project" (textT (p ^. #title)) renderedLinks
        content
        comm1 "end" "project"

renderWorkExperience :: (MonadReader RenderOptions m) => WorkExperience -> LaTeXT_ m
renderWorkExperience we = cvSubSection (we ^. #jobTitle) (we ^. #company) duration do
    shouldIncludeProjects <- lift $ asks (view #includeProjects)
    renderBlock $ we ^. #roleDescription
    case shouldIncludeProjects of
        None -> pass
        _ -> mapM_ renderProject (we ^. #projects)
  where
    duration = (we ^. #start) <> "--" <> fromMaybe "Present" (we ^. #end)

renderEmployment :: (MonadReader RenderOptions m) => [WorkExperience] -> LaTeXT_ m
renderEmployment ws = cvSection "Employment" $ mapM_ renderWorkExperience ws

renderPersonalProjects :: (MonadReader RenderOptions m) => [Project] -> LaTeXT_ m
renderPersonalProjects ps = do
    shouldIncludeProjects <- lift $ asks @RenderOptions (view #includeProjects)
    case shouldIncludeProjects of
        None -> pass
        _ -> cvSection "Personal Projects" $ mapM_ renderProject ps

renderEducation :: (Monad m) => [Education] -> LaTeXT_ m
renderEducation es = cvSection "Education" $ forM_ es \e -> do
    let duration = show (e ^. #start) <> "--" <> show (e ^. #end)
    cvSubSection (e ^. #city) (e ^. #school) duration $ textT (e ^. #degree)

renderCVToLatex :: (MonadReader RenderOptions m) => CV -> LaTeXT_ m
renderCVToLatex cv = do
    documentclass [] "waelcv"
    renderHeader $ cv ^. #generalInfo
    comm1 "begin" "document"
    comm0 "makeheader"
    renderGeneral $ cv ^. #generalInfo
    renderEmployment $ cv ^. #workExperience
    renderEducation $ cv ^. #education
    renderPersonalProjects $ cv ^. #personalProjects
    renderSkillsSection $ cv ^. #generalInfo % #skills
    comm1 "end" "document"

renderCV :: RenderOptions -> CV -> LaTeX
renderCV opts cv = runReader (snd <$> runLaTeXT (renderCVToLatex cv)) opts
