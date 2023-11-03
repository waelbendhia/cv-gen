module CV.Latex (
    renderCVToLatex,
    renderCV,
    RenderOptions (..),
    IncludeOption (..),
    SummaryLength (..),
    ProjectLength (..),
    ProjectSection (..),
    includeOptionFromText,
    projectSectionFromText,
    defaultOptions,
) where

import CV.Types
import Data.List (nub)
import Data.Text qualified as Text
import Optics
import Polysemy
import Polysemy.Reader
import Relude hiding (Reader, ask, asks, intercalate, runReader)
import Text.LaTeX hiding (lines, unlines, (&))
import Text.LaTeX.Base.Class

data IncludeOption = None | Short | Long
    deriving (Show, Eq, Enum, Bounded)

includeOptionFromText :: (MonadFail f) => Text -> f IncludeOption
includeOptionFromText text =
    case Text.toLower text of
        "short" -> pure Short
        "long" -> pure Long
        "none" -> pure None
        t -> fail $ "Unknown IncludeOption value " <> toString t

data ProjectSection = InWorkExperience | Standalone deriving (Show, Eq, Bounded, Enum)

projectSectionFromText :: (MonadFail m) => Text -> m ProjectSection
projectSectionFromText text =
    case Text.toLower text of
        "inworkexperience" -> pure InWorkExperience
        "workexperience" -> pure InWorkExperience
        "employment" -> pure InWorkExperience
        "standalone" -> pure Standalone
        "alone" -> pure Standalone
        "seperate" -> pure Standalone
        t -> fail $ "Unknown ProjectSection value " <> toString t

newtype SummaryLength = SummaryLength IncludeOption deriving (Show)
newtype ProjectLength = ProjectLength IncludeOption deriving (Show)

data RenderOptions = RenderOptions
    { summaryLength :: SummaryLength
    , projectLength :: ProjectLength
    , minimumPriority :: Int
    , projectSection :: ProjectSection
    , selectedProjects :: [Text]
    }
    deriving (Show)

makeFieldLabelsNoPrefix ''RenderOptions

defaultOptions :: RenderOptions
defaultOptions =
    RenderOptions
        { summaryLength = coerce Short
        , projectLength = coerce Short
        , minimumPriority = 2
        , projectSection = Standalone
        , selectedProjects = []
        }

newtype SummarySection = SummarySection (Maybe Text)

runAboutReader :: RenderOptions -> CV -> Sem (Reader SummarySection : r) b -> Sem r b
runAboutReader opts cv = runReader $ SummarySection case opts ^. #summaryLength % coerced of
    None -> Nothing
    Long -> about' ^. #long
    Short -> about' ^. #short
  where
    about' = cv ^. #generalInfo % #about

filterProjects :: RenderOptions -> [Project] -> [Project]
filterProjects opts =
    filter \p ->
        (p ^. #priority >= opts ^. #minimumPriority)
            && ( null (opts ^. #selectedProjects)
                    || (p ^. #title)
                    `elem` (opts ^. #selectedProjects)
               )

runWorkExperiencesReader ::
    RenderOptions ->
    CV ->
    Sem (Reader [WorkExperience] : r) a ->
    Sem r a
runWorkExperiencesReader opts cv =
    runReader
        $ (cv ^. #workExperience)
        & withLen
        & (traversed % #projects %~ filterProjects opts)
  where
    withLen =
        case (opts ^. #projectLength % coerced, opts ^. #projectSection) of
            (_, Standalone) -> traversed % #projects .~ []
            (None, _) -> traversed % #projects .~ []
            (Short, _) -> traversed % #projects % traversed % #technologies .~ []
            (Long, _) -> id

data Projects = Projects
    { personal :: [Project]
    , professional :: [Project]
    }

runProjectsReader :: RenderOptions -> CV -> Sem (Reader Projects : r) a -> Sem r a
runProjectsReader opts cv = runReader projects
  where
    withLen ps = case opts ^. #projectLength % coerced of
        None -> []
        Short -> ps & traversed % #technologies .~ []
        Long -> ps
    projects =
        (Projects `on` withLen . filterProjects opts)
            (cv ^. #personalProjects)
            case opts ^. #projectSection of
                InWorkExperience -> []
                Standalone -> cv ^. #professionalProjects

textT :: (LaTeXC l) => Text -> l
textT = fromString . toString

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

renderSummarySection :: forall r. (Members '[Reader SummarySection] r) => LaTeXT_ (Sem r)
renderSummarySection = do
    (aboutSection :: Maybe Text) <- lift $ asks @SummarySection coerce
    mapM_ (summary . renderBlock) aboutSection
  where
    summary :: LaTeXT_ (Sem r) -> LaTeXT_ (Sem r)
    summary content = do
        comm1 "begin" "summary"
        content
        comm1 "end" "summary"

itemized :: (Monad m) => [LaTeXT_ m] -> LaTeXT_ m
itemized = itemize . mapM_ (comm0 "item" *>)

renderSkill :: forall m. (Monad m) => Text -> [Text] -> LaTeXT_ m
renderSkill name = comm2 "rowlist" (textT name) . renderList
  where
    renderList :: [Text] -> LaTeXT_ m
    renderList l = sequence_ $ intersperse ", " $ textT <$> nub l

renderSkillsSection :: forall m. (Monad m) => Skills -> LaTeXT_ m
renderSkillsSection skills =
    -- TODO: render proficiency seperately
    cvSection "Languages and Technologies"
        $ itemized
            [ renderSkill "Programming Languages" $ skills ^. #programmingLanguages
            , renderSkill "Tools" $ skills ^. #tools
            , renderSkill "Languages" $ skills ^. #languages
            ]

renderHeader :: GeneralInfo -> LaTeXT_ (Sem r)
renderHeader gi = do
    headerC "phonenumber" $ gi ^. #phonenumber
    headerC "address" $ gi ^. #address
    headerC "name" $ gi ^. #fullname
    headerC "email" $ gi ^. #email
    headerC "github" $ gi ^. #github
    headerC "linkedin" $ gi ^. #linkedIn
  where
    headerC n = comm1 n . textT

renderProject :: (Monad m) => Project -> LaTeXT_ m
renderProject p = project do
    renderBlock (p ^. #description)
    mapM_ (\o -> newline *> renderBlock o) (p ^. #outcome)
    unless (null $ p ^. #technologies) do
        newline
        renderSkill "Technologies" (toText <$> p ^. #technologies)
  where
    renderLinks =
        fromMaybe pass
            $ (p ^. #links)
            & viaNonEmpty
                ( sequence_
                    . intersperse ", "
                    . toList
                    . fmap (\l -> (comm2 "href" `on` textT) (l ^. #url) (l ^. #title))
                )
    project content = do
        comm3 "begin" "project" (textT (p ^. #title)) renderLinks
        content
        comm1 "end" "project"

renderWorkExperience :: (Monad m) => WorkExperience -> LaTeXT_ m
renderWorkExperience we = cvSubSection (we ^. #jobTitle) (we ^. #company) duration do
    renderBlock $ we ^. #roleDescription
    mapM_ renderProject (we ^. #projects)
  where
    duration = (we ^. #start) <> "--" <> fromMaybe "Present" (we ^. #end)

renderEmployment ::
    (Members '[Reader [WorkExperience]] r) =>
    LaTeXT_ (Sem r)
renderEmployment = cvSection "Work Experience" do
    wes <- lift $ ask @[WorkExperience]
    unless (null wes) $ mapM_ renderWorkExperience wes

renderProjects ::
    (Members '[Reader Projects] r) => LaTeXT_ (Sem r)
renderProjects = do
    personal' <- lift $ asks @Projects personal
    professional' <- lift $ asks @Projects professional
    case (personal', professional') of
        ([], []) -> pass
        ([], ps) ->
            cvSection "Notable Projects" $ mapM_ renderProject ps
        (ps, []) ->
            cvSection "Personal Projects" $ mapM_ renderProject ps
        _ -> cvSection "Notable Projects" do
            cvSubSection "Professional" "" "" $ mapM_ renderProject professional'
            cvSubSection "Personal" "" "" $ mapM_ renderProject personal'

renderEducation :: (Monad m) => [Education] -> LaTeXT_ m
renderEducation es = cvSection "Education" $ forM_ es \e -> do
    let duration = show (e ^. #start) <> "--" <> show (e ^. #end)
    cvSubSection (e ^. #city) (e ^. #school) duration $ textT (e ^. #degree)

renderCVToLatex ::
    (Members '[Reader SummarySection, Reader [WorkExperience], Reader Projects] r) =>
    CV ->
    LaTeXT_ (Sem r)
renderCVToLatex cv = do
    documentclass [] "waelcv"
    renderHeader $ cv ^. #generalInfo
    comm1 "begin" "document"
    comm0 "makeheader"
    renderSummarySection
    renderEmployment
    renderProjects
    renderEducation $ cv ^. #education
    renderSkillsSection $ cv ^. #generalInfo % #skills
    comm1 "end" "document"

renderCV :: RenderOptions -> CV -> LaTeX
renderCV opts cv =
    snd
        . run
        . runAboutReader opts cv
        . runProjectsReader opts cv
        . runWorkExperiencesReader opts cv
        . runLaTeXT
        $ renderCVToLatex cv
