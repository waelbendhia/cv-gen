{-# LANGUAGE ImpredicativeTypes #-}

module CV.InteractiveApp (getRenderOptions) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import CV.Latex
import CV.Types
import Data.Text (toLower)
import Graphics.Vty
import Optics
import Relude
import Relude.Extra.Enum

data ProjectInclusion = Omit | Include deriving (Show, Eq, Enum, Bounded)

data AppState
    = SelectSummaryLength IncludeOption
    | SelectProjectLength SummaryLength IncludeOption
    | SetMinimumPriority SummaryLength ProjectLength Int
    | SelectProjectSection SummaryLength ProjectLength Int ProjectSection
    | SelectProjects
        SummaryLength
        ProjectLength
        Int
        ProjectSection
        (NonEmpty Project)
        [Text]
        ProjectInclusion
    | FinalState SummaryLength ProjectLength Int ProjectSection [Text]
    deriving (Show)

handleSelection :: (Eq a, Bounded a, Enum a) => a -> BrickEvent n e -> Either a a
handleSelection focus (VtyEvent (EvKey KRight [])) = Left $ next focus
handleSelection focus (VtyEvent (EvKey KLeft [])) = Left $ prev focus
handleSelection selected (VtyEvent (EvKey KEnter [])) = Right selected
handleSelection focus _ = Left focus

prevState :: CV -> AppState -> Maybe AppState
prevState _ (SelectSummaryLength{}) = Nothing
prevState _ (SelectProjectLength sLen _) = Just $ SelectSummaryLength $ coerce sLen
prevState _ (SetMinimumPriority sLen pLen _) = Just $ SelectProjectLength sLen $ coerce pLen
prevState _ (SelectProjectSection sLen pLen priority _) =
    Just $ SetMinimumPriority sLen pLen priority
prevState _ (SelectProjects sLen pLen priority pLoc _ _ _) =
    Just $ SelectProjectSection sLen pLen priority pLoc
prevState cv (FinalState sLen pLen priority pLoc _) =
    Just case coerce pLen of
        None -> SelectProjectLength sLen None
        _ ->
            (cv ^. #projects)
                & filter (\p -> p ^. #priority >= priority)
                & viaNonEmpty (\ps -> SelectProjects sLen pLen priority pLoc ps [] Omit)
                & fromMaybe (SetMinimumPriority sLen pLen priority)

reducer ::
    CV ->
    AppState ->
    BrickEvent Text e ->
    EventM Text AppState ()
reducer _ (SelectSummaryLength focus) e =
    put
        $ either SelectSummaryLength (\s -> SelectProjectLength (SummaryLength s) None)
        $ handleSelection focus e
reducer _ (SelectProjectLength sLen focus) e = put case handleSelection focus e of
    Right None -> FinalState sLen (ProjectLength None) 0 InWorkExperience []
    Right selected -> SetMinimumPriority sLen (ProjectLength selected) 0
    Left focused -> SelectProjectLength sLen focused
reducer cv s@(SetMinimumPriority sLen pLen priority) e = put case e of
    VtyEvent (EvKey KUp []) -> SetMinimumPriority sLen pLen (priority + 1)
    VtyEvent (EvKey KDown []) -> SetMinimumPriority sLen pLen (max 0 (priority - 1))
    VtyEvent (EvKey KEnter []) ->
        if all (\p -> p ^. #priority < priority) (cv ^. #projects)
            then FinalState sLen pLen priority InWorkExperience []
            else SelectProjectSection sLen pLen priority InWorkExperience
    _ -> s
reducer cv (SelectProjectSection sLen pLen priority focus) e =
    put case handleSelection focus e of
        Left focus' -> SelectProjectSection sLen pLen priority focus'
        Right selection ->
            (cv ^. #projects)
                & filter (\p -> p ^. #priority >= priority)
                & viaNonEmpty (\ps -> SelectProjects sLen pLen priority selection ps [] Omit)
                & fromMaybe (FinalState sLen pLen priority focus [])
reducer _ (SelectProjects sLen pLen priority pLoc ps selected focus) e =
    case handleSelection focus e of
        Left focus' -> put $ SelectProjects sLen pLen priority pLoc ps selected focus'
        Right selection ->
            let selected' = case selection of
                    Include -> head ps ^. #title : selected
                    Omit -> selected
             in tail ps
                    & viaNonEmpty (\ps' -> SelectProjects sLen pLen priority pLoc ps' selected' Omit)
                    & fromMaybe (FinalState sLen pLen priority pLoc selected')
                    & put
reducer _ (FinalState{}) (VtyEvent (EvKey KEnter [])) = halt
reducer _ s@(FinalState{}) _ = put s

window :: Text -> Widget n -> [Widget n]
window title content =
    one $ center $ hLimit 100 $ borderWithLabel (txt title) content

optionDialog ::
    (Bounded a, Enum a, Eq a) =>
    (a -> Text) ->
    Text ->
    Text ->
    a ->
    [Widget Text]
optionDialog render title helpText focused =
    window title
        $ vBox
            [ hCenter (txt helpText)
            , txt " "
            , hCenter
                $ hBox
                $ intersperse (txt " ")
                $ universe
                <&> \opt ->
                    withAttr (if opt == focused then buttonSelectedAttr else buttonAttr)
                        $ txt
                        $ sconcat ("  " :| [render opt, "  "])
            ]

draw :: AppState -> [Widget Text]
draw (SelectSummaryLength focused) =
    optionDialog
        show
        "Select summary length"
        "None to omit, short to use short summary or long for full summary."
        focused
draw (SelectProjectLength _ focused) = optionDialog show "Select project length" "None projects will not be included, Short no technologies line or Long with technologies" focused
draw (SetMinimumPriority _ _ priority) =
    window "Set minimum project priority"
        $ vBox
        $ (hCenter . txt)
        <$> [ "Projects with a priority lower than the selected value will be skipped"
            , show priority
            ]
draw (SelectProjectSection _ _ _ focused) =
    optionDialog
        \case
            InWorkExperience -> "Work Experience"
            Standalone -> "Standalone"
        "Project location"
        "Choose where projects will be rendered"
        focused
draw (SelectProjects _ _ _ _ ps@(p :| _) sel focused) =
    optionDialog
        show
        ("Do you want to include project: '" <> p ^. #title <> "'?")
        (unwords [show (length ps), "remaining projects,", show (length sel), "selected projects"])
        focused
draw (FinalState sLen pLen _ pLoc ps) =
    window "All done!"
        $ vBox
        $ txt
            ( case coerce sLen of
                None -> "You're omitting the summary"
                other -> "You're inlcuding a " <> toLower (show other) <> " summary"
            )
        : let noProjects = [txt "You're not including any projects"]
           in case (coerce pLen, ps) of
                (None, _) -> noProjects
                (_, []) -> noProjects
                (len, _) ->
                    txt
                        ( "You're including "
                            <> toLower (show len)
                            <> " project sections in "
                            <> case pLoc of
                                InWorkExperience -> "the work experience section"
                                Standalone -> "a standalone section"
                        )
                        : txt "You're including the project:"
                        : (ps <&> \p -> txt $ " - " <> p)

getRenderOptions :: CV -> IO (Maybe RenderOptions)
getRenderOptions cv = do
    let attributes = attrMap defAttr [(buttonSelectedAttr, bg blue)]
        handleEvent :: BrickEvent Text e -> EventM Text AppState ()
        handleEvent (VtyEvent (EvKey (KChar 'q') [])) = halt
        handleEvent (VtyEvent (EvKey KEsc [])) = do
            st <- gets $ prevState cv
            maybe halt put st
        handleEvent e = do
            currentState <- get
            reducer cv currentState e
        app =
            App
                { appDraw = draw
                , appChooseCursor = \_ _ -> Nothing
                , appHandleEvent = handleEvent
                , appStartEvent = pass
                , appAttrMap = const attributes
                }
    x <- defaultMain @Text app (SelectSummaryLength None)
    case x of
        FinalState sLen pLen priority pLoc includedProjects ->
            pure
                $ Just
                    RenderOptions
                        { summaryLength = sLen
                        , projectLength = pLen
                        , minimumPriority = priority
                        , projectSection = pLoc
                        , selectedProjects = includedProjects
                        }
        _ -> pure Nothing
