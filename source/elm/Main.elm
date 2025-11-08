module Main exposing (main)

import Board exposing (Board)
import Browser
import Browser.Events
import Color exposing (Color(..))
import Constants
import Css
import Css.Animations
import Css.Global
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (class, css)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Language exposing (Language)
import List
import Maybe.Extra
import Num exposing (Num(..))
import Phosphor
import Process
import Random
import Random.Extra as Random
import Row exposing (Row)
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as Svga
import Tailwind.Color as Twc
import Tailwind.Theme as Twt
import Tailwind.Utilities as Tw
import Task
import Texts
import Util.Html.Styled exposing (viewIfLazy)


main : Program Decode.Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { board : Board
    , turn : Turn
    , dialog : Dialog
    , viewport : Viewport
    , seed : Random.Seed
    , language : Language.Selection
    }


type Dialog
    = NoDialog
    | SettingsDialog


type CellStatus
    = Available
    | Xed
    | Picked
    | Passed
    | Unavailable


type Turn
    = NotTurn
    | TurnPicking DiceThrow DiceRotations
    | TurnPickedOnce DiceThrow DiceRotations Pick


type alias DiceThrow =
    { dieWhite1 : Pips
    , dieWhite2 : Pips
    , dieRed : Pips
    , dieYellow : Pips
    , dieGreen : Pips
    , dieBlue : Pips
    }


type alias DiceRotations =
    { dieWhite1 : Float
    , dieWhite2 : Float
    , dieRed : Float
    , dieYellow : Float
    , dieGreen : Float
    , dieBlue : Float
    }


type alias Pick =
    { color : Color
    , num : Num
    }


type alias Viewport =
    { width : Int, height : Int }



-- INIT


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        viewport : Viewport
        viewport =
            Decode.decodeValue (Decode.field "viewport" viewportDecoder) flags
                |> Result.withDefault { width = 1025, height = 768 }

        language : Language.Selection
        language =
            Decode.decodeValue (Decode.field "languages" Language.selectionDecoder) flags
                |> Result.withDefault Language.defaultSelection
    in
    ( { board = Board.init
      , turn = NotTurn
      , dialog = NoDialog
      , viewport = viewport
      , seed = Random.initialSeed 12345
      , language = language
      }
    , Random.generate GotInitialSeed Random.independentSeed
    )


viewportDecoder : Decode.Decoder Viewport
viewportDecoder =
    Decode.map2 Viewport
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)



-- UPDATE


type Msg
    = GotInitialSeed Random.Seed
    | DiceThrown Random.Seed DiceThrow DiceRotations
    | ClickedAvailableCell Pick
    | ClickedPickedCell
    | ClickedDone
    | ClickedFault
    | LanguageSelected (Maybe Language)
    | DialogRequested Dialog
    | ViewportResized Int Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        interactive =
            model.dialog == NoDialog

        ignore =
            ( model, Cmd.none )
    in
    case ( interactive, msg ) of
        ( True, GotInitialSeed newSeed ) ->
            ( { model | seed = newSeed }
            , throwDiceIfGameNotEnded model.board newSeed
            )

        ( True, DiceThrown newSeed diceThrow diceRotations ) ->
            ( { model
                | turn = TurnPicking diceThrow diceRotations
                , seed = newSeed
              }
            , Cmd.none
            )

        ( True, ClickedAvailableCell pick ) ->
            case model.turn of
                TurnPicking diceThrow diceRotations ->
                    -- Pick a cell.
                    ( { model | turn = TurnPickedOnce diceThrow diceRotations pick }
                    , Cmd.none
                    )

                TurnPickedOnce _ _ previousPick ->
                    -- X the two picks and finish the turn.
                    let
                        newBoard =
                            model.board
                                |> Board.addX previousPick.color previousPick.num
                                |> Board.addX pick.color pick.num
                    in
                    ( { model
                        | board = newBoard
                        , turn = NotTurn
                      }
                    , throwDiceIfGameNotEnded newBoard model.seed
                    )

                NotTurn ->
                    ignore

        ( True, ClickedPickedCell ) ->
            case model.turn of
                TurnPickedOnce diceThrow diceRotations _ ->
                    -- Undo the first pick.
                    ( { model | turn = TurnPicking diceThrow diceRotations }
                    , Cmd.none
                    )

                NotTurn ->
                    ignore

                TurnPicking _ _ ->
                    ignore

        ( True, ClickedDone ) ->
            case model.turn of
                TurnPickedOnce _ _ pick ->
                    -- End turn with a single X.
                    let
                        newBoard =
                            model.board
                                |> Board.addX pick.color pick.num
                    in
                    ( { model
                        | board = newBoard
                        , turn = NotTurn
                      }
                    , throwDiceIfGameNotEnded newBoard model.seed
                    )

                TurnPicking _ _ ->
                    ignore

                NotTurn ->
                    ignore

        ( True, ClickedFault ) ->
            if canAddFault model.turn then
                let
                    newBoard =
                        Board.addFault model.board
                in
                ( { model
                    | board = newBoard
                    , turn = NotTurn
                  }
                , throwDiceIfGameNotEnded newBoard model.seed
                )

            else
                ignore

        ( _, LanguageSelected selected ) ->
            ( { model | language = Language.setSelection selected model.language }
            , Cmd.none
            )

        ( _, DialogRequested dialog ) ->
            ( { model | dialog = dialog }
            , Cmd.none
            )

        ( _, ViewportResized width height ) ->
            ( { model | viewport = { width = width, height = height } }
            , Cmd.none
            )

        ( _, NoOp ) ->
            ignore

        ( False, _ ) ->
            ignore



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize ViewportResized



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Cuics"
    , body =
        [ Html.div
            [ css [ Tw.h_full, Tw.w_full, Tw.flex, Tw.flex_col, Tw.justify_center, Tw.items_center ] ]
            [ viewContent model ]
        , Css.Global.global Tw.globalStyles
        ]
            |> List.map Html.toUnstyled
    }


viewContent : Model -> Html Msg
viewContent model =
    let
        scale : Float
        scale =
            min scaleX scaleY

        scaleX : Float
        scaleX =
            toFloat model.viewport.width
                / (contentWidth * Constants.remInPx)
                |> min 1

        scaleY : Float
        scaleY =
            toFloat model.viewport.height
                / (contentHeight * Constants.remInPx)
                |> min 1

        contentWidth : Float
        contentWidth =
            53

        contentHeight : Float
        contentHeight =
            35

        language : Language
        language =
            Language.selectionToLanguage model.language
    in
    Html.div
        [ css [ Css.width (Css.rem contentWidth), Css.height (Css.rem contentHeight) ]
        , css [ Tw.flex, Tw.flex_col, Tw.justify_center, Tw.items_center, Tw.gap_2, Tw.shrink_0 ]
        , css [ Tw.relative ]
        , css [ Tw.bg_color Twt.gray_50 ]
        , css [ Tw.font_sans ]
        , css [ Css.transforms [ Css.scale scale ] ]
        ]
        [ viewTop language model.board model.turn
        , viewBoard language model.board model.turn
        , viewSettingsButton
        , viewIfLazy (model.dialog == SettingsDialog)
            (\() -> viewSettingsDialog (Language.selected model.language))
        ]



-- VIEW DIALOG


viewDialog : Html Msg -> Html Msg
viewDialog content =
    Html.div
        [ css [ Tw.w_full, Tw.h_full, Tw.flex, Tw.items_center, Tw.justify_center ]
        , css [ Tw.bg_color (Twc.withOpacity Twt.opacity50 Twt.gray_200) ]
        , css [ Tw.absolute ]
        , Events.onClick (DialogRequested NoDialog)
        ]
        [ Html.div
            [ css [ Tw.w_8over12, Tw.max_h_80, Tw.p_6 ]
            , css [ Tw.bg_color Twt.white, Tw.drop_shadow_xl ]
            , css [ Tw.rounded_xl ]
            , Events.stopPropagationOn "click" (Decode.succeed ( NoOp, True ))
            ]
            [ content ]
        ]



-- VIEW SETTINGS


viewSettingsButton : Html Msg
viewSettingsButton =
    viewButton
        [ css [ Tw.absolute, Tw.right_2, Tw.top_2 ]
        , css [ Tw.text_xl ]
        , Events.onClick (DialogRequested SettingsDialog)
        ]
        [ icon (Phosphor.wrench Phosphor.Bold) ]


viewSettingsDialog : Maybe Language -> Html Msg
viewSettingsDialog selectedLanguage =
    viewDialog
        (Html.div [ css [ Tw.flex, Tw.flex_row, Tw.gap_1 ] ]
            ([ [ viewLanguageRadioButton Nothing (selectedLanguage == Nothing) ]
             , Language.all
                |> List.map (\lang -> viewLanguageRadioButton (Just lang) (selectedLanguage == Just lang))
             ]
                |> List.concat
            )
        )


viewLanguageRadioButton : Maybe Language -> Bool -> Html Msg
viewLanguageRadioButton language selected =
    let
        languageName : String
        languageName =
            language |> Maybe.map Language.name |> Maybe.withDefault "Default"

        radioIcon : Html Msg
        radioIcon =
            if selected then
                Html.div [ css [ Tw.text_xl ] ]
                    [ icon (Phosphor.radioButton Phosphor.Fill) ]

            else
                Html.div [ css [ Tw.text_xl, Tw.text_color Twt.purple_300 ] ]
                    [ icon (Phosphor.radioButton Phosphor.Regular) ]
    in
    Html.label []
        [ Html.input
            [ Attributes.type_ "radio"
            , Attributes.value languageName
            , Attributes.name "language"
            , css [ Tw.hidden ]
            ]
            []
        , viewButton
            [ css [ Tw.flex, Tw.flex_row, Tw.gap_1, Tw.items_center ]
            , Events.onClick (LanguageSelected language)
            ]
            [ radioIcon
            , Html.text languageName
            ]
        ]



-- VIEW TOP


viewTop : Language -> Board -> Turn -> Html Msg
viewTop language board turn =
    let
        ( showingDone, enabledDone ) =
            case turn of
                NotTurn ->
                    ( False, False )

                TurnPicking _ _ ->
                    ( True, False )

                TurnPickedOnce _ _ _ ->
                    ( True, True )
    in
    Html.div [ css [ Tw.flex, Tw.flex_row, Tw.gap_4, Tw.items_center ] ]
        [ viewDiceIfThrown turn (Board.lockedRows board)
        , viewDoneButton language showingDone (not enabledDone)
        ]



-- VIEW DONE BUTTON


viewDoneButton : Language -> Bool -> Bool -> Html Msg
viewDoneButton language showing disabled =
    if showing then
        let
            conditionalStyles =
                if disabled then
                    [ css [ Tw.bg_color Twt.gray_200, Tw.cursor_not_allowed ] ]

                else
                    [ css [ Tw.bg_color Twt.purple_500 ]
                    , Events.onClick ClickedDone
                    ]
        in
        Html.button
            ([ css [ Tw.w_32, Tw.h_10, Tw.rounded_lg ]
             , css [ Tw.text_color Twt.white ]
             ]
                ++ conditionalStyles
            )
            [ Html.text (Texts.for language).done ]

    else
        Html.div [ css [ Tw.w_32 ] ] []



-- VIEW DICE


type Pips
    = Pips1
    | Pips2
    | Pips3
    | Pips4
    | Pips5
    | Pips6


type DieColor
    = DieWhite
    | DieRed
    | DieYellow
    | DieGreen
    | DieBlue


viewDiceIfThrown : Turn -> List Color -> Html Msg
viewDiceIfThrown turn lockedRows =
    case turn of
        NotTurn ->
            Html.div [ css [ Tw.h_16, Tw.m_3 ] ] []

        TurnPicking diceThrow diceRotations ->
            viewDice diceThrow diceRotations lockedRows

        TurnPickedOnce diceThrow diceRotations _ ->
            viewDice diceThrow diceRotations lockedRows


viewDice : DiceThrow -> DiceRotations -> List Color -> Html Msg
viewDice diceThrow diceRotations lockedRows =
    let
        ifNotLocked : Color -> view -> Maybe view
        ifNotLocked color v =
            if List.member color lockedRows then
                Nothing

            else
                Just v
    in
    Html.div [ css [ Tw.flex, Tw.flex_row, Tw.p_3, Tw.gap_3 ] ]
        ([ Just (viewDie DieWhite diceThrow.dieWhite1 diceRotations.dieWhite1)
         , Just (viewDie DieWhite diceThrow.dieWhite2 diceRotations.dieWhite2)
         , ifNotLocked Red (viewDie DieRed diceThrow.dieRed diceRotations.dieRed)
         , ifNotLocked Yellow (viewDie DieYellow diceThrow.dieYellow diceRotations.dieYellow)
         , ifNotLocked Green (viewDie DieGreen diceThrow.dieGreen diceRotations.dieGreen)
         , ifNotLocked Blue (viewDie DieBlue diceThrow.dieBlue diceRotations.dieBlue)
         ]
            |> Maybe.Extra.values
            |> List.indexedMap (\index v -> v index)
        )


viewDie : DieColor -> Pips -> Float -> Int -> Html Msg
viewDie dieColor pips rotationTurns showOrder =
    let
        colors =
            getDieColors dieColor

        pip =
            viewDiePip colors.pip
    in
    Html.div
        [ css [ Tw.w_16, Tw.h_16, Tw.bg_color colors.face, Tw.rounded_2xl ]
        , css [ Tw.border_2, Tw.border_color colors.border ]

        -- Appearance animation.
        , css
            [ Css.opacity (Css.num 0)
            , Css.animationName
                (Css.Animations.keyframes
                    [ ( 0
                      , [ Css.Animations.transform
                            [ Css.scale 0.3
                            , Css.rotate (Css.turn (rotationTurns + 0.1))
                            ]
                        , Css.Animations.opacity (Css.num 1)
                        ]
                      )
                    , ( 50
                      , [ Css.Animations.transform
                            [ Css.scale 1.1
                            , Css.rotate (Css.turn (rotationTurns + 0.05))
                            ]
                        ]
                      )
                    , ( 100
                      , [ Css.Animations.transform
                            [ Css.scale 1
                            , Css.rotate (Css.turn rotationTurns)
                            ]
                        , Css.Animations.opacity (Css.num 1)
                        ]
                      )
                    ]
                )
            , Css.animationDuration (Css.ms 250)
            , Css.animationDelay (Css.ms (toFloat showOrder * 90))
            , Css.property "animation-timing-function" "ease-out"
            , Css.property "animation-fill-mode" "forwards"
            ]
        ]
        [ Svg.svg [ Svga.viewBox "-6 -6 12 12" ]
            ([ -- Top left
               mergeIf (List.member pips [ Pips4, Pips5, Pips6 ])
                [ pip -1 -1 ]

             -- Top right
             , mergeIf (List.member pips [ Pips2, Pips3, Pips4, Pips5, Pips6 ])
                [ pip 1 -1 ]

             -- Bottom left
             , mergeIf (List.member pips [ Pips2, Pips3, Pips4, Pips5, Pips6 ])
                [ pip -1 1 ]

             -- Bottom right
             , mergeIf (List.member pips [ Pips4, Pips5, Pips6 ])
                [ pip 1 1 ]

             -- Center
             , mergeIf (List.member pips [ Pips1, Pips3, Pips5 ])
                [ pip 0 0 ]

             -- Left and right
             , mergeIf (List.member pips [ Pips6 ])
                [ pip -1 0
                , pip 1 0
                ]
             ]
                |> List.concat
            )
        ]


viewDiePip : Twt.Color -> Int -> Int -> Svg Msg
viewDiePip twColor xOffset yOffset =
    Svg.circle
        [ Svga.cx (String.fromInt (xOffset * 3))
        , Svga.cy (String.fromInt (yOffset * 3))
        , Svga.r "0.06rem"
        , css [ Tw.fill_color twColor ]
        ]
        []



-- VIEW BOARD


viewBoard : Language -> Board -> Turn -> Html Msg
viewBoard language board turn =
    Html.div [ css [ Tw.flex, Tw.flex_col, Tw.gap_3 ] ]
        [ viewColorRows board turn
        , viewFaults language (canAddFault turn) (Board.faults board)
        , viewScoreboard language board
        ]


viewColorRows : Board -> Turn -> Html Msg
viewColorRows board turn =
    let
        colorRow : Color -> Html Msg
        colorRow color =
            viewColorRow (Board.row color board) turn color
    in
    Html.div [ css [ Tw.flex, Tw.flex_col, Tw.gap_1 ] ]
        (Color.all |> List.map colorRow)


viewColorRow : Row -> Turn -> Color -> Html Msg
viewColorRow row turn color =
    let
        growth : Num.Growth
        growth =
            Color.growth color

        cell : Num -> Html Msg
        cell num =
            let
                status : CellStatus
                status =
                    getCellStatus growth row turn color num
            in
            viewNumCell color num status

        cells : List (Html Msg)
        cells =
            Num.all (Color.growth color)
                |> List.map cell

        colors =
            getColors color Available
    in
    Html.div
        [ css [ Tw.flex, Tw.flex_row, Tw.gap_1, Tw.p_1 ]
        , css [ Tw.bg_color colors.fg ]
        ]
        ([ cells
         , [ viewLockCell color (Row.locked (Color.growth color) row) ]
         ]
            |> List.concat
        )


viewNumCell : Color -> Num -> CellStatus -> Html Msg
viewNumCell color num status =
    let
        colors =
            getColors color status

        conditionalStyles =
            case status of
                Available ->
                    [ Events.onClick (ClickedAvailableCell { color = color, num = num })
                    , activeGlow
                    ]

                Xed ->
                    [ css [ Tw.cursor_default ] ]

                Picked ->
                    [ Events.onClick ClickedPickedCell
                    , activeGlow
                    ]

                Passed ->
                    [ css [ Tw.cursor_not_allowed ] ]

                Unavailable ->
                    [ css [ Tw.cursor_not_allowed ] ]
    in
    Html.button
        ([ css [ Tw.w_16, Tw.h_16, Tw.relative, Tw.overflow_hidden ]
         , css [ Tw.flex, Tw.justify_center, Tw.items_center ]
         , css [ Tw.text_2xl, Tw.text_color colors.fg, Tw.font_bold ]
         , css [ Tw.bg_color colors.bg ]
         , css [ Tw.border_2, Tw.border_color colors.b, Tw.rounded_lg ]
         , css [ Tw.select_none ]
         ]
            ++ conditionalStyles
        )
        ([ [ Html.text (num |> Num.toInt |> String.fromInt) ]
         , mergeIf (status == Xed)
            [ viewX colors.b ]
         , mergeIf (status == Picked)
            [ viewX Twt.purple_500 ]
         , mergeIf (status == Passed)
            [ viewStrike colors.fg ]
         ]
            |> List.concat
        )


viewLockCell : Color -> Bool -> Html Msg
viewLockCell color xed =
    let
        colors =
            getColors color Xed
    in
    Html.div
        [ css [ Tw.w_16, Tw.h_16, Tw.relative, Tw.overflow_hidden ]
        , css [ Tw.flex, Tw.justify_center, Tw.items_center ]
        , css [ Tw.text_3xl, Tw.text_color colors.fg ]
        , css [ Tw.bg_color colors.bg ]
        , css [ Tw.border_2, Tw.border_color colors.b, Tw.rounded_full ]
        , css [ Tw.select_none ]
        ]
        ([ [ icon (Phosphor.lock Phosphor.Regular) ]
         , mergeIf xed
            [ viewX colors.b ]
         ]
            |> List.concat
        )


viewX : Twt.Color -> Svg Msg
viewX twColor =
    Svg.svg
        [ Svga.viewBox "-6 -6 12 12"
        , css [ Tw.w_full, Tw.h_full, Tw.absolute ]
        ]
        [ Svg.g
            [ css [ Tw.stroke_color twColor ]
            , Svga.strokeWidth "0.5"
            ]
            [ Svg.line [ Svga.x1 "-6", Svga.y1 "-6", Svga.x2 "6", Svga.y2 "6" ] []
            , Svg.line [ Svga.x1 "6", Svga.y1 "-6", Svga.x2 "-6", Svga.y2 "6" ] []
            ]
        ]


viewStrike : Twt.Color -> Svg Msg
viewStrike twColor =
    Svg.svg
        [ Svga.viewBox "-6 -6 12 12"
        , css [ Tw.w_full, Tw.h_full, Tw.absolute ]
        ]
        [ Svg.g
            [ css [ Tw.stroke_color twColor ]
            , Svga.strokeWidth "0.5"
            ]
            [ Svg.line [ Svga.x1 "-6", Svga.y1 "0", Svga.x2 "6", Svga.y2 "0" ] [] ]
        ]



-- VIEW FAULTS


viewFaults : Language -> Bool -> Int -> Html Msg
viewFaults language active count =
    let
        faultButtons =
            [ 1, 2, 3, 4 ]
                |> List.map (\n -> viewFaultButton active (n <= count))
    in
    Html.div [ css [ Tw.flex, Tw.flex_row, Tw.gap_1, Tw.justify_end, Tw.items_center ] ]
        ([ [ Html.div [ css [ Tw.mr_3 ] ]
                [ Html.text (Texts.for language).faults ]
           ]
         , faultButtons
         ]
            |> List.concat
        )


viewFaultButton : Bool -> Bool -> Html Msg
viewFaultButton active xed =
    let
        colors =
            getFaultColors active xed

        conditionalStyles =
            [ mergeIf xed
                [ css [ Tw.cursor_default ] ]
            , mergeIf (active && not xed)
                [ Events.onClick ClickedFault
                , activeGlow
                ]
            , mergeIf (not active && not xed)
                [ css [ Tw.cursor_not_allowed ] ]
            ]
                |> List.concat
    in
    Html.button
        ([ css [ Tw.w_8, Tw.h_8, Tw.relative, Tw.overflow_hidden ]
         , css [ Tw.flex, Tw.items_center ]
         , css [ Tw.bg_color colors.bg ]
         , css [ Tw.border_2, Tw.rounded_lg, Tw.border_color colors.fg ]
         ]
            ++ conditionalStyles
        )
        (mergeIf xed
            [ viewX colors.fg ]
        )


activeGlow : Attribute Msg
activeGlow =
    css
        [ Css.boxShadow5
            -- X translation.
            (Css.px 0)
            -- Y translation.
            (Css.px 0)
            -- Blur.
            (Css.px 0)
            -- Growth.
            (Css.px 2)
            (Css.rgb 255 255 255)
        ]



-- VIEW SCOREBOARD


viewScoreboard : Language -> Board -> Html Msg
viewScoreboard language board =
    let
        between : String -> Html Msg
        between string =
            Html.div [ css [ Tw.font_bold ] ]
                [ Html.text string ]

        colorPoints : Color -> Html Msg
        colorPoints color =
            viewScoreboardColorPoints language color (Board.row color board)
    in
    Html.div [ css [ Tw.flex, Tw.flex_row, Tw.gap_2, Tw.items_center, Tw.justify_center ] ]
        [ colorPoints Red
        , between "+"
        , colorPoints Yellow
        , between "+"
        , colorPoints Green
        , between "+"
        , colorPoints Blue
        , between "−"
        , viewScoreboardPoints language (getFaultColors True True).fg (Board.faults board) (Board.faultPoints board)
        , between "="
        , viewScoreboardSquare Twt.black
            [ Html.div [ css [ Tw.font_bold, Tw.text_2xl, Tw.text_color Twt.black ] ]
                [ Html.text (String.fromInt (Board.points board) ++ (Texts.for language).p) ]
            ]
        ]


viewScoreboardColorPoints : Language -> Color -> Row -> Html Msg
viewScoreboardColorPoints language color row =
    let
        colors =
            getColors color Available
    in
    viewScoreboardPoints
        language
        colors.fg
        (Row.xCount (Color.growth color) row)
        (Row.points (Color.growth color) row)


viewScoreboardPoints : Language -> Twt.Color -> Int -> Int -> Html Msg
viewScoreboardPoints language twColor xs points =
    viewScoreboardSquare twColor
        [ Html.div [ css [ Tw.flex, Tw.flex_row, Tw.items_center ] ]
            [ Html.text (String.fromInt xs)
            , Html.div [ css [ Css.marginBottom (Css.rem -0.05) ] ]
                [ icon (Phosphor.x Phosphor.Bold) ]
            ]
        , Html.div [ css [ Tw.font_bold, Tw.text_xl, Tw.text_color twColor, Tw.leading_none ] ]
            [ Html.text
                ("{points}{p}"
                    |> String.replace "{points}" (String.fromInt points)
                    |> String.replace "{p}" (Texts.for language).p
                )
            ]
        ]


viewScoreboardSquare : Twt.Color -> List (Html Msg) -> Html Msg
viewScoreboardSquare twColor content =
    Html.div
        [ css [ Tw.w_24, Tw.h_20 ]
        , css [ Tw.flex, Tw.flex_col, Tw.gap_1, Tw.items_center, Tw.justify_center ]
        , css [ Tw.border_4, Tw.border_color twColor, Tw.rounded_lg ]
        ]
        content



-- CELL STATUS


getCellStatus : Num.Growth -> Row -> Turn -> Color -> Num -> CellStatus
getCellStatus growth row turn color num =
    let
        isPicked =
            case turn of
                TurnPickedOnce _ _ pick ->
                    pick.color == color && pick.num == num

                _ ->
                    False

        basicAvailability =
            if Row.get num row then
                Xed

            else if cellIsAvailable growth row num then
                if Num.isLast growth num then
                    if Row.xCount growth row >= 5 then
                        Available

                    else
                        Unavailable

                else
                    Available

            else
                Passed

        availableNums =
            availableNumsByDiceThrow turn color
    in
    if isPicked then
        Picked

    else
        case basicAvailability of
            Available ->
                if List.member num availableNums then
                    Available

                else
                    Unavailable

            _ ->
                basicAvailability


availableNumsByDiceThrow : Turn -> Color -> List Num
availableNumsByDiceThrow turn color =
    case turn of
        NotTurn ->
            []

        TurnPicking diceThrow _ ->
            getWhitePicks diceThrow ++ getColoredPicks diceThrow color

        TurnPickedOnce diceThrow _ pick ->
            let
                whitePicks : List Num
                whitePicks =
                    getWhitePicks diceThrow

                coloredPicks : List Num
                coloredPicks =
                    getColoredPicks diceThrow color

                filterPicks : Bool -> List Num -> List Num
                filterPicks toTheLeft =
                    let
                        growth : Num.Growth
                        growth =
                            case ( toTheLeft, Color.growth color ) of
                                ( True, growth_ ) ->
                                    growth_

                                ( False, Num.Grows ) ->
                                    Num.Shrinks

                                ( False, Num.Shrinks ) ->
                                    Num.Grows

                        rowWithXedPick : Row
                        rowWithXedPick =
                            Row.init |> Row.set pick.num True
                    in
                    List.filter (cellIsAvailable growth rowWithXedPick)
            in
            case getFirstPickType diceThrow pick of
                FirstPickedWhite ->
                    if color == pick.color then
                        -- Remove all cells to the left.
                        filterPicks True coloredPicks

                    else
                        coloredPicks

                FirstPickedColored ->
                    if color == pick.color then
                        -- Remove all cells to the right. This is because
                        -- the rules say you have to first pick white, so the
                        -- previous move was made in the wrong order and we have
                        -- to accomodate for that.
                        filterPicks False whitePicks

                    else
                        whitePicks

                FirstPickedEither ->
                    whitePicks ++ coloredPicks


type FirstPickType
    = FirstPickedWhite
    | FirstPickedColored
    | FirstPickedEither


getFirstPickType : DiceThrow -> Pick -> FirstPickType
getFirstPickType diceThrow pick =
    let
        pickedWhite =
            List.member pick.num (getWhitePicks diceThrow)

        pickedColored =
            getColoredPicks diceThrow pick.color
                |> List.member pick.num
    in
    if pickedWhite && not pickedColored then
        FirstPickedWhite

    else if pickedColored && not pickedWhite then
        FirstPickedColored

    else
        FirstPickedEither


getWhitePicks : DiceThrow -> List Num
getWhitePicks diceThrow =
    [ addPips diceThrow.dieWhite1 diceThrow.dieWhite2 ]


getColoredPicks : DiceThrow -> Color -> List Num
getColoredPicks diceThrow color =
    case color of
        Red ->
            [ addPips diceThrow.dieWhite1 diceThrow.dieRed
            , addPips diceThrow.dieWhite2 diceThrow.dieRed
            ]

        Yellow ->
            [ addPips diceThrow.dieWhite1 diceThrow.dieYellow
            , addPips diceThrow.dieWhite2 diceThrow.dieYellow
            ]

        Green ->
            [ addPips diceThrow.dieWhite1 diceThrow.dieGreen
            , addPips diceThrow.dieWhite2 diceThrow.dieGreen
            ]

        Blue ->
            [ addPips diceThrow.dieWhite1 diceThrow.dieBlue
            , addPips diceThrow.dieWhite2 diceThrow.dieBlue
            ]


cellIsAvailable : Num.Growth -> Row -> Num -> Bool
cellIsAvailable growth row num =
    case ( Row.get num row, Num.next growth num ) of
        ( True, _ ) ->
            False

        ( False, Just n ) ->
            cellIsAvailable growth row n

        ( False, Nothing ) ->
            True



-- VIEW UTILS


icon : Phosphor.IconVariant -> Html Msg
icon iconVariant =
    iconVariant
        |> Phosphor.withSize 1
        |> Phosphor.withSizeUnit "em"
        |> Phosphor.toHtml []
        |> Html.fromUnstyled


viewButton : List (Attribute Msg) -> List (Html Msg) -> Html Msg
viewButton attributes children =
    Html.button
        ([ css [ Tw.p_2, Tw.rounded_lg ]
         , css [ Tw.text_color Twt.purple_800 ]
         , css [ Tw.bg_color Twt.purple_100 ]
         ]
            ++ attributes
        )
        children



-- UTILS


throwDiceIfGameNotEnded : Board -> Random.Seed -> Cmd Msg
throwDiceIfGameNotEnded board seed =
    if Board.gameEnded board then
        Cmd.none

    else
        throwDice seed


throwDice : Random.Seed -> Cmd Msg
throwDice seed =
    Process.sleep 100
        |> Task.perform
            (\() ->
                let
                    ( ( diceThrow, diceRotations ), newSeed ) =
                        Random.step
                            (Random.map2 Tuple.pair diceThrowGenerator diceRotationsGenerator)
                            seed
                in
                DiceThrown newSeed diceThrow diceRotations
            )


diceThrowGenerator : Random.Generator DiceThrow
diceThrowGenerator =
    Random.constant DiceThrow
        |> Random.andMap pipsGenerator
        |> Random.andMap pipsGenerator
        |> Random.andMap pipsGenerator
        |> Random.andMap pipsGenerator
        |> Random.andMap pipsGenerator
        |> Random.andMap pipsGenerator


pipsGenerator : Random.Generator Pips
pipsGenerator =
    Random.int 1 6
        |> Random.map
            (\n ->
                case n of
                    1 ->
                        Pips1

                    2 ->
                        Pips2

                    3 ->
                        Pips3

                    4 ->
                        Pips4

                    5 ->
                        Pips5

                    _ ->
                        Pips6
            )


diceRotationsGenerator : Random.Generator DiceRotations
diceRotationsGenerator =
    Random.constant DiceRotations
        |> Random.andMap diceRotationGenerator
        |> Random.andMap diceRotationGenerator
        |> Random.andMap diceRotationGenerator
        |> Random.andMap diceRotationGenerator
        |> Random.andMap diceRotationGenerator
        |> Random.andMap diceRotationGenerator


diceRotationGenerator : Random.Generator Float
diceRotationGenerator =
    Random.float 0 1


addPips : Pips -> Pips -> Num
addPips pips1 pips2 =
    case ( pips1, pips2 ) of
        ( Pips1, Pips1 ) ->
            Num2

        ( Pips1, Pips2 ) ->
            Num3

        ( Pips1, Pips3 ) ->
            Num4

        ( Pips1, Pips4 ) ->
            Num5

        ( Pips1, Pips5 ) ->
            Num6

        ( Pips1, Pips6 ) ->
            Num7

        ( Pips2, Pips2 ) ->
            Num4

        ( Pips2, Pips3 ) ->
            Num5

        ( Pips2, Pips4 ) ->
            Num6

        ( Pips2, Pips5 ) ->
            Num7

        ( Pips2, Pips6 ) ->
            Num8

        ( Pips3, Pips3 ) ->
            Num6

        ( Pips3, Pips4 ) ->
            Num7

        ( Pips3, Pips5 ) ->
            Num8

        ( Pips3, Pips6 ) ->
            Num9

        ( Pips4, Pips4 ) ->
            Num8

        ( Pips4, Pips5 ) ->
            Num9

        ( Pips4, Pips6 ) ->
            Num10

        ( Pips5, Pips5 ) ->
            Num10

        ( Pips5, Pips6 ) ->
            Num11

        ( Pips6, Pips6 ) ->
            Num12

        _ ->
            -- Bigger number is the second, so we turn them around and try again.
            addPips pips2 pips1


canAddFault : Turn -> Bool
canAddFault turn =
    case turn of
        TurnPicking _ _ ->
            True

        NotTurn ->
            False

        TurnPickedOnce _ _ _ ->
            False


mergeIf : Bool -> List a -> List a
mergeIf condition items =
    if condition then
        items

    else
        []



-- COLORS


getColors : Color -> CellStatus -> { fg : Twt.Color, bg : Twt.Color, b : Twt.Color }
getColors color status =
    case ( status, color ) of
        ( Available, Red ) ->
            { fg = Twt.red_500, bg = Twt.purple_50, b = Twt.purple_500 }

        ( Available, Yellow ) ->
            { fg = Twt.yellow_500, bg = Twt.purple_50, b = Twt.purple_500 }

        ( Available, Green ) ->
            { fg = Twt.green_500, bg = Twt.purple_50, b = Twt.purple_500 }

        ( Available, Blue ) ->
            { fg = Twt.blue_500, bg = Twt.purple_50, b = Twt.purple_500 }

        ( Picked, Red ) ->
            { fg = Twt.red_500, bg = Twt.purple_900, b = Twt.purple_500 }

        ( Picked, Yellow ) ->
            { fg = Twt.yellow_500, bg = Twt.purple_900, b = Twt.purple_500 }

        ( Picked, Green ) ->
            { fg = Twt.green_500, bg = Twt.purple_900, b = Twt.purple_500 }

        ( Picked, Blue ) ->
            { fg = Twt.blue_500, bg = Twt.purple_900, b = Twt.purple_500 }

        ( Xed, Red ) ->
            { fg = Twt.red_500, bg = Twt.red_50, b = Twt.red_700 }

        ( Xed, Yellow ) ->
            { fg = Twt.yellow_500, bg = Twt.yellow_50, b = Twt.yellow_700 }

        ( Xed, Green ) ->
            { fg = Twt.green_500, bg = Twt.green_50, b = Twt.green_700 }

        ( Xed, Blue ) ->
            { fg = Twt.blue_500, bg = Twt.blue_50, b = Twt.blue_700 }

        ( Passed, Red ) ->
            { fg = Twt.gray_200, bg = Twt.gray_50, b = Twt.red_700 }

        ( Passed, Yellow ) ->
            { fg = Twt.gray_200, bg = Twt.gray_50, b = Twt.yellow_700 }

        ( Passed, Green ) ->
            { fg = Twt.gray_200, bg = Twt.gray_50, b = Twt.green_700 }

        ( Passed, Blue ) ->
            { fg = Twt.gray_200, bg = Twt.gray_50, b = Twt.blue_700 }

        ( Unavailable, Red ) ->
            { fg = Twt.red_200, bg = Twt.red_50, b = Twt.red_700 }

        ( Unavailable, Yellow ) ->
            { fg = Twt.yellow_200, bg = Twt.yellow_50, b = Twt.yellow_700 }

        ( Unavailable, Green ) ->
            { fg = Twt.green_200, bg = Twt.green_50, b = Twt.green_700 }

        ( Unavailable, Blue ) ->
            { fg = Twt.blue_200, bg = Twt.blue_50, b = Twt.blue_700 }


getFaultColors : Bool -> Bool -> { fg : Twt.Color, bg : Twt.Color }
getFaultColors active xed =
    case ( xed, active ) of
        ( False, True ) ->
            { fg = Twt.purple_500, bg = Twt.purple_50 }

        ( _, _ ) ->
            { fg = Twt.gray_400, bg = Twt.gray_50 }


getDieColors : DieColor -> { face : Twt.Color, border : Twt.Color, pip : Twt.Color }
getDieColors dieColor =
    case dieColor of
        DieWhite ->
            { face = Twt.white, border = Twt.gray_300, pip = Twt.black }

        DieRed ->
            { face = Twt.red_500, border = Twt.red_700, pip = Twt.white }

        DieYellow ->
            { face = Twt.yellow_500, border = Twt.yellow_700, pip = Twt.white }

        DieGreen ->
            { face = Twt.green_500, border = Twt.green_700, pip = Twt.white }

        DieBlue ->
            { face = Twt.blue_500, border = Twt.blue_700, pip = Twt.white }
