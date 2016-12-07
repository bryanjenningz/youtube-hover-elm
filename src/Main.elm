port module Main exposing (..)

import Html exposing (programWithFlags, text, button, div, h4, i, span, Html)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Html.Attributes exposing (class, style)

main =
  programWithFlags
    { init = init 
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Msg = ChangeIndex Int | UpdateYoutubeTime Float | AddCard Float Int

type alias TextTime =
  { text : String
  , time : Float
  }

type alias WordTranslation =
  { word : String
  , translation : Maybe (List String)
  }

type alias TimeTranslation =
  { time : Float
  , translations : List WordTranslation
  }

-- Time of the video and index of the selected word.
type alias CardTimeIndex =
  { time : Float
  , index : Int
  }

type alias Model =
  { hoverIndex : Int
  , youtubeTime : Float
  , timeTranslations : List TimeTranslation
  , textTimes : List TextTime
  , cards : List CardTimeIndex
  }

type alias Flags =
  { timeTranslations : List TimeTranslation
  , textTimes : List TextTime
  }

init : Flags -> ( Model, Cmd Msg )
init flags =
  ( Model -1 0 flags.timeTranslations flags.textTimes [], Cmd.none )

view model =
  div []
    [ div [ class "col-sm-8" ]
      [ div [ class "text-center" ]
        [ viewTextTime model
        , viewTranslatedSubs model
        ]
      ]
    , div [ class "col-sm-4" ]
      [ viewCards model ]
    ]

viewTextTime : Model -> Html Msg
viewTextTime model =
  let
    currentTextTime =
      List.foldl
      (\textTime result -> if textTime.time <= model.youtubeTime then textTime else result)
      (Maybe.withDefault (TextTime "" 0) <| List.head model.textTimes)
      model.textTimes
  in
    h4 []
      [ text currentTextTime.text ]

viewTranslatedSubs : Model -> Html Msg
viewTranslatedSubs model =
  let
    subsBeforeNow =
      List.filter
        (\sub -> sub.time <= model.youtubeTime)
        model.timeTranslations
    currentSub =
      Maybe.withDefault
        { time = 0, translations = [] }
        (List.head <| List.reverse subsBeforeNow)
  in
    h4 [] <|
      List.indexedMap
        (\index sub ->
          let 
            subTranslations =
              Maybe.withDefault [""] sub.translation
            subTranslation =
              Maybe.withDefault "" (List.head subTranslations)
          in
            span
              [ onMouseEnter (ChangeIndex index)
              , onMouseLeave (ChangeIndex -1)
              , onClick (AddCard model.youtubeTime index)
              , style <|
                [ ( "position", "relative" ) ] ++
                (if model.hoverIndex == index then [ ( "background", "cyan" ) ] else [])
              ]
              ( [ text sub.word ] ++
                ( if model.hoverIndex == index && String.length subTranslation > 0 then
                    [ span [ style translationStyle ] [ text subTranslation ] ]
                  else
                    []
                )
              )
        )
        currentSub.translations

viewCards : Model -> Html Msg
viewCards model =
  div []
    [ text (toString model.cards) ]

latestBefore : Float -> List { time : Float } -> { time : Float } -> { time : Float }
latestBefore time blocks init =
  let
    blocksBefore =
      List.filter
        (\block -> block.time <= time)
        blocks
    latestBlock =
      Maybe.withDefault init <|
        List.head <| List.reverse blocksBefore
  in
    latestBlock

translationStyle =
  [ ("background", "cyan")
  , ("position", "absolute")
  , ("top", "20px")
  , ("left", "0")
  , ("padding", "10px")
  ]

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    ChangeIndex index ->
      ( { model | hoverIndex = index }, Cmd.none )

    UpdateYoutubeTime time ->
      ( { model | youtubeTime = time }, Cmd.none )

    AddCard time index ->
      ( { model | cards = model.cards ++ [ CardTimeIndex time index ] }, Cmd.none )

port youtubeTime : (Float -> msg) -> Sub msg

subscriptions model =
  youtubeTime UpdateYoutubeTime
