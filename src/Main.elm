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

type Msg = ChangeIndex Int | UpdateYoutubeTime Float

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

type alias Model =
  { hoverIndex : Int
  , youtubeTime : Float
  , timeTranslations : List TimeTranslation
  , textTimes : List TextTime
  }

type alias Flags =
  { timeTranslations : List TimeTranslation
  , textTimes : List TextTime
  }

init : Flags -> ( Model, Cmd Msg )
init flags =
  ( Model -1 0 flags.timeTranslations flags.textTimes, Cmd.none )

view model =
  div [class "text-center"]
    [ h4 [] [text (getCurrentTextTime model.textTimes model.youtubeTime).text]
    , viewTranslatedSubs model
    ]

getCurrentTextTime : List TextTime -> Float -> TextTime
getCurrentTextTime subs time =
  List.foldl
    (\sub result -> if sub.time > time then result else sub)
    (Maybe.withDefault (TextTime "" 0) <| List.head subs)
    subs

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

port youtubeTime : (Float -> msg) -> Sub msg

subscriptions model =
  youtubeTime UpdateYoutubeTime
