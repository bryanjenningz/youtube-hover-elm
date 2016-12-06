port module Main exposing (..)

import Html exposing (programWithFlags, text, button, div, h4, i, span, Html)
import Html.Events exposing (onClick, onMouseEnter)
import Html.Attributes exposing (class, style)

type alias TranslationSub =
  { time : Float
  , line : List TranslationText
  }

type alias TranslationText =
  { text : String
  , translation : String
  }

type alias TextTime =
  { text : String
  , time : Float
  }

translationText : List TranslationText
translationText =
  [ { text = "Hola.", translation = "Hello" }
  , { text = "Como estas?", translation = "How are you?" }
  ]

japaneseText : List TranslationSub
japaneseText =
  [ { time = 0
    , line =
      [ { text = "私", translation = "I, me" }
      , { text = "は", translation = "subject particle" }
      , { text = "今日", translation = "today" }
      ]
    }
  , { time = 4.8
    , line =
      [ { text = "生まれ変わる", translation = "I will be reborn" }
      ]
    }
  ]

main =
  programWithFlags
    { init = init 
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type Msg = ChangeIndex Int | UpdateYoutubeTime Float

type alias Model =
  { hoverIndex : Int
  , youtubeTime : Float
  , translatedSubs : List
    { time : Float
    , translations : List
      { word : String
      , translation : Maybe (List String)
      }
    }
  }

type alias Flags =
  { translatedSubs : List
    { time : Float
    , translations : List
      { word : String
      , translation : Maybe (List String)
      }
    }
  }

init : Flags -> ( Model, Cmd Msg )
init flags =
  ( Model 0 0 flags.translatedSubs, Cmd.none )

view model =
  div [class "text-center"]
    [ viewTranslatedSubs model
    , h4 [] [text (getCurrentTextTime englishSubs model.youtubeTime).text]
    , h4 [] [text (getCurrentTextTime japaneseSubs model.youtubeTime).text]
    , h4 [] <|
      List.indexedMap
        (viewText model.hoverIndex)
        (getCurrentTranslationSub japaneseText model.youtubeTime).line
    ]

viewText : Int -> Int -> TranslationText -> Html Msg
viewText hoverIndex index phrase =
  span [ onMouseEnter (ChangeIndex index) ]
    [ if hoverIndex == index then
        span [ style [("position", "relative")] ]
          [ text (phrase.text ++ " ")
          , span [style translationStyle] [text phrase.translation] ]
      else
        span []
          [ text (phrase.text ++ " ") ]
    ]

--viewTranslatedSubs : List { time : Float, translations : List { word : String, translation : Maybe (List String) } } -> Float -> Html Msg
viewTranslatedSubs : Model -> Html Msg
viewTranslatedSubs model =
  let
    subsBeforeNow =
      List.filter
        (\sub -> sub.time <= model.youtubeTime)
        model.translatedSubs
    currentSub =
      Maybe.withDefault
        { time = 0, translations = [] }
        (List.head <| List.reverse subsBeforeNow)
  in
    div [] <|
      List.indexedMap
        (\index sub ->
          let 
            subTranslations =
              Maybe.withDefault [""] sub.translation
            subTranslation =
              Maybe.withDefault "" (List.head subTranslations)
          in
            span
              [ onMouseEnter (ChangeIndex index), style [ ( "position", "relative" ) ] ]
              [ text sub.word
              , if model.hoverIndex == index then
                  span [ style translationStyle ] [ text subTranslation ]
                else
                  span [] []
              ]
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

getCurrentTextTime : List TextTime -> Float -> TextTime
getCurrentTextTime subs time =
  List.foldl
    (\sub result -> if sub.time > time then result else sub)
    (Maybe.withDefault (TextTime "" 0) <| List.head subs)
    subs

getCurrentTranslationSub : List TranslationSub -> Float -> TranslationSub
getCurrentTranslationSub subs time =
  List.foldl
    (\sub result -> if sub.time > time then result else sub)
    (Maybe.withDefault (TranslationSub 0 []) <| List.head subs)
    subs

englishSubs =
  [ { time = 0, text = "Today..." }
  , { time = 4.7, text = "I will be reborn" }
  , { time = 6.2, text = "There's a reason why I woke up this early to go to Tokyo." }
  , { time = 12.1, text = "My bangs today look terrible too ..." }
  , { time = 14.1, text = "Wearing traditional Japanese footwear..." }
  , { time = 15.8, text = "I bought gifts at the store while I was drowsy." }
  , { time = 18.6, text = "Then, I headed towards the bullet-train platform" }
  , { time = 20.6, text = "Got on the train," }
  , { time = 21.7, text = "Fell asleep at the moment I got to my seat." }
  , { time = 24, text = "There's only one place I'm heading to." }
  , { time = 25.7, text = "The legendary beauty salon," }
  , { time = 27, text = "\"Ocean Tokyo\"" }
  , { time = 28.4, text = "I've asked the dream-team here to assist me in dealing with my extraordinarily damaged hair" }
  , { time = 34.3, text = "Regenerate! My spirit!" }
  , { time = 36.6, text = "Wow. Your bangs are quite something!" }
  , { time = 37.8, text = "I'm begging you to fix this!" }
  , { time = 39.7, text = "Would you like some counseling?" }
  , { time = 42.9, text = "Mr. Moru will be shampooing your hair now" }
  , { time = 45.5, text = "Hold on, Mr. Moru's going to do it? I'm so nervous!" }
  , { time = 48.4, text = "you've been filming all this time?" }
  , { time = 50.4, text = "Yes" }
  , { time = 51.6, text = "*Since nobody is watching me, I'm just going to chill here*" }
  , { time = 67.2, text = "Please sit up" }
  , { time = 68.1, text = "Thanks!" }
  , { time = 69.4, text = "Watch your step!" }
  , { time = 70.8, text = "Do you have any particular requests as to how you want your hair to look?" }
  , { time = 73.1, text = "First, I wanna fix my bangs" }
  , { time = 76.1, text = "How about color? Do you want it light? Dark?" }
  , { time = 78.5, text = "I'm in a dilemma...Is lighter better?" }
  , { time = 84.9, text = "Let's see how much I can transform myself!" }
  , { time = 86.9, text = "Has it been a while since you last cut your hair?" }
  , { time = 91.4, text = "Other than the front part, it's been a while..." }
  , { time = 93.4, text = "Your bangs..." }
  , { time = 94.2, text = "I cut it while filming one of my videos..." }
  , { time = 97.9, text = "It's quite long compared to other people right?" }
  , { time = 102.1, text = "*He can't do much besides laugh at my hair*" }
  , { time = 104.1, text = "Please make me the most handsome man in the world" }
  , { time = 108.7, text = "Your bangs are one heck of a thing (I'll take that as a compliment)" }
  , { time = 111.3, text = "*I was complimented!!!*" }
  , { time = 115.2, text = "It's disorganized and the widths are different too...." }
  , { time = 117.2, text = "It's quite a disaster." }
  , { time = 120.3, text = "Do as you please." }
  , { time = 129.4, text = "(His Yu-Gi-Oh cards...)" }
  , { time = 134.2, text = "3 years later" }
  , { time = 139.5, text = "I look like a delinquent!" }
  , { time = 143.1, text = "(To put color in my hair, we first bleached it)" }
  , { time = 144.6, text = "Last year, I had blonde/bleached hair for a while" }
  , { time = 146.6, text = "Brings back memories. It was actually similar to this color" }
  , { time = 149.2, text = "(Sorry, I felt so comfortable that I fell asleep)" }
  , { time = 157.1, text = "He's fast asleep" }
  , { time = 158.5, text = "3 more years later" }
  , { time = 160.4, text = "Rinsing my hair out" }
  , { time = 164, text = "And this is the result..." }
  , { time = 167.9, text = "Wow!!Thanks!" }
  , { time = 171.1, text = "What an amazing transformation!" }
  , { time = 174.6, text = "What color is this? It's looks like a pinkish color" }
  , { time = 177.7, text = "You seem nervous!" }
  , { time = 179.3, text = "Of course I'm nervous!" }
  , { time = 181.3, text = "(My best pose)" }
  , { time = 184.3, text = "We actually cut a big chunk of your hair" }
  , { time = 186.4, text = "Thank you very much!" }
  , { time = 194.1, text = "So, without incident, I was able to fix my terrible hairstyle" }
  , { time = 199, text = "Thank you for making me look so much better!" }
  , { time = 201.4, text = "How were my bangs?" }
  , { time = 202.6, text = "It was hard, so I'd like you to grow it out a little" }
  , { time = 205.5, text = "I will" }
  , { time = 207.6, text = "To all the viewers, come visit us at OceanTokyo Harajuku!" }
  , { time = 210.5, text = "We'll be waiting!" }
  , { time = 220.1, text = "This is so cautious" }
  , { time = 221.2, text = "I'm not being cautious!" }
  , { time = 222.7, text = "Feeling shy suddenly..." }
  ]

japaneseSubs =
  [ { time = 0, text = "私は今日" }
  , { time = 4.8, text = "生まれ変わる" }
  , { time = 6.2, text = "こんなに朝早く起きて" }
  , { time = 7.5, text = "東京に行くのには理由がある。" }
  , { time = 12.7, text = "今日もひどい前髪だ" }
  , { time = 14.4, text = "下駄を履いている" }
  , { time = 15.5, text = "ひどい睡魔とともに私は" }
  , { time = 16.9, text = "お土産屋さんでお土産を購入した。" }
  , { time = 18.5, text = "そのまま新幹線乗り場に向かい" }
  , { time = 20.2, text = "新幹線に乗る。" }
  , { time = 21.5, text = "席に座った瞬間…" }
  , { time = 22.8, text = "すぐ寝る。" }
  , { time = 24.4, text = "私が向かう先はただ一つ" }
  , { time = 25.9, text = "伝説の美容室" }
  , { time = 28.7, text = "この荒れ果てた髪型を修繕すべく" }
  , { time = 30.8, text = "最強の美容師軍団に" }
  , { time = 32.2, text = "髪の毛カットを依頼することにした。" }
  , { time = 34.2, text = "生まれ変われ！" }
  , { time = 35.2, text = "俺の！" }
  , { time = 35.8, text = "魂！" }
  , { time = 37.8, text = "いやほんと、なんとかして下さいちょっと" }
  , { time = 40, text = "あ、何か" }
  , { time = 40.5, text = "カウンセリング資格もいいすかね（？）" }
  , { time = 41.9, text = "あっカウンセリングですか？" }
  , { time = 42.7, text = "あっもう" }
  , { time = 43.2, text = "＄＃＠％" }
  , { time = 43.9, text = "シャンプー行きましょ" }
  , { time = 44.7, text = "えっもるさんがすんの？" }
  , { time = 45.9, text = "はい" }
  , { time = 46.4, text = "わっやべぇ緊張する" }
  , { time = 47.1, text = "めっちゃ久しぶりっすね" }
  , { time = 48, text = "いやぁもるさん久しぶりだねぇ" }
  , { time = 49, text = "＄＃＠％" }
  , { time = 49.5, text = "これずっと回してるんすか？" }
  , { time = 50.3, text = "うんｗずっと回してるｗ" }
  , { time = 62.1, text = "あ〜やべ" }
  , { time = 62.7, text = "シャンプーしていきます" }
  , { time = 63.3, text = "よろしくお願いします。" }
  , { time = 64, text = "お願いします。" }
  , { time = 67.2, text = "起こしていきます。" }
  , { time = 67.9, text = "ありがとうございます。" }
  , { time = 71, text = "どんな感じにしたいとかありますか？" }
  , { time = 73.1, text = "えーと、ひとまず〜" }
  , { time = 74.3, text = "前髪をなんとかしたいという" }
  , { time = 75.8, text = "色は明るい方がいい暗い方がいいありますか？" }
  , { time = 78.3, text = "迷ってるんですよ〜" }
  , { time = 79.6, text = "明るいほうが良いのかなー？" }
  , { time = 81.4, text = "お願いします。" }
  , { time = 81.9, text = "はい　よろしくお願いします。" }
  , { time = 86.6, text = "切るのも久々な感じですね" }
  , { time = 88.5, text = "久々ですね〜" }
  , { time = 91.5, text = "あぁ！すみません" }
  , { time = 92, text = "＄＃＠％" }
  , { time = 92.5, text = "前髪以外は久しぶりです…" }
  , { time = 93.5, text = "前髪…これは…" }
  , { time = 94.3, text = "動画で持ってかれて…" }
  , { time = 98.2, text = "結構伸びた方ですよでも" }
  , { time = 99.9, text = "ここ激軽(?)ですね" }
  , { time = 103.5, text = "もうっ　あの" }
  , { time = 104.6, text = "世界一のイケメンにして下さい" }
  , { time = 109, text = "前髪すごいやばいっすね" }
  , { time = 112.8, text = "あぁ　これは、" }
  , { time = 113.7, text = "やばいなぁ" }
  , { time = 114.2, text = "揃ってるのが多分よろしくないですよね" }
  , { time = 116.1, text = "幅も全然違うんで" }
  , { time = 117.4, text = "結構やばめです" }
  , { time = 121.1, text = "お気の向くままに" }
  , { time = 139.6, text = "うわ！うわうわ" }
  , { time = 140.6, text = "ヤンキーヤンキーヤンキー" }
  , { time = 142.2, text = "ヤンキーヤンキーｗ" }
  , { time = 144.4, text = "うわー" }
  , { time = 145, text = "僕、去年金髪だったんでしばらく" }
  , { time = 147, text = "うわー懐いなーこれくらいだったなー確か" }
  , { time = 157.4, text = "就寝です" }
  , { time = 168, text = "おー！ありがとうございます！" }
  , { time = 169.8, text = "すごい！前髪がなくてもここまで人は…" }
  , { time = 172.4, text = "進化できるんですね！" }
  , { time = 173.5, text = "おおー！" }
  , { time = 174.3, text = "おおー！" }
  , { time = 175, text = "な、何色？" }
  , { time = 175.6, text = "ピンクっぽい" }
  , { time = 178.2, text = "緊張されてると逆にやりずらいっていうｗ" }
  , { time = 180.1, text = "緊張しますよ！もう(照)" }
  , { time = 184.1, text = "結構バッサリ短くしました。" }
  , { time = 185.9, text = "ありがとうございます！お疲れ様でした。" }
  , { time = 191.9, text = "はい！どうも〜" }
  , { time = 193.6, text = "というわけでですねえっと〜" }
  , { time = 194.8, text = "とんでもねえ髪型を" }
  , { time = 196.5, text = "無事なんとか" }
  , { time = 197.7, text = "かなり！" }
  , { time = 198.7, text = "カッコよくさせていただきました" }
  , { time = 199.8, text = "本当に今日はありがとうございました。" }
  , { time = 201.5, text = "どうでしたかこの前髪は" }
  , { time = 202.8, text = "いやぁ 難しかったねぇ" }
  , { time = 204.2, text = "まぁ ぜひ伸ばして下さい。" }
  , { time = 205.3, text = "頑張ります。" }
  , { time = 206, text = "この機会に是非みなさん" }
  , { time = 207.3, text = "OCEAN TOKYO Harajuku 遊びに来て下さい。" }
  , { time = 209.4, text = "是非！是非！" }
  , { time = 209.9, text = "是非！" }
  , { time = 210.4, text = "お待ちしてます！" }
  , { time = 217, text = "あれ？" }
  , { time = 220.4, text = "もう使う気ですねこれ" }
  , { time = 221.5, text = "いやっ使わないっす" }
  ]
