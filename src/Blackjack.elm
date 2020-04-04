module Blackjack exposing (..)

import Browser
import Random
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Random.List exposing (shuffle)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- CARDS


type Suit
    = Spades
    | Diamonds
    | Clubs
    | Hearts


type Card
    = Card Suit Int
    | Back


viewCard : Card -> Html Msg
viewCard card =
    case card of
        Card Spades face ->
            span
                [ style "color" "black"
                , style "font-size" "10em"
                ]
                [ text (viewFace 0x0001F0A0 face) ]

        Card Diamonds face ->
            span
                [ style "color" "red"
                , style "font-size" "10em"
                ]
                [ text (viewFace 0x0001F0B0 face) ]

        Card Clubs face ->
            span
                [ style "color" "black"
                , style "font-size" "10em"
                ]
                [ text (viewFace 0x0001F0C0 face) ]

        Card Hearts face ->
            span
                [ style "color" "red"
                , style "font-size" "10em"
                ]
                [ text (viewFace 0x0001F0D0 face) ]

        Back ->
            span
                [ style "color" "black"
                , style "font-size" "10em"
                ]
                [ text (String.fromChar <| Char.fromCode 0x0001F0A0) ]


viewFace : Int -> Int -> String
viewFace suit face =
    Char.fromCode (suit + face)
        |> String.fromChar


initSuit : Suit -> List Card
initSuit suit =
    List.map (Card suit) <| List.range 1 13


initDeck : Random.Generator (List Card)
initDeck =
    let
        suits : List Suit
        suits =
            [ Spades, Diamonds, Clubs, Hearts ]
    in
    List.map initSuit suits
        |> List.concat
        |> shuffle


cardValue : Card -> Int
cardValue card =
    case card of
        Card _ face ->
            if face > 10 then
                10

            else if face == 1 then
                11

            else
                face

        _ ->
            0



-- PLAYER


type alias Player =
    List Card


score : Player -> Int
score hand =
    let
        raw : List Int
        raw =
            List.map cardValue hand

        rawScore : Int
        rawScore =
            List.sum raw
    in
    if rawScore > 21 && List.member 11 raw then
        rawScore - 10

    else
        rawScore



-- MODEL


type Game
    = Play
    | Win
    | Draw
    | Lose
    | Bust


viewGame : Game -> Html Msg
viewGame gameState =
    let
        ( color, state ) =
            case gameState of
                Play ->
                    ( "black"
                    , ""
                    )

                Win ->
                    ( "green"
                    , "You Win!"
                    )

                Draw ->
                    ( "black"
                    , "Draw"
                    )

                Lose ->
                    ( "red"
                    , "You Lost"
                    )

                Bust ->
                    ( "red"
                    , "You Busted"
                    )
    in
    span
        [ style "color" color ]
        [ text state ]


type alias Model =
    { you : Player
    , dealer : Player
    , deck : List Card
    , state : Game
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] [] [] Play
    , Random.generate Start initDeck
    )



-- UPDATE


type Msg
    = Start (List Card)
    | New
    | Hit
    | Dealer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start shuffledDeck ->
            ( new { model | deck = shuffledDeck }
            , Cmd.none
            )

        New ->
            ( new model
            , Cmd.none
            )

        Hit ->
            let
                newModel : Model
                newModel =
                    hit model
            in
            if score newModel.you > 21 then
                update Dealer { newModel | state = Bust }

            else
                ( newModel
                , Cmd.none
                )

        Dealer ->
            ( stand model
            , Cmd.none
            )



-- GAME
-- All actions move cards around within the model, so can all be
-- represented by transformations between models


hit : Model -> Model
hit model =
    case model.deck of
        c :: cd ->
            { model
                | you = List.foldr (::) [ c ] model.you
                , deck = cd
            }

        _ ->
            model


stand : Model -> Model
stand model =
    let
        dealerScore : Int
        dealerScore =
            score model.dealer

        dealer : Player
        dealer = List.filter ((/=) Back) model.dealer
    in
    if dealerScore < 17 then
        case model.deck of
            c :: cd ->
                game <|
                    stand
                        { model
                            | dealer = List.foldr (::) [ c ] dealer
                            , deck = cd
                        }

            _ ->
                model

    else
        model


new : Model -> Model
new model =
    let
        playerHand : Player
        playerHand =
            model.you

        dealerHand : Player
        dealerHand =
            List.filter ((/=) Back) model.dealer
    in
    case model.deck of
        c :: c2 :: c3 :: cd ->
            { model
                | you = [ c, c2 ]
                , dealer = [ c3, Back ]
                , deck = cd ++ playerHand ++ dealerHand
                , state = Play
            }

        _ ->
            model


game : Model -> Model
game model =
    let
        playerScore : Int
        playerScore =
            score model.you

        dealerScore : Int
        dealerScore =
            score model.dealer
    in
    if playerScore > 21 then
        { model | state = Bust }

    else if playerScore > dealerScore || dealerScore > 21 then
        { model | state = Win }

    else if playerScore == dealerScore then
        { model | state = Draw }

    else
        { model | state = Lose }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewControls
        , viewHand model.you "Player"
        , viewHand model.dealer "Dealer"
        , div [] [ viewGame model.state ]
        ]


viewHand : Player -> String -> Html Msg
viewHand player string =
    div []
        [ text (string ++ " Score: " ++ (String.fromInt <| score player))
        , div [] (List.map viewCard player)
        ]


viewControls : Html Msg
viewControls =
    ul []
        [ li [ class "elem" ] [ button [ onClick Hit ] [ text "Hit" ] ]
        , li [ class "elem" ] [ button [ onClick Dealer ] [ text "Stand" ] ]
        , li [ class "elem" ] [ button [ onClick New ] [ text "New" ] ]
        ]
