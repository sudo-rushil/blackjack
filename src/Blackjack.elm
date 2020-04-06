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
    { hand : List Card, cash : Int, bet : Int}


score : Player -> Int
score player =
    let
        raw : List Int
        raw =
            List.map cardValue player.hand

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
    | End


viewGame : Game -> Html Msg
viewGame gameState =
    let
        ( color, state ) =
            case gameState of
                Play ->
                    ( "black"
                    , " "
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

                End ->
                    ( "red"
                    , "Game Over"
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
    ( Model (Player [] 100 0) (Player [] 0 0) [] Play
    , Random.generate Start initDeck
    )



-- UPDATE


type Msg
    = Start (List Card)
    | New
    | Bet Int
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

        Bet value ->
            ( bet value model
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
            ( game <| stand model
            , Cmd.none
            )



-- GAME
-- All actions move cards around within the model, so can all be
-- represented by transformations between models


bet : Int -> Model -> Model
bet value model =
    let
        you : Player
        you = model.you
    in
    { model
        | you = { you
            | cash = you.cash - value, bet = you.bet + value
        }
    }

hit : Model -> Model
hit model =
    let
        you : Player
        you = model.you
    in
    case model.deck of
        c :: cd ->
            { model
                | you = { you | hand = List.foldr (::) [ c ] you.hand }
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

        dealerHand : List Card
        dealerHand = List.filter ((/=) Back) model.dealer.hand

        dealer : Player
        dealer = model.dealer
    in
    if dealerScore < 17 then
        case model.deck of
            c :: cd ->
                stand
                    { model
                        | dealer = { dealer | hand = List.foldr (::) [ c ] dealerHand }
                        , deck = cd
                    }

            _ ->
                model

    else
        model


new : Model -> Model
new model =
    let
        playerHand : List Card
        playerHand =
            model.you.hand

        dealerHand : List Card
        dealerHand =
            List.filter ((/=) Back) model.dealer.hand
    in
    case model.deck of
        c :: c2 :: c3 :: cd ->
            { model
                | you = Player [ c, c2 ] (model.you.cash - 1) 1
                , dealer = Player [ c3, Back ] 0 0
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

        you : Player
        you = model.you
    in
    if you.cash < 0 then
        { model | state = End }

    else if playerScore > 21 then
        { model
            | state = Bust
            , you = { you | cash = you.cash , bet = 0}
        }

    else if playerScore > dealerScore || dealerScore > 21 then
        { model
            | state = Win
            , you = { you | cash = you.cash + 2 * you.bet , bet = 0}
        }

    else if playerScore == dealerScore then
        { model
            | state = Draw
            , you = { you | cash = you.cash + you.bet , bet = 0}
        }

    else
        { model
            | state = Lose
            , you = { you | cash = you.cash , bet = 0}
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    if model.state /= End then
        div []
            [ viewHand model.dealer "Dealer"
            , viewHand model.you "Player"
            , viewControls
            , viewBets
            , viewAccount model.you
            , div [] [ viewGame model.state ]
            ]
    else
        div []
            [ viewHand model.you "Player"
            , viewHand model.dealer "Dealer"
            , viewAccount model.you
            , div [] [ viewGame model.state ]
            ]



viewHand : Player -> String -> Html Msg
viewHand player string =
    div []
        [ text (string ++ " Score: " ++ (String.fromInt <| score player))
        , div [] (List.map viewCard player.hand)
        ]

viewAccount : Player -> Html Msg
viewAccount you =
    div []
        [ div [] [text ( "Cash: $" ++ (String.fromInt <| you.cash))]
        , div [] [text ( "Bet value: $" ++ (String.fromInt <| you.bet))]
        ]


viewControls : Html Msg
viewControls =
    ul []
        [ li [ class "elem" ] [ button [ onClick Hit ] [ text "Hit" ] ]
        , li [ class "elem" ] [ button [ onClick Dealer ] [ text "Stand" ] ]
        , li [ class "elem" ] [ button [ onClick New ] [ text "New" ] ]
        ]

viewBets : Html Msg
viewBets =
    ul []
        [ li [ class "elem" ] [ button [ onClick (Bet 1) ] [ text "$1" ] ]
        , li [ class "elem" ] [ button [ onClick (Bet 5) ] [ text "$5" ] ]
        , li [ class "elem" ] [ button [ onClick (Bet 10) ] [ text "$10" ] ]
        ]
