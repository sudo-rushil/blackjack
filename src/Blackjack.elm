module Blackjack exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Random
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
    let
        ( color, face ) =
            case card of
                Card Spades value ->
                    ( "black", viewFace 0x0001F0A0 value )

                Card Diamonds value ->
                    ( "red", viewFace 0x0001F0B0 value )

                Card Clubs value ->
                    ( "black", viewFace 0x0001F0C0 value )

                Card Hearts value ->
                    ( "red", viewFace 0x0001F0D0 value )

                Back ->
                    ( "black", viewFace 0x0001F0A0 0 )
    in
    span
        [ style "color" color
        , style "font-size" "10em"
        , style "line-height" "1.2"
        ]
        [ text face ]


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
    { hand : List Card, cash : Int, bet : Int }


type alias Dealer =
    List Card


type alias Deck =
    List Card


score : List Card -> Int
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


type Outcome
    = Win
    | Draw
    | Lose
    | Bust


type alias Game =
    { you : Player
    , dealer : Dealer
    , deck : Deck
    }


type Model
    = Landing Deck
    | Betting Game
    | Playing Game
    | Scoring Game Outcome
    | End Game


init : () -> ( Model, Cmd Msg )
init _ =
    ( Landing []
    , Random.generate Start initDeck
    )


getDeck : Model -> Deck
getDeck model =
    case model of
        Landing deck ->
            deck

        Betting game ->
            game.deck

        Playing game ->
            game.deck

        Scoring game outcome ->
            game.deck

        End game ->
            game.deck


getGame : Model -> Game
getGame model =
    case model of
        Betting game ->
            game

        Playing game ->
            game

        Scoring game _ ->
            game

        _ ->
            -- Never reached
            { you = { hand = [], cash = 0, bet = 0 }, dealer = [], deck = [] }



-- UPDATE


type Move
    = NewGame
    | BetHand
    | PlayHand
    | ScoreHand
    | Home


type Action
    = Bet Int
    | Hit
    | Double
    | Stand


type Msg
    = Start (List Card)
    | Moving Move
    | Doing Action


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start shuffledDeck ->
            ( Landing shuffledDeck
            , Cmd.none
            )

        Moving moveTo ->
            updateMoving moveTo model

        Doing action ->
            updateAction action model


updateMoving : Move -> Model -> ( Model, Cmd Msg )
updateMoving moveTo model =
    let
        deck : Deck
        deck =
            getDeck model

        game : Game
        game =
            getGame model
    in
    case moveTo of
        NewGame ->
            ( Betting <| initGame deck
            , Cmd.none
            )

        BetHand ->
            let
                outcome =
                    case model of
                        Scoring _ result ->
                            result

                        _ ->
                            -- Never reached
                            Win
            in
            ( Betting <| nextGame game outcome
            , Cmd.none
            )

        PlayHand ->
            if game.you.bet > 0 then
                -- Have to bet before playing
                ( Playing game
                , Cmd.none
                )

            else
                ( model
                , Cmd.none
                )

        ScoreHand ->
            let
                result : Outcome
                result =
                    gameOutcome game

                gameOver : Bool
                gameOver =
                    isGameOver game result
            in
            if gameOver then
                ( End game
                , Cmd.none
                )

            else
                ( Scoring game result
                , Cmd.none
                )

        Home ->
            ( Landing []
            , Random.generate Start initDeck
            )


updateAction : Action -> Model -> ( Model, Cmd Msg )
updateAction action model =
    let
        game : Game
        game =
            getGame model
    in
    case action of
        Bet value ->
            ( Betting <| makeBet game value
            , Cmd.none
            )

        Hit ->
            let
                newGame : Game
                newGame =
                    hit game
            in
            if score newGame.you.hand > 21 then
                -- If player busts, hand ends
                update (Moving ScoreHand) (Playing newGame)

            else
                ( Playing newGame
                , Cmd.none
                )

        Double ->
            if game.you.cash >= game.you.bet then
                update (Moving ScoreHand) (double game)

            else
                ( model
                , Cmd.none
                )

        Stand ->
            update (Moving ScoreHand) (stand game)



-- GAME


initGame : Deck -> Game
initGame deck =
    case deck of
        c :: c2 :: c3 :: cd ->
            { you =
                { hand = [ c, c2 ]
                , cash = 100
                , bet = 0
                }
            , dealer = [ c3, Back ]
            , deck = cd
            }

        _ ->
            -- Never reached
            Game { hand = [], cash = 0, bet = 0 } [] []


makeBet : Game -> Int -> Game
makeBet game value =
    let
        player : Player
        player =
            game.you
    in
    { game | you = playerBet player value }


playerBet : Player -> Int -> Player
playerBet player value =
    if player.cash >= value then
        -- Stop invalid bets
        { player
            | cash = player.cash - value
            , bet = player.bet + value
        }

    else
        player


hit : Game -> Game
hit game =
    let
        player : Player
        player =
            game.you
    in
    case game.deck of
        c :: cd ->
            { game
                | you = { player | hand = cardToEnd player.hand c }
                , deck = cd
            }

        _ ->
            -- Never reached
            game


stand : Game -> Model
stand game =
    let
        dealerPlays : Game
        dealerPlays =
            playDealer game
    in
    Playing dealerPlays


double : Game -> Model
double game =
    let
        player : Player
        player =
            game.you

        newPlayer : Player
        newPlayer =
            { player
                | bet = player.bet * 2
                , cash = player.cash - player.bet
            }

        newGame : Game
        newGame =
            { game | you = newPlayer }
    in
    hit newGame
        |> stand


playDealer : Game -> Game
playDealer game =
    let
        dealerScore : Int
        dealerScore =
            score game.dealer

        dealer : Dealer
        dealer =
            List.filter ((/=) Back) game.dealer
    in
    if dealerScore < 17 then
        -- Dealer will hit soft 17s
        case game.deck of
            c :: cd ->
                playDealer
                    { game
                        | dealer = cardToEnd dealer c
                        , deck = cd
                    }

            _ ->
                -- Never reached
                game

    else
        game


gameOutcome : Game -> Outcome
gameOutcome game =
    let
        playerScore : Int
        playerScore =
            score game.you.hand

        dealerScore : Int
        dealerScore =
            score game.dealer
    in
    if playerScore > 21 then
        Bust

    else if dealerScore > 21 then
        Win

    else if playerScore == dealerScore then
        Draw

    else if playerScore > dealerScore then
        Win

    else
        Lose


isGameOver : Game -> Outcome -> Bool
isGameOver game outcome =
    if outcome == Bust || outcome == Lose then
        game.you.bet > game.you.cash

    else
        False


nextGame : Game -> Outcome -> Game
nextGame game outcome =
    let
        player : Player
        player =
            getBets game.you outcome

        playerHand : Deck
        playerHand =
            game.you.hand

        dealerHand : Deck
        dealerHand =
            List.filter ((/=) Back) game.dealer
    in
    case game.deck of
        c :: c2 :: c3 :: cd ->
            { game
                | you =
                    { player
                        | hand = [ c, c2 ]
                    }
                , dealer = [ c3, Back ]
                , deck = cd ++ playerHand ++ dealerHand
            }

        _ ->
            -- Never reached
            game


getBets : Player -> Outcome -> Player
getBets player outcome =
    case outcome of
        Win ->
            { player
                | cash = player.cash + 2 * player.bet
                , bet = 0
            }

        Draw ->
            { player
                | cash = player.cash + player.bet
                , bet = 0
            }

        Lose ->
            { player
                | cash = player.cash - player.bet
                , bet = 0
            }

        Bust ->
            { player
                | cash = player.cash - player.bet
                , bet = 0
            }


cardToEnd : Deck -> Card -> Deck
cardToEnd deck card =
    List.foldr (::) [ card ] deck



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Landing _ ->
            viewLanding

        Betting game ->
            viewBetting game

        Playing game ->
            viewPlaying game

        Scoring game outcome ->
            viewScoring game outcome

        End game ->
            viewEnd game


viewLanding : Html Msg
viewLanding =
    div []
        [ viewCard (Card Spades 1)
        , div [] [ button [ onClick <| Moving NewGame ] [ text "Start Game" ] ]
        ]


viewGame : Game -> Html Msg
viewGame game =
    div []
        [ viewDealer game.dealer
        , viewPlayer game.you
        , viewAccount game.you
        , br [] []
        ]


viewBetting : Game -> Html Msg
viewBetting game =
    div []
        [ viewGame game
        , viewBets
        , div [] [ button [ onClick <| Moving PlayHand ] [ text "Play" ] ]
        ]


viewPlaying : Game -> Html Msg
viewPlaying game =
    div []
        [ viewGame game
        , viewControls
        ]


viewScoring : Game -> Outcome -> Html Msg
viewScoring game outcome =
    div []
        [ viewGame game
        , viewOutcome outcome
        , br [] []
        , div [] [ button [ onClick <| Moving BetHand ] [ text "Play Again" ] ]
        , div [] [ button [ onClick <| Moving Home ] [ text "Exit" ] ]
        ]


viewDealer : Dealer -> Html Msg
viewDealer dealer =
    div []
        [ text ("Dealer Score: " ++ (String.fromInt <| score dealer))
        , div [] (List.map viewCard dealer)
        ]


viewPlayer : Player -> Html Msg
viewPlayer player =
    div []
        [ text ("Player Score: " ++ (String.fromInt <| score player.hand))
        , div [] (List.map viewCard player.hand)
        ]


viewAccount : Player -> Html Msg
viewAccount you =
    div []
        [ div [] [ text ("Cash: $" ++ (String.fromInt <| you.cash)) ]
        , div [] [ text ("Bet value: $" ++ (String.fromInt <| you.bet)) ]
        ]


viewControls : Html Msg
viewControls =
    ul []
        [ li [ class "elem" ] [ button [ onClick <| Doing Hit ] [ text "Hit" ] ]
        , li [ class "elem" ] [ button [ onClick <| Doing Double ] [ text "Double" ] ]
        , li [ class "elem" ] [ button [ onClick <| Doing Stand ] [ text "Stand" ] ]
        ]


viewBets : Html Msg
viewBets =
    ul []
        [ li [ class "elem" ] [ button [ onClick (Doing <| Bet 1) ] [ text "$1" ] ]
        , li [ class "elem" ] [ button [ onClick (Doing <| Bet 5) ] [ text "$5" ] ]
        , li [ class "elem" ] [ button [ onClick (Doing <| Bet 10) ] [ text "$10" ] ]
        ]


viewEnd : Game -> Html Msg
viewEnd game =
    div []
        [ viewGame game
        , div [] [ text "Game Over!" ]
        , div [] [ button [ onClick <| Moving Home ] [ text "Exit" ] ]
        ]


viewOutcome : Outcome -> Html Msg
viewOutcome outcome =
    let
        ( color, string ) =
            case outcome of
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
                    , "You Lose"
                    )

                Bust ->
                    ( "red"
                    , "You Busted"
                    )
    in
    span
        [ style "color" color ]
        [ text string ]
