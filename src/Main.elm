-----------------------
-- Gabriela Truta
-- 11.11.2020
-----------------------

module Main exposing (main, calculateScore, valueOfEachCard, possibleScores)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Browser.Navigation as Navigation


import Card exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool,
    lose: Bool,
    win: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True False False

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | NewCard Card
  | ToggleDeck
  | RestartGame


update : Msg -> Model  -> (Model, Cmd Msg)
update msg model=
  case msg of
    Draw ->
      ( model
      , if model.lose || model.win then
            Cmd.none
        else
            drawCard model
      )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      (
        if calculateScore model.hand > 21 then
            {model | lose = True}
        else if calculateScore model.hand == 21 then
            {model | win = True}
        else
            {model | hand = newCard::model.hand, deck = (removeCard newCard model.deck)}
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToggleDeck ->
      ( if model.showDeck then
            {model | showDeck = False}
       else
            {model | showDeck = True}
      , Cmd.none
      )

    -- Reloads the page in order to restart the game
    RestartGame ->
      ( model
      , Navigation.reload
      )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

-- Removes the given card from the deck
removeCard: Card -> List Card -> List Card
removeCard drawnCard deck = List.filter (\x -> x /= drawnCard) deck

{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hearts, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hearts, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}
-- Returns a list of integers with the values of each card from the provided list of cards
valueOfEachCard: List Card -> List Int
valueOfEachCard listCards =
    let
        helper: List Card -> List Int -> List Int
        helper cards values =
            case cards of
                [] -> values
                x::xs -> helper xs (List.append values (cardValue x))
    in
        helper listCards []

{-
Takes a list of integers and returns a list with 2 elements (representing the 2 possible scores). Because an Ace can have
both 1 and 11 as a value, we will have 2 possible scores. One possible score when the Ace is considered to be 11 and one
possible score when the Ace is considered to be 1. When we extract an Ace, the rest of Aces from the deck will
automatically have the value 1 to prevent the player from losing (if the player already has one Ace, the next Ace that
will be extracted can't take the value 11 as it will make the player to lose). The first score is the score when the
first Ace takes the value 1 and the second one when it takes the value 11. If the hand has no Ace, then the scores will
be identical.
-}
possibleScores: List Int -> List Int
possibleScores listValues =
    let
        adder values sum1 sum2 =
            case values of
                [] -> [sum1, sum2]
                x::xs -> if (x == 1) then adder (List.filter (\y -> y < 11) xs)  (sum1 + x) (sum1 + 11)
                       else adder xs (sum1 + x) (sum2 + x)
    in
        adder listValues 0 0

-- Returns the k-th integer from a list of integers
indexInt: Int -> List Int -> Int
indexInt k l =
    let
        iterationInt: Int -> List Int -> Int -> Int
        iterationInt index list q =
            case list of
                [] -> 0
                x::xs -> if (index == q) then x
                                    else iterationInt index xs (q + 1)
    in
        iterationInt k l 1

-- Returns the closest score next to 21 from the list of possible scores
calculateScore : List Card -> Int
calculateScore cards =
    let
        listVal = valueOfEachCard cards
        possVal = possibleScores listVal
        calcHelper list min =
            case list of
                [] -> min
                x::xs -> if (21 - x < 21 - min && 21 - x >= 0) then calcHelper xs x
                    else  calcHelper xs min
    in
        calcHelper possVal (indexInt 1 possVal)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"
    currentHand =
        model.hand
        |> List.map viewCard
    currentDeck =
        if model.showDeck then
            model.deck
            |> List.map viewCard
        else
            []
    gameStatus =
        if calculateScore model.hand > 21 then
            "You lost!"
        else if calculateScore model.hand == 21 then
            "You won!"
        else
            "Draw more cards to get to 21!"
  in
    div []
      [ div [] [ h1 [] [text appName] ]
      , div [] [b [] [text ("Status game: " ++ gameStatus)]]
      , div []  [b  [] [text  ("Current score: " ++ String.fromInt (calculateScore model.hand))]]
      , div [] [i [] [text ("Your hand: ")]]
      , ul [] currentHand
      , div [] [button [onClick Draw] [text "Draw a card"]
      , button [onClick ToggleDeck] [text "Show deck"]
      , button [onClick RestartGame] [text "Restart game"]]
      , ul [] currentDeck
      ]