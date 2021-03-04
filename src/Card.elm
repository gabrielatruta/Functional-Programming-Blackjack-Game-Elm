-----------------------
-- Gabriela Truta
-- 11.11.2020
-----------------------

module Card exposing (Card, Face(..), Suit(..), cardValue, viewCard, cardToString, deck, cardToUnicode)

import Html exposing (..)
import Html.Attributes exposing (style)

{-
  Replace with your definitions from assignment 1
-}
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Suit = Clubs | Diamonds | Hearts | Spades
type alias Card =
    { face: Face
    , suit: Suit
    }

--faceToString: Face -> String
--faceToString face = Debug.toString(face)
--
--suitToString: Suit -> String
--suitToString suit = Debug.toString(suit)

cardToString: Card -> String
cardToString card = Debug.toString(card.face) ++ " of" ++ " " ++ Debug.toString(card.suit)

cardValue : Card -> List Int
cardValue card =
    case card.face of
        Ace -> [1, 11]
        Two -> [2]
        Three -> [3]
        Four -> [4]
        Five -> [5]
        Six -> [6]
        Seven -> [7]
        Eight -> [8]
        Nine -> [9]
        Ten -> [10]
        Jack -> [10]
        Queen -> [10]
        King -> [10]

deck: List Card
deck =
    let
        faces = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
        suits = [Clubs, Diamonds, Hearts, Spades]
        deckHelper f s =
            List.concatMap (\x -> List.map ( \y -> (Card y x) ) f) s
    in
        deckHelper faces suits

{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode card =
   case card.face of
     Ace -> case card.suit of
       Spades ->"🂡"
       Hearts -> "🂱"
       Clubs ->  "🃑"
       Diamonds -> "🃁"
     Two -> case card.suit of
       Spades ->"🂢"
       Hearts -> "🂲"
       Clubs ->  "🃒"
       Diamonds -> "🃂"
     Three -> case card.suit of
       Spades ->"🂣"
       Hearts -> "🂳"
       Clubs ->  "🃓"
       Diamonds ->"🃃"
     Four -> case card.suit of
       Spades ->"🂤"
       Hearts -> "🂴"
       Clubs ->  "🃔"
       Diamonds -> "🃄"
     Five -> case card.suit of
       Spades ->"🂥"
       Hearts -> "🂵"
       Clubs ->  "🃕"
       Diamonds -> "🃅"
     Six -> case card.suit of
       Spades ->"🂦"
       Hearts -> "🂶"
       Clubs ->  "🃖"
       Diamonds -> "🃆"
     Seven -> case card.suit of
       Spades ->"🂧"
       Hearts -> "🂷"
       Clubs ->  "🃗"
       Diamonds -> "🃇"
     Eight -> case card.suit of
       Spades -> "🂨"
       Hearts ->  "🂸"
       Clubs ->   "🃘"
       Diamonds ->  "🃈"
     Nine -> case card.suit of
       Spades -> "🂩"
       Hearts ->  "🂹"
       Clubs ->   "🃙"
       Diamonds ->  "🃉"
     Ten -> case card.suit of
       Spades ->"🂪"
       Hearts -> "🂺"
       Clubs ->  "🃚"
       Diamonds -> "🃊"
     Jack -> case card.suit of
       Spades ->"🂫"
       Hearts -> "🂻"
       Clubs ->  "🃛"
       Diamonds -> "🃋"
     Queen -> case card.suit of
       Spades ->"🂭"
       Hearts -> "🂽"
       Clubs ->  "🃝"
       Diamonds -> "🃍"
     King -> case card.suit of
       Spades -> "🂮"
       Hearts -> "🂾"
       Clubs ->  "🃞"
       Diamonds -> "🃎"


{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard card =
   let
     suitColor s =
       case s of
         Diamonds -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode card
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "12em", style "color" (suitColor card.suit)] [text unicode],
       div [style "font-size" "0.8em"]  [text (cardToString card)]
     ]