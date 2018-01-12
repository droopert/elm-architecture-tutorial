{- OVERVIEW ------------------------------------------------------

   A "Tree" represents a binary tree. A "Node" in a binary tree
   always has two children. A tree can also be "Empty". Below I have
   defined "Tree" and a number of useful functions.

   This example also includes some challenge problems!

   ----------------------------------------------------------------
-}


module Main exposing (..)

import Html exposing (Html, div, h1, h2, h3, p, text)
import Html.Attributes exposing (style)


-- TREES


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


empty : Tree a
empty =
    Empty


singleton : a -> Tree a
singleton v =
    Node v Empty Empty


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
        Empty ->
            singleton x

        Node y left right ->
            if x > y then
                Node y left (insert x right)
            else if x < y then
                Node y (insert x left) right
            else
                tree


fromList : List comparable -> Tree comparable
fromList xs =
    List.foldl insert empty xs


depth : Tree a -> Int
depth tree =
    case tree of
        Empty ->
            0

        Node v left right ->
            1 + max (depth left) (depth right)


map : (a -> b) -> Tree a -> Tree b
map f tree =
    case tree of
        Empty ->
            Empty

        Node v left right ->
            Node (f v) (map f left) (map f right)


fold : (a -> b -> b) -> b -> Tree a -> b
fold fn accumulator tree =
    case tree of
        Empty ->
            accumulator

        Node value left right ->
            -- I couldn't get this right, so credit where it is due:
            -- An Introduction to Elm Series: Solution to ‘Binary Tree’ example
            -- by George Michael
            -- https://bytefreaks.net/programming-2/elm/an-introduction-to-elm-series-solution-to-binary-tree-example
            fn value <| fold fn (fold fn accumulator left) right


sum : Tree number -> number
sum tree =
    case tree of
        Empty ->
            0

        Node v left right ->
            v + sum left + sum right


foldSum : Tree number -> number
foldSum =
    fold (+) 0


flatten : Tree a -> List a
flatten tree =
    case tree of
        Empty ->
            []

        Node v left right ->
            List.concat [ v :: flatten left, flatten right ]


foldFlatten : Tree a -> List a
foldFlatten =
    fold (::) []


isElement : a -> Tree a -> Bool
isElement a tree =
    case tree of
        Empty ->
            False

        Node v left right ->
            if a == v then
                True
            else
                isElement a left || isElement a right


foldIsElement : a -> Tree a -> Bool
foldIsElement x =
    -- My implementation:
    -- fold (\c a -> a || c == x) False
    -- More concise implementation from same article noted in `fold`
    fold ((==) x >> (||)) False



-- PLAYGROUND


deepTree =
    fromList [ 1, 2, 3 ]


niceTree =
    fromList [ 2, 1, 3 ]


oakTree =
    Node 3 (Node 2 (Node 3 Empty (Node 11 Empty Empty)) Empty) (Node 4 Empty Empty)


main =
    div [ style [ ( "font-family", "monospace" ) ] ]
        [ h1 [] [ text "Binary Tree Example" ]
        , display "depth" deepTree Nothing <| depth deepTree
        , display "depth" niceTree Nothing <| depth niceTree
        , display "incremented" niceTree Nothing <| map ((+) 1) niceTree
        , display "sum" oakTree Nothing <| sum oakTree
        , display "foldSum" oakTree Nothing <| foldSum oakTree
        , display "flatten" oakTree Nothing <| flatten oakTree
        , display "foldFlatten" oakTree Nothing <| foldFlatten oakTree
        , display "isElement" oakTree (Just 99) <| isElement 99 oakTree
        , display "isElement" oakTree (Just 4) <| isElement 4 oakTree
        , display "foldIsElement" oakTree (Just 99) <| foldIsElement 99 oakTree
        , display "foldIsElement" oakTree (Just 4) <| foldIsElement 4 oakTree
        ]


display : String -> Tree a -> Maybe a -> b -> Html msg
display name tree element value =
    div []
        [ h2 [ style styles.h2 ] [ text name ]
        , div [ style styles.section ]
            [ h3 [] [ text "Arguments:" ]
            , p [] [ text <| "TREE ==> " ++ toString tree ]
            , displayElement element
            ]
        , div [ style styles.section ]
            [ h3 [] [ text "Result:" ]
            , text <| toString value
            ]
        ]


displayElement element =
    let
        stringified =
            Maybe.map toString element
    in
    case stringified of
        Just str ->
            p [] [ text <| "ELEMENT ==> " ++ str ]

        Nothing ->
            text ""


styles =
    { h2 = [ ( "margin-top", "30px" ), ( "text-decoration", "underline" ) ]
    , section = [ ( "margin-left", "15px" ) ]
    }



{-----------------------------------------------------------------

Exercises:

(1) Sum all of the elements of a tree.

       sum : Tree number -> number

(2) Flatten a tree into a list.

       flatten : Tree a -> List a

(3) Check to see if an element is in a given tree.

       isElement : a -> Tree a -> Bool

(4) Write a general fold function that acts on trees. The fold
    function does not need to guarantee a particular order of
    traversal.

       fold : (a -> b -> b) -> b -> Tree a -> b

(5) Use "fold" to do exercises 1-3 in one line each. The best
    readable versions I have come up have the following length
    in characters including spaces and function name:
      sum: 16
      flatten: 21
      isElement: 46
    See if you can match or beat me! Don't forget about currying
    and partial application!

(6) Can "fold" be used to implement "map" or "depth"?

(7) Try experimenting with different ways to traverse a
    tree: pre-order, in-order, post-order, depth-first, etc.
    More info at: http://en.wikipedia.org/wiki/Tree_traversal

-----------------------------------------------------------------}
