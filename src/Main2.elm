module Main2 exposing (..)

--import Html exposing (Html, button, div, input, li, text, ul)

import Browser
import Css exposing (..)
import Html exposing (Html)
import Html.Styled exposing (button, div, input, li, text, ul)
import Html.Styled.Attributes exposing (class, css, type_, value)
import Html.Styled.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = initialModel, update = update, view = view }


type Msg
    = Increment
    | Decrement
    | SaveComment
    | UpdateComment String
    | DeleteComment Id


type alias Id =
    Int


type alias CommentItem =
    { id : Id
    , comment : String
    }


type alias Model =
    { comment : String
    , number : Int
    , commentList : List CommentItem
    , commentId : Int
    }


initialModel : Model
initialModel =
    { comment = "initial"
    , number = 0
    , commentList = []
    , commentId = 0
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | number = model.number + 1 }

        Decrement ->
            { model | number = model.number - 1 }

        SaveComment ->
            { model
                | commentList = List.append model.commentList [ { id = model.commentId + 1, comment = model.comment } ]
                , comment = ""
                , commentId = model.commentId + 1
            }

        UpdateComment comment ->
            { model | comment = comment }

        DeleteComment id ->
            let
                remainItem =
                    notIdItem id
            in
            { model | commentList = List.filter remainItem model.commentList }


notIdItem : Id -> CommentItem -> Bool
notIdItem id commentItem =
    id /= commentItem.id


view : Model -> Html Msg
view model =
    div []
        [ viewNumber
            model
        , viewComment model
        , viewCommentList model
        ]
        |> Html.Styled.toUnstyled


viewNumber : Model -> Html.Styled.Html Msg
viewNumber model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.number) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


viewCommentList : Model -> Html.Styled.Html Msg
viewCommentList model =
    div [ class "comments" ]
        [ ul [] (List.map viewOldComment model.commentList)
        ]


viewOldComment : CommentItem -> Html.Styled.Html Msg
viewOldComment commentItem =
    li []
        [ text commentItem.comment
        , button [ onClick (DeleteComment commentItem.id) ] [ text "削除" ]
        ]


viewComment : Model -> Html.Styled.Html Msg
viewComment model =
    div []
        [ input
            [ type_ "text"
            , value model.comment
            , onInput UpdateComment
            ]
            []
        , button
            [ onClick SaveComment
            , btnStyle
            ]
            [ text "保存" ]
        ]


btnStyle : Html.Styled.Attribute msg
btnStyle =
    css
        [ margin (px 12)
        , color (rgb 150 50 50)
        ]
