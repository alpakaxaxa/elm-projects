module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, img, input, li, ol, span, text)
import Html.Attributes exposing (placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as JD
import Json.Encode as JE



---- MODEL ----


type alias Model =
    { todos : List Todo
    , inputText : String
    , todo : Todo
    }


type alias Todos =
    List Todo


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { todos = [], inputText = "", todo = Todo 0 "" False }, fetchTodos )



---- UPDATE ----


type Msg
    = NoOp
    | GetTodos
    | GotTodos (Result Http.Error Todos)
    | ChangeInput String
    | AddTodo
    | GotTodo (Result Http.Error Todo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetTodos ->
            ( model, fetchTodos )

        GotTodos result ->
            case result of
                Ok todos ->
                    ( { model | todos = todos }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ChangeInput text ->
            ( { model | inputText = text }, Cmd.none )

        AddTodo ->
            ( model, postTodo model )

        GotTodo result ->
            case result of
                Ok todo ->
                    ( { model | todo = todo, inputText = "" }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


fetchTodos : Cmd Msg
fetchTodos =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/todos"
        , expect = Http.expectJson GotTodos todosDecoder
        }


todoParser =
    JD.map3 Todo
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)
        (JD.field "completed" JD.bool)


todosDecoder =
    JD.list todoParser


todoEncode : String -> JE.Value
todoEncode inputText =
    JE.object
        [ ( "id", JE.int 0 )
        , ( "title", JE.string inputText )
        , ( "completed", JE.bool False )
        ]


postTodo : Model -> Cmd Msg
postTodo model =
    Http.post
        { url = "https://jsonplaceholder.typicode.com/todos"
        , body =
            todoEncode model.inputText
                |> Http.jsonBody
        , expect = Http.expectJson GotTodo todoParser
        }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Your todos" ]
        , div [] [ input [ type_ "text", value model.inputText, onInput ChangeInput ] [] ]
        , div [] [ button [ onClick AddTodo ] [ text "Submit todo" ] ]
        , ol [] (List.take 10 (List.map todoHtmlView model.todos))
        ]


todoHtmlView : Todo -> Html Msg
todoHtmlView todo =
    li []
        [ span []
            [ text todo.title
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
