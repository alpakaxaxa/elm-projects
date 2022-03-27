module Main exposing (..)

import Array
import Browser
import Debug exposing (log, toString)
import Html exposing (Html, button, div, h1, h2, img, text)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate)
import Random.List exposing (..)
import Time



---- MODEL ----


type alias Question =
    { id : Int
    , text : String
    , correctAnswerLeft : Bool
    , completed : Bool
    }


type alias Questions =
    List Question


type alias Error =
    { message : String
    , code : Int
    }


type alias Model =
    { questions : Questions
    , currentError : Error
    }


type QuestionAnswer
    = Left
    | Right


init : ( Model, Cmd Msg )
init =
    ( { questions =
            [ { id = 1
              , text = "The capital of France is Paris"
              , correctAnswerLeft = False
              , completed = False
              }
            , { id = 2
              , text = "The capital of Germany is Munich"
              , correctAnswerLeft = True
              , completed = False
              }
            , { id = 3
              , text = "The capital of Italy is Milano"
              , correctAnswerLeft = True
              , completed = False
              }
            ]
      , currentError =
            { message = ""
            , code = 0
            }
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | NextQuestion
    | AnswerQuestion Int QuestionAnswer
    | ShuffleQuestions
    | ShuffledQuestions (List Question)
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    {- let
           logmessage1 =
               log (toString model.questions)
       in
    -}
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NextQuestion ->
            ( model, Cmd.none )

        AnswerQuestion questionIndex questionAnswer ->
            if isAnswerCorrect model.questions questionIndex questionAnswer then
                ( { model | questions = answerQuestion model.questions questionIndex questionAnswer, currentError = Error "" 0 }, Cmd.none )

            else
                ( { model | currentError = Error "Wrong answer" 1 }
                , Cmd.none
                )

        ShuffleQuestions ->
            ( model, generate ShuffledQuestions (shuffle model.questions) )

        ShuffledQuestions questions ->
            ( { model | questions = questions }, Cmd.none )

        Restart ->
            ( { model | questions = resetQuestions model.questions }, Cmd.none )


answerQuestion questions questionIndex questionAnswer =
    List.map
        (\question ->
            if questionIndex == question.id && checkQuestionAnswer questionAnswer question.correctAnswerLeft then
                { question | completed = True }

            else
                question
        )
        questions


isAnswerCorrect questions questionIndex questionAnswer =
    Array.fromList questions
        |> (\array ->
                case Array.get (questionIndex - 1) array of
                    Nothing ->
                        False

                    Just question ->
                        checkQuestionAnswer questionAnswer question.correctAnswerLeft
           )


checkQuestionAnswer questionAnswer hasCorrectAnswerLeft =
    if questionAnswer == Left && hasCorrectAnswerLeft then
        True

    else if questionAnswer == Right && not hasCorrectAnswerLeft then
        True

    else
        False


resetQuestions questions =
    List.map
        (\question ->
            { question | completed = False }
        )
        questions



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "section" ]
        [ div [ class "container" ]
            [ h1 [ class "title" ] [ text "Your Elm App is working!", h2 [ class "subtitle" ] [ text "Press e for false end i for true" ] ]
            , div [] [ viewQuestionHtml ( getCurrentQuestion model.questions, model.currentError ) ]
            ]
        ]


viewQuestionHtml : ( Question, Error ) -> Html Msg
viewQuestionHtml ( question, error ) =
    Html.section [ class "hero is-fullheight" ]
        [ div [ class "hero-body" ]
            [ viewQuestionCardHtml ( question, error )
            ]
        ]


viewQuestionCardHtml : ( Question, Error ) -> Html Msg
viewQuestionCardHtml ( question, error ) =
    if question.id /= 0 then
        div [ class "card" ]
            [ displayError error
            , div [ class "card-content" ] [ text question.text ]
            , Html.footer [ class "card-footer" ]
                [ div [ class "card-footer-item" ]
                    [ button [ class "button", onClick (AnswerQuestion question.id Left) ] [ text "False" ]
                    ]
                , div [ class "card-footer-item" ]
                    [ button [ class "button", onClick (AnswerQuestion question.id Right) ] [ text "True" ]
                    ]
                ]
            ]

    else
        div []
            [ div [] [ text "Contratulations you answered everything correctly!" ]
            , div [ class "button", onClick Restart ] [ text "Play again!" ]
            ]


displayError error =
    if error.code == 0 then
        div [] []

    else
        div [ class "notification is-danger" ] [ text error.message ]


getCurrentQuestion : Questions -> Question
getCurrentQuestion questions =
    List.filter (\question -> not question.completed) questions
        |> hasValidQuestion


hasValidQuestion : Questions -> Question
hasValidQuestion questions =
    case List.head questions of
        Nothing ->
            Question 0 "" False False

        Just question ->
            question



---- PROGRAM ----
{- subscriptions : Model -> Sub Msg
   subscriptions model =
       onClick ShuffleQuestions
-}


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
