module Main exposing (..)

import Array
import Browser
import Browser.Events exposing (onKeyPress)
import Debug exposing (log, toString)
import Html exposing (Html, button, div, h1, h2, img, text)
import Html.Attributes exposing (class, classList, src, style)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Random exposing (Seed, generate)
import Random.List exposing (..)
import Task
import Time



{- import Time -}
---- MODEL ----


type alias Question =
    { id : Int
    , text : String
    , correctAnswerLeft : Bool
    , completed : Bool
    , questionType : QuestionType
    }


type alias Questions =
    List Question


type alias Error =
    { message : String
    , code : Int
    }


type QuestionType
    = Image
    | Word


type alias Model =
    { questions : Questions
    , currentError : Error
    , startTime : Time.Posix
    , passedMilliSeconds : Int
    }


type QuestionAnswer
    = Left
    | Right



-- Bei answer question anstatt Cmd.none mache ich ein Task.perform und setze passedMilliSeconds neu sowie startTime --> lit.


init : ( Model, Cmd Msg )
init =
    ( { questions =
            [ { id = 1
              , text = "The capital of Romania is Budapest"
              , correctAnswerLeft = True
              , completed = False
              , questionType = Word
              }
            , { id = 2
              , text = "The capital of Ecuador is Quito"
              , correctAnswerLeft = False
              , completed = False
              , questionType = Image
              }
            , { id = 3
              , text = "The capital of Italy is Milano"
              , correctAnswerLeft = True
              , completed = False
              , questionType = Word
              }
            , { id = 4
              , text = "The capital of Northern Macedonia is Tetovo"
              , correctAnswerLeft = True
              , completed = False
              , questionType = Word
              }
            , { id = 5
              , text = "The capital of Israel is Tel Aviv"
              , correctAnswerLeft = True
              , completed = False
              , questionType = Image
              }
            , { id = 6
              , text = "The capital of Turkey is Istanbul"
              , correctAnswerLeft = True
              , completed = False
              , questionType = Image
              }
            ]
      , currentError =
            { message = ""
            , code = 0
            }
      , startTime = Time.millisToPosix 0
      , passedMilliSeconds = 0
      }
    , Cmd.batch
        [ Task.perform GotStartTime Time.now
        ]
    )



---- UPDATE ----


type Msg
    = NoOp
    | GotStartTime Time.Posix
    | NextQuestion
    | AnswerQuestion Int QuestionAnswer
    | SetNewStartTime Time.Posix
    | ShuffleQuestions
    | ShuffledQuestions (List Question)
    | Restart
    | KeyPressed


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

        GotStartTime timeNow ->
            update ShuffleQuestions { model | startTime = timeNow, passedMilliSeconds = 0 }

        NextQuestion ->
            ( model, Cmd.none )

        AnswerQuestion questionIndex questionAnswer ->
            if isAnswerCorrect model.questions questionIndex questionAnswer then
                ( { model | questions = answerQuestion model.questions questionIndex questionAnswer, currentError = Error "" 0 }, Task.perform SetNewStartTime Time.now )

            else
                ( { model | currentError = Error "Wrong answer" 1 }
                , Cmd.none
                )

        SetNewStartTime timeNow ->
            ( { model | passedMilliSeconds = model.passedMilliSeconds + (Time.posixToMillis timeNow - Time.posixToMillis model.startTime), startTime = timeNow }, Cmd.none )

        ShuffleQuestions ->
            ( model, generate ShuffledQuestions (shuffle model.questions) )

        ShuffledQuestions questions ->
        -- Hier könnte sort question eingeführt werden, aber dies ergibt einen bug.
            ( { model | questions = alignQuestionIdAndListPosition questions }, Cmd.none )

        Restart ->
            ( { model | questions = resetQuestions model.questions }, Task.perform GotStartTime Time.now )

        KeyPressed ->
            ( model, Cmd.none )


alignQuestionIdAndListPosition : List Question -> List Question
alignQuestionIdAndListPosition questions =
    List.indexedMap (\currentIndex question -> { question | id = currentIndex + 1 }) questions


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


hasAllQuestionsCompleted questions =
    if (List.filter (\question -> not question.completed) questions |> List.length) == 0 then
        True

    else
        False


sortQuestions questions =
    mergeFilteredQuestions (List.filter isWordQuestion questions) (List.filter isImageQuestion questions)


mergeFilteredQuestions wordQuestions imageQuestions =
    List.append wordQuestions imageQuestions


isWordQuestion question =
    question.questionType == Word


isImageQuestion question =
    question.questionType == Image



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyPress (JD.succeed KeyPressed)



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "section" ]
        [ div [ class "container" ]
            [ h1 [ class "title" ] [ text "Answer all questions!", h2 [ class "subtitle" ] [ text "Press e for False or i for True" ] ]
            , div [] [ viewQuestionHtml ( getCurrentQuestion model.questions, model.currentError ) ]
            ]
        ]


viewQuestionHtml : ( Question, Error ) -> Html Msg
viewQuestionHtml ( question, error ) =
    Html.section [ class "hero is-fullheight" ]
        [ div [ class "hero-body", style "justify-content" "center" ]
            [ viewQuestionCardHtml ( question, error )
            ]
        ]


viewQuestionCardHtml : ( Question, Error ) -> Html Msg
viewQuestionCardHtml ( question, error ) =
    if question.id /= 0 then
        div [ class "card" ]
            [ displayError error
            , div [ class "card-content subtitle" ] [ text question.text ]
            , Html.footer [ class "card-footer" ]
                [ div [ class "card-footer-item" ]
                    [ button [ class "button is-large", onClick (AnswerQuestion question.id Left) ] [ text "False" ]
                    ]
                , div [ class "card-footer-item" ]
                    [ button [ class "button is-large", onClick (AnswerQuestion question.id Right) ] [ text "True" ]
                    ]
                ]
            ]

    else
        div []
            [ div []
                [ Html.article
                    [ class "message is-primary"
                    ]
                    [ div
                        [ class "message-header"
                        ]
                        [ Html.p []
                            [ text "Congratulations!" ]
                        , button
                            [ onClick Restart
                            , class "button is-link"
                            ]
                            [ text "Play again" ]
                        ]
                    , div
                        [ class "message-body"
                        ]
                        [ text "You answered all questions correctly. You can be very proud!" ]
                    ]
                ]
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
            Question 0 "" False False Word

        Just question ->
            question


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
