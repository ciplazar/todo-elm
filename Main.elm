port module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit, onClick)
import String


main : Program (Maybe Model)
main =
    App.programWithFlags
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


port setStorage : Model -> Cmd msg


type alias TodoModel =
    { id : Int
    , text : String
    , done : Bool
    }


type alias Model =
    { todo : String
    , todos : List TodoModel
    , debug : Bool
    }


model : Model
model =
    Model "" [] False


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    Maybe.withDefault model savedModel ! []


type Msg
    = Todo String
    | AddTodo
    | FlipDone TodoModel
    | DeleteDone
    | ToggleDebugger


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo todo ->
            { model | todo = todo } ! []

        AddTodo ->
            if String.isEmpty model.todo then
                model ! []
            else
                let
                    maxId =
                        model.todos
                            |> List.map .id
                            |> List.maximum

                    id =
                        (Maybe.withDefault 0 maxId) + 1

                    newTodos =
                        (TodoModel id model.todo False) :: model.todos

                    newModel =
                        { model
                            | todos = newTodos
                            , todo = ""
                        }
                in
                    ( newModel, setStorage newModel )

        FlipDone todo ->
            let
                flipDone : TodoModel -> TodoModel
                flipDone t =
                    if t.id == todo.id then
                        { t | done = not t.done }
                    else
                        t

                newTodos =
                    List.map flipDone model.todos

                newModel =
                    { model | todos = newTodos }
            in
                ( newModel, setStorage newModel )

        DeleteDone ->
            let
                newTodos =
                    List.filter (\t -> not t.done) model.todos

                newModel =
                    { model | todos = newTodos }
            in
                ( newModel, setStorage newModel )

        ToggleDebugger ->
            let
                newModel =
                    { model | debug = not model.debug }
            in
                ( newModel, setStorage newModel )


navigation : Html Msg
navigation =
    nav [ class "navbar navbar-light bg-faded" ]
        [ a [ class "navbar-brand", href "/" ]
            [ text "Tae Dae" ]
        , button
            [ type' "button"
            , class "btn btn-outline-warning btn-sm float-xs-right"
            , onClick DeleteDone
            ]
            [ text "Clear Done" ]
        ]


debugger : Model -> Html Msg
debugger model =
    footer []
        [ button
            [ type' "button"
            , class "btn btn-link float-xs-right"
            , onClick ToggleDebugger
            ]
            [ text "Debugger" ]
        , span [ classList [ ( "hide", not model.debug ) ] ]
            [ text (toString model) ]
        ]


todoItem : TodoModel -> Html Msg
todoItem todo =
    li
        [ onClick (FlipDone todo)
        , classList
            [ ( "list-group-item", True )
            , ( "done", todo.done )
            ]
        ]
        [ text todo.text ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col-md-6 offset-md-3" ]
                [ navigation
                , Html.form [ onSubmit AddTodo ]
                    [ div [ class "input-group" ]
                        [ input
                            [ type' "text"
                            , class "form-control"
                            , placeholder "Task"
                            , value model.todo
                            , onInput Todo
                            ]
                            []
                        , span [ class "input-group-btn" ]
                            [ button [ type' "submit", class "btn btn-primary" ]
                                [ text "Add" ]
                            ]
                        ]
                    ]
                , ul [ class "list-group" ] (List.map todoItem model.todos)
                , debugger model
                ]
            ]
        ]
