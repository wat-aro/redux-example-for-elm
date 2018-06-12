module Main exposing (..)

import Html exposing (Html, button, div, form, input, li, text, ul)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


---- MODEL ----


type State
    = Active
    | Completed


type alias Id =
    Int


type alias Todo =
    { id : Id, title : String, state : State }


type alias NextTodoId =
    Int


type alias Model =
    { todos : List Todo, currentValue : String, nextTodoId : NextTodoId }


initialModel : Model
initialModel =
    { todos = [], currentValue = "", nextTodoId = 1 }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = Change String
    | Add
    | ToggleTodo Id


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change string ->
            ( { model | currentValue = string }, Cmd.none )

        Add ->
            if model.currentValue == "" then
                ( model, Cmd.none )
            else
                ( addTodo model, Cmd.none )

        ToggleTodo id ->
            ( toggleTodo id model, Cmd.none )


addTodo : Model -> Model
addTodo model =
    let
        newTodos =
            List.append model.todos [ Todo model.nextTodoId model.currentValue Active ]

        nextTodoId =
            model.nextTodoId + 1
    in
    { model | todos = newTodos, currentValue = "", nextTodoId = nextTodoId }


toggleTodo : Id -> Model -> Model
toggleTodo id model =
    let
        newTodos =
            List.map
                (\todo ->
                    if todo.id == id then
                        toggle todo
                    else
                        todo
                )
                model.todos
    in
    { model | todos = newTodos }


toggle : Todo -> Todo
toggle todo =
    case todo.state of
        Active ->
            { todo | state = Completed }

        Completed ->
            { todo | state = Active }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ addTodoForm model
        , todoList model
        ]


addTodoForm : Model -> Html Msg
addTodoForm model =
    form
        [ onSubmit Add ]
        [ input [ type_ "text", onInput Change, value model.currentValue ] []
        , button [ onClick Add ] [ text "Add Todo" ]
        ]


todoList : Model -> Html Msg
todoList model =
    ul
        []
        (List.map
            (\todo -> todoLi todo)
            model.todos
        )


liClass : Todo -> String
liClass todo =
    case todo.state of
        Active ->
            "active"

        Completed ->
            "completed"


todoLi : Todo -> Html Msg
todoLi todo =
    li
        [ class <| liClass todo, onClick <| ToggleTodo todo.id ]
        [ text todo.title ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
