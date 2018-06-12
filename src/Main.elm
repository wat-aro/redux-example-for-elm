module Main exposing (..)

import Html exposing (Html, a, button, div, form, input, li, p, text, ul)
import Html.Attributes exposing (class, href, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


---- MODEL ----


type State
    = Active
    | Completed


type alias Id =
    Int


type alias Todo =
    { id : Id
    , title : String
    , state : State
    }


type alias NextTodoId =
    Int


type Filter
    = ShowAll
    | ShowActive
    | ShowCompleted


type alias Model =
    { todos : List Todo
    , currentValue : String
    , nextTodoId : NextTodoId
    , currentFilter : Filter
    }


initialModel : Model
initialModel =
    { todos = []
    , currentValue = ""
    , nextTodoId = 1
    , currentFilter = ShowAll
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = Change String
    | Add
    | ToggleTodo Id
    | ShowFilter Filter


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

        ShowFilter filter ->
            ( { model | currentFilter = filter }, Cmd.none )


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
        , filter model
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
    let
        filteredTodos =
            case model.currentFilter of
                ShowAll ->
                    model.todos

                ShowActive ->
                    List.filter (\todo -> todo.state == Active) model.todos

                ShowCompleted ->
                    List.filter (\todo -> todo.state == Completed) model.todos
    in
    ul
        []
        (List.map
            (\todo -> todoLi todo)
            filteredTodos
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


filter : Model -> Html Msg
filter model =
    div
        [ class "filter-list" ]
        [ p [] [ text "Show: " ]
        , showFilter ShowAll model
        , showFilter ShowActive model
        , showFilter ShowCompleted model
        ]


showFilter : Filter -> Model -> Html Msg
showFilter filter model =
    if filter == model.currentFilter then
        p [ class "selected" ] [ text <| toString filter ]
    else
        p [ class "not-selected", onClick <| ShowFilter filter ] [ text <| toString filter ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
