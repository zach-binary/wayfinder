module App exposing (..)

import Debug exposing (log)
import Html exposing (Html, div, h1, img, li, text, ul)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import String
import Svg exposing (circle, polyline, svg)
import Svg.Attributes as Svg exposing (..)
import Tuple


---- MODEL ----


type alias Node =
    { id : Int
    , color : String
    , name : String
    , x : Int
    , y : Int
    }


type alias Edge =
    { color : String
    , from : Int
    , to : Int
    }


type alias Vertex =
    { node : Node
    , neighbors : List Int
    }


type alias Graph =
    { nodes : List Node
    , edges : List Edge
    }


vertexFor : Graph -> Int -> Maybe Vertex
vertexFor graph id =
    let
        node =
            List.head (List.filter (\node -> node.id == id) graph.nodes)

        edges =
            List.filter (\edge -> edge.to == id || edge.from == id) graph.edges
    in
    case node of
        Just node ->
            Just (Vertex node (List.map (neighborsFor node) edges))

        Nothing ->
            Nothing


neighborsFor : Node -> Edge -> Int
neighborsFor node edge =
    if edge.to == node.id then
        edge.from
    else if edge.from == node.id then
        edge.to
    else
        -1


type alias Model =
    { destination : Maybe Node
    , path : List Int
    , floor : Maybe Floor
    , floors : List Floor
    , rooms : List Node
    }


type alias Floor =
    { graph : Graph
    , destination : Maybe Node
    , path : List Int
    , name : String
    }


type alias RoomName =
    String


searchRoom : String -> List Floor -> List Node
searchRoom room floors =
    let
        strippedSearchTerm =
            String.trim room |> String.toLower

        search graph =
            List.filter (\node -> String.contains strippedSearchTerm <| String.toLower node.name) graph.nodes
    in
    if String.isEmpty room then
        []
    else
        List.concat <| List.map search <| List.map .graph floors


init : String -> ( Model, Cmd Msg )
init path =
    ( { destination = Nothing
      , path = []
      , floor = List.head floors
      , floors = floors
      , rooms = []
      }
    , Cmd.none
    )


floors : List Floor
floors =
    [ Floor
        { nodes =
            [ Node 0 "red" "ICU" 40 120
            , Node 1 "black" "" 40 180
            , Node 2 "red" "Room 1111" 100 180
            , Node 3 "red" "Cafe" 600 60
            , Node 4 "black" "" 40 240
            , Node 5 "black" "" 600 240
            , Node 6 "black" "" 400 60
            , Node 7 "black" "" 400 240
            , Node 8 "yellow" "Elevator Floor 1" 400 300
            ]
        , edges =
            [ Edge "red" 1 0
            , Edge "red" 1 2
            , Edge "black" 4 7
            , Edge "black" 7 5
            , Edge "black" 1 4
            , Edge "red" 5 3
            , Edge "black" 7 6
            , Edge "red" 3 6
            , Edge "yellow" 8 7
            ]
        }
        Nothing
        []
        "Floor 1"
    , Floor
        { nodes =
            [ Node 100 "red" "Room 2112" 40 120
            , Node 101 "black" "" 40 180
            , Node 102 "red" "Room 2111" 100 180
            , Node 103 "red" "Room 2500" 500 60
            , Node 104 "black" "" 40 240
            , Node 105 "black" "" 600 240
            , Node 106 "black" "" 400 60
            , Node 107 "black" "" 400 240
            , Node 108 "yellow" "Elevator Floor 2" 400 300
            ]
        , edges =
            [ Edge "red" 101 100
            , Edge "red" 101 102
            , Edge "black" 104 107
            , Edge "black" 107 105
            , Edge "black" 101 104
            , Edge "red" 105 103
            , Edge "black" 107 106
            , Edge "red" 103 106
            , Edge "yellow" 108 107
            ]
        }
        Nothing
        []
        "Floor 2"
    , Floor
        { nodes =
            [ Node 200 "red" "ICU" 40 120
            , Node 201 "black" "" 40 180
            , Node 202 "red" "Room 1111" 100 180
            , Node 203 "red" "Cafe" 600 60
            , Node 204 "black" "" 40 240
            , Node 205 "black" "" 600 240
            , Node 206 "black" "" 400 60
            , Node 207 "black" "" 400 240
            , Node 208 "yellow" "Elevator Floor 3" 400 300
            ]
        , edges =
            [ Edge "red" 201 200
            , Edge "red" 201 202
            , Edge "black" 204 207
            , Edge "black" 207 205
            , Edge "black" 201 204
            , Edge "red" 205 203
            , Edge "black" 207 206
            , Edge "red" 203 206
            , Edge "yellow" 208 207
            ]
        }
        Nothing
        []
        "Floor 3"
    ]



---- UPDATE ----


type Msg
    = NewDestination Node
    | FindPath Node
    | ChangeFloor Floor
    | SearchRooms RoomName


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case log "Msg" msg of
        NewDestination node ->
            ( { model | destination = Just node }, Cmd.none )

        FindPath node ->
            case ( model.destination, model.floor ) of
                ( Just destination, Just floor ) ->
                    ( { model | path = findPath floor.graph destination.id [] node.id }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeFloor floor ->
            ( { model | floor = Just floor, path = [] }, Cmd.none )

        SearchRooms room ->
            ( { model | rooms = searchRoom room model.floors }, Cmd.none )



-- starting at id, explore each node connected to it until we find goal
-- return the path that has the least number of nodes


findPath : Graph -> Int -> List Int -> Int -> List Int
findPath graph goal visited id =
    case ( vertexFor graph id, List.member id visited ) of
        ( Just node, False ) ->
            let
                path =
                    List.head <|
                        List.sortBy List.length <|
                            List.filter (List.member goal) <|
                                List.map (findPath graph goal <| id :: visited) <|
                                    node.neighbors
            in
            case path of
                Just path ->
                    path

                Nothing ->
                    []

        _ ->
            visited



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ renderFloor model
        , renderDestination model.destination
        , div [] [ text (toString model.path) ]
        ]


renderFloor : Model -> Html Msg
renderFloor model =
    case model.floor of
        Just floor ->
            Html.fieldset []
                [ Html.legend [] [ text floor.name ]
                , div [ class "floor-info" ]
                    [ svg [ Svg.width "640", Svg.height "480" ]
                        [ renderGraph floor.graph
                        , renderPath floor model.path
                        ]
                    , div []
                        [ div []
                            [ Html.input
                                [ placeholder "Search for a room"
                                , onInput SearchRooms
                                ]
                                []
                            , renderSearch model.rooms
                            ]
                        , renderFloorList model.floors
                        ]
                    ]
                ]

        Nothing ->
            div [] [ text "No floor, something has gone wrong...!" ]


renderSearch : List Node -> Html Msg
renderSearch rooms =
    let
        result room =
            li [ onClick <| NewDestination room ] [ text room.name ]
    in
    case rooms of
        [] ->
            text ""

        _ ->
            ul [ class "room-search" ] <| List.map result rooms


renderFloorList : List Floor -> Html Msg
renderFloorList floors =
    let
        floorLink floor =
            li [] [ Html.a [ onClick (ChangeFloor floor) ] [ text floor.name ] ]
    in
    Html.nav []
        [ ul [ class "floors" ] <| List.map floorLink floors
        ]


renderDestination : Maybe Node -> Html Msg
renderDestination destination =
    case destination of
        Just node ->
            div [] [ text node.name ]

        Nothing ->
            div [] []


renderGraph : Graph -> Svg.Svg Msg
renderGraph graph =
    Svg.g []
        (List.append (List.map (renderEdge graph.nodes) graph.edges)
            (List.map renderNode graph.nodes)
        )


renderPath : Floor -> List Int -> Svg.Svg Msg
renderPath floor path =
    let
        test =
            List.map (\node -> { node | color = "green" }) <|
                List.map .node <|
                    List.filterMap identity <|
                        List.map (vertexFor floor.graph) path
    in
    Svg.g [] (List.map renderNode test)


renderEdge : List Node -> Edge -> Svg.Svg Msg
renderEdge nodes edge =
    let
        to_ =
            List.head (List.filter (\node_ -> node_.id == edge.to) nodes)

        from_ =
            List.head (List.filter (\node_ -> node_.id == edge.from) nodes)

        path_ =
            case ( to_, from_ ) of
                ( Just to_, Just from_ ) ->
                    Just
                        { x1 = toString to_.x
                        , y1 = toString to_.y
                        , x2 = toString from_.x
                        , y2 = toString from_.y
                        }

                _ ->
                    Nothing
    in
    case path_ of
        Nothing ->
            Svg.text ""

        Just aPath ->
            Svg.g []
                [ Svg.line
                    [ Svg.x1 aPath.x1
                    , Svg.y1 aPath.y1
                    , Svg.x2 aPath.x2
                    , Svg.y2 aPath.y2
                    , Svg.stroke edge.color
                    , Svg.strokeWidth "2"
                    ]
                    []
                ]


renderNode : Node -> Svg.Svg Msg
renderNode node =
    let
        cx_ =
            toString node.x

        cy_ =
            toString node.y

        textX =
            toString (node.x + 15)

        textY =
            toString (node.y - 15)

        onNodeClick =
            FindPath node
    in
    Svg.g []
        [ circle
            [ Svg.cx cx_
            , Svg.cy cy_
            , Svg.r "10"
            , Svg.stroke node.color
            , Svg.fill node.color
            , onClick onNodeClick
            , Svg.class node.color
            ]
            []
        , Svg.text_ [ Svg.x textX, Svg.y textY ] [ text node.name ]
        ]



---- PROGRAM ----


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
