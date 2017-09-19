module App exposing (..)

import Html exposing (Html, text, div, img)
import Html.Events exposing (onClick)
import Svg exposing (svg, polyline, circle)
import Svg.Attributes as Svg exposing (..)
import Debug exposing (log)
import Tuple exposing (first)

---- MODEL ----

type alias Node = 
    { id:Int
    , color:String
    , name:String
    , x:Int
    , y:Int
    }

type alias Edge = 
    { color:String 
    , from:Int
    , to:Int
    }

type alias Vertex =
    { node:Node 
    , edges:List Int
    }

type alias Graph =
    { nodes:List Node
    , edges:List Edge
    }

getNode: Graph -> Int -> Maybe Vertex
getNode graph id =
    let
        node = 
            List.head (List.filter (\node -> node.id == id) graph.nodes)

        edges =
            List.filter (\edge -> edge.to == id || edge.from == id) graph.edges
    in
        case node of
            Just node ->
                Just (Vertex node (List.map (getConnectedNode node) edges))
            
            Nothing ->
                Nothing

getConnectedNode: Node -> Edge -> Int 
getConnectedNode node edge =
    if edge.to == node.id then edge.from
    else if edge.from == node.id then edge.to
    else -1

type alias Model =
    { graph:Graph
    , destination:Node
    , path:List Int
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { graph = 
        { nodes = nodes 
        , edges = edges
        }
      , destination = Node -1 "" "" 0 0
      , path = []
      }, Cmd.none )

nodes: List Node
nodes = 
    [ Node 0 "red" "ICU" 40 120
    , Node 1 "black" "" 40 180
    , Node 2 "red" "Room 1111" 100 180
    , Node 3 "red" "Cafe" 600 60
    , Node 4 "black" "" 40 240
    , Node 5 "black" "" 600 240
    , Node 6 "black" "" 400 60
    , Node 7 "black" "" 400 240
    ]

edges: List Edge
edges = 
    [ Edge "red" 1 0 
    , Edge "red" 1 2
    , Edge "black" 4 7
    , Edge "black" 7 5
    , Edge "black" 1 4
    , Edge "red" 5 3
    , Edge "black" 7 6
    , Edge "red" 3 6
    ]

---- UPDATE ----


type Msg
    = NewDestination Node
    | FindPath Node


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case log "Msg" msg of 
        NewDestination node ->
            ( { model | destination = node }, Cmd.none )

        FindPath node ->
            if model.destination.id == -1 then 
                ( model, Cmd.none )
            else
                ( { model | path = findPath model.graph model.destination.id [] node.id }, Cmd.none )


-- find all edges connecting to start
-- find all nodes on the other end of those edges
-- check if they match end
-- if not, for each new node, find all edges connected to them
-- repeat

-- pick edge and follow branch until hitting a "red" node
-- if red node == end, finish
findPath: Graph -> Int -> List Int -> Int -> List Int
findPath graph goal visited id =
    if List.member id visited then visited
    else
        let
            node = 
                getNode graph id

            explore = 
                findPath graph goal <| id :: visited
            
            isPath path =
                List.member goal path
        in
            case node of
                Just node ->
                    let
                        path = List.head (List.sortBy List.length 
                            <| List.filter isPath (List.map explore node.edges))
                    in
                        case path of 
                            Just path -> path
                            Nothing -> []

                Nothing -> []



            

---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ svg [ Svg.width "640", Svg.height "480" ] 
            [ renderGraph model.graph
            , renderPath model
            ]
        , div [] [ text model.destination.name ]
        , div [] [ text (toString model.path)]
        ]

renderGraph: Graph -> Svg.Svg Msg
renderGraph graph =
    Svg.g [] (List.append (List.map (renderEdge graph.nodes) graph.edges)
                          (List.map renderNode graph.nodes))

renderPath: Model -> Svg.Svg Msg
renderPath model =
    let
        vertices = List.map (getNode model.graph) model.path
        justVertices = List.filterMap identity vertices
        nodes = List.map (\v -> v.node) justVertices
        greenNodes = List.map (\node -> { node | color = "green" }) nodes
    in
        Svg.g [] (List.map renderNode greenNodes)


renderEdge: List Node -> Edge -> Svg.Svg Msg
renderEdge nodes edge =
    let 
        to_ = List.head (List.filter (\node_ -> node_.id == edge.to) nodes)
        from_ = List.head (List.filter (\node_ -> node_.id == edge.from) nodes)
        path_ = case (to_, from_) of
            (Just to_, Just from_) ->
                Just { x1 = (toString to_.x)
                     , y1 = (toString to_.y)
                     , x2 = (toString from_.x)
                     , y2 = (toString from_.y)
                     }

            _ ->
                Nothing
    in 
        case path_ of 
            Nothing -> Svg.text ""
            Just aPath ->
                Svg.g [] 
                [ Svg.line 
                  [ Svg.x1 aPath.x1
                  , Svg.y1 aPath.y1
                  , Svg.x2 aPath.x2
                  , Svg.y2 aPath.y2
                  , Svg.stroke edge.color
                  , Svg.strokeWidth "2"
                  ] []
                ]

renderNode: Node -> Svg.Svg Msg
renderNode node =
    let 
        cx_ = (toString node.x)
        cy_ = (toString node.y)
        textX = toString (node.x + 15)
        textY = toString (node.y - 15)
        onNodeClick = 
            case node.color of 
                "red" -> NewDestination node
                _ -> FindPath node
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
            ] []
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
