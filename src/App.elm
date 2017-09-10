module App exposing (..)

import Html exposing (Html, text, div, img)
import Svg exposing (svg, polyline, circle)
import Svg.Attributes as Svg exposing (..)
import Debug exposing (log)

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

type alias Graph =
    { nodes:List Node
    , edges:List Edge
    }

type alias Model =
    { graph:Graph
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { graph = 
        { nodes = nodes 
        , edges = edges
        }
      }, Cmd.none )

nodes: List Node
nodes = 
    [ Node 0 "red" "ICU" 40 120
    , Node 1 "black" "" 40 180
    , Node 2 "red" "Room 1111" 100 180
    , Node 3 "red" "Cafe" 600 60
    , Node 4 "black" "" 40 240
    , Node 5 "black" "" 600 240
    ]

edges: List Edge
edges = 
    [ Edge "red" 1 0 
    , Edge "red" 1 2
    , Edge "black" 4 5
    , Edge "black" 1 4
    , Edge "red" 5 3
    ]

---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ svg [ Svg.width "640", Svg.height "480" ] 
            [ renderGraph model.graph
            ]
        ]

renderGraph: Graph -> Svg.Svg msg
renderGraph graph =
    Svg.g [] (List.append (List.map (renderEdge graph.nodes) graph.edges)
                          (List.map renderNode graph.nodes))

renderEdge: List Node -> Edge -> Svg.Svg msg
renderEdge nodes edge =
    let 
        to_ = List.head (List.filter (\node_ -> node_.id == edge.to) nodes)
        from_ = List.head (List.filter (\node_ -> node_.id == edge.from) nodes)
        path_ = case log "To, From: "(to_, from_) of
            (Just to_, Just from_) ->
                Just { x1 = (toString to_.x)
                     , y1 = (toString to_.y)
                     , x2 = (toString from_.x)
                     , y2 = (toString from_.y)
                     }

            _ ->
                Nothing
    in 
        case log "Path: " path_ of 
            Nothing -> Svg.text "Oh No!"
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

renderNode: Node -> Svg.Svg msg
renderNode node =
    let 
        cx_ = (toString node.x)
        cy_ = (toString node.y)
        textX = toString (node.x + 15)
        textY = toString (node.y - 15)
    in
        Svg.g []
        [ circle 
            [ Svg.cx cx_
            , Svg.cy cy_
            , Svg.r "10"
            , Svg.stroke node.color 
            , Svg.fill node.color
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
