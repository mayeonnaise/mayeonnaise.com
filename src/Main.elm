module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onMouseMove, onResize, onClick, onKeyDown)
import Html exposing (..)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder)
import Math.Vector2 exposing (Vec2, vec2)
import Random
import Task
import WebGL exposing (Mesh)
import WebGL.Settings.Blend as Blend
import WebGL.Texture as Texture exposing (Error, Texture)


{-| Types
-}
type Action
    = MouseMove Float
    | Animate Float
    | TextureLoad Texture
    | TextureError Error
    | BackgroundTextureLoad Texture
    | BackgroundImageTextureLoad Texture
    | Resize Float Float
    | FirstResize Float Float

type AnimateState
    = Stationary
    | Moving

type alias Model =
    { width : Float
    , height : Float
    , displacement : Float
    , target : Float
    , current : Float
    , maybeTexture : Maybe Texture
    , maybeBackgroundTexture : Maybe Texture
    , maybeBackgroundImageTexture : Maybe Texture
    , elapsed : Float
    , frame : Int
    , animateState : AnimateState
    }

type Direction
  = Left
  | Right
  | Other

{-| Constants
-}

gameWidth : Float
gameWidth = 1024

gameHeight : Float
gameHeight = 512

backgroundWidth : Float
backgroundWidth = 2048

backgroundHeight : Float
backgroundHeight = 1024

spriteHeight : Float
spriteHeight = 256

spriteWidth : Float
spriteWidth = 128

{-| Program
-}
main : Program () Model Action
main =
    Browser.element
        { init = always init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : ( Model, Cmd Action )
init =
    ( { width = 1024
      , height = 512
      , target = 0
      , current = 0
      , displacement = 0
      , maybeTexture = Nothing
      , maybeBackgroundTexture = Nothing
      , maybeBackgroundImageTexture = Nothing
      , elapsed = 0
      , frame = 0
      , animateState = Stationary
      }
    , Cmd.batch
        [ Texture.load "./assets/sprite_run_right.png"
            |> Task.attempt
                (\result ->
                    case result of
                        Err err ->
                            TextureError err

                        Ok val ->
                            TextureLoad val
                )
        , Texture.load "./assets/background.png"
            |> Task.attempt
                (\result ->
                    case result of
                        Err err ->
                            TextureError err

                        Ok val ->
                            BackgroundTextureLoad val
                )
        , Texture.load "./assets/background_image.png"
            |> Task.attempt
                (\result ->
                    case result of
                        Err err ->
                            TextureError err

                        Ok val ->
                            BackgroundImageTextureLoad val
                )
        , Task.perform (\{ viewport } -> FirstResize viewport.width viewport.height) getViewport
        ]
    )


subscriptions : Model -> Sub Action
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Animate
        , onClick mousePosition
        , onResize (\w h -> Resize (toFloat w) (toFloat h))
        ]


mousePosition : Decoder Action
mousePosition =
    Decode.map MouseMove
        (Decode.field "pageX" Decode.float)

limitMousePosition : Float -> Float -> Float
limitMousePosition point displacement =
    if point < 20
        then 20
    else if point > 850
        then 850
    else
        point



update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        MouseMove target ->
            ( { model | target = limitMousePosition (target - (spriteWidth / 2) - model.displacement) (spriteWidth / 2), animateState = Moving }
            , Cmd.none
            )

        Animate elapsed ->
            ( animate elapsed model
            , Cmd.none
            )

        TextureLoad texture ->
            ( { model | maybeTexture = Just texture }
            , Cmd.none
            )

        BackgroundTextureLoad texture ->
            ( { model | maybeBackgroundTexture = Just texture }
            , Cmd.none
            )

        BackgroundImageTextureLoad texture ->
            ( { model | maybeBackgroundImageTexture = Just texture }
            , Cmd.none
            )

        TextureError _ ->
            ( model
            , Cmd.none
            )

        Resize width _ ->
            let
                displacement = ((width - 1024) / 2)
                target = model.current
            in
                ( { model | displacement = displacement, target = target}
                , Cmd.none
                )

        FirstResize width _ ->
            let
                displacement = ((width - 1024) / 2)
                current = 20
                target = 20
            in
                ( { model | displacement = displacement, target = target, current = current}
                , Cmd.none
                )




absDifference : Float -> Float -> Float
absDifference a b =
    abs (a - b)

animate : Float -> Model -> Model
animate elapsed model =
    let
        timeout =
            150

        newElapsed =
            elapsed + model.elapsed

    in
    if newElapsed > timeout then
        case model.animateState of
            Stationary ->
                { model
                    | elapsed = 0
                }
            Moving ->
                let
                    moveDirection =
                        if model.current < model.target then
                            (+)
                        else
                            (-)

                    newCurrent =
                        if (absDifference model.current model.target) < 20 then
                            model.target
                        else
                            moveDirection model.current 20

                    newState =
                        if (absDifference model.current model.target) < 20 then
                            Stationary
                        else
                            Moving
                in
                { model
                    | frame = modBy 8 (model.frame + 1)
                    , elapsed = newElapsed - timeout
                    , current = newCurrent
                    , animateState = newState
                }
    else
        { model
            | elapsed = newElapsed
        }


view : Model -> Html Action
view { width, height, current, maybeTexture, frame, maybeBackgroundTexture, maybeBackgroundImageTexture } =
    div
        [ Attributes.style "height" "100%"
        , Attributes.style "width" "100%"
        , Attributes.style "margin-top" "1em"
        , Attributes.style "display" "flex"
        , Attributes.style "justify-content" "space-evenly"
        , Attributes.style "align-items" "center"
        , Attributes.style "flex-direction" "column"
        ]
    [ WebGL.toHtml
        [ Attributes.width (round width)
        , Attributes.height (round height)
        ]
        (case (maybeTexture, maybeBackgroundTexture, maybeBackgroundImageTexture) of
            (Nothing, _ , _)->
                []

            (_, Nothing, _) ->
                []

            (_, _, Nothing) ->
                []

            (Just spriteTexture, Just backgroundTexture, Just backgroundImageTexture) ->
                [ WebGL.entityWith
                    [ Blend.add Blend.one Blend.oneMinusSrcAlpha ]
                    backgroundVertexShader
                    backgroundFragmentShader
                    backgroundMesh
                    { screenSize = vec2 width height
                    , offset = vec2 0 0
                    , texture = backgroundImageTexture
                    , textureSize = vec2 (toFloat (Tuple.first (Texture.size backgroundImageTexture))) (toFloat (Tuple.second (Texture.size backgroundImageTexture)))
                    }
                , WebGL.entityWith
                    [ Blend.add Blend.one Blend.oneMinusSrcAlpha ]
                    vertexShader
                    fragmentShader
                    mesh
                    { screenSize = vec2 width height
                    , offset = vec2 (current) 310
                    , texture = spriteTexture
                    , frame = frame
                    , textureSize = vec2 (toFloat (Tuple.first (Texture.size spriteTexture))) (toFloat (Tuple.second (Texture.size spriteTexture)))
                    , frameSize = vec2 128 256
                    }
                ]
        ),
        div
            [ Attributes.style "display" "flex"
            , Attributes.style "justify-content" "space-evenly"
            , Attributes.style "align-items" "center"
            , Attributes.style "flex-direction" "row"
            , Attributes.style "margin-top" "0.3em"
            ]
            [ a
                [ Attributes.href "https://github.com/mayeonnaise"
                ]
                [ img
                    [ Attributes.src "assets/github_logo_white.png"
                    ]
                    []
                ]
            ],
        span [Attributes.style "margin-top" "1em"]
            [text "Built with Elm and WebGL"]

    ]



{- Mesh and shaders -}

mesh : Mesh { position : Vec2 }
mesh =
    WebGL.triangles
        [ ( { position = vec2 0 0 }
          , { position = vec2 128 256 }
          , { position = vec2 128 0 }
          )
        , ( { position = vec2 0 0 }
          , { position = vec2 0 256 }
          , { position = vec2 128 256 }
          )
        ]

backgroundMesh : Mesh { position : Vec2 }
backgroundMesh =
    WebGL.triangles
        [ ( { position = vec2 0 0 }
          , { position = vec2 1024 512 }
          , { position = vec2 1024 0 }
          )
        , ( { position = vec2 0 0 }
          , { position = vec2 0 512 }
          , { position = vec2 1024 512 }
          )
        ]

backgroundImageMesh : Mesh { position : Vec2 }
backgroundImageMesh =
    WebGL.triangles
        [ ( { position = vec2 0 0 }
          , { position = vec2 1024 1024 }
          , { position = vec2 1024 0 }
          )
        , ( { position = vec2 0 0 }
          , { position = vec2 0 1024 }
          , { position = vec2 1024 1024 }
          )
        ]


vertexShader : WebGL.Shader { attr | position : Vec2 } { unif | screenSize : Vec2, offset : Vec2 } { texturePos : Vec2 }
vertexShader =
    [glsl|
        attribute vec2 position;
        uniform vec2 offset;
        uniform vec2 screenSize;
        varying vec2 texturePos;
        void main () {
            vec2 clipSpace = (position + offset) / screenSize * 2.0 - 1.0;
            gl_Position = vec4(clipSpace.x, -clipSpace.y, 0, 1);
            texturePos = position;
        }
    |]

backgroundVertexShader : WebGL.Shader { attr | position : Vec2 } { unif | screenSize : Vec2, offset : Vec2 } { texturePos : Vec2 }
backgroundVertexShader =
    [glsl|
        attribute vec2 position;
        uniform vec2 screenSize;
        uniform vec2 offset;
        varying vec2 texturePos;
        void main () {
            vec2 clipSpace = (position + offset) / screenSize * 2.0 - 1.0;
            gl_Position = vec4(clipSpace.x, -clipSpace.y, 0, 1);
            texturePos = position;
        }
    |]
backgroundFragmentShader : WebGL.Shader {} { u | texture : Texture, textureSize : Vec2 } { texturePos : Vec2 }
backgroundFragmentShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        uniform vec2 textureSize;
        varying vec2 texturePos;
        void main () {
            vec2 textureClipSpace = texturePos / textureSize;
            vec4 temp = texture2D(texture, vec2(textureClipSpace.x, -textureClipSpace.y));
            float a = temp.a;
            gl_FragColor = vec4(temp.r * a, temp.g * a, temp.b * a, a);
        }
    |]


fragmentShader : WebGL.Shader {} { u | texture : Texture, textureSize : Vec2, frameSize : Vec2, frame : Int } { texturePos : Vec2 }
fragmentShader =
    [glsl|
    precision mediump float;
    uniform sampler2D texture;
    uniform vec2 textureSize;
    uniform vec2 frameSize;
    uniform int frame;
    varying vec2 texturePos;
    void main () {
        vec2 size = frameSize / textureSize;
        int frames = int(1.0 / size.x);
        vec2 frameOffset = size * vec2(float(frame - frame / frames * frames), -float(frame / frames));
        vec2 textureClipSpace = texturePos / textureSize;
        vec4 temp = texture2D(texture, vec2(textureClipSpace.x, -textureClipSpace.y) + frameOffset);
        float a = temp.a;
        gl_FragColor = vec4(temp.r * a, temp.g * a, temp.b * a, a);
    }
    |]
