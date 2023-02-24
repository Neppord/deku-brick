module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Compactable (compact)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Deku.Attribute ((!:=), (<:=>))

import Deku.DOM.Elt.Circle (circle)
import Deku.DOM.Elt.Rect (rect)
import Deku.DOM.Elt.Svg (svg)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Behavior (Behavior, behavior, sampleBy, sample_)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Class (fold, (*|>))
import FRP.Event.Time (interval)
import Deku.DOM.Attr.Cx (Cx(Cx)) as D
import Deku.DOM.Attr.Cy (Cy(Cy)) as D
import Deku.DOM.Attr.Fill (Fill(Fill)) as D
import Deku.DOM.Attr.Height (Height(Height)) as D
import Deku.DOM.Attr.R (R(R)) as D
import Deku.DOM.Attr.Width (Width(Width)) as D
import Deku.DOM.Attr.X (X(X)) as D
import Deku.DOM.Attr.Y (Y(Y)) as D
import FRP.Event.Keyboard (down) as Keyboard
import Effect.Ref (modify, modify_, new, read) as Ref
import Data.Set (empty) as Set
import Effect.Ref (Ref)
import Deku.Control ((<#~>))
import Data.Tuple (Tuple, Tuple(..), fst)
import Data.Function (flip)
import FRP.Event.AnimationFrame (animationFrame)
import Data.Array (concatMap, foldMap, (..))
import Data.Tuple.Nested ((/\))
import Data.Set (filter)
import Data.Array (difference, filter) as Array
import FRP.Event.Mouse (down) as Mouse

x_max :: Int
x_max = 1920

y_max :: Int
y_max = 1008

type Ball =
  { x :: Int
  , y :: Int
  , dx :: Int
  , dy :: Int
  }

type Brick =
  { x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  }

mkBrick x y = { x, y, width: 50, height: 25 }

ball_init :: Ball
ball_init = { x: x_max / 2, y: y_max / 2, dx: 10, dy: -10 }

ref_to_behaviour :: Ref ~> Behavior
ref_to_behaviour ref = behavior \event ->
  makeEvent \k ->
    subscribe event \f -> do
      value <- Ref.read ref
      k $ f value

main :: Effect Unit
main = do
  bricks <- Ref.new $ (1..10) # concatMap \ x -> [mkBrick (100*x) 20, mkBrick (100*x) 60 ]
  let brick_behaviour = ref_to_behaviour bricks
  runInBody do
    let
      update :: Tuple Ball (Array Brick) -> Tuple Int (Array Brick) -> Tuple Ball (Array Brick)
      update (Tuple { x, y, dx, dy } _) (Tuple px live_bricks) =
        { x: x + dx
        , y: y + dy
        , dx: if x < 0 then abs dx else if x_max < x then abs dx * -1 else dx
        , dy:
            if y < 0 then abs dy
            else if px < x && x < px + 200 && y < y_max + 20 && y_max - 20 < y then abs dy * -1
            else dy
        } /\ (live_bricks # Array.filter \ {x: bx, y: by, width, height} ->
            bx < x && x < bx + width && by < y && y < by + height)

      move_paddle d x = max 0 (x + d) # min (x_max - 200)

      state :: Event (Tuple Ball (Array Brick))
      state = pure (ball_init /\ []) <|> fold update (ball_init /\ []) paddle_and_bricks

      ball :: Event Ball
      ball = makeEvent \k -> do
        subscribe state \ (b /\ dead_bricks) -> do
            k b
            bricks # Ref.modify_ \bs -> Array.difference bs dead_bricks

      paddle_x :: Event Int
      paddle_x = pure 880 <|>
        ( fold move_paddle 880 $ interval 20 *|> Keyboard.down
            <#> case _ of
              "ArrowRight" -> Just 10
              "ArrowLeft" -> Just $ -10
              _ -> Nothing
            # compact
        )

      paddle_and_bricks :: Event (Tuple Int (Array Brick))
      paddle_and_bricks = sampleBy (flip Tuple) brick_behaviour paddle_x

    svg (D.Width !:= "100vw" <|> D.Height !:= "100vh")
      [ rect
          ( D.Width !:= "200"
              <|> D.Height !:= "20"
              <|> D.X <:=> show <$> paddle_x
              <|> D.Y !:= show (y_max - 20)
          )
          []
      , ball <#~> \{ x, y }  -> circle
          ( D.R !:= "20"
              <|> D.Cx !:= show x
              <|> D.Cy !:= show y
              <|> D.Fill !:= "red"
          )
          []
      , sample_ brick_behaviour animationFrame <#~> foldMap \{ x, y, width, height } -> rect
          ( D.Width !:= show width
              <|> D.Height !:= show height
              <|> D.X !:= show x
              <|> D.Y !:= show y
          )
          []
      ]
