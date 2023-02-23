module Main where

import Prelude

import Effect (Effect)

import Deku.Toplevel (runInBody)

import Deku.DOM.Elt.Svg (svg)
import Deku.DOM.Attr.Width (Width(Width)) as D
import Deku.Attribute ((!:=), (<:=>))
import Deku.DOM.Attr.Height (Height(Height)) as D
import Control.Alt ((<|>))
import Deku.DOM.Elt.Rect (rect)
import Deku.DOM.Attr.X (X(X)) as D
import Deku.DOM.Attr.Y (Y(Y)) as D
import FRP.Event.Time (interval)
import FRP.Event.Class (fold, (*|>))
import FRP.Event.Keyboard (down) as Keyboard
import FRP.Event (Event)

import Data.Ord (abs)
import Deku.DOM.Elt.Circle (circle)
import Deku.DOM.Attr.R (R(R)) as D
import Deku.DOM.Attr.Cx (Cx(Cx)) as D
import Deku.DOM.Attr.Cy (Cy(Cy)) as D
import Deku.DOM.Attr.Fill (Fill(Fill)) as D
import Data.Compactable (compact)
import Data.Maybe (Maybe(..))

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

ball_init :: Ball
ball_init = { x: x_max / 2, y: y_max / 2, dx: 10, dy: -10 }

main :: Effect Unit
main = do
  runInBody do
    let
      move_ball :: Ball -> Int -> Ball
      move_ball { x, y, dx, dy } px =
        { x: x + dx
        , y: y + dy
        , dx: if x < 0 then abs dx else if x_max < x then abs dx * -1 else dx
        , dy:
            if y < 0 then abs dy
            else if px < x && x < px + 200 && y < y_max + 20 && y_max - 20 < y then abs dy * -1
            else dy
        }
      move_paddle d x = max 0 (x + d) # min (x_max - 200)

      ball :: Event Ball
      ball = pure ball_init <|> fold move_ball ball_init paddle_x

      paddle_x :: Event Int
      paddle_x = pure 880 <|>
        ( fold move_paddle 880 $ interval 20 *|> Keyboard.down
            <#> case _ of
              "ArrowRight" -> Just 10
              "ArrowLeft" -> Just $ -10
              _ -> Nothing
            # compact
        )
    svg (D.Width !:= "100vw" <|> D.Height !:= "100vh")
      [ rect
          ( D.Width !:= "200"
              <|> D.Height !:= "20"
              <|> D.X <:=> show <$> paddle_x
              <|> D.Y !:= show (y_max - 20)
          )
          []
      , circle
          ( D.R !:= "20"
              <|> D.Cx <:=> (ball <#> (_.x >>> show))
              <|> D.Cy <:=> (ball <#> (_.y >>> show))
              <|> D.Fill !:= "red"
          )
          []
      ]
