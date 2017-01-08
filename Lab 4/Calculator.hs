import Control.Monad (when)

import Haste hiding (eval)
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

import Pages

import Expr

import Data.Maybe



canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw e c = do 
                Just exp  <- (getValue e)
                render    <- (render c (stroke (path (points (fromJust (readExpr exp)) 0.04 (canWidth, canHeight)))))
                return render

zoom :: Elem -> Elem -> Canvas -> IO()
zoom expr zoom c = do
                Just exp   <- (getValue expr)
                Just zoom' <- (getValue zoom)
                render     <- (render c (stroke (path (points (fromJust (readExpr exp)) ((1/zoom')*0.04) (canWidth, canHeight)))))
                return render

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
    zoomV   <- mkInput 20 "Enter zoom value"
    doZoom  <- mkButton "Zoom"
    doDiff  <- mkButton "Differntiate"
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw,zoomV,doZoom,doDiff]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  Click  $ \_    -> readAndDraw input can
    onEvent input KeyUp  $ \code -> when (code==13) $ readAndDraw input can
    onEvent doZoom Click $ \_    -> zoom input zoomV can
    onEvent doDiff Click $ \_    -> do
                                    Just expr <- getValue input
                                    set input [prop "value" =: (showExpr(simplify (differentiate (fromJust (readExpr expr)))))]
                                    readAndDraw input can

      -- "Enter" key has code 13

points :: Expr -> Double -> (Int, Int) -> [Point]
points e scale (w,h) = [((fromIntegral x), (realToPix (eval e (pixToReal (fromIntegral x))))) 
        | x <- [0..w]]
            where
                -- converts a pixel x-coordinate to a real x-coordinate
                pixToReal :: Double -> Double
                pixToReal x = scale*(x-((fromIntegral w)/2))

                -- converts a real y-coordinate to a pixel y-coordinate
                realToPix :: Double -> Double
                realToPix y = (-(y/scale))+((fromIntegral h)/2)