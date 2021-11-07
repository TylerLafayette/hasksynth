module Main where

import Synth
import Graphics.EasyPlot

main :: IO ()
main = do
    _ <- plot 
        (PNG "fm.png") 
        (Synth.calculate (Synth.fm 1.0 Synth.sineShape Synth.sawShape) . (* 48000.0))
    _ <- plot 
        (PNG "square.png") 
        (Synth.calculate Synth.squareShape . (* 48000.0))
    return ()
