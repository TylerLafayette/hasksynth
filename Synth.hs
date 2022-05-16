module Synth
( Samples
, Sample
, Hz
, Semitones
, sampleRate
, OscShape
, calculate
, sawShape
, sineShape
, squareShape
, a
, render
, fm
) where

import           Data.Foldable
import           Data.Fixed
import GHC.Float.RealFracMethods (roundFloatInt)

type Samples = Float
type Sample = Float
type Hz = Float
type Semitones = Float
type Octave = Int
-- | The type 'Duration' represents a duration in seconds.
type Duration = Float

data Note = A
          | ASharp
          | B
          | C
          | CSharp
          | D
          | DSharp
          | E
          | F
          | FSharp
          | G
          | GSharp
          deriving (Enum, Show, Eq)

-- | The 'noteToOffset' function takes a Note enum and converts it to an offset from 0 semitones (A).
noteToOffset :: Note -> Semitones
noteToOffset note = case note of
                      A      -> 0
                      ASharp -> 1
                      B      -> 2
                      C      -> 3
                      CSharp -> 4
                      D      -> 5
                      DSharp -> 6
                      E      -> 7
                      F      -> 8
                      FSharp -> 9
                      G      -> 10
                      GSharp -> 11

sampleRate :: Samples
sampleRate = 48000.0

-- | The 'a' value represents the tuning of the note A4.
a :: Hz
a = 440.0

-- | The 'transpose' function transposes a frequency in Hz
-- by the given number of Semitones.
transpose :: Semitones -> Hz -> Hz
transpose delta input = input * 2 ** (delta /12)

-- | The 'semitonesToHz' function returns the transpose of
-- A4 of the given number of Semitones.
semitonesToHz :: Semitones -> Hz
semitonesToHz = flip transpose a

-- | The 'noteToHz' function is a convenience function
-- which converts a note to a Hz value in the 4th octave.
noteToHz :: Note -> Hz
noteToHz = semitonesToHz . noteToOffset

-- | The 'transposeOctave' function is a convenience function
-- which multiplies the octave by 12 Semitones and applies it as
-- a transpose to the given frequency.
transposeOctave :: Octave -> Hz -> Hz
transposeOctave octave = transpose semitones
          where semitones = fromIntegral (octave * 12)

volume :: Float
volume = 0.3

newtype OscShape = OscShape {
          calculate :: Float -> Sample
                         } 


sineShape :: OscShape
sineShape = OscShape $ sin 
                     . (* (2*pi/sampleRate))

sawShape :: OscShape
sawShape = OscShape $ normalize 
                    . modOne
                    . divisorFirst sampleRate
          where mod = flip mod'
                modOne = mod 1.0
                divisorFirst = flip (/)
                normalize = (\x -> x - 1.0) . (* 2)

squareShape :: OscShape
squareShape = OscShape $ fromIntegral
                       . roundFloatInt 
                       . modOne
                       . divisorFirst sampleRate 
          where mod = flip mod'
                modOne = mod 1.0
                divisorFirst = flip (/)

-- | The 'fm' function applies frequency modulation from a modulator oscillator
-- to a carrier oscillator.
fm :: Float     -- ^ FM magnitude from 0.0 to 1.0.
   -> OscShape  -- ^ Modulator shape.
   -> OscShape  -- ^ Carrier shape.
   -> OscShape  -- ^ An oscillator shape with the FM applied.
fm amt modulator carrier = OscShape $ calculate carrier
                                    . (* sampleRate)
                                    . calculate modulator

renderNote :: (Note, Duration) -> [Sample]
renderNote (note, duration) = map (((* volume) . calculate (sawShape)) . (* noteToHz note)) [0.0..sampleRate*duration]

renderSequence :: [(Note, Duration)] -> [Sample]
renderSequence = concatMap renderNote

-- | Mixes two samples together.
mix :: Sample -> Sample -> Sample
mix = (+) 

-- | Renders notes together and mixes them.
renderNoteGroup :: [(Note, Duration)] -> [Sample]
renderNoteGroup notes = foldl folder container padded 
          where 
                    folder = zipWith mix
                    m = map renderNote notes
                    maxLength = maximum $ map length m
                    container = replicate maxLength 0.0
                    padded = map (++ repeat 0.0) m

render :: [Sample]
render = renderNoteGroup [ (C, 5.0)
                         , (E, 5.0)
                         , (G, 5.0)
                        ]
-- play :: Note -> Octave -> Duration -> [Sample]
-- play note octave duration =
