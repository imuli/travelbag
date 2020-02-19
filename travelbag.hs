{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- colors to indicate folds
valley amt = lcA (blue `withOpacity` amt) . lw thin
mountain amt = lcA (red `withOpacity` amt) . lw thin
-- and edge of material
edge = lc black . lw thin

-- dimensions
bx = 250
by = 150
bz = 360
-- pocket dimensions
px = bx
py = 25
pz = bz - 2 * py - 2 * bm
-- margin
bm = 8

bxy = sqrt (bx * bx + by * by)

mkPath :: [(Double, Double)] -> Diagram B
mkPath = strokePath . fromOffsets . map r2

baseD :: Diagram B
baseD = center $ valley 0.5 $ mkPath
  [ (bx, 0)
  , (0, -by)
  ]

xflap :: Diagram B
xflap = translate (r2 (bx/2, -by/2)) $ valley 0.75 $ mkPath
  [ (bz, 0)
  , (bx, 0)
  , (-bx, by)
  , (-bz, 0)
  ]

topfold :: Diagram B
topfold = translate (r2 (bx/2 + bz, -by/2)) $ valley 0.5 $ mkPath
  [ (0, by) ]

centerfold :: Diagram B
centerfold = translate (r2 (bx/2, by/2 + 2*bm)) $ valley 1 $ mkPath
  [ (bxy, 0)
  , (0, bz - 2*bm)
  , (-bxy, 0)
  ]

-- this is actually more complicated, and probably a bit longer
centerfoldPocket :: Diagram B
centerfoldPocket = translate (r2 (bxy, 0)) centerfold

outerPocket :: Diagram B
outerPocket = translate (r2 (-bx/2 - py, by/2 + py + 2*bm)) $ mountain 0.5 $ mkPath
  [ (-px, 0)
  , (0, pz)
  , (px, 0)
  , (0, -pz)
  ]

outerPocketSeam :: Diagram B
outerPocketSeam = translate (r2 (-bx/2, by/2 + 2*bm)) $ mountain 0.75 $ mkPath
  [ (-px - 2*py, 0)
  , (0, pz + 2 * py)
  , (px + 2*py, 0)
  , (0, - pz - 2 * py)
  ]

yflap :: Diagram B
yflap = translate (r2 (-bx/2, by/2)) $ valley 0.75 $ mkPath
  [ (0, bz)
  , (bx, 0)
  , (0, -bz)
  ]

folds :: Diagram B
folds = mconcat pieces # frame bm
  where
    pieces = 
      [ baseD
      , outerPocketSeam
      , outerPocket
      , centerfoldPocket
      , centerfold
      , yflap
      , topfold
      , xflap
      ]

bothFolds = folds `atop` folds # rotateBy 0.5

main = mainWith $ bothFolds `atop` boundingRect bothFolds # edge # frame bm
