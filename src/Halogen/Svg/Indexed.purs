module Halogen.Svg.Indexed
  ( CoreAttributes
  , GlobalEventAttributes
  , GlobalAttributes
  , StrokeAttributes
  , StokeEndAttributes
  , StrokeJoinAttributes
  , FillAttributes
  , MarkerAttributes
  , FontAttributes
  , AllPresentationAttributes
  , SVGsvg
  , SVGg
  , SVGforeignObject
  , SVGmarker
  , SVGcircle
  , SVGellipse
  , SVGline
  , SVGpath
  , SVGrect
  , SVGtext
  , AnimationAttributes
  , SVGanimate
  , SVGanimateMotion
  , SVGmpath
  , SVGtitle
  ) where

import Type.Row (type (+))
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)

{-
The table below show which groups of attributes apply to which elements. This
table is compiled by looking up each attribute on MDN
(e.g., https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke)
and looking at the list labeled "You can use this attribute with the following
SVG elements:". Groups are formed from attributes that have the same element
applicability.

element            stroke | strokeEnd | strokeJoin | fill  | font  | marker
circle                X   |     -     |     -      |   X   |   -   |    X
ellipse               X   |     -     |     -      |   X   |   -   |    X
line                  X   |     X     |     -      |   -   |   -   |    X
path                  X   |     X     |     X      |   X   |   -   |    X
rect                  X   |     -     |     X      |   X   |   -   |    X
text                  X   |     X     |     X      |   X   |   X   |    -
svg                   C   |     C     |     C      |   C   |   C   |    C
g                     C   |     C     |     C      |   C   |   C   |    C
marker                C   |     C     |     C      |   C   |   C   |    C
foreignObject         C   |     C     |     C      |   C   |   C   |    C

X indicates that the collection of attributes applies to that element
- indicates that the collection of attributes does not apply to that element
C indicates that the collection of attributes does not apply to that element
  but may apply to a child element and hence can still be set
-}

-- These core attributes are applicable to every element
type CoreAttributes r = ( id :: String, "class" :: String | r)

-- Subset of events that work on Firefox 60/Chromium 66
type GlobalEventAttributes r =
  ( onClick :: MouseEvent
  , onDoubleClick :: MouseEvent
  , onContextMenu :: MouseEvent
  , onKeyDown :: KeyboardEvent
  , onKeyPress :: KeyboardEvent
  , onKeyUp :: KeyboardEvent
  , onMouseDown :: MouseEvent
  , onMouseEnter :: MouseEvent
  , onMouseLeave :: MouseEvent
  , onMouseMove :: MouseEvent
  , onMouseOut :: MouseEvent
  , onMouseOver :: MouseEvent
  , onMouseUp :: MouseEvent
  , onWheel :: WheelEvent
  | r)

-- These can also be done with CSS
type PresentationAttributes r =
  ( stroke :: String
  , strokeWidth :: Number
  , fill :: String
  | r
  )

type MarkerAttributes r =
  ( markerStart :: String
  , markerMid :: String
  , markerEnd :: String
  | r
  )

type GlobalAttributes r = CoreAttributes + GlobalEventAttributes + r

-- Presentation attributes, grouped by applicability (see table above) ---------
type StrokeAttributes r =
  ( stroke :: String
  , strokeDasharray :: String
  , strokeDashoffset :: Number
  , strokeOpacity :: Number
  , strokeWidth :: Number
  | r
  )

type StokeEndAttributes r =
  ( strokeLinecap :: String
  | r
  )

type StrokeJoinAttributes r =
  ( strokeLinejoin :: String
  , strokeMiterlimit :: String
  | r
  )

type FillAttributes r =
  ( fill :: String
  , fillOpacity :: Number
  | r
  )

type FontAttributes r =
  ( font_family :: String
  , font_size :: String
  , font_sizeAdjust :: Number
  , font_stretch :: String
  , font_style :: String
  , font_variant :: String
  , font_weight :: String
  | r
  )

type AllPresentationAttributes r
  = StrokeAttributes + StrokeJoinAttributes + StokeEndAttributes
  + FillAttributes + FontAttributes + MarkerAttributes + r

-- Specific SVG elements -------------------------------------------------------
type SVGsvg
  = GlobalAttributes + AllPresentationAttributes
  + ( width :: Number
    , height :: Number
    , viewBox :: String
    , preserveAspectRatio :: String
    )

type SVGg
  = GlobalAttributes + AllPresentationAttributes
  + ( transform :: String )

type SVGforeignObject
  = GlobalAttributes + AllPresentationAttributes
  + ( x :: Number
    , y :: Number
    , height :: Number
    , width :: Number
    , style :: Number
    )

type SVGmarker
  = GlobalAttributes + AllPresentationAttributes
  + ( markerWidth :: Number
    , markerHeight :: Number
    , strokeWidth :: Number
    , refX :: Number
    , refY :: Number
    , orient :: String
    , markerUnits :: String
    )

type SVGcircle
  = GlobalAttributes + StrokeAttributes + FillAttributes + MarkerAttributes
  + ( cx :: Number
    , cy :: Number
    , r :: Number
    , transform :: String
    )

type SVGellipse
  = GlobalAttributes + StrokeAttributes + FillAttributes + MarkerAttributes
  + ( cx :: Number
    , cy :: Number
    , rx :: Number
    , ry :: Number
    , transform :: String
    )

type SVGline
  = GlobalAttributes + StrokeAttributes + StokeEndAttributes + MarkerAttributes
  + ( x1 :: Number
    , y1 :: Number
    , x2 :: Number
    , y2 :: Number
    , transform :: String
    )

type SVGpath
  = GlobalAttributes + StrokeAttributes + StokeEndAttributes
  + StrokeJoinAttributes + FillAttributes + MarkerAttributes
  + ( d :: String
    , transform :: String
    )

type SVGrect
  = GlobalAttributes + StrokeAttributes + StrokeJoinAttributes
  + FillAttributes + MarkerAttributes
  + ( x :: Number
    , y :: Number
    , rx :: Number
    , ry :: Number
    , width :: Number
    , height :: Number
    , transform :: String
    )

type SVGtext
  = GlobalAttributes + StrokeAttributes + StokeEndAttributes
  + StrokeJoinAttributes + FillAttributes + FontAttributes
  + ( x :: Number
    , y :: Number
    , text_anchor :: String
    , dominant_baseline :: String
    , transform :: String
    )

--------------------------------------------------------------------------------

type AnimationAttributes r = GlobalAttributes
  ( from :: String
  , to :: String
  , begin :: String
  , dur :: String
  , repeatCount :: Int
  , fill :: String
  | r
  )
{- ^ Unlike `fill` in `GlobalAttributes`, `fill` in `AnimationAttributes` is
     intended to record a `FillState` via `fillAnim`. -}

type SVGanimate = AnimationAttributes (attributeName :: String)

type SVGanimateMotion = AnimationAttributes (path :: String)

-- TODO should this have GlobalAttributes?
type SVGmpath = (xlinkHref :: String)

--------------------------------------------------------------------------------

type SVGtitle = GlobalAttributes ()
