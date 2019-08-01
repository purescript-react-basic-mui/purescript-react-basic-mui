module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import MUI.Core.Typography (typography, typography_component)
import React.Basic (JSX, ReactComponent, fragment)
import React.Basic.DOM (Props_h1, render, unsafeCreateDOMComponent)
import React.Basic.DOM as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

regularTypography :: JSX
regularTypography = typography { children : [ R.text "This is regular typography" ] }

h1Typography :: JSX
h1Typography = do
  let (component :: ReactComponent { | Props_h1 }) = unsafeCreateDOMComponent "h1"
  typography_component
    { component
    , children : [ R.text "This is h1 typography" ] 
    } 

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Container element not found."
    Just c  ->
      let app = fragment [ regularTypography, h1Typography ]
       in render app c
