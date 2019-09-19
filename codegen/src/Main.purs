module Main where

import Prelude

import Codegen (codegen, write)
import Codegen.Core.AppBar as AppBar
import Codegen.Core.Avatar as Avatar
import Codegen.Core.Backdrop as Backdrop
import Codegen.Core.Badge as Badge
import Codegen.Core.BottomNavigation as BottomNavigation
import Codegen.Core.BottomNavigationAction as BottomNavigationAction
import Codegen.Core.Breadcrumbs as Breadcrumbs
import Codegen.Core.Button as Button
import Codegen.Core.ButtonBase as ButtonBase
import Codegen.Core.ButtonBase.TouchRipple as TouchRipple
import Codegen.Core.Card as Card
import Codegen.Core.CardActionArea as CardActionArea
import Codegen.Core.CardActions as CardActions
import Codegen.Core.CardContent as CardContent
import Codegen.Core.CardHeader as CardHeader
import Codegen.Core.CardMedia as CardMedia
import Codegen.Core.Checkbox as Checkbox
import Codegen.Core.Chip as Chip
import Codegen.Core.CircularProgress as CircularProgress
import Codegen.Core.ExpansionPanel as ExpansionPanel
import Codegen.Core.Grid as Grid
import Codegen.Core.IconButton as IconButton
import Codegen.Core.InputBase as InputBase
import Codegen.Core.Typography as Typography
import Codegen.Model (Component)
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)

components :: Array Component
components = 
  [ AppBar.component
  , Avatar.component
  , Backdrop.component
  , Badge.component
  , BottomNavigation.component
  , BottomNavigationAction.component
  , Breadcrumbs.component
  , ButtonBase.component
  , TouchRipple.component
  , Button.component
  , Card.component
  , CardActions.component
  , CardActionArea.component
  , CardContent.component
  , CardHeader.component
  , CardMedia.component
  , Checkbox.component
  , Chip.component
  , CircularProgress.component
  , ExpansionPanel.component
  , IconButton.component
  , InputBase.component
  , Grid.component
  , Typography.component
  ]

main :: Effect Unit
main = launchAff_ do
  let code = join $ map codegen components
  traverse_ (write "../src") code