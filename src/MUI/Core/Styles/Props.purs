module MUI.Core.Styles.Props where

import MUI.Core.AppBar (AppBarProps)
import MUI.Core.Avatar (AvatarProps)
import MUI.Core.Backdrop (BackdropProps)
import MUI.Core.Badge (BadgeProps)
import MUI.Core.BottomNavigation (BottomNavigationProps)
import MUI.Core.BottomNavigationAction (BottomNavigationActionPropsPartial)
import MUI.Core.Breadcrumbs (BreadcrumbsPropsPartial)
import MUI.Core.Button (ButtonPropsPartial)
import MUI.Core.ButtonBase (ButtonBasePropsPartial)
import MUI.Core.ButtonGroup (ButtonGroupPropsPartial)
import MUI.Core.Card (CardPropsPartial)
import MUI.Core.CardActionArea (CardActionAreaPropsPartial)
import MUI.Core.CardActions (CardActionsPropsPartial)
import MUI.Core.CardContent (CardContentPropsPartial)
import MUI.Core.CardHeader (CardHeaderPropsPartial)
import MUI.Core.CardMedia (CardMediaPropsPartial)
import MUI.Core.Divider (DividerPropsPartial)
import MUI.Core.Drawer (DrawerPropsPartial)
import MUI.Core.Grid (GridProps)
import MUI.Core.Icon (IconPropsPartial)
import MUI.Core.IconButton (IconButtonPropsPartial)
import MUI.Core.LinearProgress (LinearProgressPropsPartial)
import MUI.Core.Link (LinkPropsPartial)
import MUI.Core.List (ListPropsPartial)
import MUI.Core.ListItem (ListItemPropsPartial)
import MUI.Core.ListItemIcon (ListItemIconPropsPartial)
import MUI.Core.ListItemText (ListItemTextPropsPartial)
import MUI.Core.Modal (ModalPropsPartial)
import MUI.Core.Paper (PaperPropsPartial)
import MUI.Core.SvgIcon (SvgIconPropsPartial)
import MUI.Core.Tab (TabPropsPartial)
import MUI.Core.Table (TablePropsPartial)
import MUI.Core.TableBody (TableBodyPropsPartial)
import MUI.Core.TableCell (TableCellPropsPartial)
import MUI.Core.TableFooter (TableFooterPropsPartial)
import MUI.Core.TableHead (TableHeadPropsPartial)
import MUI.Core.TablePagination (TablePaginationPropsPartial)
import MUI.Core.TableRow (TableRowPropsPartial)
import MUI.Core.TableSortLabel (TableSortLabelPropsPartial)
import MUI.Core.Tabs (TabsPropsPartial)
import MUI.Core.TextField (TextFieldPropsPartial)
import MUI.Core.Typography (TypographyPropsPartial)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)


type ComponentsProps =
  ( 
    "MuiAppBar" :: AppBarProps
  , "MuiAvatar" :: AvatarProps
  , "MuiBackdrop" :: BackdropProps
  , "MuiBadge" :: BadgeProps
  , "MuiBottomNavigation" :: BottomNavigationProps
  , "MuiBottomNavigationAction" :: BottomNavigationActionPropsPartial
  , "MuiBreadcrumbs" :: BreadcrumbsPropsPartial
  , "MuiButton" :: ButtonPropsPartial
  , "MuiButtonBase" :: ButtonBasePropsPartial
  , "MuiButtonGroup" :: ButtonGroupPropsPartial
  , "MuiCard" :: CardPropsPartial
  , "MuiCardActionArea" :: CardActionAreaPropsPartial
  , "MuiCardActions" :: CardActionsPropsPartial
  , "MuiCardContent" :: CardContentPropsPartial
  , "MuiCardHeader" :: CardHeaderPropsPartial
  , "MuiCardMedia" :: CardMediaPropsPartial
  -- , "MuiCheckbox" :: CheckboxPropsPartial
  -- , "MuiChip" :: ChipPropsPartial
  -- , "MuiCircularProgress" :: CircularProgressPropsPartial
  -- , "MuiCollapse" :: CollapsePropsPartial
  -- , "MuiCssBaseline" :: CssBaselinePropsPartial
  -- , "MuiDialog" :: DialogPropsPartial
  -- , "MuiDialogActions" :: DialogActionsPropsPartial
  -- , "MuiDialogContent" :: DialogContentPropsPartial
  -- , "MuiDialogContentText" :: DialogContentTextPropsPartial
  -- , "MuiDialogTitle" :: DialogTitlePropsPartial
  , "MuiDivider" :: DividerPropsPartial
  , "MuiDrawer" :: DrawerPropsPartial
  -- , "MuiExpansionPanel" :: ExpansionPanelPropsPartial
  -- , "MuiExpansionPanelActions" :: ExpansionPanelActionsPropsPartial
  -- , "MuiExpansionPanelDetails" :: ExpansionPanelDetailsPropsPartial
  -- , "MuiExpansionPanelSummary" :: ExpansionPanelSummaryPropsPartial
  -- , "MuiFilledInput" :: FilledInputPropsPartial
  -- , "MuiFormControl" :: FormControlPropsPartial
  -- , "MuiFormControlLabel" :: FormControlLabelPropsPartial
  -- , "MuiFormGroup" :: FormGroupPropsPartial
  -- , "MuiFormHelperText" :: FormHelperTextPropsPartial
  -- , "MuiFormLabel" :: FormLabelPropsPartial
  , "MuiGrid" :: GridProps
  -- , "MuiGridList" :: GridListPropsPartial
  -- , "MuiGridListTile" :: GridListTilePropsPartial
  -- , "MuiGridListTileBar" :: GridListTileBarPropsPartial
  , "MuiIcon" :: IconPropsPartial
  , "MuiIconButton" :: IconButtonPropsPartial
  -- , "MuiInput" :: InputPropsPartial
  -- , "MuiInputAdornment" :: InputAdornmentPropsPartial
  -- , "MuiInputBase" :: InputBasePropsPartial
  -- , "MuiInputLabel" :: InputLabelPropsPartial
  , "MuiLinearProgress" :: LinearProgressPropsPartial
  , "MuiLink" :: LinkPropsPartial
  , "MuiList" :: ListPropsPartial
  , "MuiListItem" :: ListItemPropsPartial
  -- , "MuiListItemAvatar" :: ListItemAvatarPropsPartial
  , "MuiListItemIcon" :: ListItemIconPropsPartial
  -- , "MuiListItemSecondaryAction" :: ListItemSecondaryActionPropsPartial
  , "MuiListItemText" :: ListItemTextPropsPartial
  -- , "MuiListSubheader" :: ListSubheaderPropsPartial
  -- , "MuiMenu" :: MenuPropsPartial
  -- , "MuiMenuItem" :: MenuItemPropsPartial
  -- , "MuiMenuList" :: MenuListPropsPartial
  -- , "MuiMobileStepper" :: MobileStepperPropsPartial
  , "MuiModal" :: ModalPropsPartial
  -- , "MuiNativeSelect" :: NativeSelectPropsPartial
  -- , "MuiOutlinedInput" :: OutlinedInputPropsPartial
  , "MuiPaper" :: PaperPropsPartial
  -- , "MuiPopover" :: PopoverPropsPartial
  -- , "MuiRadio" :: RadioPropsPartial
  -- , "MuiRadioGroup" :: RadioGroupPropsPartial
  -- , "MuiSelect" :: SelectPropsPartial
  -- , "MuiSlider" :: SliderPropsPartial
  -- , "MuiSnackbar" :: SnackbarPropsPartial
  -- , "MuiSnackbarContent" :: SnackbarContentPropsPartial
  -- , "MuiStep" :: StepPropsPartial
  -- , "MuiStepButton" :: StepButtonPropsPartial
  -- , "MuiStepConnector" :: StepConnectorPropsPartial
  -- , "MuiStepContent" :: StepContentPropsPartial
  -- , "MuiStepIcon" :: StepIconPropsPartial
  -- , "MuiStepLabel" :: StepLabelPropsPartial
  -- , "MuiStepper" :: StepperPropsPartial
  , "MuiSvgIcon" :: SvgIconPropsPartial
  -- , "MuiSwitch" :: SwitchPropsPartial
  , "MuiTab" :: TabPropsPartial
  , "MuiTable" :: TablePropsPartial
  , "MuiTableBody" :: TableBodyPropsPartial
  , "MuiTableCell" :: TableCellPropsPartial
  , "MuiTableFooter" :: TableFooterPropsPartial
  , "MuiTableHead" :: TableHeadPropsPartial
  , "MuiTablePagination" :: TablePaginationPropsPartial
  , "MuiTableRow" :: TableRowPropsPartial
  , "MuiTableSortLabel" :: TableSortLabelPropsPartial
  , "MuiTabs" :: TabsPropsPartial
  , "MuiTextField" :: TextFieldPropsPartial
  --, "MuiToolbar" :: ToolbarPropsPartial
  -- , "MuiTooltip" :: TooltipPropsPartial
  -- , "MuiTouchRipple" :: TouchRipplePropsPartial
  , "MuiTypography" :: TypographyPropsPartial
  --, "MuiWithWidth" :: WithWidthOptions
  )

foreign import data ComponentsPropsPartial :: Type

componentsPropsPartial :: ∀ props props_
  .  Union props props_ ComponentsProps
  => Record props 
  -> ComponentsPropsPartial
componentsPropsPartial = unsafeCoerce
