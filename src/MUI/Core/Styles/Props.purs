module MUI.Core.Styles.Props where

import MUI.Core.AppBar (AppBarProps)
import MUI.Core.Avatar (AvatarProps)
import MUI.Core.Backdrop (BackdropProps)
import MUI.Core.Badge (BadgeProps)
import MUI.Core.BottomNavigation (BottomNavigationProps)
import MUI.Core.BottomNavigationAction (BottomNavigationActionProps)
import MUI.Core.Breadcrumbs (BreadcrumbsProps)
-- import MUI.Core.Button (ButtonProps)
-- import MUI.Core.ButtonBase (ButtonBaseProps)
-- import MUI.Core.ButtonGroup (ButtonGroupProps)
import MUI.Core.Card (CardProps)
import MUI.Core.CardActionArea (CardActionAreaProps)
import MUI.Core.CardActions (CardActionsProps)
import MUI.Core.CardContent (CardContentProps)
import MUI.Core.CardHeader (CardHeaderProps)
import MUI.Core.CardMedia (CardMediaProps)
import MUI.Core.Divider (DividerPropsPartial)
import MUI.Core.Drawer (DrawerPropsPartial)
import MUI.Core.Grid (GridProps)
import MUI.Core.Icon (IconPropsPartial)
import MUI.Core.IconButton (IconButtonPropsPartial)
-- import MUI.Core.LinearProgress (LinearProgressPropsPartial)
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
import MUI.Core.Typography (TypographyProps)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)


type ComponentsProps =
  ( 
    "MuiAppBar" :: AppBarProps
  , "MuiAvatar" :: AvatarProps
  , "MuiBackdrop" :: BackdropProps
  , "MuiBadge" :: BadgeProps
  , "MuiBottomNavigation" :: BottomNavigationProps
  , "MuiBottomNavigationAction" :: BottomNavigationActionProps
  , "MuiBreadcrumbs" :: BreadcrumbsProps
--  , "MuiButton" :: ButtonProps
--  , "MuiButtonBase" :: ButtonBaseProps
--  , "MuiButtonGroup" :: ButtonGroupProps
  , "MuiCard" :: CardProps
  , "MuiCardActionArea" :: CardActionAreaProps
  , "MuiCardActions" :: CardActionsProps
  , "MuiCardContent" :: CardContentProps
  , "MuiCardHeader" :: CardHeaderProps
  , "MuiCardMedia" :: CardMediaProps
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
  -- , "MuiLinearProgress" :: LinearProgressPropsPartial
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
  , "MuiTypography" :: TypographyProps
  --, "MuiWithWidth" :: WithWidthOptions
  )

foreign import data ComponentsPropsPartial :: Type

componentsPropsPartial :: ∀ props props_
  .  Union props props_ ComponentsProps
  => Record props 
  -> ComponentsPropsPartial
componentsPropsPartial = unsafeCoerce
