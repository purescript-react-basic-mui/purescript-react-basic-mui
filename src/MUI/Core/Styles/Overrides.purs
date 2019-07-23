module MUI.Core.Styles.Overrides where

import MUI.Core.AppBar (AppBarClassKey)
import MUI.Core.Avatar (AvatarClassKey)
import MUI.Core.Backdrop (BackdropClassKey)
import MUI.Core.Badge (BadgeClassKey)
import MUI.Core.BottomNavigation (BottomNavigationClassKey)
import MUI.Core.BottomNavigationAction (BottomNavigationActionClassKey)
import MUI.Core.Breadcrumbs (BreadcrumbsClassKey)
import MUI.Core.Button (ButtonClassKey)
import MUI.Core.ButtonBase (ButtonBaseClassKey)
import MUI.Core.ButtonGroup (ButtonGroupClassKey)
import MUI.Core.Card (CardClassKey)
import MUI.Core.CardActionArea (CardActionAreaClassKey)
import MUI.Core.CardActions (CardActionsClassKey)
import MUI.Core.CardContent (CardContentClassKey)
import MUI.Core.CardHeader (CardHeaderClassKey)
import MUI.Core.CardMedia (CardMediaClassKey)
import MUI.Core.Divider (DividerClassKey)
import MUI.Core.Drawer (DrawerClassKey)
import MUI.Core.Grid (GridClassKey)
import MUI.Core.Icon (IconClassKey)
import MUI.Core.IconButton (IconButtonClassKey)
import MUI.Core.LinearProgress (LinearProgressClassKey)
import MUI.Core.Link (LinkClassKey)
import MUI.Core.List (ListClassKey)
import MUI.Core.ListItem (ListItemClassKey)
import MUI.Core.ListItemIcon (ListItemIconClassKey)
import MUI.Core.ListItemText (ListItemTextClassKey)
import MUI.Core.Modal (ModalClassKey)
import MUI.Core.Paper (PaperClassKey)
import MUI.Core.SvgIcon (SvgIconClassKey)
import MUI.Core.Table (TableClassKey)
import MUI.Core.TableBody (TableBodyClassKey)
import MUI.Core.TableCell (TableCellClassKey)
import MUI.Core.TableFooter (TableFooterClassKey)
import MUI.Core.TableHead (TableHeadClassKey)
import MUI.Core.TablePagination (TablePaginationClassKey)
import MUI.Core.TableRow (TableRowClassKey)
import MUI.Core.TableSortLabel (TableSortLabelClassKey)
import MUI.Core.TextField (TextFieldClassKey)
import MUI.Core.Typography (TypographyClassKey)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type Overrides = 
  (
    "MuiAppBar" :: AppBarClassKey
  , "MuiAvatar" :: AvatarClassKey
  , "MuiBackdrop" :: BackdropClassKey
  , "MuiBadge" :: BadgeClassKey
  , "MuiBottomNavigation" :: BottomNavigationClassKey
  , "MuiBottomNavigationAction" :: BottomNavigationActionClassKey
  , "MuiBreadcrumbs" :: BreadcrumbsClassKey
  , "MuiButton" :: ButtonClassKey
  , "MuiButtonBase" :: ButtonBaseClassKey
  , "MuiButtonGroup" :: ButtonGroupClassKey
  , "MuiCard" :: CardClassKey
  , "MuiCardActionArea" :: CardActionAreaClassKey
  , "MuiCardActions" :: CardActionsClassKey
  , "MuiCardContent" :: CardContentClassKey
  , "MuiCardHeader" :: CardHeaderClassKey
  , "MuiCardMedia" :: CardMediaClassKey
  --, "MuiCheckbox" :: CheckboxClassKey
  --, "MuiChip" :: ChipClassKey
  --, "MuiCircularProgress" :: CircularProgressClassKey
  --, "MuiCollapse" :: CollapseClassKey
  --, "MuiCssBaseline" :: CssBaselineClassKey
  --, "MuiDialog" :: DialogClassKey
  --, "MuiDialogActions" :: DialogActionsClassKey
  --, "MuiDialogContent" :: DialogContentClassKey
  --, "MuiDialogContentText" :: DialogContentTextClassKey
  --, "MuiDialogTitle" :: DialogTitleClassKey
  , "MuiDivider" :: DividerClassKey
  , "MuiDrawer" :: DrawerClassKey
  --, "MuiExpansionPanel" :: ExpansionPanelClassKey
  --, "MuiExpansionPanelActions" :: ExpansionPanelActionsClassKey
  --, "MuiExpansionPanelDetails" :: ExpansionPanelDetailsClassKey
  --, "MuiExpansionPanelSummary" :: ExpansionPanelSummaryClassKey
  --, "MuiFab" :: FabClassKey
  --, "MuiFilledInput" :: FilledInputClassKey
  --, "MuiFormControl" :: FormControlClassKey
  --, "MuiFormControlLabel" :: FormControlLabelClassKey
  --, "MuiFormGroup" :: FormGroupClassKey
  --, "MuiFormHelperText" :: FormHelperTextClassKey
  --, "MuiFormLabel" :: FormLabelClassKey
  , "MuiGrid" :: GridClassKey
  --, "MuiGridList" :: GridListClassKey
  --, "MuiGridListTile" :: GridListTileClassKey
  --, "MuiGridListTileBar" :: GridListTileBarClassKey
  , "MuiIcon" :: IconClassKey
  , "MuiIconButton" :: IconButtonClassKey
  --, "MuiInput" :: InputClassKey
  --, "MuiInputAdornment" :: InputAdornmentClassKey
  --, "MuiInputBase" :: InputBaseClassKey
  --, "MuiInputLabel" :: InputLabelClassKey
  , "MuiLinearProgress" :: LinearProgressClassKey
  , "MuiLink" :: LinkClassKey
  , "MuiList" :: ListClassKey
  , "MuiListItem" :: ListItemClassKey
  --, "MuiListItemAvatar" :: ListItemAvatarClassKey
  , "MuiListItemIcon" :: ListItemIconClassKey
  --, "MuiListItemSecondaryAction" :: ListItemSecondaryActionClassKey
  , "MuiListItemText" :: ListItemTextClassKey
  --, "MuiListSubheader" :: ListSubheaderClassKey
  --, "MuiMenu" :: MenuClassKey
  --, "MuiMenuItem" :: MenuItemClassKey
  --, "MuiMobileStepper" :: MobileStepperClassKey
  , "MuiModal" :: ModalClassKey
  --, "MuiNativeSelect" :: NativeSelectClassKey
  --, "MuiOutlinedInput" :: OutlinedInputClassKey
  , "MuiPaper" :: PaperClassKey
  --, "MuiPopover" :: PopoverClassKey
  --, "MuiRadio" :: RadioClassKey
  --, "MuiSelect" :: SelectClassKey
  --, "MuiSlider" :: SliderClassKey
  --, "MuiSnackbar" :: SnackbarClassKey
  --, "MuiSnackbarContent" :: SnackbarContentClassKey
  --, "MuiStep" :: StepClasskey
  --, "MuiStepButton" :: StepButtonClasskey
  --, "MuiStepConnector" :: StepConnectorClasskey
  --, "MuiStepContent" :: StepContentClasskey
  --, "MuiStepIcon" :: StepIconClasskey
  --, "MuiStepLabel" :: StepLabelClasskey
  --, "MuiStepper" :: StepperClasskey
  , "MuiSvgIcon" :: SvgIconClassKey
  --, "MuiSwitch" :: SwitchClassKey
  --, "MuiTab" :: TabClassKey
  , "MuiTable" :: TableClassKey
  , "MuiTableBody" :: TableBodyClassKey
  , "MuiTableCell" :: TableCellClassKey
  , "MuiTableFooter" :: TableFooterClassKey
  , "MuiTableHead" :: TableHeadClassKey
  , "MuiTablePagination" :: TablePaginationClassKey
  , "MuiTableRow" :: TableRowClassKey
  , "MuiTableSortLabel" :: TableSortLabelClassKey
  --, "MuiTabs" :: TabsClassKey
  , "MuiTextField" :: TextFieldClassKey
  --, "MuiToolbar" :: ToolbarClassKey
  --, "MuiTooltip" :: TooltipClassKey
  --, "MuiTouchRipple" :: TouchRippleClassKey
  , "MuiTypography" :: TypographyClassKey
)

foreign import data OverridesPartial :: Type

overridesPartial :: ∀ props props_
  .  Union props props_ Overrides 
  => Record props 
  -> OverridesPartial
overridesPartial = unsafeCoerce
