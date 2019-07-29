module MUI.Core.Styles.Overrides where

import MUI.Core.AppBar (AppBarClassKeyJSS)
import MUI.Core.Avatar (AvatarClassKeyJSS)
import MUI.Core.Backdrop (BackdropClassKeyJSS)
import MUI.Core.Badge (BadgeClassKeyJSS)
import MUI.Core.BottomNavigation (BottomNavigationClassKeyJSS)
import MUI.Core.BottomNavigationAction (BottomNavigationActionClassKeyJSS)
import MUI.Core.Breadcrumbs (BreadcrumbsClassKeyJSS)
import MUI.Core.Button (ButtonClassKeyJSS)
import MUI.Core.ButtonBase (ButtonBaseClassKeyJSS)
import MUI.Core.ButtonGroup (ButtonGroupClassKeyJSS)
import MUI.Core.Card (CardClassKeyJSS)
import MUI.Core.CardActionArea (CardActionAreaClassKeyJSS)
import MUI.Core.CardActions (CardActionsClassKeyJSS)
import MUI.Core.CardContent (CardContentClassKeyJSS)
import MUI.Core.CardHeader (CardHeaderClassKeyJSS)
import MUI.Core.CardMedia (CardMediaClassKeyJSS)
import MUI.Core.Divider (DividerClassKeyJSS)
import MUI.Core.Drawer (DrawerClassKeyJSS)
import MUI.Core.Grid (GridClassKeyJSS)
import MUI.Core.Icon (IconClassKeyJSS)
import MUI.Core.IconButton (IconButtonClassKeyJSS)
import MUI.Core.LinearProgress (LinearProgressClassKeyJSS)
import MUI.Core.Link (LinkClassKeyJSS)
import MUI.Core.List (ListClassKeyJSS)
import MUI.Core.ListItem (ListItemClassKeyJSS)
import MUI.Core.ListItemIcon (ListItemIconClassKeyJSS)
import MUI.Core.ListItemText (ListItemTextClassKeyJSS)
import MUI.Core.Modal (ModalClassKeyJSS)
import MUI.Core.Paper (PaperClassKeyJSS)
import MUI.Core.SvgIcon (SvgIconClassKeyJSS)
import MUI.Core.Tab (TabClassKeyJSS)
import MUI.Core.Table (TableClassKeyJSS)
import MUI.Core.TableBody (TableBodyClassKeyJSS)
import MUI.Core.TableCell (TableCellClassKeyJSS)
import MUI.Core.TableFooter (TableFooterClassKeyJSS)
import MUI.Core.TableHead (TableHeadClassKeyJSS)
import MUI.Core.TablePagination (TablePaginationClassKeyJSS)
import MUI.Core.TableRow (TableRowClassKeyJSS)
import MUI.Core.TableSortLabel (TableSortLabelClassKeyJSS)
import MUI.Core.Tabs (TabsClassKeyJSS)
import MUI.Core.TextField (TextFieldClassKeyJSS)
import MUI.Core.Typography (TypographyClassKeyJSS)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type Overrides = 
  (
    "MuiAppBar" :: AppBarClassKeyJSS
  , "MuiAvatar" :: AvatarClassKeyJSS
  , "MuiBackdrop" :: BackdropClassKeyJSS
  , "MuiBadge" :: BadgeClassKeyJSS
  , "MuiBottomNavigation" :: BottomNavigationClassKeyJSS
  , "MuiBottomNavigationAction" :: BottomNavigationActionClassKeyJSS
  , "MuiBreadcrumbs" :: BreadcrumbsClassKeyJSS
  , "MuiButton" :: ButtonClassKeyJSS
  , "MuiButtonBase" :: ButtonBaseClassKeyJSS
  , "MuiButtonGroup" :: ButtonGroupClassKeyJSS
  , "MuiCard" :: CardClassKeyJSS
  , "MuiCardActionArea" :: CardActionAreaClassKeyJSS
  , "MuiCardActions" :: CardActionsClassKeyJSS
  , "MuiCardContent" :: CardContentClassKeyJSS
  , "MuiCardHeader" :: CardHeaderClassKeyJSS
  , "MuiCardMedia" :: CardMediaClassKeyJSS
  --, "MuiCheckbox" :: CheckboxClassKeyJSS
  --, "MuiChip" :: ChipClassKeyJSS
  --, "MuiCircularProgress" :: CircularProgressClassKeyJSS
  --, "MuiCollapse" :: CollapseClassKeyJSS
  --, "MuiCssBaseline" :: CssBaselineClassKeyJSS
  --, "MuiDialog" :: DialogClassKeyJSS
  --, "MuiDialogActions" :: DialogActionsClassKeyJSS
  --, "MuiDialogContent" :: DialogContentClassKeyJSS
  --, "MuiDialogContentText" :: DialogContentTextClassKeyJSS
  --, "MuiDialogTitle" :: DialogTitleClassKeyJSS
  , "MuiDivider" :: DividerClassKeyJSS
  , "MuiDrawer" :: DrawerClassKeyJSS
  --, "MuiExpansionPanel" :: ExpansionPanelClassKeyJSS
  --, "MuiExpansionPanelActions" :: ExpansionPanelActionsClassKeyJSS
  --, "MuiExpansionPanelDetails" :: ExpansionPanelDetailsClassKeyJSS
  --, "MuiExpansionPanelSummary" :: ExpansionPanelSummaryClassKeyJSS
  --, "MuiFab" :: FabClassKeyJSS
  --, "MuiFilledInput" :: FilledInputClassKeyJSS
  --, "MuiFormControl" :: FormControlClassKeyJSS
  --, "MuiFormControlLabel" :: FormControlLabelClassKeyJSS
  --, "MuiFormGroup" :: FormGroupClassKeyJSS
  --, "MuiFormHelperText" :: FormHelperTextClassKeyJSS
  --, "MuiFormLabel" :: FormLabelClassKeyJSS
  , "MuiGrid" :: GridClassKeyJSS
  --, "MuiGridList" :: GridListClassKeyJSS
  --, "MuiGridListTile" :: GridListTileClassKeyJSS
  --, "MuiGridListTileBar" :: GridListTileBarClassKeyJSS
  , "MuiIcon" :: IconClassKeyJSS
  , "MuiIconButton" :: IconButtonClassKeyJSS
  --, "MuiInput" :: InputClassKeyJSS
  --, "MuiInputAdornment" :: InputAdornmentClassKeyJSS
  --, "MuiInputBase" :: InputBaseClassKeyJSS
  --, "MuiInputLabel" :: InputLabelClassKeyJSS
  , "MuiLinearProgress" :: LinearProgressClassKeyJSS
  , "MuiLink" :: LinkClassKeyJSS
  , "MuiList" :: ListClassKeyJSS
  , "MuiListItem" :: ListItemClassKeyJSS
  --, "MuiListItemAvatar" :: ListItemAvatarClassKeyJSS
  , "MuiListItemIcon" :: ListItemIconClassKeyJSS
  --, "MuiListItemSecondaryAction" :: ListItemSecondaryActionClassKeyJSS
  , "MuiListItemText" :: ListItemTextClassKeyJSS
  --, "MuiListSubheader" :: ListSubheaderClassKeyJSS
  --, "MuiMenu" :: MenuClassKeyJSS
  --, "MuiMenuItem" :: MenuItemClassKeyJSS
  --, "MuiMobileStepper" :: MobileStepperClassKeyJSS
  , "MuiModal" :: ModalClassKeyJSS
  --, "MuiNativeSelect" :: NativeSelectClassKeyJSS
  --, "MuiOutlinedInput" :: OutlinedInputClassKeyJSS
  , "MuiPaper" :: PaperClassKeyJSS
  --, "MuiPopover" :: PopoverClassKeyJSS
  --, "MuiRadio" :: RadioClassKeyJSS
  --, "MuiSelect" :: SelectClassKeyJSS
  --, "MuiSlider" :: SliderClassKeyJSS
  --, "MuiSnackbar" :: SnackbarClassKeyJSS
  --, "MuiSnackbarContent" :: SnackbarContentClassKeyJSS
  --, "MuiStep" :: StepClasskey
  --, "MuiStepButton" :: StepButtonClasskey
  --, "MuiStepConnector" :: StepConnectorClasskey
  --, "MuiStepContent" :: StepContentClasskey
  --, "MuiStepIcon" :: StepIconClasskey
  --, "MuiStepLabel" :: StepLabelClasskey
  --, "MuiStepper" :: StepperClasskey
  , "MuiSvgIcon" :: SvgIconClassKeyJSS
  --, "MuiSwitch" :: SwitchClassKeyJSS
  , "MuiTab" :: TabClassKeyJSS
  , "MuiTable" :: TableClassKeyJSS
  , "MuiTableBody" :: TableBodyClassKeyJSS
  , "MuiTableCell" :: TableCellClassKeyJSS
  , "MuiTableFooter" :: TableFooterClassKeyJSS
  , "MuiTableHead" :: TableHeadClassKeyJSS
  , "MuiTablePagination" :: TablePaginationClassKeyJSS
  , "MuiTableRow" :: TableRowClassKeyJSS
  , "MuiTableSortLabel" :: TableSortLabelClassKeyJSS
  , "MuiTabs" :: TabsClassKeyJSS
  , "MuiTextField" :: TextFieldClassKeyJSS
  --, "MuiToolbar" :: ToolbarClassKeyJSS
  --, "MuiTooltip" :: TooltipClassKeyJSS
  --, "MuiTouchRipple" :: TouchRippleClassKeyJSS
  , "MuiTypography" :: TypographyClassKeyJSS
)

foreign import data OverridesPartial :: Type

overridesPartial :: ∀ props props_
  .  Union props props_ Overrides 
  => Record props 
  -> OverridesPartial
overridesPartial = unsafeCoerce
