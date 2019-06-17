module React.Basic.MUI.Core.Styles.Props where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.AppBar (AppBarProps)
import React.Basic.MUI.Core.Avatar (AvatarProps)
import React.Basic.MUI.Core.Backdrop (BackdropProps)
import React.Basic.MUI.Core.Badge (BadgeProps)
import React.Basic.MUI.Core.BottomNavigation (BottomNavigationProps)
import React.Basic.MUI.Core.BottomNavigationAction (BottomNavigationActionProps)
import React.Basic.MUI.Core.Breadcrumbs (BreadcrumbsProps)
import React.Basic.MUI.Core.Button (ButtonProps)
import React.Basic.MUI.Core.ButtonBase (ButtonBaseProps)
import React.Basic.MUI.Core.ButtonGroup (ButtonGroupProps)
import React.Basic.MUI.Core.Card (CardProps)
import React.Basic.MUI.Core.CardActions (CardActionsProps)
import React.Basic.MUI.Core.CardContent (CardContentProps)
import React.Basic.MUI.Core.CardHeader (CardHeaderProps)
import React.Basic.MUI.Core.CardMedia (CardMediaProps)
import React.Basic.MUI.Core.Checkbox (CheckboxProps)
import React.Basic.MUI.Core.Chip (ChipProps)
import React.Basic.MUI.Core.CircularProgress (CircularProgressProps)
import React.Basic.MUI.Core.Collapse (CollapseProps)
import React.Basic.MUI.Core.CssBaseline (CssBaselineProps)
import React.Basic.MUI.Core.Dialog (DialogProps)
import React.Basic.MUI.Core.DialogActions (DialogActionsProps)
import React.Basic.MUI.Core.DialogContent (DialogContentProps)
import React.Basic.MUI.Core.DialogContentText (DialogContentTextProps)
import React.Basic.MUI.Core.DialogTitle (DialogTitleProps)
import React.Basic.MUI.Core.Divider (DividerProps)
import React.Basic.MUI.Core.Drawer (DrawerProps)
import React.Basic.MUI.Core.ExpansionPanel (ExpansionPanelProps)
import React.Basic.MUI.Core.ExpansionPanelActions (ExpansionPanelActionsProps)
import React.Basic.MUI.Core.ExpansionPanelDetails (ExpansionPanelDetailsProps)
import React.Basic.MUI.Core.ExpansionPanelSummary (ExpansionPanelSummaryProps)
import React.Basic.MUI.Core.FormControl (FormControlProps)
import React.Basic.MUI.Core.FormControlLabel (FormControlLabelProps)
import React.Basic.MUI.Core.FormGroup (FormGroupProps)
import React.Basic.MUI.Core.FormHelperText (FormHelperTextProps)
import React.Basic.MUI.Core.FormLabel (FormLabelProps)
import React.Basic.MUI.Core.Grid (GridProps)
import React.Basic.MUI.Core.GridList (GridListProps)
import React.Basic.MUI.Core.GridListTile (GridListTileProps)
import React.Basic.MUI.Core.GridListTileBar (GridListTileBarProps)
import React.Basic.MUI.Core.Icon (IconProps)
import React.Basic.MUI.Core.IconButton (IconButtonProps)
import React.Basic.MUI.Core.Input (InputProps)
import React.Basic.MUI.Core.InputAdornment (InputAdornmentProps)
import React.Basic.MUI.Core.InputLabel (InputLabelProps)
import React.Basic.MUI.Core.LinearProgress (LinearProgressProps)
import React.Basic.MUI.Core.Link (LinkProps)
import React.Basic.MUI.Core.List (ListProps)
import React.Basic.MUI.Core.ListItem (ListItemProps)
import React.Basic.MUI.Core.ListItemAvatar (ListItemAvatarProps)
import React.Basic.MUI.Core.ListItemIcon (ListItemIconProps)
import React.Basic.MUI.Core.ListItemSecondaryAction (ListItemSecondaryActionProps)
import React.Basic.MUI.Core.ListItemText (ListItemTextProps)
import React.Basic.MUI.Core.ListSubheader (ListSubheaderProps)
import React.Basic.MUI.Core.Menu (MenuProps)
import React.Basic.MUI.Core.MenuItem (MenuItemProps)
import React.Basic.MUI.Core.MenuList (MenuListProps)
import React.Basic.MUI.Core.MobileStepper (MobileStepperProps)
import React.Basic.MUI.Core.Modal (ModalProps)
import React.Basic.MUI.Core.NativeSelect (NativeSelectProps)
import React.Basic.MUI.Core.Paper (PaperProps)
import React.Basic.MUI.Core.Popover (PopoverProps)
import React.Basic.MUI.Core.Radio (RadioProps)
import React.Basic.MUI.Core.RadioGroup (RadioGroupProps)
import React.Basic.MUI.Core.Select (SelectProps)
import React.Basic.MUI.Core.Snackbar (SnackbarProps)
import React.Basic.MUI.Core.SnackbarContent (SnackbarContentProps)
import React.Basic.MUI.Core.Step (StepProps)
import React.Basic.MUI.Core.StepButton (StepButtonProps)
import React.Basic.MUI.Core.StepConnector (StepConnectorProps)
import React.Basic.MUI.Core.StepContent (StepContentProps)
import React.Basic.MUI.Core.StepIcon (StepIconProps)
import React.Basic.MUI.Core.StepLabel (StepLabelProps)
import React.Basic.MUI.Core.Stepper (StepperProps)
import React.Basic.MUI.Core.SvgIcon (SvgIconProps)
import React.Basic.MUI.Core.Switch (SwitchProps)
import React.Basic.MUI.Core.Internal.SwitchBase (SwitchBaseProps)
import React.Basic.MUI.Core.Tab (TabProps)
import React.Basic.MUI.Core.Table (TableProps)
import React.Basic.MUI.Core.TableCell (TableCellProps)
import React.Basic.MUI.Core.TablePagination (TablePaginationProps)
import React.Basic.MUI.Core.TableRow (TableRowProps)
import React.Basic.MUI.Core.TableSortLabel (TableSortLabelProps)
import React.Basic.MUI.Core.Tabs (TabsProps)
import React.Basic.MUI.Core.TextField (TextFieldProps)
import React.Basic.MUI.Core.Toolbar (ToolbarProps)
import React.Basic.MUI.Core.Tooltip (TooltipProps)
import React.Basic.MUI.Core.ButtonBase.TouchRipple (TouchRippleProps)
import React.Basic.MUI.Core.Typography (TypographyProps)
import React.Basic.MUI.Core.WithWidth (WithWidthOptions)

type ComponentsProps = Foreign

type ComponentsPropsList_required  optional =
  ( "MuiAppBar" :: AppBarProps 
  , "MuiAvatar" :: AvatarProps 
  , "MuiBackdrop" :: BackdropProps 
  , "MuiBadge" :: BadgeProps 
  , "MuiBottomNavigation" :: BottomNavigationProps 
  , "MuiBottomNavigationAction" :: BottomNavigationActionProps 
  , "MuiBreadcrumbs" :: BreadcrumbsProps 
  , "MuiButton" :: ButtonProps 
  , "MuiButtonBase" :: ButtonBaseProps 
  , "MuiButtonGroup" :: ButtonGroupProps 
  , "MuiCard" :: CardProps 
  , "MuiCardActions" :: CardActionsProps 
  , "MuiCardContent" :: CardContentProps 
  , "MuiCardHeader" :: CardHeaderProps 
  , "MuiCardMedia" :: CardMediaProps 
  , "MuiCheckbox" :: CheckboxProps 
  , "MuiChip" :: ChipProps 
  , "MuiCircularProgress" :: CircularProgressProps 
  , "MuiCollapse" :: CollapseProps 
  , "MuiCssBaseline" :: CssBaselineProps 
  , "MuiDialog" :: DialogProps 
  , "MuiDialogActions" :: DialogActionsProps 
  , "MuiDialogContent" :: DialogContentProps 
  , "MuiDialogContentText" :: DialogContentTextProps 
  , "MuiDialogTitle" :: DialogTitleProps 
  , "MuiDivider" :: DividerProps 
  , "MuiDrawer" :: DrawerProps 
  , "MuiExpansionPanel" :: ExpansionPanelProps 
  , "MuiExpansionPanelActions" :: ExpansionPanelActionsProps 
  , "MuiExpansionPanelDetails" :: ExpansionPanelDetailsProps 
  , "MuiExpansionPanelSummary" :: ExpansionPanelSummaryProps 
  , "MuiFormControl" :: FormControlProps 
  , "MuiFormControlLabel" :: FormControlLabelProps 
  , "MuiFormGroup" :: FormGroupProps 
  , "MuiFormHelperText" :: FormHelperTextProps 
  , "MuiFormLabel" :: FormLabelProps 
  , "MuiGrid" :: GridProps 
  , "MuiGridList" :: GridListProps 
  , "MuiGridListTile" :: GridListTileProps 
  , "MuiGridListTileBar" :: GridListTileBarProps 
  , "MuiIcon" :: IconProps 
  , "MuiIconButton" :: IconButtonProps 
  , "MuiInput" :: InputProps 
  , "MuiInputAdornment" :: InputAdornmentProps 
  , "MuiInputLabel" :: InputLabelProps 
  , "MuiLinearProgress" :: LinearProgressProps 
  , "MuiLink" :: LinkProps 
  , "MuiList" :: ListProps 
  , "MuiListItem" :: ListItemProps 
  , "MuiListItemAvatar" :: ListItemAvatarProps 
  , "MuiListItemIcon" :: ListItemIconProps 
  , "MuiListItemSecondaryAction" :: ListItemSecondaryActionProps 
  , "MuiListItemText" :: ListItemTextProps 
  , "MuiListSubheader" :: ListSubheaderProps 
  , "MuiMenu" :: MenuProps 
  , "MuiMenuItem" :: MenuItemProps 
  , "MuiMenuList" :: MenuListProps 
  , "MuiMobileStepper" :: MobileStepperProps 
  , "MuiModal" :: ModalProps 
  , "MuiNativeSelect" :: NativeSelectProps 
  , "MuiPaper" :: PaperProps 
  , "MuiPopover" :: PopoverProps 
  , "MuiRadio" :: RadioProps 
  , "MuiRadioGroup" :: RadioGroupProps 
  , "MuiSelect" :: SelectProps 
  , "MuiSnackbar" :: SnackbarProps 
  , "MuiSnackbarContent" :: SnackbarContentProps 
  , "MuiStep" :: StepProps 
  , "MuiStepButton" :: StepButtonProps 
  , "MuiStepConnector" :: StepConnectorProps 
  , "MuiStepContent" :: StepContentProps 
  , "MuiStepIcon" :: StepIconProps 
  , "MuiStepLabel" :: StepLabelProps 
  , "MuiStepper" :: StepperProps 
  , "MuiSvgIcon" :: SvgIconProps 
  , "MuiSwitch" :: SwitchProps 
  , "MuiSwitchBase" :: SwitchBaseProps 
  , "MuiTab" :: TabProps 
  , "MuiTable" :: TableProps 
  , "MuiTableCell" :: TableCellProps 
  , "MuiTablePagination" :: TablePaginationProps 
  , "MuiTableRow" :: TableRowProps 
  , "MuiTableSortLabel" :: TableSortLabelProps 
  , "MuiTabs" :: TabsProps 
  , "MuiTextField" :: TextFieldProps 
  , "MuiToolbar" :: ToolbarProps 
  , "MuiTooltip" :: TooltipProps 
  , "MuiTouchRipple" :: TouchRippleProps 
  , "MuiTypography" :: TypographyProps 
  , "MuiWithWidth" :: WithWidthOptions 
  | optional )

type ComponentsPropsList_optional =
  ( 
  )

foreign import data ComponentsPropsList :: Type 

componentsPropsList
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ComponentsPropsList_optional )
  => Record (ComponentsPropsList_required attrs)
  -> ComponentsPropsList
componentsPropsList = unsafeCoerce