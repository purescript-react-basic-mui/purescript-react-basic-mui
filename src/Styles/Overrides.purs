module React.Basic.MUI.Styles.Overrides where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Step (StepClasskey)
import React.Basic.MUI.StepButton (StepButtonClasskey)
import React.Basic.MUI.StepConnector (StepConnectorClasskey)
import React.Basic.MUI.StepContent (StepContentClasskey)
import React.Basic.MUI.StepIcon (StepIconClasskey)
import React.Basic.MUI.StepLabel (StepLabelClasskey)
import React.Basic.MUI.Stepper (StepperClasskey)

type Overrides = Foreign

type ComponentNameToClassKey_required optional =
  ( "MuiAppBar" :: Foreign
  , "MuiAvatar" :: Foreign
  , "MuiBackdrop" :: Foreign
  , "MuiBadge" :: Foreign
  , "MuiBottomNavigation" :: Foreign
  , "MuiBottomNavigationAction" :: Foreign
  , "MuiBreadcrumbs" :: Foreign
  , "MuiButton" :: Foreign
  , "MuiButtonBase" :: Foreign
  , "MuiButtonGroup" :: Foreign
  , "MuiCard" :: Foreign
  , "MuiCardActionArea" :: Foreign
  , "MuiCardActions" :: Foreign
  , "MuiCardContent" :: Foreign
  , "MuiCardHeader" :: Foreign
  , "MuiCardMedia" :: Foreign
  , "MuiCheckbox" :: Foreign
  , "MuiChip" :: Foreign
  , "MuiCircularProgress" :: Foreign
  , "MuiCollapse" :: Foreign
  , "MuiCssBaseline" :: Foreign
  , "MuiDialog" :: Foreign
  , "MuiDialogActions" :: Foreign
  , "MuiDialogContent" :: Foreign
  , "MuiDialogContentText" :: Foreign
  , "MuiDialogTitle" :: Foreign
  , "MuiDivider" :: Foreign
  , "MuiDrawer" :: Foreign
  , "MuiExpansionPanel" :: Foreign
  , "MuiExpansionPanelActions" :: Foreign
  , "MuiExpansionPanelDetails" :: Foreign
  , "MuiExpansionPanelSummary" :: Foreign
  , "MuiFab" :: Foreign
  , "MuiFilledInput" :: Foreign
  , "MuiFormControl" :: Foreign
  , "MuiFormControlLabel" :: Foreign
  , "MuiFormGroup" :: Foreign
  , "MuiFormHelperText" :: Foreign
  , "MuiFormLabel" :: Foreign
  , "MuiGrid" :: Foreign
  , "MuiGridList" :: Foreign
  , "MuiGridListTile" :: Foreign
  , "MuiGridListTileBar" :: Foreign
  , "MuiIcon" :: Foreign
  , "MuiIconButton" :: Foreign
  , "MuiInput" :: Foreign
  , "MuiInputAdornment" :: Foreign
  , "MuiInputBase" :: Foreign
  , "MuiInputLabel" :: Foreign
  , "MuiLinearProgress" :: Foreign
  , "MuiLink" :: Foreign
  , "MuiList" :: Foreign
  , "MuiListItem" :: Foreign
  , "MuiListItemAvatar" :: Foreign
  , "MuiListItemIcon" :: Foreign
  , "MuiListItemSecondaryAction" :: Foreign
  , "MuiListItemText" :: Foreign
  , "MuiListSubheader" :: Foreign
  , "MuiMenu" :: Foreign
  , "MuiMenuItem" :: Foreign
  , "MuiMobileStepper" :: Foreign
  , "MuiModal" :: Foreign
  , "MuiNativeSelect" :: Foreign
  , "MuiOutlinedInput" :: Foreign
  , "MuiPaper" :: Foreign
  , "MuiPopover" :: Foreign
  , "MuiRadio" :: Foreign
  , "MuiSelect" :: Foreign
  , "MuiSnackbar" :: Foreign
  , "MuiSnackbarContent" :: Foreign
  , "MuiStep" :: StepClasskey 
  , "MuiStepButton" :: StepButtonClasskey 
  , "MuiStepConnector" :: StepConnectorClasskey 
  , "MuiStepContent" :: StepContentClasskey 
  , "MuiStepIcon" :: StepIconClasskey 
  , "MuiStepLabel" :: StepLabelClasskey 
  , "MuiStepper" :: StepperClasskey 
  , "MuiSvgIcon" :: Foreign
  , "MuiSwitch" :: Foreign
  , "MuiSwitchBase" :: Foreign
  , "MuiTab" :: Foreign
  , "MuiTable" :: Foreign
  , "MuiTableBody" :: Foreign
  , "MuiTableCell" :: Foreign
  , "MuiTableFooter" :: Foreign
  , "MuiTableHead" :: Foreign
  , "MuiTablePagination" :: Foreign
  , "MuiTableRow" :: Foreign
  , "MuiTableSortLabel" :: Foreign
  , "MuiTabs" :: Foreign
  , "MuiTextField" :: Foreign
  , "MuiToolbar" :: Foreign
  , "MuiTooltip" :: Foreign
  , "MuiTouchRipple" :: Foreign
  , "MuiTypography" :: Foreign
  | optional )

type ComponentNameToClassKey_optional =
  ( 
  )

foreign import data ComponentNameToClassKey :: Type 

componentNameToClassKey
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ComponentNameToClassKey_optional)
  => Record (ComponentNameToClassKey_required attrs)
  -> ComponentNameToClassKey
componentNameToClassKey = unsafeCoerce