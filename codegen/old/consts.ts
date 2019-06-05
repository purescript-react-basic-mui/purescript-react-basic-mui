import { ImportMatcher } from "./types";

export const fieldTypeNameReplacements: {[key:string]:string;} = 
  { "Date" : "JSDate",
    "Element.JSX" : "JSX",
    "JSX.Element" : "JSX",
    "React.ElementType": "JSX",
    "ElementType.React": "JSX",
    "ReactNode": "JSX",
    "ReactNode.React": "JSX",
    "React.ReactNode": "JSX",
    "Ref.React": "Ref",
    "ComponentType.React": "Component",
    "React.ComponentType": "Component",
    "ItemT" : "itemT",
    "ReactElement.React" : "React",
    "SectionT" : "sectionT",
    "StyleProp" : "CSS",
    "ScrollViewProps" : "(Record ScrollViewProps)",
    "(ComponentType.React Foreign)" : "JSX",
    "ReadonlyArray" : "Array",
    "IndexSignature": "(Object Foreign)",
    "Object": "(Object Foreign)",
    "ObjectType": "(Object Foreign)",
    "Any": "Foreign",
    "CSSProperties": "CSS",
    "PropTypes.Alignment": "Alignment",
    "Alignment.PropTypes": "Alignment",
    //"ChangeEvent.React": "Foreign",
    //"React.ChangeEvent": "Foreign",
    "PropTypes.Color": "Color",
    "Color.PropTypes": "Color",
    //"TransitionProps": "Foreign",
    //"React.SyntheticEvent": "Foreign",
    //"SyntheticEvent.React": "Foreign",
    //"FocusEventHandler.React": "Foreign",
    //"React.FocusEventHandler": "Foreign",
    //"InputHTMLAttributes.React": "Foreign",
    //"React.InputHTMLAttributes": "Foreign",
    //"HTMLAttributes.React": "Foreign",
    //"React.HTMLAttributes": "Foreign",
  }

export const ignoreForeignDataList: string[] =
  [ "NativeSyntheticEvent",
    "Array",
    "CSS",
    "Component",
    "Date",
    "Element.JSX",
    "JSDate",
    "JSX",
    "StyleProp",
    "(Record ScrollViewProps)",
    "ReactElement.React",
    "iTemT",
    "itemT",
    "ComponentType.React",
    "Any",
    "(Object Foreign)",
    "Ref",
    "Foreign"
  ]

export const additionalForeignData: { [key:string]:string[] } = 
  { "Popover": ["PopoverPropsAnchorEl", "PopoverPropsTransitionDuration"],
    "InputBase": ["InputBasePropsInputRef"]
  }

export const importMatchers: ImportMatcher[] = [
  { regex: new RegExp("\\bUndefinable\\b", "m"), module: "Data.Undefinable (Undefinable)" },
  { regex: new RegExp("\\bEffect\\b", "m"), module: "Effect (Effect)" },
  { regex: new RegExp("\\bEffectFn2\\b", "m"), module: "Effect.Uncurried (EffectFn2)" },
  { regex: new RegExp("\\bForeign\\b", "m"), module: "Foreign (Foreign)" },
  { regex: new RegExp("\\bObject\\b", "m"), module: "Foreign.Object (Object)" },
  { regex: new RegExp("\\bUnion\\b", "m"), module: "Prim.Row (class Union)" },
  { regex: new RegExp("\\bUnit\\b", "m"), module: "Prelude" },
  { regex: new RegExp("\\bComponent\\b", "m"), module: "React.Basic (Component)" },
  { regex: new RegExp("\\bJSX\\b", "m"), module: "React.Basic (JSX)" },
  { regex: new RegExp("\\bReactComponent\\b", "m"), module: "React.Basic (ReactComponent)" },
  { regex: new RegExp("\\belement\\b", "m"), module: "React.Basic (element)" },
  { regex: new RegExp("\\bRef\\b", "m"), module: "React.Basic (Ref)" },
  { regex: new RegExp("\\bCSS\\b", "m"), module: "React.Basic.DOM.Internal (CSS)" },
  { regex: new RegExp("\\bEventHandler\\b", "m"), module: "React.Basic.Events (EventHandler)" },
]


export const additionalImports: { [key:string]:string[] } = 
  { "Snackbar"  : ["Component", "JSX", "EffectFn2", "Foreign", "Object", "Union", "Unit", "element"],
    "InputBase" : ["Effect", "EventHandler", "Foreign", "JSX", "Union", "Unit", "element"],
    "Popover"   : ["Component", "EventHandler", "Foreign", "JSX", "Object", "Union", "element"],
  }


export const noChildren: string[] = []
