import * as ts from "typescript"

export type InterfaceToFile = { fileName: string, iface : ts.InterfaceDeclaration }
export type InterfaceMap = { [key:string]: InterfaceToFile }
export type TypeAliasMap = { [key:string]:ts.TypeAliasDeclaration; }
export type FunctionToFile = { fileName: string, func : ts.FunctionDeclaration }
export type ClassToFile = { fileName: string, clazz : ts.ClassDeclaration }
export type ExportAssignmentToFile = { fileName: string, assignment : ts.ExportAssignment }
export type ImportMatcher = { regex: RegExp, module: string }

export interface FieldType {
  name: string
  foreignData?: string[]
  isOptional?: boolean
}

export interface Field {
  name: string
  fieldType: FieldType
  isOptional: boolean
  comments: string | undefined
}

export interface Props {
  name: string
  fields: Field[]
  typeParameters: string[]
  types: string[]
  isComponentProps: boolean,
  classNames: string[],
  parents: Props[],
  fileName: string
  comments?: string,
}

export interface WrittenProps { 
  fns: string[]
  props: string[]
  foreignData: string[] 
  fileName: string
}

export interface BaseInterface {
  iface: ts.InterfaceDeclaration
  fileName: string
  classNames: string[]
  comments?: string
}



export const strCompare = (str1: string, str2: string) => {
  if(str1 === str2) return 0
  if(str1 > str2) return 1
  return -1
}

export const fieldCompare = (field1: Field, field2: Field) => 
  strCompare(field1.name, field2.name)
   
export const propsCompare = (props1: Props, props2: Props) => 
  strCompare(props1.name, props2.name)

