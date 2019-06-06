import * as ts from "typescript"

export interface DeclarationSourceFile {
  tag: "DeclarationSourceFile"
  contents: {
    fileName: string
    declarationModuleElements: DeclarationModuleElement[]
  }
}

export type DeclarationModuleElement = 
  DeclarationElement |
  ImportDeclaration |
  ImportEqualsDeclaration |
  ExportDeclaration |
  ExportDefaultDeclaration |
  ExportAssignment

 
export interface DeclarationElement { tag: "DeclarationElement", contents: DeclarationElements }
export interface ImportDeclaration { tag: "ImportDeclaration" }
export interface ImportEqualsDeclaration { tag: "ImportEqualsDeclaration" }
export interface ExportDeclaration { tag: "ExportDeclaration" }
export interface ExportDefaultDeclaration { tag: "ExportDefaultDeclaration" }
export interface ExportAssignment { tag: "ExportAssignment" }


export type DeclarationElements =  
  InterfaceDeclaration |
  TypeAliasDeclaration |
  AmbientDeclaration 

export interface InterfaceDeclaration { tag: "InterfaceDeclaration", contents: { name: string, fullyQualifiedName: string } }
export interface TypeAliasDeclaration { tag: "TypeAliasDeclaration", contents: { name: string, aliasName?: string } }
export interface AmbientDeclaration { tag: "AmbientDeclaration" }

export type TSType = 
  UnionOrIntersectionOrPrimaryType |
  FunctionType |
  ConstructorType

export type UnionOrIntersectionOrPrimaryType = UnionType | IntersectionOrPrimaryType
export type IntersectionOrPrimaryType = IntersectionType | PrimaryType
export type PrimaryType =
  ParenthesizedType |
 PredefinedType |
 TypeReference | 
 ObjectType |
 ArrayType | 
 TupleType | 
 TypeQuery | 
 ThisType 

export interface ArrayType {
  tag: "ArrayType"
  contents: PrimaryType[]
}

export interface TypeQuery { tag: "TypeQuery" } 
export interface ThisType { tag: "ThisType" }
export interface TupleType { tag: "TupleType", contents: TSType[] }
export interface ParenthesizedType { tag: "ParenthesizedType", contents: TSType }
export type PredefinedType = AnyType | NumberType | BooleanType | StringType | SymbolType | VoidType

export interface BooleanType { tag: "BooleanType" }
export interface StringType { tag: "StringType" }
export interface NumberType { tag: "NumberType" }
export interface VoidType { tag: "VoidType" }
export interface AnyType { tag: "AnyType" }
export interface SymbolType { tag: "SymbolType" }
export interface TypeReference { tag: "TypeReference" }
export interface ObjectType { tag: "ObjectType", contents: TypeMember[] }

export type TypeMember = PropertySignature | CallSignature | ConstructSignature | IndexSignature | MethodSignature 

export interface PropertySignature {
  tag: "PropertySignature",
  contents: { name?: PropertyName, typeAnnotation?: TSType[] }
}

export type PropertyName = IdentifierName | StringLiteral | NumericLiteral

export interface IdentifierName {
  tag: "IdentifierName"
  contents: string
}

export interface StringLiteral {
  tag: "StringLiteral"
  contents: string
}

export interface NumericLiteral {
  tag: "NumericLiteral"
  contents: number
}

export interface CallSignature {
  tag: "CallSignature"
  contents: {
    typeParameters?: TypeParameter[],
    parameterList?: Parameter,
    typeAnnotation?: TSType
  }
}

export type Parameter = RequiredParameter | OptionalParameter | Rest

export interface RequiredParameter { tag: "RequiredParameter" }
export interface OptionalParameter { tag: "OptionalParameter" }
export interface Rest { tag: "Rest" } 

export interface TypeParameter { tag: "TypeParameter" }

export interface UnionType { tag: "UnionType" }
export interface FunctionType { tag: "FunctionType" }
export interface ConstructorType { tag: "ConstructorType" }

export interface IntersectionType { tag: "IntersectionType" }
export interface NamespaceDeclaration { tag: "NamespaceDeclaration" }

export interface ConstructSignature { tag: "ConstructSignature" }
export interface IndexSignature { tag: "IndexSignature" }
export interface MethodSignature { tag: "MethodSignature" }


/*
DeclarationModuleElement:
  DeclarationElement
  ImportDeclaration
  ImportAliasDeclaration
  ExportDeclarationElement
  ExportDefaultDeclarationElement
  ExportAssignment

DeclarationElement:
  InterfaceDeclaration
  TypeAliasDeclaration
  NamespaceDeclaration
  AmbientDeclaration
  ImportAliasDeclaration

{
  "VariableStatement": 1016,
  "FunctionDeclaration": 124,
  "InterfaceDeclaration": 1696,
  "TypeAliasDeclaration": 1028,
  "ModuleDeclaration": 57,
  "ClassDeclaration": 12,
  "ImportDeclaration": 594,
  "ExportAssignment": 147,
  "NamespaceExportDeclaration": 1,
  "ExportDeclaration": 408,
  "ImportEqualsDeclaration": 2
}

*/



const stats: { [key:string]: number} = {} 
const addToStats = (str: string) => 
  stats[str] ? stats[str] = stats[str] + 1 : stats[str] = 1

const isAvatar = (str: string): boolean => (str.indexOf("Avatar") >= 0 && str.indexOf("ListItemAvatar") < 0)

export const _sourceFiles = (): DeclarationSourceFile[] => {
  const options = ts.getDefaultCompilerOptions()
  const program = ts.createProgram(["./node_modules/@material-ui/core/index.d.ts"], options)
  const sources = program.getSourceFiles()
  const checker = program.getTypeChecker()


  const handleTypeAliasDeclaration = (node: ts.TypeAliasDeclaration): TypeAliasDeclaration => {
    const type = checker.getTypeAtLocation(node)
    const name = node.name.escapedText.toString()
    const aliasName = type.aliasSymbol ? type.aliasSymbol.name : undefined
    const typeParameters = (node.typeParameters) ? node.typeParameters : []
    return { tag: "TypeAliasDeclaration", contents: { name, aliasName } }
  }

  const handleInterfaceDeclaration = (node: ts.InterfaceDeclaration): InterfaceDeclaration => {
    const name = node.name.escapedText.toString()
    const type = checker.getTypeAtLocation(node)
    const fullyQualifiedName = checker.getFullyQualifiedName(type.symbol)
    return { tag: "InterfaceDeclaration", contents: { name, fullyQualifiedName } }
  }

  const handleDeclarationElement = (node: ts.Node): DeclarationElements => {
    if(ts.isInterfaceDeclaration(node)) return handleInterfaceDeclaration(node)
    if(ts.isTypeAliasDeclaration(node)) return handleTypeAliasDeclaration(node)
    return { tag: "AmbientDeclaration" }
  }

  const handleDeclarationModuleElements = (node: ts.Node): DeclarationModuleElement => {
    if(ts.isImportDeclaration(node)) return { tag: "ImportDeclaration" }
    if(ts.isImportEqualsDeclaration(node)) return { tag: "ImportEqualsDeclaration" }
    if(ts.isExportAssignment(node) && (node.flags & ts.ModifierFlags.Default)) return { tag: "ExportDefaultDeclaration" }
    if(ts.isExportDeclaration(node)) return { tag: "ExportDeclaration" }
    if(ts.isExportAssignment(node)) return { tag: "ExportAssignment" }
    return { tag: "DeclarationElement", contents: handleDeclarationElement(node) }
  }

  const srcs: DeclarationSourceFile[] = sources.map(src => {
    const fileName = src.fileName
    const declarationModuleElements: DeclarationModuleElement[] = src.statements.map(handleDeclarationModuleElements) 
    return { tag: "DeclarationSourceFile", contents: { fileName, declarationModuleElements } }
  })

  return srcs

}
