import * as ts from "typescript"

export type Member = { name: string, type: TSType, optional: boolean }

export type TSType = 
  BooleanType |
  StringType |
  NumberType |
  NullType |
  AnyType |
  VoidType |
  StringLiteralType |
  NumericLiteralType |
  UnionType |
  FunctionType |
  TypeReference |
  AnonymousObject |
  InterfaceReference |
  UnknownObject |
  TypeParam |
  Unknown |
  TypeAlias

export interface BooleanType { _tag: "Boolean" }
export interface StringType { _tag: "String" }
export interface NumberType { _tag: "Number" }
export interface NullType { _tag: "Null" }
export interface VoidType { _tag: "Unit" }
export interface AnyType { _tag: "Any" }
export interface UnionType { _tag: "Union", types: TSType[] }
export interface StandardType { _tag: "Standard", type: string }
export interface StringLiteralType { _tag: "StringLiteral", value: string }
export interface NumericLiteralType { _tag: "NumericLiteral", value: number }
export interface FunctionType { _tag: "Function", parameters: Member[], returnType?: TSType }
export interface TypeReference { _tag: "TypeReference", name: string, typeParams: TSType[], flags: ts.TypeFlags, objFlags: ts.ObjectFlags }
export interface AnonymousObject { _tag: "AnonymousObject", members: any }
export interface InterfaceReference { _tag: "InterfaceReference", name: string }
export interface UnknownObject { _tag: "UnknownObject", flags: ts.ObjectFlags }
export interface TypeParam { _tag: "TypeParam", name: string }
export interface Unknown { _tag: "Unknown", name: string, flags: ts.TypeFlags }
export interface TypeAlias { _tag: "TypeAlias", typeReference: string, typeParams: TSType[], type: TSType }



const options = ts.getDefaultCompilerOptions()
const program = ts.createProgram(["./node_modules/@material-ui/core/index.d.ts"], options)
const sources = program.getSourceFiles()
const checker = program.getTypeChecker()


const isNodeExported = (node: ts.Node): boolean => 
  (ts.getCombinedNodeFlags(node) & ts.ModifierFlags.Export) !== 0 ||
    (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile)


const getTSType = (type: ts.Type): TSType => {
  if(type.isUnionOrIntersection()) return { _tag: "Union", types: (type.types.map(getWithAliasProps).filter(t => t) as unknown as TSType[]) }
  if(type.flags & ts.TypeFlags.String) return { _tag: "String" }
  if(type.flags & ts.TypeFlags.BooleanLike) return { _tag: "Boolean" }
  if(type.flags & ts.TypeFlags.Number) return { _tag: "Number" }
  if(type.flags & ts.TypeFlags.Null) return { _tag: "Null" }
  if(type.flags & ts.TypeFlags.VoidLike) return { _tag: "Unit" }
  if(type.flags & ts.TypeFlags.Any) return { _tag: "Any" }
  if(type.isStringLiteral()) return { _tag: "StringLiteral", value: type.value }
  if(type.isNumberLiteral()) return { _tag: "NumericLiteral", value: type.value }

  const callSigs = type.getCallSignatures()
  
  if(callSigs.length){
    const sig = callSigs[0] 
    const parameters = sig.getParameters().map(p => optionalMember(p))
    const returnType = getWithAliasProps(sig.getReturnType())
    return { _tag: "Function", parameters, returnType }
  }

  if(type.flags & ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive){
    const objFlags = (<ts.ObjectType>type).objectFlags
    
    if(objFlags & ts.ObjectFlags.Reference){
      try{
        const tr = (<ts.TypeReference>type)
        const name = checker.getFullyQualifiedName(type.symbol)
        const typeParams = tr.typeArguments ? (tr.typeArguments.map(getWithAliasProps).filter(i => i) as unknown as TypeAlias[]) : ([] as TypeAlias[])
        const flags = type.flags
        return { _tag: "TypeReference", name, typeParams, flags, objFlags }
      } catch (e) {
        console.error("Reference didn't work", checker.typeToString(type))
        return { _tag: "Unknown", name: checker.typeToString(type), flags: type.flags }
      }
    }

    if(objFlags & ts.ObjectFlags.Anonymous){
      const members = convertProperties(type)
      return { _tag: "AnonymousObject", members }
    }

    if(objFlags & ts.ObjectFlags.Interface){
      const name = checker.getFullyQualifiedName(type.symbol) 
      return { _tag: "InterfaceReference", name }
    }

    return { _tag: "UnknownObject", flags: objFlags }

  }

  if(type.flags & ts.TypeFlags.TypeParameter){
    const name = checker.typeToString(type)
    return { _tag: "TypeParam", name }
  }

  return { _tag: "Unknown", name: checker.typeToString(type), flags: type.flags }

}

const getWithAliasProps = (t: ts.Type): TSType => {
  const type = getTSType(t)
  if(t.aliasSymbol){
    const typeReference = checker.getFullyQualifiedName(t.aliasSymbol)
    const typeParams = t.aliasTypeArguments ? t.aliasTypeArguments.map(getTSType) : []
    return { _tag: "TypeAlias", typeReference, typeParams, type }
  }
  return type
}


const optionalMember = (sym: ts.Symbol, node?: ts.Node): Member => {
  const optional = ((sym.flags & ts.SymbolFlags.Optional) === ts.SymbolFlags.Optional)
  const type = getWithAliasProps(checker.getTypeOfSymbolAtLocation(sym, node ? node : sym.valueDeclaration))
  return { name: sym.name, type, optional }
}

const convertProperties = (nodeType: ts.Type, node?: ts.Node): any => 
  nodeType.getProperties().map((sym: ts.Symbol) => optionalMember(sym, node))

const visit = (node: ts.Node) => {
  if(ts.isInterfaceDeclaration(node)){
    const symbol = checker.getSymbolAtLocation(node.name)
    if(symbol){
      const nodeType = checker.getTypeAtLocation(node)
      if(nodeType.isClassOrInterface()){
        let members = convertProperties(nodeType, node)
      }
    }
  } else if(ts.isModuleDeclaration(node)){
    ts.forEachChild(node, visit)
  }
}

sources.forEach( source => {
  ts.forEachChild(source, visit)
})


