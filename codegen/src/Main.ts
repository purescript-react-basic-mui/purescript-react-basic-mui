import * as ts from "typescript"

export type Member = { name: string, type: TSType, optional: boolean }

export type TSType = 
  AnonymousObject |
  AnyType |
  BooleanType |
  ExceptionType |
  FunctionType |
  InterfaceReference |
  NullType |
  NumberType |
  NumericLiteralType |
  StringLiteralType |
  StringType |
  TupleType |
  TypeAlias |
  TypeParam |
  TypeReference |
  UnionType |
  Unknown |
  UnknownObject |
  VoidType 

export interface BooleanType { readonly tag: "Boolean" }

export interface StringType { readonly tag: "String" }

export interface NumberType { readonly tag: "Number" }

export interface NullType { readonly tag: "Null" }

export interface VoidType { readonly tag: "Unit" }

export interface AnyType { readonly tag: "Any" }

export interface UnionType {
  readonly tag: "Union",
  readonly contents: { 
    readonly types: TSType[] 
  } 
}

export interface StandardType { 
  readonly tag: "Standard",
  readonly contents: { 
    readonly type: string 
  } 
}

export interface StringLiteralType {
  readonly tag: "StringLiteral",
  readonly contents: {
    readonly value: string
  }
}

export interface NumericLiteralType {
  readonly tag: "NumericLiteral",
  readonly contents: {
    readonly value: number
  }
}

export interface FunctionType {
  readonly tag: "Function",
  readonly contents: {
    readonly parameters: Member[],
    readonly returnType?: TSType
  }
}

export interface TypeReference {
  readonly tag: "TypeReference",
  readonly contents: {
    readonly name: string,
    readonly typeParams: TSType[],
    readonly flags: ts.TypeFlags,
    readonly objFlags: ts.ObjectFlags
  }
}

export interface AnonymousObject {
  readonly tag: "AnonymousObject",
  readonly contents: {
    readonly members: Member[]
  }
}

export interface InterfaceReference {
  readonly tag: "InterfaceReference",
  readonly contents: { readonly name: string }
}

export interface UnknownObject {
  readonly tag: "UnknownObject",
  readonly contents: {
    readonly flags: ts.ObjectFlags 
  }
}

export interface TypeParam {
  readonly tag: "TypeParam",
  readonly contents: {
    readonly name: string
  }
}

export interface Unknown {
  readonly tag: "Unknown",
  readonly contents: {
    readonly name: string,
    readonly flags: ts.TypeFlags
  }
}

export interface TypeAlias {
  readonly tag: "TypeAlias",
  readonly contents: {
    readonly typeReference: string,
    readonly typeParams: TSType[],
    readonly type: TSType
  }
}

export interface TupleType {
  readonly tag: "TupleType",
  readonly contents: {
    readonly types: TSType[]
  }
}

export interface ExceptionType {
  readonly tag: "ExceptionType",
  readonly contents: {
    readonly e: Error,
    readonly type: string,
    readonly flags: ts.TypeFlags
  }
}


const readTypes = (): TSType[] => {
  
  const output: TSType[] = []

  const getTSType = (type: ts.Type): TSType => {
    try{
      if(type.isUnionOrIntersection()) return { tag: "Union", contents: { types: (type.types.map(getWithAliasProps).filter(t => t) as unknown as TSType[]) } }
      if(type.flags & ts.TypeFlags.String) return { tag: "String" }
      if(type.flags & ts.TypeFlags.BooleanLike) return { tag: "Boolean" }
      if(type.flags & ts.TypeFlags.Number) return { tag: "Number" }
      if(type.flags & ts.TypeFlags.Null) return { tag: "Null" }
      if(type.flags & ts.TypeFlags.VoidLike) return { tag: "Unit" }
      if(type.flags & ts.TypeFlags.Any) return { tag: "Any" }
      if(type.isStringLiteral()) return { tag: "StringLiteral", contents: { value: type.value } }
      if(type.isNumberLiteral()) return { tag: "NumericLiteral", contents: { value: type.value } }
  
      const callSigs = type.getCallSignatures()
      
      if(callSigs.length){
        const sig = callSigs[0] 
        const parameters = sig.getParameters().map(p => optionalMember(p))
        const returnType = getWithAliasProps(sig.getReturnType())
        return { tag: "Function", contents: { parameters, returnType } }
      }
  
      if(type.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)){
        const objFlags = (<ts.ObjectType>type).objectFlags
  
        if(objFlags & ts.ObjectFlags.Tuple){
          const types: TSType[] = []
          return { tag: "TupleType", contents: { types } }
        }
        
        if(objFlags & ts.ObjectFlags.Anonymous){
          const members = convertProperties(type)
          return { tag: "AnonymousObject", contents: { members } }
        }
  
        if(objFlags & ts.ObjectFlags.Reference){
          const tr = (<ts.TypeReference>type)
          const name = checker.getFullyQualifiedName(type.symbol)
          const typeParams = tr.typeArguments ? (tr.typeArguments.map(getWithAliasProps).filter(i => i) as unknown as TypeAlias[]) : ([] as TypeAlias[])
          const flags = type.flags
          return { tag: "TypeReference", contents: { name, typeParams, flags, objFlags } }
        }
  
        if(objFlags & ts.ObjectFlags.Interface){
          const name = checker.getFullyQualifiedName(type.symbol) 
          return { tag: "InterfaceReference", contents: { name } }
        }
  
        return { tag: "UnknownObject", contents: { flags: objFlags } }
  
      }
  
      if(type.flags & ts.TypeFlags.TypeParameter){
        const name = checker.typeToString(type)
        return { tag: "TypeParam", contents: { name } }
      }
  
      return { tag: "Unknown", contents: { name: checker.typeToString(type), flags: type.flags } }
  
    } catch (e) {
      console.log(checker.typeToString(type))
      return { tag: "ExceptionType", contents: { e, type: checker.typeToString(type), flags: type.flags } }
    }
  
  }
  
  const getWithAliasProps = (t: ts.Type): TSType => {
    const type = getTSType(t)
    if(t.aliasSymbol){
      const typeReference = checker.getFullyQualifiedName(t.aliasSymbol)
      const typeParams = t.aliasTypeArguments ? t.aliasTypeArguments.map(getTSType) : []
      return { tag: "TypeAlias", contents: { typeReference, typeParams, type } }
    }
    return type
  }
  
  
  const optionalMember = (sym: ts.Symbol, node?: ts.Node): Member => {
    const optional = ((sym.flags & ts.SymbolFlags.Optional) === ts.SymbolFlags.Optional)
    const type = getWithAliasProps(checker.getTypeOfSymbolAtLocation(sym, node ? node : sym.valueDeclaration))
    return { name: sym.name, type, optional }
  }
  
  const convertProperties = (nodeType: ts.Type, node?: ts.Node): Member[] => 
    nodeType.getProperties().map((sym: ts.Symbol) => optionalMember(sym, node))

  const isNodeExported = (node: ts.Node): boolean => 
    (ts.getCombinedNodeFlags(node) & ts.ModifierFlags.Export) !== 0 ||
      (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile)
 
  const visit = (node: ts.Node) => {
    if(isNodeExported(node)){
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
  }
  
  const options = ts.getDefaultCompilerOptions()
  const program = ts.createProgram(["./node_modules/@material-ui/core/index.d.ts"], options)
  const sources = program.getSourceFiles()
  const checker = program.getTypeChecker()
  
  sources.forEach(source => {
    ts.forEachChild(source, visit)
  })

  return output
}

readTypes()


