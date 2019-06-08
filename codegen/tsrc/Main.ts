import * as ts from "typescript"

export type Interface = { 
  readonly tag: "Interface",
  readonly contents: {
    readonly fileName: string,
    readonly name: string,
    members: Member[] 
  }
}

export type VariableStatement = {
  readonly tag: "VariableStatement",
  readonly contents: {
    fileName: string,
    name: string,
    typeArguments: Member[]
  }
}

export type Member = { 
  readonly tag: "Member",
  readonly contents: {
    readonly name: string,
    readonly type: TSType,
    readonly optional: boolean 
  }
}

export type TSType = 
  AnonymousObject |
  AnyType |
  BooleanType |
  SymbolType |
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

export interface BooleanType { readonly tag: "BooleanType" }

export interface StringType { readonly tag: "StringType" }

export interface NumberType { readonly tag: "NumberType" }

export interface NullType { readonly tag: "NullType" }

export interface VoidType { readonly tag: "VoidType" }

export interface AnyType { readonly tag: "AnyType" }

export interface SymbolType { readonly tag: "SymbolType", contents: string }

export interface UnionType {
  readonly tag: "UnionType",
  readonly contents: { 
    readonly types: TSType[] 
  } 
}

export interface StringLiteralType {
  readonly tag: "StringLiteralType",
  readonly contents: {
    readonly value: string
  }
}

export interface NumericLiteralType {
  readonly tag: "NumericLiteralType",
  readonly contents: {
    readonly value: number
  }
}

export interface FunctionType {
  readonly tag: "FunctionType",
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
    readonly type: string,
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
    readonly error: string,
    readonly type: string,
    readonly flags: ts.TypeFlags
  }
}


export const _interfaces = (): Interface[] => {
  
  const output: Interface[] = []

  const getTSType = (type: ts.Type): TSType => {
    try{
      if(type.isUnionOrIntersection()) return { tag: "UnionType", contents: { types: (type.types.map(getWithAliasProps).filter(t => t) as unknown as TSType[]) } }
      if(type.flags & ts.TypeFlags.String) return { tag: "StringType" }
      if(type.flags & ts.TypeFlags.BooleanLike) return { tag: "BooleanType" }
      if(type.flags & ts.TypeFlags.Number) return { tag: "NumberType" }
      if(type.flags & ts.TypeFlags.Null) return { tag: "NullType" }
      if(type.flags & ts.TypeFlags.VoidLike) return { tag: "VoidType" }
      if(type.flags & ts.TypeFlags.Any) return { tag: "AnyType" }
      if(type.isStringLiteral()) return { tag: "StringLiteralType", contents: { value: type.value } }
      if(type.isNumberLiteral()) return { tag: "NumericLiteralType", contents: { value: type.value } }
  
      const callSigs = type.getCallSignatures()
      
      if(callSigs.length){
        const sig = callSigs[0] 
        const parameters = sig.getParameters().map(p => optionalMember(p))
        const returnType = getWithAliasProps(sig.getReturnType())
        return { tag: "FunctionType", contents: { parameters, returnType } }
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
  
        return { tag: "UnknownObject", contents: { type: checker.typeToString(type), flags: objFlags } }
  
      }
  
      if(type.flags & ts.TypeFlags.TypeParameter){
        const name = checker.typeToString(type)
        return { tag: "TypeParam", contents: { name } }
      }
  
      return { tag: "Unknown", contents: { name: checker.typeToString(type), flags: type.flags } }
  
    } catch (e) {
      const error = JSON.stringify(e)
      return { tag: "ExceptionType", contents: { error, type: checker.typeToString(type), flags: type.flags } }
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
    return { tag: "Member", contents: { name: sym.name, type, optional } }
  }
  
  const convertProperties = (nodeType: ts.Type, node?: ts.Node): Member[] => 
    nodeType.getProperties().map((sym: ts.Symbol) => optionalMember(sym, node))

  const isNodeExported = (node: ts.Node): boolean => 
    (ts.getCombinedNodeFlags(node) & ts.ModifierFlags.Export) !== 0 ||
      (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile)
 
  const visit = (fileName: string) => (node: ts.Node) => {
    if(isNodeExported(node)){
      if(ts.isInterfaceDeclaration(node)){
        const symbol = checker.getSymbolAtLocation(node.name)
        if(symbol){
          const nodeType = checker.getTypeAtLocation(node)
          const name = checker.getFullyQualifiedName(nodeType.symbol)
          if(nodeType.isClassOrInterface()){
            let members = convertProperties(nodeType, node)
            output.push({ tag: "Interface", contents: { fileName, name, members }})
          }
        }
      } else if(ts.isVariableStatement(node)) {
        if(fileName.indexOf("Avatar.d.ts") >= 0 && fileName.indexOf("ListItemAvatar") < 0){
          const type = checker.getTypeAtLocation(node)
          node.forEachChild( node => {
            if(ts.isVariableDeclarationList(node)){
              node.declarations.map(decl => {
                const nodeType = checker.getTypeAtLocation(decl)
                //console.log(nodeType)
                const type = getWithAliasProps(nodeType)
                console.log(type)
                //console.log("\n\n\n")
                //console.log(JSON.stringify(type, null, 2))
                /*
                if(nodeType.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)){
                  const objFlags = (<ts.ObjectType>nodeType).objectFlags
                }
                  if(ts.isIdentifier(decl.name)){
                    const name = decl.name.escapedText.toString()
                    if(nodeType.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)){
                      const objFlags = (<ts.ObjectType>nodeType).objectFlags
                      const tr = (<ts.TypeReference>nodeType)
                      const typeParams = tr.typeArguments ? (tr.typeArguments.map(getWithAliasProps).filter(i => i) as unknown as TypeAlias[]) : ([] as TypeAlias[])
                      console.log(name, objFlags, JSON.stringify(typeParams))
                    } else {
                      console.log("here", ts.SyntaxKind[node.kind])
                    }
                  }
                */
              }) 
            } else {
              
            }
          })
        } 
      } else if(ts.isModuleDeclaration(node)){
        ts.forEachChild(node, visit(fileName))
      } else {
        if(fileName.indexOf("Avatar.d.ts") >= 0 && fileName.indexOf("ListItemAvatar") < 0){
          if(node.kind == ts.SyntaxKind.EndOfFileToken && node.parent && ts.isSourceFile(node.parent)){
            const source = node.parent 
            source.statements.forEach(statement => {
              if(isNodeExported(statement)){ 
                console.log(ts.SyntaxKind[statement.kind])
              }
            })
          }
        }
      }
    }
  }
  
  const options = ts.getDefaultCompilerOptions()
  const program = ts.createProgram(["./node_modules/@material-ui/core/index.d.ts"], options)
  const sources = program.getSourceFiles()
  const checker = program.getTypeChecker()
  
  sources.forEach(source => {
    const fileName = source.fileName
    ts.forEachChild(source, visit(fileName))
  })

  const slice = output.slice(0).filter(i => i.contents.name.indexOf(".GridProps") < 0)
  console.log("Output: " + output.length, "Slice: " + slice.length)
  return slice
}


