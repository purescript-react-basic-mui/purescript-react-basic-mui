import * as ts from "typescript"

export interface DeclarationSourceFile {
  tag: "DeclarationSourceFile"
  contents: {
    fileName: string
    declarationModuleElements: DeclarationModuleElement[]
  }
}

export type DeclarationModuleElement = DeclarationElement | ImportDeclaration | ImportEqualsDeclaration | ExportDeclaration | ExportDefaultDeclaration | ExportAssignment 

export interface DeclarationElement { tag: "DeclarationElement", contents: DeclarationElements }
export interface ImportDeclaration { tag: "ImportDeclaration" }
export interface ImportEqualsDeclaration { tag: "ImportEqualsDeclaration" }
export interface ExportDeclaration { tag: "ExportDeclaration" }
export interface ExportDefaultDeclaration { tag: "ExportDefaultDeclaration" }
export interface ExportAssignment { tag: "ExportAssignment" }

export type DeclarationElements =  InterfaceDeclaration | TypeAliasDeclaration | AmbientDeclaration 

export interface InterfaceDeclaration { 
  tag: "InterfaceDeclaration",
  contents: { 
    name: string,
    typeParameters: TypeParameter[]
    typeMembers: TypeMember[]
  }
}
export interface TypeAliasDeclaration { 
  tag: "TypeAliasDeclaration",
  contents: { 
    name: string,
    typeParameters: TypeParameter[],
    type: TSType
  }
}

export interface AmbientDeclaration { tag: "AmbientDeclaration" }

export type TSType =
  UnionType |
  IntersectionType |
  ClassType |
  FunctionType |
  ConstructorType |
  ParenthesizedType |
  TypeReference | 
  AnonymousObjectType |
  TupleType | 
  ArrayType | 
  AnyType |
  TypeOperator |
  ThisType |
  TypeQuery | 
  NumberType |
  BooleanType |
  StringType |
  ObjectType |
  SymbolType |
  VoidType |
  BigIntType |
  UnknownType |
  LiteralType |
  NeverType |
  IndexAccessType |
  ConditionalType

export interface ArrayType { tag: "ArrayType", contents: TSType }
export interface ObjectType { tag: "ObjectType" }
export interface TypeOperator { tag: "TypeOperator" }
export interface TypeQuery { tag: "TypeQuery" } 
export interface ThisType { tag: "ThisType" }
export interface NeverType { tag: "NeverType" }
export interface TupleType { tag: "TupleType", contents: TSType[] }
export interface ParenthesizedType { tag: "ParenthesizedType", contents: TSType }
export interface BooleanType { tag: "BooleanType" }
export interface StringType { tag: "StringType" }
export interface NumberType { tag: "NumberType" }
export interface BigIntType { tag: "BigIntType" }
export interface VoidType { tag: "VoidType" }
export interface AnyType { tag: "AnyType" }
export interface UnknownType { tag: "UnknownType" }
export interface SymbolType { tag: "SymbolType" }
export interface TypeReference { tag: "TypeReference", contents: { name: EntityName, typeParameters: TSType[] } }
export interface LiteralType { tag: "LiteralType", contents: LiteralValue }
export interface ConditionalType { 
  tag: "ConditionalType",
  contents: {
    checkType: TSType,
    extendsType: TSType,
    trueType: TSType,
    falseType: TSType
  }
}

export interface ClassType { 
  tag: "ClassType",
  contents: { 
    name?: string,
    typeParameters: TypeParameter[]
    typeMembers: TypeMember[]
  }
}

export interface IndexAccessType {
  tag: "IndexAccessType",
  contents: {
    indexType: TSType,
    objectType: TSType
  }
}

export interface AnonymousObjectType { 
  tag: "AnonymousObjectType",
  contents: TypeMember[]
}

export type EntityName = Identifier | QualifiedName

export interface Identifier {
  tag: "Identifier",
  contents: string
}

export interface QualifiedName {
  tag: "QualifiedName",
  contents: { left: EntityName, right: string }
}

export type TypeMember = PropertySignature | CallSignature | ConstructSignature | IndexSignature | MethodSignature 

export interface PropertySignature {
  tag: "PropertySignature",
  contents: { name?: PropertyName, type: TSType, isOptional: boolean }
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
    typeParameters: TSType[],
    parameters: TypeMember[],
    returnType: TSType
  }
}

export type LiteralValue = LiteralStringValue | LiteralNumericValue | LiteralBigIntValue

export interface LiteralStringValue {
  tag: "LiteralStringValue",
  contents: string
}

export interface LiteralNumericValue {
  tag: "LiteralNumericValue",
  contents: string 
}

export interface LiteralBigIntValue {
  tag: "LiteralBigIntValue",
  contents: string
}

export type Parameter = RequiredParameter | OptionalParameter | Rest

export interface RequiredParameter { tag: "RequiredParameter" }
export interface OptionalParameter { tag: "OptionalParameter" }
export interface Rest { tag: "Rest" } 

export interface TypeParameter { tag: "TypeParameter", contents: string }

export interface UnionType { tag: "UnionType", contents: TSType[] }
export interface IntersectionType { tag: "IntersectionType", contents: TSType[] }

export interface ConstructorType { 
  tag: "ConstructorType",
  contents: {
    typeParameters: TSType[],
    parameters: TypeMember[],
    returnType: TSType
  }
}

export interface FunctionType { 
  tag: "FunctionType",
  contents: {
    typeParameters: TSType[],
    parameters: TypeMember[],
    returnType: TSType
  }
}


export interface NamespaceDeclaration { tag: "NamespaceDeclaration" }

export interface ConstructSignature { tag: "ConstructSignature" }
export interface IndexSignature { tag: "IndexSignature" }
export interface MethodSignature { tag: "MethodSignature" }


const isThing = (thing: string, str: string): boolean => {
  const regexStr = `\\b${thing}\\b`
  const results = str.match(new RegExp(regexStr))
  return results !== null && results.length > 0
}

export const _sourceFiles = (): DeclarationSourceFile[] => {
  const options = ts.getDefaultCompilerOptions()
  const program = ts.createProgram(["./node_modules/@material-ui/core/index.d.ts"], options)
  const sources = program.getSourceFiles()
  const checker = program.getTypeChecker()

  const handleArrayType = (node: ts.ArrayTypeNode): TSType => {
    const type = checker.getTypeFromTypeNode(node.elementType)
    return { tag: "ArrayType", contents: handleTSType(type) }
  }

  const handleAnonymousObjectType = (type: ts.Type): TSType => {
    const contents: TypeMember[] = type.getProperties().map(symbol => handleTypeMember(symbol)) 
    return { tag: "AnonymousObjectType", contents }
  }
  
  const handleEntityName = (name: ts.EntityName): EntityName => {
    if(ts.isIdentifier(name)) return { tag: "Identifier", contents: name.escapedText.toString() }
    return { tag: "QualifiedName", contents: { left: handleEntityName(name.left), right: name.right.escapedText.toString() } }
  }

  const handleTypeReference = (type: ts.Type, node: ts.TypeReferenceNode): TSType => {
    const name = handleEntityName(node.typeName)
    const typeParameters: TSType[] = (node.typeArguments) ? node.typeArguments.map(nt => handleTSType(checker.getTypeFromTypeNode(nt))) : []
    return { tag: "TypeReference", contents: { name, typeParameters } }
  }

  const handleCallSignature = (type: ts.Type): CallSignature => {
    const callSigs = type.getCallSignatures()
    if(callSigs[0]){
      const sig = callSigs[0]
      const params = sig.getTypeParameters()
      const typeParameters: TSType[] = params ? params.map(handleTSType) : []
      const parameters = sig.getParameters().map(handleTypeMember)
      const returnType = handleTSType(sig.getReturnType())
      return { tag: "CallSignature", contents: { typeParameters, parameters, returnType }}
    }
    return { tag: "CallSignature", contents: { typeParameters: [], parameters: [], returnType: { tag: "VoidType" }} }
  }

  const handleConstructor = (type: ts.Type): ConstructorType => {
    const { contents } = handleCallSignature(type)
    return { tag: "ConstructorType", contents }
  }

  const handleFunction = (type: ts.Type): FunctionType => {
    const { contents } = handleCallSignature(type)
    return { tag: "FunctionType", contents }
  }

  const handleClass = (node: ts.ClassDeclaration): ClassType => {
    const name = node.name ? node.name.escapedText.toString() : undefined
    const type = checker.getTypeAtLocation(node)
    const typeParameters: TypeParameter[] = (node.typeParameters) ? node.typeParameters.map(handleTypeParameter) : []
    const typeMembers: TypeMember[] = type.getProperties().map(symbol => handleTypeMember(symbol)) 
    return { tag: "ClassType", contents: { name, typeParameters, typeMembers } }
  }

  const handleLiteralType = (node: ts.LiteralTypeNode): TSType => {

    if(node.literal.kind === ts.SyntaxKind.StringLiteral){
      const str = (<ts.StringLiteral>node.literal).text
      const contents: LiteralValue = { tag: "LiteralStringValue", contents: str }
      return { tag: "LiteralType", contents }
 
    }
    if(node.literal.kind === ts.SyntaxKind.NumericLiteral){
      const str = (<ts.NumericLiteral>node.literal).text
      const contents: LiteralValue = { tag: "LiteralNumericValue", contents: str }
      return { tag: "LiteralType", contents }
    }

    if(node.literal.kind === ts.SyntaxKind.BigIntLiteral){
      const str = (<ts.BigIntLiteral>node.literal).text
      const contents: LiteralValue = { tag: "LiteralBigIntValue", contents: str }
      return { tag: "LiteralType", contents }
    }

    const str = (<ts.NumericLiteral>node.literal).text
    const contents: LiteralValue = { tag: "LiteralStringValue", contents: str }
    return { tag: "LiteralType", contents }
  }

  const handleIndexAccessType = (node: ts.IndexedAccessTypeNode): TSType => {
    const indexType = handleTSType(checker.getTypeFromTypeNode(node.indexType))
    const objectType = handleTSType(checker.getTypeFromTypeNode(node.objectType))
    return {
      tag: "IndexAccessType",
      contents: { indexType, objectType }
    }
  }

  const handleConditionalType = (node: ts.ConditionalTypeNode): TSType => {
    const checkType = handleTSType(checker.getTypeAtLocation(node.checkType))
    const extendsType = handleTSType(checker.getTypeAtLocation(node.extendsType))
    const trueType = handleTSType(checker.getTypeAtLocation(node.trueType))
    const falseType = handleTSType(checker.getTypeAtLocation(node.falseType))
    return { tag: "ConditionalType", contents: { checkType, extendsType, trueType, falseType } }
  }

  const handleTSType = (type: ts.Type): TSType => {
    if(type.flags & ts.TypeFlags.String) return { tag: "StringType" }
    if(type.flags & ts.TypeFlags.Number) return { tag: "NumberType" }
    if(type.flags & ts.TypeFlags.BooleanLike) return { tag: "BooleanType" }
    if(type.flags & ts.TypeFlags.VoidLike) return { tag: "VoidType" }
    if(type.flags & ts.TypeFlags.BigInt) return { tag: "BigIntType" }
    if(type.flags & ts.TypeFlags.Unknown) return { tag: "UnknownType" }
    if(type.flags & ts.TypeFlags.Never) return { tag: "NeverType" }
    if(type.symbol && type.symbol.name === "Symbol") return { tag: "SymbolType" }

    const node = checker.typeToTypeNode(type)
    if(node && ts.isIndexedAccessTypeNode(node)) return handleIndexAccessType(node)
    if(node && ts.isLiteralTypeNode(node)) return handleLiteralType(node)
    if(node && ts.isTypeOperatorNode(node)) return { tag: "TypeOperator" }
    if(node && node.kind === ts.SyntaxKind.SymbolKeyword) return { tag: "SymbolType" }
    if(node && ts.isTypeReferenceNode(node)) return handleTypeReference(type, node)
    if(node && ts.isUnionTypeNode(node)) return { tag: "UnionType", contents: node.types.map(t => handleTSType(checker.getTypeFromTypeNode(t))) }
    if(node && ts.isIntersectionTypeNode(node)) return { tag: "IntersectionType", contents: node.types.map(t => handleTSType(checker.getTypeFromTypeNode(t))) }
    if(node && ts.isConstructorTypeNode(node)) return handleConstructor(type)
    if(node && ts.isFunctionLike(node)) return handleFunction(type)
    if(node && node.kind === ts.SyntaxKind.ArrayType) return handleArrayType(node as unknown as ts.ArrayTypeNode)
    if(node && node.kind === ts.SyntaxKind.TypeQuery) return { tag: "TypeQuery" }
    if(node && ts.isTupleTypeNode(node)) return { tag: "TupleType", contents: node.elementTypes.map(t => handleTSType(checker.getTypeFromTypeNode(t))) }
    if(node && ts.isParenthesizedTypeNode(node)) return { tag: "ParenthesizedType", contents: handleTSType(checker.getTypeFromTypeNode(node.type)) }
    if(node && node.kind === ts.SyntaxKind.ObjectKeyword) return { tag: "ObjectType" }
    if(node && ts.isConditionalTypeNode(node)) return handleConditionalType(node)
    if(type.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)){
      const objFlags = (<ts.ObjectType>type).objectFlags
      if(objFlags & ts.ObjectFlags.Reference && node && ts.isTypeReferenceNode(node)) return handleTypeReference(type, node)
      if(objFlags & ts.ObjectFlags.Class && node && ts.isClassDeclaration(node)) return handleClass(node)
      if(objFlags & (ts.ObjectFlags.Mapped | ts.ObjectFlags.Anonymous | ts.ObjectFlags.ObjectLiteral | ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties)) return handleAnonymousObjectType(type)
      if(objFlags === 96) return handleAnonymousObjectType(type)
    }

    if(type.flags & ts.TypeFlags.Any) return { tag: "AnyType" }

    console.log("Type not found", type, node)
    return { tag: "AnyType" }
  }

  const handleTypeParameter = (param: ts.TypeParameterDeclaration): TypeParameter => {
    const contents = param.name.escapedText.toString()
    return { tag: "TypeParameter", contents }
  }

  const handleTypeAliasDeclaration = (node: ts.TypeAliasDeclaration): TypeAliasDeclaration => {
    const name = node.name.escapedText.toString()
    const type = handleTSType(checker.getTypeFromTypeNode(node.type))
    const typeParameters = (node.typeParameters) ? node.typeParameters.map(handleTypeParameter) : []
    return { tag: "TypeAliasDeclaration", contents: { name, typeParameters, type } }
  }

  const handleTypeMember = (symbol: ts.Symbol): TypeMember => {
    const name: PropertyName = { tag: "IdentifierName",  contents: symbol.name }
    const declarations = symbol.declarations ? symbol.declarations : []
    const declaration = declarations[0]
    const isOptional: boolean = (symbol.flags & ts.SymbolFlags.Optional) === ts.SymbolFlags.Optional
    if(declaration && ts.isPropertySignature(declaration) && declaration.type){
      const nodeType = declaration.type
      const propertyType: TSType = handleTSType(checker.getTypeAtLocation(nodeType))
      return { tag: "PropertySignature", contents: { name, isOptional, type: propertyType } }
    }
    return { tag: "PropertySignature", contents: { name, isOptional, type: { tag: "AnyType" } } }
  }

  const handleInterfaceDeclaration = (node: ts.InterfaceDeclaration): InterfaceDeclaration => {
    const name = node.name.escapedText.toString()
    const type = checker.getTypeAtLocation(node)
    const typeParameters: TypeParameter[] = (node.typeParameters) ? node.typeParameters.map(handleTypeParameter) : []
    const typeMembers: TypeMember[] = type.getProperties().map(symbol => handleTypeMember(symbol)) 
    return { tag: "InterfaceDeclaration", contents: { name, typeParameters, typeMembers } }
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
