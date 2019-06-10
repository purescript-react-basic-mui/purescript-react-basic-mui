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
  ParenthesizedType |
  TypeReference | 
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
  ConditionalType |
  NullType |
  UndefinedType |
  TypeLiteral

export interface ArrayType { tag: "ArrayType", contents: TSType }
export interface ObjectType { tag: "ObjectType" }
export interface NullType { tag: "NullType" }
export interface UndefinedType { tag: "UndefinedType" }
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
export interface TypeReference { tag: "TypeReference", contents: { name: EntityName, typeArguments: TSType[] } }
export interface LiteralType { tag: "LiteralType", contents: LiteralValue }

export interface TypeLiteral { tag: "TypeLiteral", contents: TypeMember[] }

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

export type EntityName = Identifier | QualifiedName

export interface Identifier {
  tag: "Identifier",
  contents: string
}

export interface QualifiedName {
  tag: "QualifiedName",
  contents: { left: EntityName, right: string }
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
  contents: string 
}


export type LiteralValue = LiteralStringValue | LiteralNumericValue | LiteralBigIntValue | LiteralBooleanValue

export interface LiteralStringValue {
  tag: "LiteralStringValue",
  contents: string
}

export interface LiteralBooleanValue {
  tag: "LiteralBooleanValue",
  contents: boolean 
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

export type TypeMember = PropertySignature | CallSignature | ConstructSignature | IndexSignature | MethodSignature 

export interface PropertySignature {
  tag: "PropertySignature",
  contents: { name?: PropertyName, type: TSType, isOptional: boolean }
}

export interface FunctionType { 
  tag: "FunctionType",
  contents: {
    typeParameters: TypeParameter[],
    parameters: TypeMember[],
    returnType: TSType
  }
}

export interface CallSignature {
  tag: "CallSignature"
  contents: {
    name?: PropertyName,
    isOptional: boolean,
    typeParameters: TypeParameter[],
    parameters: TypeMember[],
    returnType: TSType
  }
}

export interface ConstrucSignature {
  tag: "ConstrucSignature"
  contents: {
    name?: PropertyName,
    isOptional: boolean,
    typeParameters: TypeParameter[],
    parameters: TypeMember[],
    returnType: TSType
  }
}

export interface IndexSignature {
  tag: "IndexSignature"
  contents: {
    name?: PropertyName,
    isOptional: boolean,
    typeParameters: TypeParameter[],
    parameters: TypeMember[],
  }
}

export interface MethodSignature {
  tag: "MethodSignature"
  contents: {
    name?: PropertyName,
    isOptional: boolean,
    typeParameters: TypeParameter[],
    parameters: TypeMember[],
    returnType: TSType
  }
}

export interface NamespaceDeclaration { tag: "NamespaceDeclaration" }
export interface ConstructSignature { tag: "ConstructSignature" }


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
    return { tag: "ArrayType", contents: handleTSType(node.elementType) }
  }
 
  const handleEntityName = (name: ts.EntityName): EntityName => {
    if(ts.isIdentifier(name)) return { tag: "Identifier", contents: name.escapedText.toString() }
    return { tag: "QualifiedName", contents: { left: handleEntityName(name.left), right: name.right.escapedText.toString() } }
  }

  const handleTypeReference = (node: ts.TypeReferenceNode): TSType => {
    const name = handleEntityName(node.typeName)
    const typeArguments: TSType[] = (node.typeArguments) ? node.typeArguments.map(handleTSType) : []
    return { tag: "TypeReference", contents: { name, typeArguments } }
  }

  const handleParameter = (node: ts.ParameterDeclaration): TypeMember => {
    const isOptional: boolean = node.questionToken !== undefined 
    const type: TSType = node.type ? handleTSType(node.type) : { tag: "AnyType" }
    const name = undefined
    return { tag: "PropertySignature", contents: { isOptional, type, name } }
  }

  const handleFunction = (node: ts.SignatureDeclaration): FunctionType => {
    const typeParameters: TypeParameter[] = node.typeParameters ? node.typeParameters.map(handleTypeParameter) : []
    const parameters: TypeMember[] = node.parameters ? node.parameters.map(handleParameter) : []
    const returnType: TSType = node.type ? handleTSType(node.type) : { tag: "AnyType" }
    return { tag: "FunctionType", contents: { typeParameters, parameters, returnType } }
  }

  const handleClass = (node: ts.ClassDeclaration): ClassType => {
    const name = node.name ? node.name.escapedText.toString() : undefined
    const type = checker.getTypeAtLocation(node)
    const typeParameters: TypeParameter[] = (node.typeParameters) ? node.typeParameters.map(handleTypeParameter) : []
    const typeMembers: TypeMember[] = type.getProperties().map(symbol => handleTypeMember(symbol)) 
    return { tag: "ClassType", contents: { name, typeParameters, typeMembers } }
  }

  const handleLiteralType = (node: ts.LiteralTypeNode): TSType => {

    if(node.literal.kind === ts.SyntaxKind.FalseKeyword) {
      const contents: LiteralValue = { tag: "LiteralBooleanValue", contents: false }
      return { tag: "LiteralType", contents }
    }

    if(node.literal.kind === ts.SyntaxKind.TrueKeyword) {
      const contents: LiteralValue = { tag: "LiteralBooleanValue", contents: true }
      return { tag: "LiteralType", contents }
    }
 
    if(ts.isNumericLiteral(node.literal)){
      const str = node.literal.text
      const contents: LiteralValue = { tag: "LiteralNumericValue", contents: str }
      return { tag: "LiteralType", contents }
    }

    if(ts.isBigIntLiteral(node.literal)){
      const str = node.literal.text
      const contents: LiteralValue = { tag: "LiteralBigIntValue", contents: str }
      return { tag: "LiteralType", contents }
    }

    if(ts.isStringLiteral(node.literal)){
      const str = node.literal.text
      const contents: LiteralValue = { tag: "LiteralStringValue", contents: str }
      return { tag: "LiteralType", contents }
    }

    const contents: LiteralValue = { tag: "LiteralStringValue", contents: "UNKNOWN" }
    return { tag: "LiteralType", contents }
  }

  const handlePropertyName = (name?: ts.PropertyName): PropertyName | undefined => {
    if(name !== undefined){
      if(ts.isIdentifier(name)) return { tag: "IdentifierName", contents: name.text }
      if(ts.isStringLiteral(name))  return { tag: "StringLiteral", contents: name.text }
      if(ts.isNumericLiteral(name)) return { tag: "NumericLiteral", contents: name.text }
      if(ts.isComputedPropertyName(name)){
        console.log("Found computed property name and haven't implemented it yet")
        return { tag: "StringLiteral", contents: "ComputedExpressionNotImplemented" }
      }
    }
    return undefined
  }

  const handleIndexAccessType = (node: ts.IndexedAccessTypeNode): TSType => {
    const indexType = handleTSType(node.indexType)
    const objectType = handleTSType(node.objectType)
    return {
      tag: "IndexAccessType",
      contents: { indexType, objectType }
    }
  }

  const handleConditionalType = (node: ts.ConditionalTypeNode): TSType => {
    const checkType = handleTSType(node.checkType)
    const extendsType = handleTSType(node.extendsType)
    const trueType = handleTSType(node.trueType)
    const falseType = handleTSType(node.falseType)
    return { tag: "ConditionalType", contents: { checkType, extendsType, trueType, falseType } }
  }

  const handleTypeLiteral = (node: ts.TypeLiteralNode): TSType => {
    const contents: TypeMember[] = node.members.map(t => {
      
      if(ts.isPropertySignature(t)){
        const name: PropertyName | undefined = handlePropertyName(t.name)
        const isOptional: boolean = t.questionToken === undefined
        const type: TSType = t.type ? handleTSType(t.type) : { tag: "AnyType" }
        return { tag: "PropertySignature", contents: { name, isOptional, type } }
      }
      
      if(ts.isCallSignatureDeclaration(t)){
        const name: PropertyName | undefined  = handlePropertyName(t.name)
        const isOptional: boolean = t.questionToken === undefined
        const returnType: TSType = t.type ? handleTSType(t.type) : { tag: "AnyType" }
        const typeParameters: TypeParameter[] = t.typeParameters ? t.typeParameters.map(handleTypeParameter) : []
        const parameters: TypeMember[] = t.parameters.map(handleParameter)
        return { tag: "CallSignature", contents: { name, isOptional, returnType, typeParameters, parameters } }
      }
      
      if(ts.isIndexSignatureDeclaration(t)){
        const name: PropertyName | undefined  = handlePropertyName(t.name)
        const isOptional: boolean = t.questionToken === undefined
        const typeParameters: TypeParameter[] = t.typeParameters ? t.typeParameters.map(handleTypeParameter) : []
        const parameters: TypeMember[] = t.parameters.map(handleParameter)
        return { tag: "IndexSignature", contents: { name, isOptional, typeParameters, parameters } }
      }
      
      if(ts.isMethodSignature(t)){
        const name: PropertyName | undefined  = handlePropertyName(t.name)
        const isOptional: boolean = t.questionToken === undefined
        const returnType: TSType = t.type ? handleTSType(t.type) : { tag: "AnyType" }
        const typeParameters: TypeParameter[] = t.typeParameters ? t.typeParameters.map(handleTypeParameter) : []
        const parameters: TypeMember[] = t.parameters.map(handleParameter)
        return { tag: "MethodSignature", contents: { name, isOptional, returnType, typeParameters, parameters } }
      }
      
      if(ts.isConstructSignatureDeclaration(t)){
        const name: PropertyName | undefined  = handlePropertyName(t.name)
        const isOptional: boolean = t.questionToken === undefined
        const returnType: TSType = t.type ? handleTSType(t.type) : { tag: "AnyType" }
        const typeParameters: TypeParameter[] = t.typeParameters ? t.typeParameters.map(handleTypeParameter) : []
        const parameters: TypeMember[] = t.parameters.map(handleParameter)
        return { tag: "ConstructSignature", contents: { name, isOptional, returnType, typeParameters, parameters } }
      }
      
      console.log("No TypeLiteral impl for: " + ts.SyntaxKind[t.kind])
      const name: PropertyName | undefined = undefined
      return { tag: "PropertySignature", contents: { name, isOptional: true, type: { tag: "AnyType" } } }
    })
    
    return { tag: "TypeLiteral", contents }
  }

  const handleTSType = (node: ts.Node): TSType => {
    switch(node.kind){

      case ts.SyntaxKind.StringKeyword:   return { tag: "StringType" }
      case ts.SyntaxKind.NumberKeyword:   return { tag: "NumberType" }
      case ts.SyntaxKind.BooleanKeyword:  return { tag: "BooleanType" }
      case ts.SyntaxKind.VoidKeyword:     return { tag: "VoidType" }
      case ts.SyntaxKind.NullKeyword:     return { tag: "NullType" }
      case ts.SyntaxKind.UndefinedKeyword:return { tag: "UndefinedType" }
      case ts.SyntaxKind.BigIntKeyword:   return { tag: "BigIntType" }
      case ts.SyntaxKind.UnknownKeyword:  return { tag: "UnknownType" }
      case ts.SyntaxKind.NeverKeyword:    return { tag: "NeverType" }
      case ts.SyntaxKind.SymbolKeyword:   return { tag: "SymbolType" }
      case ts.SyntaxKind.TypeQuery:       return { tag: "TypeQuery" }
      case ts.SyntaxKind.ObjectKeyword:   return { tag: "ObjectType" }
      case ts.SyntaxKind.TypeOperator:    return { tag: "TypeOperator" }
      case ts.SyntaxKind.AnyKeyword:      return { tag: "AnyType" }

    }
    
    if(ts.isTypeReferenceNode(node))      return handleTypeReference(node)
    if(ts.isTypeLiteralNode(node))        return handleTypeLiteral(node)
    if(ts.isUnionTypeNode(node))          return { tag: "UnionType", contents: node.types.map(handleTSType) }
    if(ts.isLiteralTypeNode(node))        return handleLiteralType(node)
    if(ts.isIntersectionTypeNode(node))   return { tag: "IntersectionType", contents: node.types.map(t => handleTSType(t)) }
    if(ts.isTupleTypeNode(node))          return { tag: "TupleType", contents: node.elementTypes.map(t => handleTSType(t)) }
    if(ts.isParenthesizedTypeNode(node))  return { tag: "ParenthesizedType", contents: handleTSType(node.type) }
    if(ts.isArrayTypeNode(node))          return handleArrayType(node)
    if(ts.isFunctionLike(node))           return handleFunction(node)
    if(ts.isIndexedAccessTypeNode(node))  return handleIndexAccessType(node)
    if(ts.isConditionalTypeNode(node))    return handleConditionalType(node)
/*    if(ts.isConstructorTypeNode(node))    return handleConstructor(node)
  
*/
    /*
    if(type.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)){
      const objFlags = (<ts.ObjectType>type).objectFlags
      if(objFlags & ts.ObjectFlags.Reference && node && ts.isTypeReferenceNode(node)) return handleTypeReference(type, node)
      if(objFlags & ts.ObjectFlags.Class && node && ts.isClassDeclaration(node)) return handleClass(node)
      if(objFlags & (ts.ObjectFlags.Mapped | ts.ObjectFlags.Anonymous | ts.ObjectFlags.ObjectLiteral | ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties)) return handleAnonymousObjectType(type)
      if(objFlags === 96) return handleAnonymousObjectType(type)
    }
    */
    //console.log(ts.SyntaxKind[node.kind])
    return { tag: "AnyType" }
  }

  const handleTypeParameter = (param: ts.TypeParameterDeclaration): TypeParameter => {
    const contents = param.name.escapedText.toString()
    return { tag: "TypeParameter", contents }
  }

  const handleTypeAliasDeclaration = (node: ts.TypeAliasDeclaration): TypeAliasDeclaration => {
    const name = node.name.escapedText.toString()
    const type = handleTSType(node.type)
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
      const propertyType: TSType = handleTSType(nodeType)
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
