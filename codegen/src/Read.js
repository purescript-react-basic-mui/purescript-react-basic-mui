"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var ts = require("typescript");
var isThing = function (thing, str) {
    var regexStr = "\\b" + thing + "\\b";
    var results = str.match(new RegExp(regexStr));
    return results !== null && results.length > 0;
};
exports._sourceFiles = function () {
    var options = ts.getDefaultCompilerOptions();
    var program = ts.createProgram(["./node_modules/@material-ui/core/index.d.ts"], options);
    var sources = program.getSourceFiles();
    var checker = program.getTypeChecker();
    var handleArrayType = function (node) {
        return { tag: "ArrayType", contents: handleTSType(node.elementType) };
    };
    var handleEntityName = function (name) {
        if (ts.isIdentifier(name))
            return { tag: "Identifier", contents: name.escapedText.toString() };
        return { tag: "QualifiedName", contents: { left: handleEntityName(name.left), right: name.right.escapedText.toString() } };
    };
    var handleTypeReference = function (node) {
        var name = handleEntityName(node.typeName);
        var typeArguments = (node.typeArguments) ? node.typeArguments.map(handleTSType) : [];
        return { tag: "TypeReference", contents: { name: name, typeArguments: typeArguments } };
    };
    var handleParameter = function (node) {
        var isOptional = node.questionToken !== undefined;
        var type = node.type ? handleTSType(node.type) : { tag: "AnyType" };
        var name = undefined;
        return { tag: "PropertySignature", contents: { isOptional: isOptional, type: type, name: name } };
    };
    var handleFunction = function (node) {
        var typeParameters = node.typeParameters ? node.typeParameters.map(handleTypeParameter) : [];
        var parameters = node.parameters ? node.parameters.map(handleParameter) : [];
        var returnType = node.type ? handleTSType(node.type) : { tag: "AnyType" };
        return { tag: "FunctionType", contents: { typeParameters: typeParameters, parameters: parameters, returnType: returnType } };
    };
    var handleClass = function (node) {
        var name = node.name ? node.name.escapedText.toString() : undefined;
        var type = checker.getTypeAtLocation(node);
        var typeParameters = (node.typeParameters) ? node.typeParameters.map(handleTypeParameter) : [];
        var typeMembers = type.getProperties().map(function (symbol) { return handleTypeMember(symbol); });
        return { tag: "ClassType", contents: { name: name, typeParameters: typeParameters, typeMembers: typeMembers } };
    };
    var handleLiteralType = function (node) {
        if (node.literal.kind === ts.SyntaxKind.FalseKeyword) {
            var contents_1 = { tag: "LiteralBooleanValue", contents: false };
            return { tag: "LiteralType", contents: contents_1 };
        }
        if (node.literal.kind === ts.SyntaxKind.TrueKeyword) {
            var contents_2 = { tag: "LiteralBooleanValue", contents: true };
            return { tag: "LiteralType", contents: contents_2 };
        }
        if (ts.isNumericLiteral(node.literal)) {
            var str = node.literal.text;
            var contents_3 = { tag: "LiteralNumericValue", contents: str };
            return { tag: "LiteralType", contents: contents_3 };
        }
        if (ts.isBigIntLiteral(node.literal)) {
            var str = node.literal.text;
            var contents_4 = { tag: "LiteralBigIntValue", contents: str };
            return { tag: "LiteralType", contents: contents_4 };
        }
        if (ts.isStringLiteral(node.literal)) {
            var str = node.literal.text;
            var contents_5 = { tag: "LiteralStringValue", contents: str };
            return { tag: "LiteralType", contents: contents_5 };
        }
        var contents = { tag: "LiteralStringValue", contents: "UNKNOWN" };
        return { tag: "LiteralType", contents: contents };
    };
    var handlePropertyName = function (name) {
        if (name !== undefined) {
            if (ts.isIdentifier(name))
                return { tag: "IdentifierName", contents: name.text };
            if (ts.isStringLiteral(name))
                return { tag: "StringLiteral", contents: name.text };
            if (ts.isNumericLiteral(name))
                return { tag: "NumericLiteral", contents: name.text };
            if (ts.isComputedPropertyName(name)) {
                console.log("Found computed property name and haven't implemented it yet");
                return { tag: "StringLiteral", contents: "ComputedExpressionNotImplemented" };
            }
        }
        return undefined;
    };
    var handleIndexAccessType = function (node) {
        var indexType = handleTSType(node.indexType);
        var objectType = handleTSType(node.objectType);
        return {
            tag: "IndexAccessType",
            contents: { indexType: indexType, objectType: objectType }
        };
    };
    var handleConditionalType = function (node) {
        var checkType = handleTSType(node.checkType);
        var extendsType = handleTSType(node.extendsType);
        var trueType = handleTSType(node.trueType);
        var falseType = handleTSType(node.falseType);
        return { tag: "ConditionalType", contents: { checkType: checkType, extendsType: extendsType, trueType: trueType, falseType: falseType } };
    };
    var handleTypeLiteral = function (node) {
        var contents = node.members.map(function (t) {
            if (ts.isPropertySignature(t)) {
                var name_1 = handlePropertyName(t.name);
                var isOptional = t.questionToken === undefined;
                var type = t.type ? handleTSType(t.type) : { tag: "AnyType" };
                return { tag: "PropertySignature", contents: { name: name_1, isOptional: isOptional, type: type } };
            }
            if (ts.isCallSignatureDeclaration(t)) {
                var name_2 = handlePropertyName(t.name);
                var isOptional = t.questionToken === undefined;
                var returnType = t.type ? handleTSType(t.type) : { tag: "AnyType" };
                var typeParameters = t.typeParameters ? t.typeParameters.map(handleTypeParameter) : [];
                var parameters = t.parameters.map(handleParameter);
                return { tag: "CallSignature", contents: { name: name_2, isOptional: isOptional, returnType: returnType, typeParameters: typeParameters, parameters: parameters } };
            }
            if (ts.isIndexSignatureDeclaration(t)) {
                var name_3 = handlePropertyName(t.name);
                var isOptional = t.questionToken === undefined;
                var typeParameters = t.typeParameters ? t.typeParameters.map(handleTypeParameter) : [];
                var parameters = t.parameters.map(handleParameter);
                return { tag: "IndexSignature", contents: { name: name_3, isOptional: isOptional, typeParameters: typeParameters, parameters: parameters } };
            }
            if (ts.isMethodSignature(t)) {
                var name_4 = handlePropertyName(t.name);
                var isOptional = t.questionToken === undefined;
                var returnType = t.type ? handleTSType(t.type) : { tag: "AnyType" };
                var typeParameters = t.typeParameters ? t.typeParameters.map(handleTypeParameter) : [];
                var parameters = t.parameters.map(handleParameter);
                return { tag: "MethodSignature", contents: { name: name_4, isOptional: isOptional, returnType: returnType, typeParameters: typeParameters, parameters: parameters } };
            }
            if (ts.isConstructSignatureDeclaration(t)) {
                var name_5 = handlePropertyName(t.name);
                var isOptional = t.questionToken === undefined;
                var returnType = t.type ? handleTSType(t.type) : { tag: "AnyType" };
                var typeParameters = t.typeParameters ? t.typeParameters.map(handleTypeParameter) : [];
                var parameters = t.parameters.map(handleParameter);
                return { tag: "ConstructSignature", contents: { name: name_5, isOptional: isOptional, returnType: returnType, typeParameters: typeParameters, parameters: parameters } };
            }
            console.log("No TypeLiteral impl for: " + ts.SyntaxKind[t.kind]);
            var name = undefined;
            return { tag: "PropertySignature", contents: { name: name, isOptional: true, type: { tag: "AnyType" } } };
        });
        return { tag: "TypeLiteral", contents: contents };
    };
    var handleTSType = function (node) {
        switch (node.kind) {
            case ts.SyntaxKind.StringKeyword: return { tag: "StringType" };
            case ts.SyntaxKind.NumberKeyword: return { tag: "NumberType" };
            case ts.SyntaxKind.BooleanKeyword: return { tag: "BooleanType" };
            case ts.SyntaxKind.VoidKeyword: return { tag: "VoidType" };
            case ts.SyntaxKind.NullKeyword: return { tag: "NullType" };
            case ts.SyntaxKind.UndefinedKeyword: return { tag: "UndefinedType" };
            case ts.SyntaxKind.BigIntKeyword: return { tag: "BigIntType" };
            case ts.SyntaxKind.UnknownKeyword: return { tag: "UnknownType" };
            case ts.SyntaxKind.NeverKeyword: return { tag: "NeverType" };
            case ts.SyntaxKind.SymbolKeyword: return { tag: "SymbolType" };
            case ts.SyntaxKind.TypeQuery: return { tag: "TypeQuery" };
            case ts.SyntaxKind.ObjectKeyword: return { tag: "ObjectType" };
            case ts.SyntaxKind.TypeOperator: return { tag: "TypeOperator" };
            case ts.SyntaxKind.AnyKeyword: return { tag: "AnyType" };
        }
        if (ts.isTypeReferenceNode(node))
            return handleTypeReference(node);
        if (ts.isTypeLiteralNode(node))
            return handleTypeLiteral(node);
        if (ts.isUnionTypeNode(node))
            return { tag: "UnionType", contents: node.types.map(handleTSType) };
        if (ts.isLiteralTypeNode(node))
            return handleLiteralType(node);
        if (ts.isIntersectionTypeNode(node))
            return { tag: "IntersectionType", contents: node.types.map(function (t) { return handleTSType(t); }) };
        if (ts.isTupleTypeNode(node))
            return { tag: "TupleType", contents: node.elementTypes.map(function (t) { return handleTSType(t); }) };
        if (ts.isParenthesizedTypeNode(node))
            return { tag: "ParenthesizedType", contents: handleTSType(node.type) };
        if (ts.isArrayTypeNode(node))
            return handleArrayType(node);
        if (ts.isFunctionLike(node))
            return handleFunction(node);
        if (ts.isIndexedAccessTypeNode(node))
            return handleIndexAccessType(node);
        if (ts.isConditionalTypeNode(node))
            return handleConditionalType(node);
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
        return { tag: "AnyType" };
    };
    var handleTypeParameter = function (param) {
        var contents = param.name.escapedText.toString();
        return { tag: "TypeParameter", contents: contents };
    };
    var handleTypeAliasDeclaration = function (node) {
        var name = node.name.escapedText.toString();
        var type = handleTSType(node.type);
        var typeParameters = (node.typeParameters) ? node.typeParameters.map(handleTypeParameter) : [];
        return { tag: "TypeAliasDeclaration", contents: { name: name, typeParameters: typeParameters, type: type } };
    };
    var handleTypeMember = function (symbol) {
        var name = { tag: "IdentifierName", contents: symbol.name };
        var declarations = symbol.declarations ? symbol.declarations : [];
        var declaration = declarations[0];
        var isOptional = (symbol.flags & ts.SymbolFlags.Optional) === ts.SymbolFlags.Optional;
        if (declaration && ts.isPropertySignature(declaration) && declaration.type) {
            var nodeType = declaration.type;
            var propertyType = handleTSType(nodeType);
            return { tag: "PropertySignature", contents: { name: name, isOptional: isOptional, type: propertyType } };
        }
        return { tag: "PropertySignature", contents: { name: name, isOptional: isOptional, type: { tag: "AnyType" } } };
    };
    var handleInterfaceDeclaration = function (node) {
        var name = node.name.escapedText.toString();
        var type = checker.getTypeAtLocation(node);
        var typeParameters = (node.typeParameters) ? node.typeParameters.map(handleTypeParameter) : [];
        var typeMembers = type.getProperties().map(function (symbol) { return handleTypeMember(symbol); });
        return { tag: "InterfaceDeclaration", contents: { name: name, typeParameters: typeParameters, typeMembers: typeMembers } };
    };
    var handleDeclarationElement = function (node) {
        if (ts.isInterfaceDeclaration(node))
            return handleInterfaceDeclaration(node);
        if (ts.isTypeAliasDeclaration(node))
            return handleTypeAliasDeclaration(node);
        return { tag: "AmbientDeclaration" };
    };
    var handleDeclarationModuleElements = function (node) {
        if (ts.isImportDeclaration(node))
            return { tag: "ImportDeclaration" };
        if (ts.isImportEqualsDeclaration(node))
            return { tag: "ImportEqualsDeclaration" };
        if (ts.isExportAssignment(node) && (node.flags & ts.ModifierFlags.Default))
            return { tag: "ExportDefaultDeclaration" };
        if (ts.isExportDeclaration(node))
            return { tag: "ExportDeclaration" };
        if (ts.isExportAssignment(node))
            return { tag: "ExportAssignment" };
        return { tag: "DeclarationElement", contents: handleDeclarationElement(node) };
    };
    var srcs = sources.map(function (src) {
        var fileName = src.fileName;
        var declarationModuleElements = src.statements.map(handleDeclarationModuleElements);
        return { tag: "DeclarationSourceFile", contents: { fileName: fileName, declarationModuleElements: declarationModuleElements } };
    });
    return srcs;
};
