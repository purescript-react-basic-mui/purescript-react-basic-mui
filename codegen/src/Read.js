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
        var type = checker.getTypeFromTypeNode(node.elementType);
        return { tag: "ArrayType", contents: handleTSType(type) };
    };
    var handleAnonymousObjectType = function (type) {
        var contents = type.getProperties().map(function (symbol) { return handleTypeMember(symbol); });
        return { tag: "AnonymousObjectType", contents: contents };
    };
    var handleEntityName = function (name) {
        if (ts.isIdentifier(name))
            return { tag: "Identifier", contents: name.escapedText.toString() };
        return { tag: "QualifiedName", contents: { left: handleEntityName(name.left), right: name.right.escapedText.toString() } };
    };
    var handleTypeReference = function (type, node) {
        var name = handleEntityName(node.typeName);
        var typeParameters = (node.typeArguments) ? node.typeArguments.map(function (nt) { return handleTSType(checker.getTypeFromTypeNode(nt)); }) : [];
        return { tag: "TypeReference", contents: { name: name, typeParameters: typeParameters } };
    };
    var handleCallSignature = function (type) {
        var callSigs = type.getCallSignatures();
        if (callSigs[0]) {
            var sig = callSigs[0];
            var params = sig.getTypeParameters();
            var typeParameters = params ? params.map(handleTSType) : [];
            var parameters = sig.getParameters().map(handleTypeMember);
            var returnType = handleTSType(sig.getReturnType());
            return { tag: "CallSignature", contents: { typeParameters: typeParameters, parameters: parameters, returnType: returnType } };
        }
        return { tag: "CallSignature", contents: { typeParameters: [], parameters: [], returnType: { tag: "VoidType" } } };
    };
    var handleConstructor = function (type) {
        var contents = handleCallSignature(type).contents;
        return { tag: "ConstructorType", contents: contents };
    };
    var handleFunction = function (type) {
        var contents = handleCallSignature(type).contents;
        return { tag: "FunctionType", contents: contents };
    };
    var handleClass = function (node) {
        var name = node.name ? node.name.escapedText.toString() : undefined;
        var type = checker.getTypeAtLocation(node);
        var typeParameters = (node.typeParameters) ? node.typeParameters.map(handleTypeParameter) : [];
        var typeMembers = type.getProperties().map(function (symbol) { return handleTypeMember(symbol); });
        return { tag: "ClassType", contents: { name: name, typeParameters: typeParameters, typeMembers: typeMembers } };
    };
    var handleLiteralType = function (node) {
        if (node.literal.kind === ts.SyntaxKind.StringLiteral) {
            var str_1 = node.literal.text;
            var contents_1 = { tag: "LiteralStringValue", contents: str_1 };
            return { tag: "LiteralType", contents: contents_1 };
        }
        if (node.literal.kind === ts.SyntaxKind.NumericLiteral) {
            var str_2 = node.literal.text;
            var contents_2 = { tag: "LiteralNumericValue", contents: str_2 };
            return { tag: "LiteralType", contents: contents_2 };
        }
        if (node.literal.kind === ts.SyntaxKind.BigIntLiteral) {
            var str_3 = node.literal.text;
            var contents_3 = { tag: "LiteralBigIntValue", contents: str_3 };
            return { tag: "LiteralType", contents: contents_3 };
        }
        var str = node.literal.text;
        var contents = { tag: "LiteralStringValue", contents: str };
        return { tag: "LiteralType", contents: contents };
    };
    var handleIndexAccessType = function (node) {
        var indexType = handleTSType(checker.getTypeFromTypeNode(node.indexType));
        var objectType = handleTSType(checker.getTypeFromTypeNode(node.objectType));
        return {
            tag: "IndexAccessType",
            contents: { indexType: indexType, objectType: objectType }
        };
    };
    var handleConditionalType = function (node) {
        var checkType = handleTSType(checker.getTypeAtLocation(node.checkType));
        var extendsType = handleTSType(checker.getTypeAtLocation(node.extendsType));
        var trueType = handleTSType(checker.getTypeAtLocation(node.trueType));
        var falseType = handleTSType(checker.getTypeAtLocation(node.falseType));
        return { tag: "ConditionalType", contents: { checkType: checkType, extendsType: extendsType, trueType: trueType, falseType: falseType } };
    };
    var handleUnionType = function (node) {
        node.types.map(function (typeNode) {
            console.log(ts.SyntaxKind[typeNode.kind]);
            var type = checker.getTypeFromTypeNode(typeNode);
        });
        return { tag: "AnyType" };
    };
    var handleTSType = function (type) {
        if (type.flags & ts.TypeFlags.String)
            return { tag: "StringType" };
        if (type.flags & ts.TypeFlags.Number)
            return { tag: "NumberType" };
        if (type.flags & ts.TypeFlags.BooleanLike)
            return { tag: "BooleanType" };
        if (type.flags & ts.TypeFlags.VoidLike)
            return { tag: "VoidType" };
        if (type.flags & ts.TypeFlags.BigInt)
            return { tag: "BigIntType" };
        if (type.flags & ts.TypeFlags.Unknown)
            return { tag: "UnknownType" };
        if (type.flags & ts.TypeFlags.Never)
            return { tag: "NeverType" };
        if (type.symbol && type.symbol.name === "Symbol")
            return { tag: "SymbolType" };
        var node = checker.typeToTypeNode(type);
        if (node && ts.isIndexedAccessTypeNode(node))
            return handleIndexAccessType(node);
        if (node && ts.isLiteralTypeNode(node))
            return handleLiteralType(node);
        if (node && ts.isTypeOperatorNode(node))
            return { tag: "TypeOperator" };
        if (node && node.kind === ts.SyntaxKind.SymbolKeyword)
            return { tag: "SymbolType" };
        if (node && ts.isTypeReferenceNode(node))
            return handleTypeReference(type, node);
        if (node && ts.isUnionTypeNode(node))
            return handleUnionType(node); ////return { tag: "UnionType", contents: node.types.map(t => handleTSType(checker.getTypeFromTypeNode(t))) }
        if (node && ts.isIntersectionTypeNode(node))
            return { tag: "IntersectionType", contents: node.types.map(function (t) { return handleTSType(checker.getTypeFromTypeNode(t)); }) };
        if (node && ts.isConstructorTypeNode(node))
            return handleConstructor(type);
        if (node && ts.isFunctionLike(node))
            return handleFunction(type);
        if (node && node.kind === ts.SyntaxKind.ArrayType)
            return handleArrayType(node);
        if (node && node.kind === ts.SyntaxKind.TypeQuery)
            return { tag: "TypeQuery" };
        if (node && ts.isTupleTypeNode(node))
            return { tag: "TupleType", contents: node.elementTypes.map(function (t) { return handleTSType(checker.getTypeFromTypeNode(t)); }) };
        if (node && ts.isParenthesizedTypeNode(node))
            return { tag: "ParenthesizedType", contents: handleTSType(checker.getTypeFromTypeNode(node.type)) };
        if (node && node.kind === ts.SyntaxKind.ObjectKeyword)
            return { tag: "ObjectType" };
        if (node && ts.isConditionalTypeNode(node))
            return handleConditionalType(node);
        if (type.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)) {
            var objFlags = type.objectFlags;
            if (objFlags & ts.ObjectFlags.Reference && node && ts.isTypeReferenceNode(node))
                return handleTypeReference(type, node);
            if (objFlags & ts.ObjectFlags.Class && node && ts.isClassDeclaration(node))
                return handleClass(node);
            if (objFlags & (ts.ObjectFlags.Mapped | ts.ObjectFlags.Anonymous | ts.ObjectFlags.ObjectLiteral | ts.ObjectFlags.ObjectLiteralPatternWithComputedProperties))
                return handleAnonymousObjectType(type);
            if (objFlags === 96)
                return handleAnonymousObjectType(type);
        }
        if (type.flags & ts.TypeFlags.Any)
            return { tag: "AnyType" };
        console.log("Type not found", type, node);
        return { tag: "AnyType" };
    };
    var handleTypeParameter = function (param) {
        var contents = param.name.escapedText.toString();
        return { tag: "TypeParameter", contents: contents };
    };
    var handleTypeAliasDeclaration = function (node) {
        var name = node.name.escapedText.toString();
        var type = handleTSType(checker.getTypeFromTypeNode(node.type));
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
            var propertyType = handleTSType(checker.getTypeAtLocation(nodeType));
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
