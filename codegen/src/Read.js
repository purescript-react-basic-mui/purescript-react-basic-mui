"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var ts = require("typescript");
var path = require("path");
var isThing = function (thing, str) {
    var regexStr = "\\b" + thing + "\\b";
    var results = str.match(new RegExp(regexStr));
    return results !== null && results.length > 0;
};
exports._typescript = function (fileName) { return function (filterRegex) { return function () {
    var options = ts.getDefaultCompilerOptions();
    var program = ts.createProgram([fileName], options);
    var sources = program.getSourceFiles();
    var checker = program.getTypeChecker();
    var nodeModules = path.resolve(process.cwd(), "./node_modules/");
    var getFullyQualifiedName = function (type) {
        if (type.symbol) {
            var tokens = checker.getFullyQualifiedName(type.symbol).split(nodeModules + "/");
            if (tokens.length === 1)
                return tokens[0];
            if (tokens.length === 2)
                return tokens[1];
        }
        return undefined;
    };
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
        var type = checker.getTypeAtLocation(node);
        var fullyQualifiedName = getFullyQualifiedName(type);
        var record = (type && type.aliasSymbol)
            ? { aliasName: { tag: "Identifier", contents: type.aliasSymbol.name },
                aliasTypeArguments: type.aliasTypeArguments ? type.aliasTypeArguments.map(function (t) {
                    var nodeType = checker.typeToTypeNode(t);
                    return nodeType ? handleTSType(nodeType) : { tag: "AnyType" };
                }) : []
            }
            : { aliasName: undefined, aliasTypeArguments: undefined };
        return { tag: "TypeReference", contents: { name: name, fullyQualifiedName: fullyQualifiedName, typeArguments: typeArguments, aliasName: record.aliasName, aliasTypeArguments: record.aliasTypeArguments } };
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
            // shouldn't get here unless typescript changes
            var name = undefined;
            return { tag: "PropertySignature", contents: { name: name, isOptional: true, type: { tag: "AnyType" } } };
        });
        return { tag: "TypeLiteral", contents: contents };
    };
    var handleMappedType = function (node) {
        var isOptional = node.questionToken === undefined;
        var type = node.type ? handleTSType(node.type) : { tag: "AnyType" };
        var typeParameter = node.typeParameter ? handleTypeParameter(node.typeParameter) : undefined;
        return { tag: "MappedType", contents: { isOptional: isOptional, type: type, typeParameter: typeParameter } };
    };
    var handleTypeQuery = function (node) {
        return { tag: "TypeQuery", contents: handleEntityName(node.exprName) };
    };
    var handleInferType = function (node) {
        var contents = handleTypeParameter(node.typeParameter);
        return { tag: "InferType", contents: contents };
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
            case ts.SyntaxKind.ObjectKeyword: return { tag: "ObjectType" };
            case ts.SyntaxKind.TypeOperator: return { tag: "TypeOperator" };
            case ts.SyntaxKind.AnyKeyword: return { tag: "AnyType" };
            case ts.SyntaxKind.TrueKeyword: return { tag: "TrueType" };
            case ts.SyntaxKind.FalseKeyword: return { tag: "FalseType" };
        }
        if (ts.isTypeAliasDeclaration(node))
            return handleTypeAliasType(node);
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
        if (ts.isMappedTypeNode(node))
            return handleMappedType(node);
        if (ts.isIndexedAccessTypeNode(node))
            return handleIndexAccessType(node);
        if (ts.isConditionalTypeNode(node))
            return handleConditionalType(node);
        if (ts.isInferTypeNode(node))
            return handleInferType(node);
        if (ts.isTypeQueryNode(node))
            return handleTypeQuery(node);
        console.log("Don't have a handler for: " + ts.SyntaxKind[node.kind], "It will be converted to an AnyType");
        return { tag: "AnyType" };
    };
    var handleTypeParameter = function (param) {
        var contents = param.name.escapedText.toString();
        return { tag: "TypeParameter", contents: contents };
    };
    var handleTypeAliasType = function (node) {
        var contents = handleTypeAliasDeclaration(node);
        return { tag: "TypeAliasType", contents: contents };
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
        var fullyQualifiedName = getFullyQualifiedName(type);
        var typeParameters = (node.typeParameters) ? node.typeParameters.map(handleTypeParameter) : [];
        var typeMembers = type.getProperties().map(function (symbol) { return handleTypeMember(symbol); });
        return { tag: "InterfaceDeclaration", contents: { name: name, fullyQualifiedName: fullyQualifiedName, typeParameters: typeParameters, typeMembers: typeMembers } };
    };
    var handleNamespaceBody = function (node) {
        if (ts.isModuleBlock(node))
            return { tag: "ModuleBlock", contents: node.statements.map(handleDeclarationElements) };
        if (ts.isIdentifier(node))
            return { tag: "NamespaceDeclaration", contents: { name: node.text, body: { tag: "ModuleBlock", contents: [] } } };
        return { tag: "ModuleBlock", contents: [] };
    };
    var handleModuleDeclaration = function (node) {
        var name = node.name.text;
        var body = node.body ? { tag: "NamespaceBodyDefinition", contents: handleNamespaceBody(node.body) } : undefined;
        return { tag: "ModuleDeclaration", contents: { name: name, body: body } };
    };
    var handleVariableStatement = function (node) {
        var contents = node.declarationList.declarations.map(function (d) {
            var name = d.name.getText();
            var type = d.type ? handleTSType(d.type) : { tag: "AnyType" };
            var fullyQualifiedName = getFullyQualifiedName(checker.getTypeAtLocation(node));
            return { tag: "VariableDeclaration", contents: { name: name, fullyQualifiedName: fullyQualifiedName, type: type } };
        });
        return { tag: "VariableStatement", contents: contents };
    };
    var handleFunctionDeclaration = function (node) {
        var name = node.name ? node.name.text : undefined;
        var type = checker.getTypeAtLocation(node);
        var fullyQualifiedName = getFullyQualifiedName(type);
        var typeParameters = node.typeParameters ? node.typeParameters.map(handleTypeParameter) : [];
        var parameters = node.parameters ? node.parameters.map(handleParameter) : [];
        if (name === "createMuiTheme") {
            //console.log(node, type)
            console.log(node.parameters.length, parameters.length, parameters);
        }
        var returnType = node.type ? handleTSType(node.type) : { tag: "AnyType" };
        return { tag: "FunctionElement", contents: { name: name, fullyQualifiedName: fullyQualifiedName, typeParameters: typeParameters, parameters: parameters, returnType: returnType } };
    };
    var handleClassDeclaration = function (node) {
        var name = node.name ? node.name.text : undefined;
        return { tag: "ClassElement", contents: { name: name } };
    };
    var handleNamespaceExplorationDeclaration = function (node) {
        var contents = node.name.text;
        return { tag: "NamespaceExportDeclaration", contents: contents };
    };
    var handleExportAssignment = function (node) {
        if (ts.isIdentifier(node.expression)) {
            var name_6 = node.expression.text;
            return { tag: "ExportAssignment", contents: name_6 };
        }
        return { tag: "ExportAssignment" };
    };
    var handleDeclarationElements = function (node) {
        if (ts.isImportDeclaration(node))
            return { tag: "ImportDeclaration" };
        if (ts.isImportEqualsDeclaration(node))
            return { tag: "ImportEqualsDeclaration" };
        if (ts.isExportAssignment(node))
            return handleExportAssignment(node);
        if (ts.isExportDeclaration(node))
            return { tag: "ExportDeclaration" };
        if (ts.isInterfaceDeclaration(node))
            return handleInterfaceDeclaration(node);
        if (ts.isTypeAliasDeclaration(node))
            return handleTypeAliasDeclaration(node);
        if (ts.isModuleDeclaration(node))
            return handleModuleDeclaration(node);
        if (ts.isVariableStatement(node))
            return handleVariableStatement(node);
        if (ts.isFunctionDeclaration(node))
            return handleFunctionDeclaration(node);
        if (ts.isClassDeclaration(node))
            return handleClassDeclaration(node);
        if (ts.isNamespaceExportDeclaration(node))
            return handleNamespaceExplorationDeclaration(node);
        console.log("There is no handler for " + ts.SyntaxKind[node.kind] + " it will be given the 'AmbientDeclaration' type");
        return { tag: "AmbientDeclaration" };
    };
    var getName = function (name, fullyQualifiedName) {
        return (fullyQualifiedName && fullyQualifiedName !== "__type" && fullyQualifiedName !== "__type.bivarianceHack")
            ? fullyQualifiedName : name ? name : "";
    };
    var types = {};
    var tallyTypes = function (elements) {
        var setName = function (element, name, fullyQualifiedName) {
            if (name)
                types[name] = element;
            if (fullyQualifiedName)
                types[fullyQualifiedName] = element;
        };
        elements.forEach(function (element) {
            if (element.tag === "ClassElement")
                setName(element, element.contents.name, element.contents.fullyQualifiedName);
            if (element.tag === "FunctionElement")
                setName(element, element.contents.name, element.contents.fullyQualifiedName);
            if (element.tag === "InterfaceDeclaration")
                setName(element, element.contents.name, element.contents.fullyQualifiedName);
            if (element.tag === "TypeAliasDeclaration")
                setName(element, element.contents.name, element.contents.fullyQualifiedName);
            if (element.tag === "VariableStatement") {
                element.contents.forEach(function (decl) {
                    types[decl.contents.name] = element;
                    if (decl.contents.fullyQualifiedName)
                        types[decl.contents.fullyQualifiedName] = element;
                });
            }
            if (element.tag === "ModuleDeclaration") {
                if (element.contents.body && element.contents.body.tag === "NamespaceBodyDefinition") {
                    if (Array.isArray(element.contents.body.contents.contents))
                        tallyTypes(element.contents.body.contents.contents);
                }
            }
        });
    };
    var srcs = sources.map(function (src) {
        var fileName = src.fileName;
        //console.log("Reading " + fileName)
        var elements = src.statements.map(handleDeclarationElements);
        tallyTypes(elements);
        return { tag: "DeclarationSourceFile", contents: { fileName: fileName, elements: elements } };
    }).filter(function (decl) {
        var result = decl.contents.fileName.match(filterRegex);
        return result !== null && result.length > 0;
    });
    //Object.keys(types).sort().forEach(e => console.log("key", e))
    return { sources: srcs, types: types };
}; }; };
exports._fileName = function (src) { return function () { return src.contents.fileName; }; };
