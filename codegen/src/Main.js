"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var ts = require("typescript");
exports._interfaces = function () {
    var output = [];
    var getTSType = function (type) {
        try {
            if (type.isUnionOrIntersection())
                return { tag: "UnionType", contents: { types: type.types.map(getWithAliasProps).filter(function (t) { return t; }) } };
            if (type.flags & ts.TypeFlags.String)
                return { tag: "StringType" };
            if (type.flags & ts.TypeFlags.BooleanLike)
                return { tag: "BooleanType" };
            if (type.flags & ts.TypeFlags.Number)
                return { tag: "NumberType" };
            if (type.flags & ts.TypeFlags.Null)
                return { tag: "NullType" };
            if (type.flags & ts.TypeFlags.VoidLike)
                return { tag: "VoidType" };
            if (type.flags & ts.TypeFlags.Any)
                return { tag: "AnyType" };
            if (type.isStringLiteral())
                return { tag: "StringLiteralType", contents: { value: type.value } };
            if (type.isNumberLiteral())
                return { tag: "NumericLiteralType", contents: { value: type.value } };
            var callSigs = type.getCallSignatures();
            if (callSigs.length) {
                var sig = callSigs[0];
                var parameters = sig.getParameters().map(function (p) { return optionalMember(p); });
                var returnType = getWithAliasProps(sig.getReturnType());
                return { tag: "FunctionType", contents: { parameters: parameters, returnType: returnType } };
            }
            if (type.flags & (ts.TypeFlags.Object | ts.TypeFlags.NonPrimitive)) {
                var objFlags = type.objectFlags;
                if (objFlags & ts.ObjectFlags.Tuple) {
                    var types = [];
                    return { tag: "TupleType", contents: { types: types } };
                }
                if (objFlags & ts.ObjectFlags.Anonymous) {
                    var members = convertProperties(type);
                    return { tag: "AnonymousObject", contents: { members: members } };
                }
                if (objFlags & ts.ObjectFlags.Reference) {
                    var tr = type;
                    var name_1 = checker.getFullyQualifiedName(type.symbol);
                    var typeParams = tr.typeArguments ? tr.typeArguments.map(getWithAliasProps).filter(function (i) { return i; }) : [];
                    var flags = type.flags;
                    return { tag: "TypeReference", contents: { name: name_1, typeParams: typeParams, flags: flags, objFlags: objFlags } };
                }
                if (objFlags & ts.ObjectFlags.Interface) {
                    var name_2 = checker.getFullyQualifiedName(type.symbol);
                    return { tag: "InterfaceReference", contents: { name: name_2 } };
                }
                return { tag: "UnknownObject", contents: { type: checker.typeToString(type), flags: objFlags } };
            }
            if (type.flags & ts.TypeFlags.TypeParameter) {
                var name_3 = checker.typeToString(type);
                return { tag: "TypeParam", contents: { name: name_3 } };
            }
            return { tag: "Unknown", contents: { name: checker.typeToString(type), flags: type.flags } };
        }
        catch (e) {
            var error = JSON.stringify(e);
            return { tag: "ExceptionType", contents: { error: error, type: checker.typeToString(type), flags: type.flags } };
        }
    };
    var getWithAliasProps = function (t) {
        var type = getTSType(t);
        if (t.aliasSymbol) {
            var typeReference = checker.getFullyQualifiedName(t.aliasSymbol);
            var typeParams = t.aliasTypeArguments ? t.aliasTypeArguments.map(getTSType) : [];
            return { tag: "TypeAlias", contents: { typeReference: typeReference, typeParams: typeParams, type: type } };
        }
        return type;
    };
    var optionalMember = function (sym, node) {
        var optional = ((sym.flags & ts.SymbolFlags.Optional) === ts.SymbolFlags.Optional);
        var type = getWithAliasProps(checker.getTypeOfSymbolAtLocation(sym, node ? node : sym.valueDeclaration));
        return { tag: "Member", contents: { name: sym.name, type: type, optional: optional } };
    };
    var convertProperties = function (nodeType, node) {
        return nodeType.getProperties().map(function (sym) { return optionalMember(sym, node); });
    };
    var isNodeExported = function (node) {
        return (ts.getCombinedNodeFlags(node) & ts.ModifierFlags.Export) !== 0 ||
            (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile);
    };
    var visit = function (fileName) { return function (node) {
        if (isNodeExported(node)) {
            if (ts.isInterfaceDeclaration(node)) {
                var symbol = checker.getSymbolAtLocation(node.name);
                if (symbol) {
                    var nodeType = checker.getTypeAtLocation(node);
                    var name_4 = checker.getFullyQualifiedName(nodeType.symbol);
                    if (nodeType.isClassOrInterface()) {
                        var members = convertProperties(nodeType, node);
                        output.push({ tag: "Interface", contents: { fileName: fileName, name: name_4, members: members } });
                    }
                }
            }
            else if (ts.isVariableStatement(node)) {
                if (fileName.indexOf("Avatar.d.ts") >= 0 && fileName.indexOf("ListItemAvatar") < 0) {
                    var type = checker.getTypeAtLocation(node);
                    node.forEachChild(function (node) {
                        if (ts.isVariableDeclarationList(node)) {
                            node.declarations.map(function (decl) {
                                var nodeType = checker.getTypeAtLocation(decl);
                                //console.log(nodeType)
                                var type = getWithAliasProps(nodeType);
                                console.log(type);
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
                            });
                        }
                        else {
                        }
                    });
                }
            }
            else if (ts.isModuleDeclaration(node)) {
                ts.forEachChild(node, visit(fileName));
            }
            else {
                if (fileName.indexOf("Avatar.d.ts") >= 0 && fileName.indexOf("ListItemAvatar") < 0) {
                    if (node.kind == ts.SyntaxKind.EndOfFileToken && node.parent && ts.isSourceFile(node.parent)) {
                        var source = node.parent;
                        source.statements.forEach(function (statement) {
                            if (isNodeExported(statement)) {
                                console.log(ts.SyntaxKind[statement.kind]);
                            }
                        });
                    }
                }
            }
        }
    }; };
    var options = ts.getDefaultCompilerOptions();
    var program = ts.createProgram(["./node_modules/@material-ui/core/index.d.ts"], options);
    var sources = program.getSourceFiles();
    var checker = program.getTypeChecker();
    sources.forEach(function (source) {
        var fileName = source.fileName;
        ts.forEachChild(source, visit(fileName));
    });
    var slice = output.slice(0).filter(function (i) { return i.contents.name.indexOf(".GridProps") < 0; });
    console.log("Output: " + output.length, "Slice: " + slice.length);
    return slice;
};
