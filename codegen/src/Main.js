"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var ts = require("typescript");
var readTypes = function () {
    var output = [];
    var getTSType = function (type) {
        try {
            if (type.isUnionOrIntersection())
                return { tag: "Union", contents: { types: type.types.map(getWithAliasProps).filter(function (t) { return t; }) } };
            if (type.flags & ts.TypeFlags.String)
                return { tag: "String" };
            if (type.flags & ts.TypeFlags.BooleanLike)
                return { tag: "Boolean" };
            if (type.flags & ts.TypeFlags.Number)
                return { tag: "Number" };
            if (type.flags & ts.TypeFlags.Null)
                return { tag: "Null" };
            if (type.flags & ts.TypeFlags.VoidLike)
                return { tag: "Unit" };
            if (type.flags & ts.TypeFlags.Any)
                return { tag: "Any" };
            if (type.isStringLiteral())
                return { tag: "StringLiteral", contents: { value: type.value } };
            if (type.isNumberLiteral())
                return { tag: "NumericLiteral", contents: { value: type.value } };
            var callSigs = type.getCallSignatures();
            if (callSigs.length) {
                var sig = callSigs[0];
                var parameters = sig.getParameters().map(function (p) { return optionalMember(p); });
                var returnType = getWithAliasProps(sig.getReturnType());
                return { tag: "Function", contents: { parameters: parameters, returnType: returnType } };
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
                return { tag: "UnknownObject", contents: { flags: objFlags } };
            }
            if (type.flags & ts.TypeFlags.TypeParameter) {
                var name_3 = checker.typeToString(type);
                return { tag: "TypeParam", contents: { name: name_3 } };
            }
            return { tag: "Unknown", contents: { name: checker.typeToString(type), flags: type.flags } };
        }
        catch (e) {
            console.log(type.flags, checker.typeToString(type));
            return { tag: "ExceptionType", contents: { e: e, type: checker.typeToString(type), flags: type.flags } };
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
    var visit = function (node) {
        if (isNodeExported(node)) {
            if (ts.isInterfaceDeclaration(node)) {
                var symbol = checker.getSymbolAtLocation(node.name);
                if (symbol) {
                    var nodeType = checker.getTypeAtLocation(node);
                    var name_4 = checker.getFullyQualifiedName(nodeType.symbol);
                    if (nodeType.isClassOrInterface()) {
                        var members = convertProperties(nodeType, node);
                        output.push({ tag: "Interface", contents: { name: name_4, members: members } });
                    }
                }
            }
            else if (ts.isModuleDeclaration(node)) {
                ts.forEachChild(node, visit);
            }
        }
    };
    var options = ts.getDefaultCompilerOptions();
    var program = ts.createProgram(["./node_modules/@material-ui/core/index.d.ts"], options);
    var sources = program.getSourceFiles();
    var checker = program.getTypeChecker();
    sources.forEach(function (source) {
        ts.forEachChild(source, visit);
    });
    console.log("output", output.length);
    return output;
};
readTypes();
