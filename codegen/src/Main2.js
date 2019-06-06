"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var ts = require("typescript");
/*
DeclarationModuleElement:
  DeclarationElement
  ImportDeclaration
  ImportAliasDeclaration
  ExportDeclarationElement
  ExportDefaultDeclarationElement
  ExportAssignment

DeclarationElement:
  InterfaceDeclaration
  TypeAliasDeclaration
  NamespaceDeclaration
  AmbientDeclaration
  ImportAliasDeclaration

{
  "VariableStatement": 1016,
  "FunctionDeclaration": 124,
  "InterfaceDeclaration": 1696,
  "TypeAliasDeclaration": 1028,
  "ModuleDeclaration": 57,
  "ClassDeclaration": 12,
  "ImportDeclaration": 594,
  "ExportAssignment": 147,
  "NamespaceExportDeclaration": 1,
  "ExportDeclaration": 408,
  "ImportEqualsDeclaration": 2
}

*/
var stats = {};
var addToStats = function (str) {
    return stats[str] ? stats[str] = stats[str] + 1 : stats[str] = 1;
};
var isAvatar = function (str) { return (str.indexOf("Avatar") >= 0 && str.indexOf("ListItemAvatar") < 0); };
exports._sourceFiles = function () {
    var options = ts.getDefaultCompilerOptions();
    var program = ts.createProgram(["./node_modules/@material-ui/core/index.d.ts"], options);
    var sources = program.getSourceFiles();
    var checker = program.getTypeChecker();
    var handleTypeAliasDeclaration = function (node) {
        var type = checker.getTypeAtLocation(node);
        var name = node.name.escapedText.toString();
        var aliasName = type.aliasSymbol ? type.aliasSymbol.name : undefined;
        var typeParameters = (node.typeParameters) ? node.typeParameters : [];
        return { tag: "TypeAliasDeclaration", contents: { name: name, aliasName: aliasName } };
    };
    var handleInterfaceDeclaration = function (node) {
        var name = node.name.escapedText.toString();
        var type = checker.getTypeAtLocation(node);
        var fullyQualifiedName = checker.getFullyQualifiedName(type.symbol);
        return { tag: "InterfaceDeclaration", contents: { name: name, fullyQualifiedName: fullyQualifiedName } };
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
