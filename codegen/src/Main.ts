import * as ts from "typescript"

const options = ts.getDefaultCompilerOptions()
const program = ts.createProgram(["./node_modules/@material-ui/core/index.d.ts"], options)
const sources = program.getSourceFiles()
const checker = program.getTypeChecker()


const isNodeExported = (node: ts.Node): boolean => 
  (ts.getCombinedNodeFlags(node) & ts.ModifierFlags.Export) !== 0 ||
  (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile)

const getSyntaxKindName = (kind: ts.SyntaxKind): string => ts.SyntaxKind[kind]

const syntaxNames: {[key:string]:number} = {}

const go  = (node: ts.Node) => {
  (isNodeExported(node) && syntaxNames[getSyntaxKindName(node.kind)])
    ? syntaxNames[getSyntaxKindName(node.kind)] = syntaxNames[getSyntaxKindName(node.kind)] + 1
    : syntaxNames[getSyntaxKindName(node.kind)] = 1
}

sources.forEach( source => {
  ts.forEachChild(source, go)
})

Object.keys(syntaxNames).sort().forEach(name => console.log(name, syntaxNames[name]))
console.log("Total Kinds: " + Object.keys(syntaxNames).length)
console.log("Total things: " + Object.keys(syntaxNames).reduce((total, key) => total + syntaxNames[key], 0))



