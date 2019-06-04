import * as ts from "typescript"
import * as fs from "fs"

import { 
  createInterfaceMap,
  getBaseInterfaces,
  getInterfaces,
  handleInterface,
  getTypeAliases,
  createTypeAliasMap,
  getFunctions,
  getClasses,
  getExportAssignments
} from "./parser"

import { propsCompare, Field, WrittenProps, Props, InterfaceToFile, FunctionToFile, ClassToFile, TypeAliasMap, ExportAssignmentToFile, ImportMatcher } from "./types" 
import { collectForeignData, top, writeForeignData, writeProps, componentName } from "./writer"
import { ignoreForeignDataList, additionalForeignData, importMatchers, additionalImports } from "./consts";
import { lowerCaseFirstLetter, capitalize } from "./utils";

const getModuleName = (writtenProps: WrittenProps): string => {
  const tokens: string[] = writtenProps.fileName.split("@material-ui/core/")[1].split("/")
  if(tokens.length > 1){
    return (tokens[0] && (tokens[0].toLowerCase()) === tokens[0]) 
        ? `${capitalize(tokens[0])}.${capitalize(removeDeclaration(tokens[1]))}`
        : `${capitalize(removeDeclaration(tokens[1]))}`
  } else {
    return `${capitalize(removeDeclaration(tokens[0]))}`
  }
}

const removeDeclaration = (str: string | undefined): string => 
  (str) ? str.replace(/\.d\.ts$/, "") : ""

const getFunctionName = (writtenProps: WrittenProps): string => {
  const tokens: string[] = writtenProps.fileName.split("@material-ui/core/")[1].split("/")
  return (tokens.length > 1)
    ? ("_" + lowerCaseFirstLetter(removeDeclaration(tokens[1])))
    : ("_" + lowerCaseFirstLetter(removeDeclaration(tokens[0])))
}

const getFileName = (writtenProps: WrittenProps, fileType: string): string => {
  const tokens: string[] = writtenProps.fileName.split("@material-ui/core/")[1].split("/")
  if(tokens.length > 1){
    return (tokens[0] && (tokens[0].toLowerCase()) === tokens[0]) 
      ? `../src/${tokens[0]}/${capitalize(tokens[1].replace(/\.d\.ts$/, fileType))}`
      : `../src/${capitalize(tokens[1].replace(/\.d\.ts$/, fileType))}`
  } else {
    return `../src/${capitalize(tokens[0].replace(/\.d\.ts$/, fileType))}`
  }
}

const printWrittenProps = (writtenProps: WrittenProps[]): void =>
  writtenProps.forEach((p) => {
    console.log(p.props.join("\n\n"))
    if(p.fns.length){
      console.log("\n")
      console.log(p.fns.join("\n\n"))
    }
    console.log("\n")
  })

const writeJS = (writtenProps: WrittenProps): void => {
  const moduleName = getModuleName(writtenProps)
  const fileName = getFileName(writtenProps, ".js")
  if(writtenProps.fileName.indexOf("@material-ui/core") >= 0 && !fs.existsSync(fileName)){
    const js = `exports.${getFunctionName(writtenProps)} = require("@material-ui/core/${moduleName}")`
    fs.writeFileSync(fileName, js)
  }
}


const getImports = (foreignData: string, writtenProps: WrittenProps): string => {
  const additionalForeignData = additionalImports[getModuleName(writtenProps)] ? additionalImports[getModuleName(writtenProps)].join("\n") : ""
  const props = foreignData + " ReactComponent \n" + additionalForeignData + "\n" + writtenProps.fns.join("\n") + "\n" + writtenProps.props.join("\n")
  
  const react = importMatchers.filter(matcher => matcher.module.match(/^React\.Basic \(/))
  const notReact = importMatchers.filter(matcher => !matcher.module.match(/^React\.Basic \(/))

  let notReactMatches = notReact.reduce((acc: string[], matcher: ImportMatcher) => 
    (props.match(matcher.regex) && acc.indexOf("import " + matcher.module) < 0) ? acc.concat(["import " + matcher.module]) : acc
  , []).sort().join("\n")


  let reactMatches = react.reduce((acc: string[], matcher: ImportMatcher) => 
    (props.match(matcher.regex) && acc.indexOf("import " + matcher.module) < 0) ? acc.concat(["import " + matcher.module]) : acc
  , []).sort().map(str => str.substr(20, str.length - 21)).join(", ")

  reactMatches = (reactMatches.trim()) ? "import React.Basic (" + reactMatches + ")" : ""

  return notReactMatches + "\n" + reactMatches 
}

const missingProps: string[] = []

const writePS = (writtenProps: WrittenProps) => (props: Props[]): void => {
  const moduleName = getModuleName(writtenProps)
  const fileName = getFileName(writtenProps, ".purs")
  const functionName = moduleName[0].toLowerCase() + moduleName.slice(1)

  const foreignData: string[] = additionalForeignData[moduleName] ? additionalForeignData[moduleName] : []
  let imports = ""
  if(writtenProps.fileName.indexOf("@material-ui/core") >= 0){
    if(!fs.existsSync(fileName)){
      writtenProps.foreignData.forEach(name => {
        let referencedProps: undefined | Props = undefined
        props.forEach(prop => {
          if(prop.name === name){
            referencedProps = prop
            const foreignModuleName = capitalize(getModuleName(writeProps(prop)))
            console.log("Found: " + name + " as  " + foreignModuleName + " for " + moduleName)
            if(moduleName !== "Global" && moduleName.toUpperCase() !== foreignModuleName.toUpperCase()) imports += `import MaterialUI.Basic.${foreignModuleName} (${name})\n`
          }
        })
        if(referencedProps === undefined && ignoreForeignDataList.indexOf(name) < 0){
          foreignData.push(name)
          if(missingProps.indexOf(name) < 0 ){
            missingProps.push(name) 
          }
        }
        referencedProps = undefined
      })
      const foreignDataStr = foreignData.map(f => "foreign import data " + f + " :: Type").join("\n")
      fs.writeFileSync(fileName, `-- ${writtenProps.fileName}
module MaterialUI.Basic.${capitalize(moduleName)} where 
${getImports(foreignDataStr, writtenProps)}

${imports}

${foreignDataStr}

foreign import ${getFunctionName(writtenProps)} :: forall a. ReactComponent a

`)

    }
    const ps = `${writtenProps.props.join("\n\n")}

${writtenProps.fns.join("\n\n")}  
`
    fs.writeFileSync(fileName, ps, { flag: "a"})
  }
}

const options = ts.getDefaultCompilerOptions()
const program = ts.createProgram(["./node_modules/@material-ui/core/index.d.ts"], options)
const sources =program.getSourceFiles()

const interfaces: InterfaceToFile[] = 
  sources.map(src => {
    const fileName = src.fileName
    const interfaces = getInterfaces(src)
    return interfaces.map(iface => ({ fileName, iface }))
  })
  .reduce((acc, a) => acc.concat(a), [])

const interfaceMap = createInterfaceMap(interfaces)
const typeAliasMap: TypeAliasMap = createTypeAliasMap((sources.map(src => getTypeAliases(src))).reduce((acc, a) => acc.concat(a)))

const exportAssignments: ExportAssignmentToFile[] = sources.map(src => {
    const fileName = src.fileName
    const assignments = getExportAssignments(src)
    return assignments.map(assignment => ({ fileName, assignment }))
  })
  .reduce((acc, a) => acc.concat(a), [])

const baseInterfaces = getBaseInterfaces(interfaceMap)(exportAssignments)

const functions: FunctionToFile[] = 
  sources.map(src => {
    const fileName = src.fileName
    const funcs = getFunctions(src)
    return funcs.map(func => ({ fileName, func }))
  })
  .reduce((acc, a) => acc.concat(a), [])
const baseFunctions = functions.filter(({fileName}) => fileName.indexOf("@material-ui") >= 0)

const classes: ClassToFile[] = sources.map(src => {
    const fileName = src.fileName
    const clazzes = getClasses(src)
    return clazzes.map(clazz => ({ fileName, clazz }))
  })
  .reduce((acc, a) => acc.concat(a), [])

const props = baseInterfaces.map(handleInterface(true)(typeAliasMap)(interfaceMap)).sort(propsCompare)

const remainingTypeNames: string[] = collectForeignData(([] as Field[]).concat(...props.map((prop) => prop.fields)))

const buildAdditionalProps = (names: string[], existingNames: string[], count: number): Props[] => {

  if(names.length === 0) return []
  if(count > 10) throw ("propbably in a cycle while building additional props")

  const additionalProps = 
    names 
    .filter(name => existingNames.indexOf(name) < 0)
    .filter(name => ignoreForeignDataList.indexOf(name) < 0)
    .filter(name => interfaceMap[name] !== undefined)
    .map(name => {
      const { iface, fileName } = interfaceMap[name]  
      return handleInterface(false)(typeAliasMap)(interfaceMap)({ iface, fileName, classNames: []})
    })

    
  const additionalTypeNames = 
    collectForeignData(([] as Field[])
      .concat(...additionalProps.map((prop) => prop.fields)))
      .filter(name => ignoreForeignDataList.indexOf(name) < 0)
      .filter(name => names.indexOf(name) < 0)

  return additionalProps.concat(buildAdditionalProps(additionalTypeNames, existingNames.concat(names), count + 1))
}


const remainingProps = buildAdditionalProps(remainingTypeNames, props.map(p => p.name), 0)

const fileNames: string[] = []
const allProps = props.concat(remainingProps).sort(propsCompare)
allProps.map(writeProps).forEach((prop: WrittenProps) => {
  writeJS(prop)  
  writePS(prop)(allProps)
})
//console.log("Props: " + allProps.length)
//console.log("File Names: " + fileNames.length)
//fileNames.sort().forEach(fileName => console.log(fileName))

missingProps.sort().forEach(name => {
  console.log(name)
})


