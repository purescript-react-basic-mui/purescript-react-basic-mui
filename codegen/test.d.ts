import * as react from "react"
import core from "@material-ui/core/Card"

export = TestNamepsace 
export as namespace TestNamepsace;

declare namespace TestNamepsace {


  export interface TestInterface{
    foo: string
  }

    namespace AnotherNamespace{
      export type TypeAlias = { a: string }
    }

  export class TestClass{}

  export const testStatement: number
}

declare const foo: number

declare function testFunction(blah: boolean): TestNamepsace.TestInterface





//export const foo = (p0: string, p1?: number) => boolean