# stack-demo

Creating project using `stack`.

`stack new stack-demo` - creates empty project
`stack ghci` - compiles the project including Main and Lib 

```
app/Main.hs -> main method importing Lib and calling someFunction
src/Lib.hs -> libraries, code
test/Spec.hs -> all tests here
stack-demo.cabal -> config file
```

`stack build` - builds project
`stack exec stack-demo-exe` - executes stack-demo project

in **stack-demo.cabal** 
 - we can rename executable to whatever we want
 - add to `ghc-options` -dynamic -> to decrese output size of executable

`stack test` - to run all tests

