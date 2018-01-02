#   GopiandCode's Satisfiable Propositional Logic Formula Generator   
 This simple tool will generate a set of (un/)satisfiable propositional
 logic formulae.

 You can customize the complexity of the constructed formula by 
 increasing the branching amount.
 
##  Command Line Parameters"
``` propositional_generator count complexity output [seed] ```
 where
    `count`
        - the number of formula to generate
     `complexity`
        - the number of formula to generate
     `output`
        - the output file - will be overwritten if exists
     `seed`
        - optional - the seed used for generating random formula
 
## Testing
you can also use the system to test propositional satisfiability 
 checkers which conform to a specific format.
 
``` propositional_generator --test count [inputSize] [complexity] [seed] ```
 where
     `count`
        - the number of tests to run
     `inputSize`
        - the number of formulas per file submitted to the parser - 
          defaults to 10
     `complexity`
        - roughly related to the size of the formulas - defaults to 3
     `seed`
        - seed used to generate formula
 Note: Due to the terrible design I've used for accepting inputs, the
 	     only accepted permuations of arguments are: 
 ```propositional_generator --test count```
 ```propositional_generator --test count seed```
 ```propositional_generator --test count complexity seed```
 ```propositional_generator --test count inputSize complexity seed```
