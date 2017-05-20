# A little library of my experiments in Parsing using Haskell
## Could have only achieved this having learnt how monads work

At the moment it parses a simple set of if statements with some basic expressions. I'm starting to see why haskell is used for this kinda stuff. I
It's just beautiful when you can just chain together prior statements to make a parser. Very nice.

Also, doesn't use the parsec library - all homegrown!

Example Parsing:
The String


      "while(x>0) x:=x+1"



produces



      (WhileLoop (BinOp Gt (Var "x") (Num 0)) (Assign (Var "x") (BinOp Add (Var "x") (Num 1))))



or



      if(x>0) then x:=x+1; else x:= 0;




produces



         (IfCond (BinOp Gt (Var "x") (Num 0)) (Assign (Var "x") (BinOp Add (Var "x") (Num 1))) (Assign (Var "x") (Num 0)))
 
 So it kinda works.
Also, not entirely sure about the terminology, the grammar contains Left recursive structures so is the parser LR?
Needs more research.

# NewStuff: PreProcessor
## A C-Preprocessor to work along side a C OOP library I'm building
I'm currently working on a C OOP library to make my C code more portable for future coding sessions. As part of this effort, I've been working on implementing an OOP system in C - to make the process of coding in this framework easier, I'm building a preprocessor to generate the boilerplate code.
Obviously, I'm making the parser for this boilerplate in Haskell - so far the parser is complete, just need to work on synthesizing the correct output.
An Example of the boilderplate syntax:

      Point : Object with PointClass <
	      int x;
	      int y;
	      int z;
      >
 which is then parsed into
 
      (Class 
            "Point" 
            "PointClass" 
            "Object" 
            [
            Attr (Type "int" False) "x",
            Attr (Type "int" False) "y",
            Attr (Type "int" False) "z"
            ]
       )
Or

            PointClass : Class <
                  static int *something(int x, int y);
                  dynamic int *nothing(int x, int y);
            >
which is then parsed into

      (MetaClass 
            "PointClass"
            "Class"
            [
                  Func (Type "int" True) "something" [
                              Attr (Type "int" False) "x",
                              Attr (Type "int" False) "y"
                                                     ] True,
                   Func (Type "int" True) "nothing" [
                              Attr (Type "int" False) "x",
                              Attr (Type "int" False) "y"
                                                    ] False
	           ]
       )
       
 So that's kinda half the battle down!
