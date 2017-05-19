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
