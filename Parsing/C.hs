import System.IO
import Data.List
import Control.Monad
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Program = Program [Extern_Declaration]
program = sepBy extern_declaration whiteSpace >>= \eds -> return $ Program eds

instance Show Program where
    show (Program d) = (concat . intersperse " " . map show $ d) 

data Extern_Declaration = Ext_Function Function_Definition
                        | Ext_Declaration Declaration

extern_declaration = (function_definition >>= \f -> return $ Ext_Function f)
                 <|> (declaration >>= \d -> return $ Ext_Declaration d)

instance Show Extern_Declaration where
    show (Ext_Function f) = show f
    show (Ext_Declaration d) = show d

data Function_Definition = Function [Declaration_Specifier] Declarator [Declaration] Compound_Statement

function_definition = (sepBy (declaration_specifier) semi) >>= \dss -> declarator >>= \d -> parens (commaSep declaration) >>= \ds -> compound_statement >>= \cs -> return $ Function dss d ds cs

instance Show Function_Definition where
    show (Function dsp d dl cs) = (concat . intersperse " " . map show $ dsp) ++ " " ++ (show d) ++ "(" ++ (concat . intersperse ", " . map show $ dl) ++ ") " ++ (show cs)


data Declaration_Specifier = Declaration_Storage Storage_Class_Specifier
                           | Declaration_Type_Specifier Type_Specifier
                           | Declaration_Type_Qualifier Type_Qualifier

declaration_specifier = (storage_class_specifier >>= \sp -> return $ Declaration_Storage sp)
                    <|> (type_specifier >>= \sp -> return $ Declaration_Type_Specifier sp)
                    <|> (type_qualifier >>= \ql -> return $ Declaration_Type_Qualifier ql)


instance Show Declaration_Specifier where
    show (Declaration_Storage s) = show s
    show (Declaration_Type_Qualifier t) = show t
    show (Declaration_Type_Specifier s) = show s

data Storage_Class_Specifier = Auto
                             | Register
                             | Static
                             | Extern
                             | Typedef

storage_class_specifier = (reserved "auto"     >> (return $ Auto))
                      <|> (reserved "register" >> (return $ Register))
                      <|> (reserved "static"   >> (return $ Static))
                      <|> (reserved "extern"   >> (return $ Extern))
                      <|> (reserved "typedef"  >> (return $ Typedef))

instance Show Storage_Class_Specifier where
    show (Auto) = "auto"
    show (Register) = "register"
    show (Static) = "static"
    show (Extern) = "extern"
    show (Typedef) = "typedef"

data Type_Specifier = Type_Void
                    | Type_Short
                    | Type_Char
                    | Type_Int
                    | Type_Long
                    | Type_Float
                    | Type_Double
                    | Type_Signed
                    | Type_Unsigned
                    | Type_Struct Struct_Specifier
                    | Type_Union Union_Specifier
                    | Type_Enum Enum_Specifier
                    | Type_Typedef Typedef_Name

type_specifier = (reserved "void" >> return Type_Void)
             <|> (reserved "short" >> return Type_Short)
             <|> (reserved "char" >> return Type_Char)
             <|> (reserved "int" >> return Type_Int)
             <|> (reserved "long" >> return Type_Long)
             <|> (reserved "float" >> return Type_Float)
             <|> (reserved "double" >> return Type_Double)
             <|> (reserved "signed" >> return Type_Signed)
             <|> (reserved "unsigned" >> return Type_Unsigned)
             <|> (struct_specifier >>= \expr -> return $ Type_Struct expr)
             <|> (union_specifier >>= \expr -> return $ Type_Union expr)
             <|> (enum_specifier >>= \expr -> return $ Type_Enum expr)
             <|> (identifier >>= \expr -> return $ Type_Typedef expr)

instance Show Type_Specifier where
    show Type_Void            = "void"
    show Type_Short           = "short"
    show Type_Char            = "char"
    show Type_Int             = "int"
    show Type_Long            = "long"
    show Type_Float           = "float"
    show Type_Double          = "double"
    show Type_Signed          = "signed"
    show Type_Unsigned        = "unsigned"
    show (Type_Struct struct) = show struct
    show (Type_Union union)   = show union 
    show (Type_Enum enum)     = show enum 
    show (Type_Typedef name)  = name 

data Struct_Specifier = Full_Struct Identifier [Struct_Declaration]
                      | Anonymous_Struct [Struct_Declaration]
                      | Partial_Struct Identifier

struct_specifier = (reserved "struct" >> identifier >>= \name -> braces (semiSep struct_declaration) >>= \nms -> return $ Full_Struct name nms)
               <|> (reserved "struct" >> braces (semiSep struct_declaration) >>= \nms -> return $ Anonymous_Struct nms)
               <|> (reserved "struct" >> identifier >>= \name -> return $ Partial_Struct name )

instance Show Struct_Specifier where
    show (Full_Struct i xs) = "struct " ++ i ++ "{" ++ (concat . intersperse ";" . map show $ xs) ++ "}"
    show (Anonymous_Struct xs) = "struct {" ++ (concat . intersperse ";" . map show $ xs) ++ "}"
    show (Partial_Struct id) = "struct " ++ id

data Union_Specifier = Full_Union Identifier [Struct_Declaration]
                      | Anonymous_Union [Struct_Declaration]
                      | Partial_Union Identifier

union_specifier = (reserved "union" >> identifier >>= \name -> braces (semiSep struct_declaration) >>= \nms -> return $ Full_Union name nms)
               <|> (reserved "union" >> braces (semiSep struct_declaration) >>= \nms -> return $ Anonymous_Union nms)
               <|> (reserved "union" >> identifier >>= \name -> return $ Partial_Union name )

instance Show Union_Specifier where
    show (Full_Union i xs) = "union " ++ i ++ "{" ++ (concat . intersperse ";" . map show $ xs) ++ "}"
    show (Anonymous_Union xs) = "union {" ++ (concat . intersperse ";" . map show $ xs) ++ "}"
    show (Partial_Union id) = "union " ++ id



data Enum_Specifier = Full_Enum Identifier [Enumerator]
                    | Anonymous_Enum [Enumerator]
                    | Partial_Enum Identifier


enum_specifier = (reserved "enum" >> identifier >>= \name -> braces (semiSep enumerator) >>= \nms -> return $ (Full_Enum name nms))
               <|> (reserved "enum" >> braces (semiSep enumerator) >>= \nms -> return $ (Anonymous_Enum nms))
               <|> (reserved "enum" >> identifier >>= \name -> return $ (Partial_Enum name ))

instance Show Enum_Specifier where
    show (Full_Enum i xs) = "enum " ++ i ++ "{" ++ (concat . intersperse ";" . map show $ xs) ++ "}"
    show (Anonymous_Enum xs) = "enum {" ++ (concat . intersperse ";" . map show $ xs) ++ "}"
    show (Partial_Enum id) = "enum " ++ id


data Type_Descriptor = Type_Descriptor_Specifier Type_Specifier
                     | Type_Descriptor_Qualifier Type_Qualifier

type_descriptor = (type_qualifier >>= \q -> return $ Type_Descriptor_Qualifier q)
              <|> (type_specifier >>= \s -> return $ Type_Descriptor_Specifier s)

instance Show Type_Descriptor where
    show (Type_Descriptor_Specifier ts) = show ts
    show (Type_Descriptor_Qualifier tq) = show tq

data Struct_Declaration = Struct_Field [Type_Descriptor] [Struct_Declarator]

struct_declaration = ((sepBy type_descriptor whiteSpace) >>= \tys -> (sepBy struct_declarator whiteSpace) >>= \str -> return $ (Struct_Field tys str))

instance Show Struct_Declaration where
    show (Struct_Field ds ts) = (concat . intersperse " " . map show $ ds) ++ " " ++ (concat . intersperse " " . map show $ ts)

data Struct_Declarator = Struct_Full_Field Declarator
                       | Struct_Bit_Field Declarator Expression
                       | Struct_Empty_Field Expression

struct_declarator = (declarator >>= \decl -> reservedOp ":" >> expression >>= \expr -> return $ Struct_Bit_Field decl expr)
                <|> (declarator >>= \decl -> return $ Struct_Full_Field decl)
                <|> (reservedOp ":" >> expression >>= \expr -> return $ Struct_Empty_Field expr)

instance Show Struct_Declarator where
    show (Struct_Full_Field ds) = show ds
    show (Struct_Bit_Field ds e) = show ds ++ " : " ++ show e
    show (Struct_Empty_Field e) = " : " ++ show e 

data Declarator = Declarator_Pointer Pointer Direct_Declarator
                | Declarator_Only Direct_Declarator

declarator = (pointer >>= \p -> direct_declarator >>= \dd -> return $ Declarator_Pointer p dd )
         <|> (direct_declarator >>= \p -> return $ Declarator_Only p)

instance Show Declarator where
    show (Declarator_Pointer ps dd) = (show $ ps) ++ " " ++ (show $ dd)
    show (Declarator_Only dd)       = (show $ dd)

data Pointer = Pointer_Only Type_Qualifier
             | Pointer_And Type_Qualifier Pointer

pointer_type = (reservedOp "*" >> type_qualifier >>= \t -> (return $ t))
pointer = many pointer_type >>= \res -> if length res == 1 then (return $ Pointer_Only (head res)) else (return $ foldr (Pointer_And) (Pointer_Only (last res)) (init res))

instance Show Pointer where
    show (Pointer_Only qualifier) = "* " ++  (show qualifier)
    show (Pointer_And a b)        = "* " ++ (show a) ++ "* " ++ (show b)

data Type_Qualifier = Type_Const | Type_Volatile

type_qualifier = (reserved "const" >> return Type_Const)
             <|> (reserved "volatile" >> return Type_Volatile)

instance Show Type_Qualifier where
    show (Type_Const)    = "const"
    show (Type_Volatile) = "volatile"

data Direct_Declarator = Declarator_Identifier Identifier
                       | Declarator_Parens Declarator
                       | Declarator_Arr_Empty Direct_Declarator
                       | Declarator_Arr  Direct_Declarator Expression
                       | Declarator_Func Direct_Declarator [Parameter_Declaration]
r_base_declarator = (parens declarator >>= \d -> return $ (Declarator_Parens d ))
                <|> (identifier >>= \id -> return $ (Declarator_Identifier id ))

r_operator_declarator = (brackets expression >>= \exp -> return $ Just $ Left exp)
                    <|> (parens (commaSep parameter_declaration) >>= \e -> return $ Just $ Right e)
                    <|> (reservedOp "[" >> reservedOp "]" >> (return $ Nothing))

direct_declarator = r_base_declarator >>= \id -> many r_operator_declarator >>= \fs -> return $ foldl (\acc x -> case x of
                                                                                                        (Just (Right e)) -> Declarator_Func acc e
                                                                                                        (Just (Left e))  -> Declarator_Arr acc e 
                                                                                                        Nothing          -> Declarator_Arr_Empty acc) (id) fs

instance Show Direct_Declarator where
    show (Declarator_Identifier i) = i
    show (Declarator_Parens d) = "(" ++ (show d) ++ ")"
    show (Declarator_Arr_Empty a) = (show a) ++ "[]"
    show (Declarator_Arr a b) = (show a) ++ "[" ++ (show b) ++ "]"
    show (Declarator_Func a b) = (show a) ++ "(" ++ (concat . intersperse ", " . map show $ b) ++ ")"

data Expression = Expression_Identifier         Identifier
                | Expression_Constant           Constant
                | Expression_String             String
                | Expression_Parens             Expression
                | Expression_Comma              Expression Expression
                | Expression_Ternary            Expression Expression Expression
                | Expression_Or                 Expression Expression
                | Expression_And                Expression Expression
                | Expression_Bit_Or             Expression Expression
                | Expression_Bin_Neg            Expression
                | Expression_Xor                Expression Expression
                | Expression_Bit_And            Expression Expression
                | Expression_Relation_Eq        Expression Expression
                | Expression_Relation_Neq       Expression Expression
                | Expression_Relation_Lt        Expression Expression
                | Expression_Relation_Gt        Expression Expression
                | Expression_Relation_Lte       Expression Expression
                | Expression_Relation_Gte       Expression Expression
                | Expression_Bit_RShift         Expression Expression
                | Expression_Bit_LShift         Expression Expression
                | Expression_Relational_Not     Expression
                | Expression_Plus               Expression Expression
                | Expression_Minus              Expression Expression
                | Expression_Mult               Expression Expression
                | Expression_Div                Expression Expression
                | Expression_Mod                Expression Expression
                | Expression_Cast Type_Name     Expression
                | Expression_Pre_Increment      Expression
                | Expression_Pre_Decrement      Expression
                | Expression_SizeOf_Expression  Expression
                | Expression_SizeOf_Type        Type_Name
                | Expression_Address_Of         Expression
                | Expression_Array              Expression Expression
                | Expression_Function           Expression [Expression]
                | Expression_Dereference        Expression
                | Expression_Struct_Dot         Expression Identifier
                | Expression_Struct_Arrow       Expression Identifier
                | Expression_Post_Increment     Expression
                | Expression_Post_Decrement     Expression
                | Expression_Assignment         Expression Assignment_Operator Expression

instance Show Expression where
    show (Expression_Identifier i)       = i
    show (Expression_Constant c)         = show c
    show (Expression_String s)           = s
    show (Expression_Parens expr)        = "(" ++ (show expr) ++ ")"
    show (Expression_Comma a b)          = (show a) ++ ", " ++ (show b)
    show (Expression_Ternary a b c)      = (show a) ++ " ? " ++ (show b) ++ " : " ++ (show c)
    show (Expression_Bin_Neg a)          = "~" ++ (show a)
    show (Expression_Or a b)             = (show a) ++ " || " ++ (show b)
    show (Expression_And a b)            = (show a) ++ " && " ++ (show b)
    show (Expression_Bit_Or a b)         = (show a) ++ " | " ++ (show b)
    show (Expression_Xor a b)            = (show a) ++ " ^ " ++ (show b)
    show (Expression_Bit_And a b)        = (show a) ++ " & " ++ (show b)
    show (Expression_Relation_Eq a b)    = (show a) ++ " == " ++ (show b)
    show (Expression_Relation_Neq a b)   = (show a) ++ " != " ++ (show b)
    show (Expression_Relation_Lt a b)    = (show a) ++ " < " ++ (show b)
    show (Expression_Relation_Gt a b)    = (show a) ++ " > " ++ (show b)
    show (Expression_Relation_Lte a b)   = (show a) ++ " <= " ++ (show b)
    show (Expression_Relation_Gte a b)   = (show a) ++ " >= " ++ (show b)
    show (Expression_Relational_Not a)   = "!" ++ (show a)
    show (Expression_Bit_RShift a b)     = (show a) ++ " >> " ++ (show b)
    show (Expression_Bit_LShift a b)     = (show a) ++ " << " ++ (show b)
    show (Expression_Plus a b)           = (show a) ++ " + " ++ (show b)
    show (Expression_Minus a b)          = (show a) ++ " - " ++ (show b)
    show (Expression_Mult a b)           = (show a) ++ " * " ++ (show b)
    show (Expression_Div a b)            = (show a) ++ " / " ++ (show b)
    show (Expression_Mod a b)            = (show a) ++ " % " ++ (show b)
    show (Expression_Cast t a)           = "("  ++ (show t) ++ ")" ++ (show a)
    show (Expression_Pre_Increment a)    = "++" ++ (show a)
    show (Expression_Pre_Decrement a)    = "--" ++ (show a)
    show (Expression_SizeOf_Expression a)= "sizeof " ++ (show a)
    show (Expression_SizeOf_Type t)      = "sizeof (" ++ (show t) ++ ")"
    show (Expression_Address_Of a)       = "&" ++ (show a)
    show (Expression_Array a b)          = (show a) ++ "[" ++  (show b) ++ "]"
    show (Expression_Function a b)       = (show a) ++ "(" ++ (concat  .  intersperse ", "  . map (show) $ b) ++ ")"
    show (Expression_Dereference a)      = "*" ++ (show a)
    show (Expression_Struct_Dot a b)     = (show a) ++ "." ++ b
    show (Expression_Struct_Arrow a b)   = (show a) ++ "->" ++ b
    show (Expression_Post_Increment a)   = (show a) ++ "++"
    show (Expression_Post_Decrement a)   = (show a) ++ "--"
    show (Expression_Assignment a op b)  = (show a) ++ " " ++ (show op) ++ " " ++ (show b)

data Constant = Integer_Constant Integer
              | Character_Constant Char
              | Floating_Constant Double 
              | Enumaration_Constant String

constant = (integer >>= \i -> return $ Integer_Constant i)
       <|> (reservedOp "'" >> anyChar >>= \c -> reservedOp "'" >> (return $ Character_Constant c))
       <|> (float >>= \f -> return $ Floating_Constant f)
       <|> (identifier >>= \i -> return $ Enumaration_Constant i)


instance Show Constant where
    show (Integer_Constant i)   = show i
    show (Character_Constant c) = show c
    show (Floating_Constant f)  = show f
    show (Enumaration_Constant s) = s

data Assignment_Operator = Assignment
                         | Mult_Assignment
                         | Div_Assignment
                         | Mod_Assignment
                         | Add_Assignment
                         | Sub_Assignment
                         | LShift_Assignment
                         | RShift_Assignment
                         | Bin_And_Assignment
                         | Bin_Xor_Assignment
                         | Bin_Or_Assignment

assignment_operator = (reservedOp "=" >> return Assignment)
                  <|> (reservedOp "*=" >> return Mult_Assignment)
                  <|> (reservedOp "/=" >> return Div_Assignment)
                  <|> (reservedOp "%=" >> return Mod_Assignment)
                  <|> (reservedOp "+=" >> return Add_Assignment)
                  <|> (reservedOp "-=" >> return Sub_Assignment)
                  <|> (reservedOp "<<=" >> return LShift_Assignment)
                  <|> (reservedOp ">>=" >> return RShift_Assignment)
                  <|> (reservedOp "&=" >> return Bin_And_Assignment)
                  <|> (reservedOp "^=" >> return Bin_Xor_Assignment)
                  <|> (reservedOp "|=" >> return Bin_Or_Assignment)

instance Show Assignment_Operator where
    show (Assignment)        = "="
    show (Mult_Assignment)   = "*="
    show (Div_Assignment)    = "/="
    show (Mod_Assignment)    = "%="
    show (Add_Assignment)    = "+="
    show (Sub_Assignment)    = "-="
    show (LShift_Assignment) = "<<="
    show (RShift_Assignment) = ">>="
    show (Bin_And_Assignment)= "&="
    show (Bin_Xor_Assignment)= "^="
    show (Bin_Or_Assignment) = "|="

data Type_Name = Type_Abstract [Type_Descriptor] Abstract_Declarator
               | Type_Concrete [Type_Descriptor]

type_name = (sepBy1 type_descriptor (whiteSpace) >>= \t -> abstract_declarator >>= \a -> return $ Type_Abstract t a)
        <|> (sepBy1 type_descriptor (whiteSpace) >>= \t -> return $ Type_Concrete t)

instance Show Type_Name where
    show (Type_Concrete a) =  (concat . intersperse " " . map show $ a)
    show (Type_Abstract a b) = (concat . intersperse " " . map show $ a) ++ " " ++ (show $ b)


data Abstract_Declarator = Abstract_Declaration_Pointer_Only Pointer
                         | Abstract_Declaration_Pointer Pointer Direct_Abstract_Declarator
                         | Abstract_Declaration_Declarator Direct_Abstract_Declarator


abstract_declarator = (pointer >>= \p -> direct_abstract_declarator >>= \a -> return $ Abstract_Declaration_Pointer p a)
                  <|> (direct_abstract_declarator >>= \a -> return $ Abstract_Declaration_Declarator a)
                  <|> (pointer >>= \p -> return $ Abstract_Declaration_Pointer_Only p)


instance Show Abstract_Declarator where
    show (Abstract_Declaration_Pointer_Only p) = show p
    show (Abstract_Declaration_Pointer p a) = (show p) ++ " " ++ (show a)
    show (Abstract_Declaration_Declarator a) = (show a)


data Direct_Abstract_Declarator = Abstract_Declarator_Parens                               Abstract_Declarator
                                | Abstract_Declarator_Arr                                  Direct_Abstract_Declarator Expression
                                | Abstract_Declarator_Arr_Empty                            Direct_Abstract_Declarator
                                | Abstract_Declarator_Arr_Partial                          Expression
                                | Abstract_Declarator_Arr_Nill
                                | Abstract_Declarator_Function                             Direct_Abstract_Declarator [Parameter_Declaration]
                                | Abstract_Declarator_Function_Empty                       [Parameter_Declaration]

direct_abstract_declarator = (parens direct_abstract_declarator >>= \p -> brackets expression >>= \e -> return $ Abstract_Declarator_Arr p e)
                         <|> (parens direct_abstract_declarator >>= \p -> reservedOp "[" >> reservedOp "]" >> (return $ Abstract_Declarator_Arr_Empty p))
                         <|> (brackets expression >>= \e -> return $ Abstract_Declarator_Arr_Partial e)
                         <|> (reservedOp "[" >> reservedOp "]" >> (return $ Abstract_Declarator_Arr_Nill))
                         <|> (parens direct_abstract_declarator >>= \p -> parens (commaSep parameter_declaration) >>= \e -> return $ Abstract_Declarator_Function p e)
                         <|> (parens (commaSep parameter_declaration) >>= \e -> return $ Abstract_Declarator_Function_Empty e)
                         <|> (parens abstract_declarator >>= \a -> return $ Abstract_Declarator_Parens a)


instance Show Direct_Abstract_Declarator where
  show (Abstract_Declarator_Parens p)         = show p
  show (Abstract_Declarator_Arr a e)          = (show a) ++ "[" ++ (show e) ++ "]"
  show (Abstract_Declarator_Arr_Empty a)      = (show a) ++ "[]"
  show (Abstract_Declarator_Arr_Partial e)    = "[" ++ (show e) ++ "]"
  show (Abstract_Declarator_Arr_Nill)         = "[]"
  show (Abstract_Declarator_Function a e)     = (show a) ++ "(" ++ (concat . intersperse ", " . map show $ e) ++ ")"
  show (Abstract_Declarator_Function_Empty e) = "(" ++ (concat . intersperse ", " . map show $ e) ++ ")"




data Parameter_Declaration = Parameter_Declaration [Declaration_Specifier] Declarator
                           | Parameter_Abstract_Declaration [Declaration_Specifier] Abstract_Declarator
                           | Parameter_Prototype_Declaration [Declaration_Specifier]
                           | Parameter_Declaration_Ellipses

parameter_declaration = ((sepBy declaration_specifier whiteSpace) >>= \i -> declarator >>= \d -> return $ Parameter_Declaration i d)
                    <|> ((sepBy declaration_specifier whiteSpace) >>= \i -> abstract_declarator >>= \d -> return $ Parameter_Abstract_Declaration i d)
                    <|> ((sepBy declaration_specifier whiteSpace) >>= \i -> return $ Parameter_Prototype_Declaration i)
                    <|> (reserved "..." >>( return $ Parameter_Declaration_Ellipses))

instance Show Parameter_Declaration where
    show (Parameter_Declaration sp decl) = (concat . intersperse " " . map show $ sp ) ++ show decl
    show (Parameter_Abstract_Declaration a decl) = (concat . intersperse " " . map show $ a) ++ show decl
    show (Parameter_Prototype_Declaration a) = (concat . intersperse " " . map show $ a)
    show (Parameter_Declaration_Ellipses) = "..."



data Enumerator = Unspecified_Enum_Identifier Identifier
                | Specified_Enum_Identifier Identifier Expression

enumerator = (identifier >>= \i -> reservedOp "=" >> expression >>= \x -> return $ Specified_Enum_Identifier i x)
         <|> (identifier >>= \i -> return $ Unspecified_Enum_Identifier i)

instance Show Enumerator where
    show (Unspecified_Enum_Identifier i) = i
    show (Specified_Enum_Identifier i e) = i ++ " = " ++ (show e)

data Declaration = Declaration [Declaration_Specifier] [Declarator_Initialize]

declaration = ((sepBy declaration_specifier whiteSpace) >>= \d -> sepBy declarator_initialize whiteSpace >>= \e -> return $ Declaration d e)

instance Show Declaration where
    show (Declaration spec init) = (concat . intersperse " " . map show $ spec) ++ " " ++ (concat . intersperse " " . map show $ init)

data Declarator_Initialize = Uninitialized_Declarator Declarator
                           | Initialized_Declarator Declarator Initializer

declarator_initialize = (declarator >>= \d -> reservedOp "=" >> initializer >>= \n -> return $ Initialized_Declarator d n)
                    <|> (declarator >>= \d -> return $ Uninitialized_Declarator d)

instance Show Declarator_Initialize where
    show (Uninitialized_Declarator d) = show d
    show (Initialized_Declarator d i) = (show d) ++ " = " ++ (show i)

data Initializer = Initializer_Expression Expression
                 | Initializer_List [Initializer]

initializer = (expression >>= \e -> return $ Initializer_Expression e)
          <|> (braces (commaSep initializer) >>= \x -> return $ Initializer_List x)

instance Show Initializer where
    show (Initializer_Expression e) = show e
    show (Initializer_List xs) = "{" ++ (concat . intersperse ", " . map show $ xs) ++ "}"

data Compound_Statement = Compound_Statement [Declaration] [Statement]

compound_statement = (braces (sepBy ((declaration >>= \d -> return $ Left d) <|> (statement >>= \s -> return $ Right s)) (whiteSpace <|> reservedOp ";")) >>= \ls -> return $ (let (ds,ss) = foldr (\x (a,b) -> case x of
                                                                                                                                                                                                            (Left y) -> (y:a, b)
                                                                                                                                                                                                            (Right y)-> (a, y:b)) ([],[]) ls in Compound_Statement ds ss))

instance Show Compound_Statement where
    show (Compound_Statement d s) = "{" ++ (concat . intersperse "; " . map show $ d) ++ "; " ++ (concat . intersperse " " . map show $ s) ++ "}"

data Statement = Statement_Labeled     Labeled_Statement
               | Statement_Expression  Expression_Statement
               | Statement_Compound    Compound_Statement
               | Statement_Selection   Selection_Statement
               | Statement_Iteration   Iteration_Statement
               | Statement_Jump        Jump_Statement

statement = (labeled_statement >>= \l -> return $ Statement_Labeled l)
        <|> (expression_statement >>= \l -> return $ Statement_Expression l)
        <|> (compound_statement >>= \l -> return $ Statement_Compound l)
        <|> (selection_statement >>= \l -> return $ Statement_Selection l)
        <|> (iteration_statement >>= \l -> return $ Statement_Iteration l)
        <|> (jump_statement >>= \l -> return $ Statement_Jump l)

instance Show Statement where
    show (Statement_Labeled s) = show s
    show (Statement_Expression s) = show s
    show (Statement_Compound s) = show s
    show (Statement_Selection s) = show s
    show (Statement_Iteration s) = show s
    show (Statement_Jump s) = show s

data Labeled_Statement = Label Identifier Statement
                       | Case Expression Statement
                       | Default Statement

labeled_statement = (reserved "default:" >> statement >>= \s -> return $ Default s)
                <|> (reserved "case" >> expression >>= \e -> reservedOp ":" >> statement >>= \s -> return $ Case e s)
                <|> (identifier >>= \i -> reservedOp ":" >> statement >>= \s -> return $ Label i s)

instance Show Labeled_Statement where
    show (Label i s) = i ++ ": " ++ (show s)
    show (Case e s)  = "case " ++ (show e) ++ ": " ++ (show s)
    show (Default s) = "default: " ++ (show s)

data Expression_Statement = Expression_Statement Expression

expression_statement = (expression >>= \e -> semi >> (return $ Expression_Statement e))

instance Show Expression_Statement where
    show (Expression_Statement e) = (show e) ++ ";"

data Selection_Statement = If_Statement Expression Statement
                         | If_Else_Statement Expression Statement Statement
                         | Switch_Statement Expression Statement

selection_statement = (reserved "if" >> parens expression >>= \e -> statement >>= \s1 -> reserved "else" >> statement >>= \s2 -> return $ If_Else_Statement e s1 s2)
                  <|> (reserved "if" >> parens expression >>= \e -> statement >>= \s1 -> return $ If_Statement e s1)
                  <|> (reserved "switch" >> parens expression >>= \e -> statement >>= \s1 -> return $ Switch_Statement e s1)

instance Show Selection_Statement where
    show (If_Statement e st) = "if(" ++ (show e) ++ ")" ++ (show st)
    show (If_Else_Statement e st1 st2) = "if(" ++ (show e) ++ ")" ++ (show st1) ++ "else " ++ (show st2)
    show (Switch_Statement e st1) = "switch(" ++ (show e) ++ ")" ++ (show st1)

data Iteration_Statement = While_Statement Expression Statement
                         | Do_While_Statement Expression Statement
                         | For_Statement (Maybe Expression) (Maybe Expression) (Maybe Expression) Statement

iteration_statement = (reserved "while" >> parens expression >>= \e -> statement >>= \s -> return $ While_Statement e s)
                  <|> (reserved "do" >> statement >>= \s -> reserved "while" >> parens expression >>= \e -> semi >> (return $ Do_While_Statement e s))
-- (0,0,0)
                  <|> (reserved "for" >> parens (reserved ";" >> reserved ";") >> statement >>= \s -> (return $ For_Statement Nothing Nothing Nothing s))
-- (1,0,0)
                  <|> (reserved "for" >> parens (expression >>= \e1 -> reserved ";" >>                       reserved ";" >>                       return e1)         >>= \e1         -> statement >>= \s -> (return $ For_Statement (Just e1)  Nothing   Nothing s))
-- (1,1,0)
                  <|> (reserved "for" >> parens (expression >>= \e1 -> reserved ";" >> expression >>= \e2 -> reserved ";" >>                       return (e1,e2))    >>= \(e1, e2)   -> statement >>= \s -> (return $ For_Statement (Just e1) (Just e2)  Nothing  s))
-- (1,0,1)
                  <|> (reserved "for" >> parens (expression >>= \e1 -> reserved ";" >>                       reserved ";" >> expression >>= \e3 -> return (e1,e3))    >>= \(e1,e3)    -> statement >>= \s -> (return $ For_Statement (Just e1)  Nothing  (Just e3) s))
-- (0,1,0)
                  <|> (reserved "for" >> parens (                      reserved ";" >> expression >>= \e2 -> reserved ";" >>                       return e2)         >>= \e2         -> statement >>= \s -> (return $ For_Statement  Nothing  (Just e2)  Nothing  s))
-- (0,1,1)
                  <|> (reserved "for" >> parens (                      reserved ";" >> expression >>= \e2 -> reserved ";" >> expression >>= \e3 -> return (e2,e3))    >>= \(e2, e3)   -> statement >>= \s -> (return $ For_Statement  Nothing  (Just e2) (Just e3) s))
-- (0,0,1)
                  <|> (reserved "for" >> parens (                      reserved ";" >>                       reserved ";" >> expression >>= \e3 -> return e3)         >>= \e3         -> statement >>= \s -> (return $ For_Statement  Nothing   Nothing  (Just e3) s))
-- (1,1,1)
                  <|> (reserved "for" >> parens (expression >>= \e1 -> reserved ";" >> expression >>= \e2 -> reserved ";" >> expression >>= \e3 -> return (e1,e2,e3)) >>= \(e1,e2,e3) -> statement >>= \s -> (return $ For_Statement (Just e1) (Just e2) (Just e3) s))


instance Show Iteration_Statement where
    show (While_Statement e st) = "while " ++ (show e) ++ " " ++ (show st)
    show (Do_While_Statement e st) = "do " ++ (show st) ++ " while " ++ (show e) ++ ";"
    show (For_Statement  Nothing   Nothing   Nothing  st) = "for(;;)" ++ (show st)
    show (For_Statement (Just e1)  Nothing   Nothing  st) = "for(" ++ (show e1) ++ ";;)" ++ (show st)
    show (For_Statement (Just e1) (Just e2)  Nothing  st) = "for(" ++ (show e1) ++ ";" ++ (show e2) ++";)" ++ (show st)
    show (For_Statement (Just e1)  Nothing  (Just e3) st) = "for(" ++ (show e1) ++ ";;" ++ (show e3) ++")" ++ (show st)
    show (For_Statement  Nothing  (Just e2)  Nothing  st) = "for(;"++ (show e2) ++";)" ++ (show st)
    show (For_Statement  Nothing  (Just e2) (Just e3) st) = "for(;"++ (show e2) ++";"++ (show e3) ++")" ++ (show st)
    show (For_Statement  Nothing   Nothing  (Just e3) st) = "for(;;"++ (show e3) ++")" ++ (show st)
    show (For_Statement (Just e1) (Just e2) (Just e3) st) = "for(" ++ (show e1) ++ ";" ++ (show e2) ++ ";" ++ (show e3) ++ ")" ++ (show st)


data Jump_Statement = Goto_Statement Identifier
                    | Continue_Statement
                    | Break_Statement
                    | Return_Statement Expression

jump_statement = (reserved "goto" >> identifier >>= \i -> semi >> (return $ Goto_Statement i))
             <|> (reserved "continue" >> semi >> (return $ Continue_Statement))
             <|> (reserved "break" >> semi >> (return $ Break_Statement))
             <|> (reserved "return" >> expression >>= \i -> semi >> (return $ Return_Statement i))

instance Show Jump_Statement where
    show (Goto_Statement i) = "goto " ++ i ++ ";"
    show (Continue_Statement) = "continue;"
    show (Break_Statement) = "break;"
    show (Return_Statement e) = "return " ++ (show e) ++ ";"

type Identifier = String
type Typedef_Name = String


languageDef = 
    emptyDef {
        Token.commentStart          = "/*",
        Token.commentEnd            = "*/",
        Token.commentLine           = "//",
        Token.identStart            = letter,
        Token.identLetter           = alphaNum,
        Token.reservedNames         = [
                                         "auto",
                                         "register",
                                         "static",
                                         "extern",
                                         "typedef",
                                         "void",
                                         "short",
                                         "char",
                                         "int",
                                         "long",
                                         "float",
                                         "double",
                                         "signed",
                                         "unsigned",
                                         "struct",
                                         "union",
                                         "enum",
                                         "const",
                                         "volatile",
                                         "case",
                                         "default",
                                         "switch",
                                         "break",
                                         "continue",
                                         "if",
                                         "else",
                                         "while",
                                         "do",
                                         "for",
                                         "goto",
                                         "return"
                                      ],
        Token.reservedOpNames       = [ "+",
                                        "-",
                                        "*",
                                        "/",
                                        "=",
                                        ">",
                                        "<",
                                        ">=",
                                        "<=",
                                        ":",
                                        "?",
                                        "||",
                                        "&&",
                                        "|",
                                        "^",
                                        "&",
                                        "==",
                                        "!=",
                                        ">>",
                                        "<<",
                                        "%",
                                        "++",
                                        "--",
                                        "sizeof",
                                        ".",
                                        "->",
                                        "*=",
                                        "/=",
                                        "%=",
                                        "+=",
                                        "-=",
                                        "<<=",
                                        ">>=",
                                        "&=",
                                        "^=",
                                        "|=",
                                        "!",
                                        "~",
                                        "\"",
                                        "[",
                                        "(",
                                        "'"

                                ]
    }

lexer = Token.makeTokenParser languageDef

identifier   = Token.identifier   lexer
reserved     = Token.reserved     lexer
reservedOp   = Token.reservedOp   lexer
parens       = Token.parens       lexer
integer      = Token.integer      lexer
whiteSpace   = Token.whiteSpace   lexer
float        = Token.float        lexer
braces       = Token.braces       lexer
brackets     = Token.brackets     lexer
semi         = Token.semi         lexer
semiSep      = Token.semiSep      lexer
semiSep1     = Token.semiSep1     lexer
colon        = Token.colon        lexer
dot          = Token.dot          lexer
commaSep     = Token.commaSep     lexer
commaSep1    = Token.commaSep1    lexer

operators = [
                [
                    Postfix (reservedOp "++" >> return Expression_Post_Increment ) , 
                    Postfix (reservedOp "--" >> return Expression_Post_Decrement ) ,
                    Postfix (reservedOp "->" >> identifier >>= \ident -> return $ flip Expression_Struct_Arrow ident) ,
                    Postfix (reservedOp "."  >> identifier >>= \ident -> return $ flip Expression_Struct_Dot ident),
                    Postfix (lookAhead (reservedOp "[") >> brackets expression >>= \expr -> return $ flip Expression_Array expr),

                    Postfix (lookAhead (reservedOp "(") >> parens (commaSep expression) >>= \expr -> return $ flip Expression_Function expr),
                    Postfix (reservedOp "?" >> expression >>= \expr1 -> reservedOp ":" >> expression >>= \expr2 -> return $ (\x -> Expression_Ternary x expr1 expr2)) 
                ],
                [
                    Prefix (reservedOp "++" >> return Expression_Pre_Increment ) ,
                    Prefix (reservedOp "--" >> return Expression_Pre_Decrement ) ,
                    Prefix (reservedOp "!"  >> return Expression_Relational_Not) ,
                    Prefix (reservedOp "~"  >> return Expression_Bin_Neg       ) ,
                    Prefix (reservedOp "*"  >> return Expression_Dereference   ) ,
                    Prefix (reservedOp "&"  >> return Expression_Address_Of    ) 
                ],
                [
                    Infix (reservedOp "*" >> return Expression_Mult            ) AssocLeft,
                    Infix (reservedOp "/" >> return Expression_Div             ) AssocLeft,
                    Infix (reservedOp "%" >> return Expression_Mod             ) AssocLeft
                ],
                [
                    Infix (reservedOp "+" >> return Expression_Plus            ) AssocLeft,
                    Infix (reservedOp "-" >> return Expression_Minus           ) AssocLeft
                ],
                [
                    Infix (reservedOp "<<" >> return Expression_Bit_LShift     ) AssocLeft,
                    Infix (reservedOp ">>" >> return Expression_Bit_RShift     ) AssocLeft
                ], 
                [
                    Infix (reservedOp "<=" >> return Expression_Relation_Lte   ) AssocLeft,
                    Infix (reservedOp "<"  >> return Expression_Relation_Lt    ) AssocLeft
                ],
                [
                    Infix (reservedOp ">=" >> return Expression_Relation_Gte   ) AssocLeft,
                    Infix (reservedOp ">"  >> return Expression_Relation_Gt    ) AssocLeft
                ],
                [
                    Infix (reservedOp "==" >> return Expression_Relation_Eq    ) AssocLeft,
                    Infix (reservedOp "!=" >> return Expression_Relation_Neq   ) AssocLeft
                ],
                [Infix (reservedOp "&" >> return Expression_Bit_And         ) AssocLeft],
                [Infix (reservedOp "^" >> return Expression_Xor             ) AssocLeft],
                [Infix (reservedOp "|" >> return Expression_Bit_Or          ) AssocLeft],
                [Infix (reservedOp "&&" >> return Expression_And            ) AssocLeft],
                [Infix (reservedOp "||" >> return Expression_Or             ) AssocLeft]
            ]
           
 
expression = buildExpressionParser operators terms
   



terms = (parens expression >>= \expr -> return $ Expression_Parens expr) 
    <|> (identifier >>= \expr -> return $ Expression_Identifier expr) 
    <|> (constant >>= \expr -> return $ Expression_Constant expr)
    <|> (c_string   >>= \expr -> return $ Expression_String expr)
    <|> (commaSep1 expression >>= \expr -> return $ foldr1 Expression_Comma expr)
    <|> (parens c_type >>= \ty -> expression >>= \expr -> return $ Expression_Cast ty expr)
    <|> (reservedOp "sizeof" >> expression >>= \expr -> return $ Expression_SizeOf_Expression expr)
    <|> (reservedOp "sizeof" >> parens c_type >>= \ty -> return $ Expression_SizeOf_Type ty)

escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf"
    return [d,c]

nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character = fmap return nonEscape <|> escape



c_type = type_name
c_string = do
    char '"'
    strings <- many character
    char '"'
    return $ concat strings

parseString :: String -> Program
parseString str = case parse program "" str of
        Left e  -> error $ show e
        Right r -> r


