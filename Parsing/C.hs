type Program = [Extern_Declaration]

data Extern_Declaration = Ext_Function Function_Definition
                        | Ext_Declaration Declaration

data Function_Definition = Function [Declaration_Specifier] Declarator [Declaration] Compound_Statement

data Declaration_Specifier = Declaration_Storage Storage_Class_Specifier
                           | Declaration_Type_Specifier Type_Specifier
                           | Declaration_Type_Qualifier Type_Qualifier

data Storage_Class_Specifier = Auto
                             | Register
                             | Static
                             | Extern
                             | Typedef

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

data Struct_Specifier = Full_Struct Identifier [Struct_Declaration]
                      | Anonymous_Struct [Struct_Declaration]
                      | Partial_Struct Identifier

data Union_Specifier = Full_Union Identifier [Struct_Declaration]
                      | Anonymous_Union [Struct_Declaration]
                      | Partial_Union Identifier

data Enum_Specifier = Full_Enum Identifier [Enumerator]
                    | Anonymous_Enum [Enumerator]
                    | Partial_Enum Identifier

data Type_Descriptor = Type_Descriptor_Specifier Type_Specifier
                     | Type_Descriptor_Qualifier Type_Qualifier

data Struct_Declaration = Struct_Field [Type_Descriptor] [Struct_Declarator]

data Struct_Declarator = Struct_Full_Field Declarator
                       | Struct_Bit_Field Declarator Expression
                       | Struct_Empty_Field Expression

data Declarator = Declarator [Pointer] [Direct_Declarator]

data Pointer = Pointer Type_Qualifier

data Type_Qualifier = Type_Const | Type_Volatile

data Direct_Declarator = Declarator_Identifier Identifier
                       | Declarator_Parens Declarator
                       | Declarator_Arr  Direct_Declarator Expression
                       | Declarator_Func Direct_Declarator [Parameter_Declaration]

data Expression = Expression_Identifier         Identifier
                | Expression_Constant           Constant
                | Expression_String             String
                | Expression_Parens             Expression
                | Expression_Comma              Expression Expression
                | Expression_Ternary            Expression Expression Expression
                | Expression_Or                 Expression Expression
                | Expression_And                Expression Expression
                | Expression_Bit_Or             Expression Expression
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
                | Expression_Array              Expression Expression
                | Expression_Function           Expression [Expression]
                | Expression_Struct_Dot         Expression Identifier
                | Expression_Struct_Arrow       Expression Identifier
                | Expression_Post_Increment     Expression
                | Expression_Post_Decrement     Expression
                | Expression_Assignment         Expression Assignment_Operator Expression

data Constant = Integer_Constant Int
              | Character_Constant Char
              | Floating_Constant Float
              | Enumaration_Constant String

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

data Type_Name = Type [Type_Descriptor] [Abstract_Declarator]

data Abstract_Declarator = Abstract_Declaration_Pointer_Only Pointer
                         | Abstract_Declaration_Pointer Pointer Abstract_Declarator
                         | Abstract_Declaration_Arr [Abstract_Declarator] Expression
                         | Abstract_Declaration_Func [Abstract_Declarator] [Parameter_Declaration]

data Parameter_Declaration = Parameter_Declaration [Declaration_Specifier] Declarator
                           | Parameter_Abstract_Declaration [Declaration_Specifier] Abstract_Declarator
                           | Parameter_Prototype_Declaration [Declaration_Specifier]
                           | Parameter_Declaration_Ellipses


data Enumerator = Unspecified_Enum_Identifier Identifier
                | Specified_Enum_Identifier Identifier Expression

data Declaration = Declaration [Declaration_Specifier] [Declarator_Initialize]

data Declarator_Initialize = Uninitialized_Declarator Declarator
                           | Initialized_Declarator Declarator Initializer

data Initializer = Initializer_Expression Expression
                 | Initializer_List [Initializer]

data Compound_Statement = Compound_Statement [Declaration] [Statement]

data Statement = Statement_Labeled     Labeled_Statement
               | Statement_Expression  Expression_Statement
               | Statement_Compound    Compound_Statement
               | Statement_Selection   Selection_Statement
               | Statement_Iteration   Iteration_Statement
               | Statement_Jump        Jump_Statement

data Labeled_Statement = Label Identifier Statement
                       | Case Expression Statement
                       | Default Statement

data Expression_Statement = Expression_Statement Expression

data Selection_Statement = If_Statement Expression Statement
                         | If_Else_Statement Expression Statement Statement
                         | Switch_Statement Expression Statement

data Iteration_Statement = While_Statement Expression Statement
                         | Do_While_Statement Expression Statement
                         | For_Statement Expression Expression Expression Statement

data Jump_Statement = Goto_Statement Identifier
                    | Continue_Statement
                    | Break_Statement
                    | Return_Statement Expression

type Identifier = String
type Typedef_Name = String
