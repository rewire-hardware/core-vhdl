module VHDL.CoreVHDL where

type Identifier = String
type Label = String
data Literal = CharLiteral Char | StringLiteral String | IntegerLiteral Integer deriving (Show) -- Or NUm of some kind

--type SelectedName = String
--
--
----Top level declarations for parsing:
--
data TopDecl = TEntity Entity 
               | TArch Architecture
               | TPkg Package
               | TPkgBody PackageBody
--               | TCDecl ConfigurationSpecification
               | TCtxDecl ContextDeclaration
               | TCtx Context deriving (Show)

data Context = CtxUC UseClause 
               | CtxLC LibraryClause
               | CtxCC ContextClause deriving (Show)

--These three must have be at least singleton lists
data UseClause = UseClause [SelectedName] deriving (Show)
data LibraryClause = LibraryClause [Identifier] deriving (Show)
data ContextClause = ContextClause [SelectedName] deriving (Show)

data Entity = Entity Identifier (Maybe GenericList) 
                                (Maybe PortList) 
                                Declarations 
                                (Maybe ConcurrentStatements) deriving (Show)

type GenericList = [ConstantInterfaceDeclaration] --Singleton minimum
type PortList    = [SignalInterfaceDeclaration] --Singleton minimum

type Architecture = ((Identifier,Identifier),Declarations,ConcurrentStatements)

type Package = (Identifier,Declarations)
type PackageBody = (Identifier, Declarations)
type ContextDeclaration = (Identifier,Context)

type ConcurrentStatements = [ConcurrentStatement]

data ConcurrentStatement = CSBlock BlockStatement
                           | CSProc ProcessStatement
                           | CSCPC  ConcurrentProcedureCall
                           | CSCA   ConcurrentAssertion
                           | CSSA   ConcurrentSignalAssignment
                           | CSCI   ComponentInstance
                           | CSGS   GenerateStatement deriving (Show)

type BlockStatement = (Label, Declarations, ConcurrentStatements)

type ProcessStatement = (Maybe Label,Maybe SensitivityList,Declarations,SequentialStatements)

type SensitivityList = [Name] --Signal Names
type ConcurrentProcedureCall = (Maybe Label,ProcedureCall)
type ConcurrentAssertion = (Maybe Label,Assertion)

data ConcurrentSignalAssignment = CSAC (Maybe Label) ConditionalSignalAssignment
                                  | CSAS (Maybe Label) SelectedSignalAssignment deriving (Show)

type ConditionalSignalAssignment = (Target,[(Expression,Expression)],Expression)

type SelectedSignalAssignment = (Expression, Target, [(Expression,Choices)])--, (Expression,Choices))

type ComponentInstance = ((Label,Name), Maybe AssociationList, Maybe AssociationList)

data GenerateStatement = GSFor ForGenerate | GSIf IfGenerate deriving (Show)

type ForGenerate = (Label, Identifier, DiscreteRange, ConcurrentStatements)

type IfGenerate  = (Label, Expression, ConcurrentStatements)

type SequentialStatements = [SequentialStatement]

data SequentialStatement = SSWait WaitStatement
                           | SSA  Assertion
                           | SSSA SignalAssignment
                           | SSVA VariableAssignment
                           | SSPC ProcedureCall
                           | SSIS IfStatement
                           | SSCS CaseStatement
                           | SSFL ForLoop
                           | SSNS NextStatement
                           | SSES ExitStatement
                           | SSRS ReturnStatement
                           | SSNull NullStatement deriving (Show)

type WaitStatement = (Maybe SensitivityList, Maybe Expression)

type Assertion = (Expression, Maybe Expression, Maybe Expression)

type SignalAssignment = (Target, Expression)
type VariableAssignment = (Target, Expression)
type ProcedureCall = (Identifier, Maybe AssociationList)
type IfStatement = ((Expression,SequentialStatements),[(Expression,SequentialStatements)], Maybe SequentialStatements)

type CaseStatement = (Expression, [(Choices,SequentialStatements)])

type ForLoop = (Maybe Label, Identifier, DiscreteRange, SequentialStatements)

type NextStatement = (Maybe Label, Maybe Expression)
type ExitStatement = (Maybe Label, Maybe Expression)

data ReturnStatement = RSFR FunctionReturn | RSPR ProcedureReturn deriving (Show)

type FunctionReturn = Expression
type ProcedureReturn = ()

type NullStatement = ()

--Modded this one to make it more sane
data Expression = ExprR  Relation 
                  | And  Relation Expression 
                  | Or   Relation Expression
                  | Xor  Relation Expression
                  | Xnor Relation Expression
                  | Nand Relation Expression
                  | Nor  Relation Expression deriving (Show)

data Relation = RelSE ShiftExpression 
                | EQ ShiftExpression ShiftExpression 
                | NEQ ShiftExpression ShiftExpression 
                | LT ShiftExpression ShiftExpression 
                | LTE ShiftExpression ShiftExpression 
                | GT ShiftExpression ShiftExpression 
                | GTE ShiftExpression ShiftExpression deriving (Show)
--data RelationalOperator = EQ | NEQ | LT | LTE | GT | GTE
data ShiftExpression = SESimple SimpleExpression 
                       | SLL SimpleExpression SimpleExpression
                       | SRL SimpleExpression SimpleExpression
                       | SLA SimpleExpression SimpleExpression
                       | SRA SimpleExpression SimpleExpression
                       | ROL SimpleExpression SimpleExpression
                       | ROR SimpleExpression SimpleExpression deriving (Show)

--data ShiftOperator = SLL | SRL | SLA | SRA | ROL | ROR
data SimpleExpression = SimpleExp (Maybe Sign) Term
                        | SimpleAdd (Maybe Sign) Term Term
                        | SimpleSub (Maybe Sign) Term Term
                        | SimpleAnd (Maybe Sign) Term Term deriving (Show)

data Sign = Plus | Minus deriving (Show)

--data AddingOperator = APlus | AMinus | AAnd
data Term = SingleTerm Factor 
            | TMult Factor Factor
            | TDiv Factor Factor
            | TMod Factor Factor
            | TRem Factor Factor deriving (Show)

--data MultiplyingOperator = MMult | MDiv | MMod | MRem
data Factor = PrimSingle Primary | PrimExp Primary Primary | PrimAbs Primary | PrimNot Primary deriving (Show)

data Primary = PName Name | PLit Literal | PAgg Aggregate | PFun FunctionCall
               | PQual QualifiedExpression | PType TypeConversion | PExp Expression deriving (Show)

data Name = NID Identifier | NOS OperatorSymbol | NSN SelectedName | NI IndexedName
            | NSLN SliceName | NAN AttributeName deriving (Show)


type OperatorSymbol = String --String Literal
type SelectedName = (Prefix, Suffix)
data Prefix = PFN Name | PFC FunctionCall deriving (Show)
data Suffix = SFI Identifier | SFC String | SFO OperatorSymbol | SFALL deriving (Show) --SFC Char, All is ?
type IndexedName = (Prefix, [Expression])
type SliceName = (Prefix,DiscreteRange)
type AttributeName = (Prefix, Identifier, Maybe Expression)
type Aggregate = (Maybe Choices, Expression, [(Maybe Choices,Expression)])
type FunctionCall = (FunctionName, Maybe AssociationList)
data FunctionName = FNID Identifier | FNOS OperatorSymbol deriving (Show)
data QualifiedExpression = QEIE Identifier Expression | QEIA Identifier Aggregate deriving (Show)
type TypeConversion = (Identifier, Expression)
type Choices = [Choice]
data Choice = CID Identifier | CSE SimpleExpression | CDR DiscreteRange | Others deriving (Show)

type AssociationList = [AssociationElement]
type AssociationElement = (Maybe Name, ActualPart)
data ActualPart = APE Expression | APOpen  deriving (Show)
data DiscreteRange = DRS SubtypeIndication | DRR Range deriving (Show)
type SubtypeIndication = (Identifier, Maybe Constraint)
data Constraint = CSTR RangeConstraint | CSTI IndexConstraint deriving (Show)
type RangeConstraint = (Range, Range)
type IndexConstraint = [DiscreteRange]
data Range = RangeAttr AttributeName | RangeSE SimpleExpression Direction SimpleExpression deriving (Show)
data Direction = To | DownTo deriving (Show)
data Target = TName Name | TAG Aggregate deriving (Show)

type Declarations = [Declaration]
data Declaration  = DFD FunctionDeclaration
                    | DFB FunctionBody
                    | DPD ProcedureDeclaration
                    | DPB ProcedureBody
                    | DTD TypeDeclaration
                    | DSD SubtypeDeclaration
                    | DCD ConstantDeclaration
                    | DVD VariableDeclaration
                    | DSigD SignalDeclaration
                    | DComD ComponentDeclaration
                    | DConfD ConfigurationSpecification deriving (Show)

type FunctionDeclaration = (FunctionDesignator, Maybe InterfaceList, Identifier)
data FunctionDesignator = FDID Identifier | FDOS OperatorSymbol deriving (Show)
type FunctionBody = (FunctionDesignator, Maybe InterfaceList, 
                     Identifier, Declarations, SequentialStatements)
type ProcedureDeclaration = (Identifier, Maybe InterfaceList)
type ProcedureBody = (Identifier, Maybe InterfaceList, Declarations, SequentialStatements)
type InterfaceList = [InterfaceDeclaration]
data InterfaceDeclaration = IDC ConstantInterfaceDeclaration 
                            | IDV VariableInterfaceDeclaration
                            | IDS SignalInterfaceDeclaration deriving (Show)
type ConstantInterfaceDeclaration = (Bool, IdentifierList, Maybe Mode, SubtypeIndication, Maybe Expression)
type VariableInterfaceDeclaration = (Bool, IdentifierList, Maybe Mode, SubtypeIndication, Maybe Expression)
type SignalInterfaceDeclaration = (Bool, IdentifierList, Maybe Mode, SubtypeIndication, Maybe Expression)

type IdentifierList = [Identifier]
data Mode = IN | OUT | INOUT | BUFFER deriving (Show)

type TypeDeclaration = (Identifier, TypeDefinition)
data TypeDefinition = TDE EnumerationType | TDI IntegerType | TDA ArrayType | TDR RecordType deriving (Show)
type EnumerationType = [EnumerationLiteral]
data EnumerationLiteral = ELID Identifier | ELC String deriving (Show)
type IntegerType = (Range, Range)
type ArrayType = (ArrayConstraint, SubtypeIndication)
data ArrayConstraint = ARRConI Identifier Range | ARRConD DiscreteRange deriving (Show)
type RecordType = ([(IdentifierList,SubtypeIndication)])
type SubtypeDeclaration = (Identifier, SubtypeIndication)
type ConstantDeclaration = (IdentifierList, SubtypeIndication, Expression)
type VariableDeclaration = (IdentifierList, SubtypeIndication, Maybe Expression)
type SignalDeclaration = (IdentifierList, SubtypeIndication, Maybe Expression)
type ComponentDeclaration = (Identifier, Maybe GenericList, Maybe PortList)
--type ConfigurationSpecification = (Instances, Identifier, Entity, SelectedName, Maybe Identifier)
type ConfigurationSpecification = (Instances, Identifier, SelectedName, Maybe Identifier)
data Instances = InstI [Identifier] | IAll | IOthers deriving (Show)
