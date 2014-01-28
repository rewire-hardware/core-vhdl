module VHDL.AST where 

type ObjectSimpleName = SimpleName
type Ident = String

data AbstractLiteral = DL DecimalLiteral | BL BasedLiteral deriving (Show)
type AccessTypeDefinition   = SubtypeIndication 

data ActualDesignator =   ADExpr Expression
                        | SigName Name
                        | VarName Name
                        | FileName Name
                        | ADOpen deriving (Show)

data ActualPart = APD ActualDesignator
                  | APF Name ActualDesignator
                  | APT TypeMark ActualDesignator deriving (Show)

type ActualParameterPart = AssociationList

data AddingOperator  = APlus | AMinus | AAnd deriving (Show)

data Aggregate = Aggregate [ElementAssocation] deriving (Show)

data AliasDeclaration = AliasDeclaration AliasDesignator [SubtypeIndication] Name [Signature] deriving (Show)

data AliasDesignator = ADIdent Ident | ADChar Char | ADOpSym OperatorSymbol deriving (Show)

data Allocator = NewSTI SubtypeIndication | NewQual QualifiedExpression deriving (Show)

type AssociationElement = (Maybe FormalPart, ActualPart)
type AssociationList = [AssociationElement]

type Assertion = (Condition, Maybe Expression, Maybe Expression)

type AssertionStatement = (Maybe Label, Assertion)

type ArchitectureBody = (Ident, Name, ArchitectureDeclarativePart, 
                         ArchitectureStatementPart, (Maybe ArchitectureSimpleName))
type ArchitectureDeclarativePart = [BlockDeclarativeItem]

type ArchitectureSimpleName = Ident
type ArchitectureStatementPart = [ConcurrentStatement]

data ArrayTypeDefinition = ARTDU UnconstrainedArrayDef | ARTDC ConstrainedArrayDef deriving (Show)


data AttributeName = AttributeName Prefix (Maybe Signature) AttributeDesignator (Maybe Expression) deriving (Show)
type AttributeDesignator = AttributeSimpleName
type AttributeSimpleName = SimpleName
data AttributeSpecification = AttrSpec AttributeDesignator EntitySpecification Expression deriving (Show)

type AttributeDeclaration = (Identifier, TypeMark)

type Base            = Integer
data BasedLiteral    = BasedLiteral Base BasedInteger [BasedInteger] [Exponent] deriving (Show)
data BasedInteger = BasedInteger [Char] deriving (Show)

data BaseSpecifier = B | O | X deriving (Show) -- | UB | UO | UX | SB | SO | SX | D
type BaseUnitDeclaration = Ident

data BasicCharacter = BCG Char deriving (Show) -- | BCF FormatEffector


type BindingIndication = (Maybe EntityAspect,Maybe GenericMapAspect,Maybe PortMapAspect)
data BitStringLiteral = BitStringLiteral [Integer] BaseSpecifier [BitValue] deriving (Show)
type BitValue = (Char, [(Bool, Char)])

data BlockDeclarativeItem =   BDSubPD SubProgramDeclaration
                            | BDSubPB SubProgramBody 
                          --  | BDPkgD  PackageDeclaration
                          --  | BDPkgB  PackageBody
                          --  | BDPkgID PackageInstantiationDeclaration
                            | BDTD    TypeDeclaration
                            | BDSTD   SubTypeDeclaration
                            | BDCstD  ConstantDeclaration
                            | BDSD    SignalDeclaration
                            | BDSVD   VariableDeclaration
                            | BDFD    FileDeclaration
                            | BDAD    AliasDeclaration
                            | BDCmpD  ComponentDeclaration
                            | BDAttrD AttributeDeclaration
                            | BDAttrS AttributeSpecification
                            | BDConfS ConfigurationSpecification
                            | BDDiscS DisconnectionSpecification
                            | BDUseC  UseClause
                            | BDGTD   GroupTemplateDeclaration
                            | BDGD    GroupDeclaration deriving (Show)
                           -- | BDPSLP  PSLPropertyDeclaration
                           -- | BDSQD   PSLSequenceDeclaration
                           -- | BDClkD  PSLClockDeclaration

type BlockDeclarativePart = [BlockDeclarativeItem]
type BlockHeader = (Maybe GenericClause, Maybe GenericMapAspect, Maybe PortClause, Maybe PortMapAspect)
data BlockSpecification = BSA ArchitectureSimpleName 
                          | BSBS Label 
                          | BSGS Label (Maybe IndexSpecification) deriving (Show)

type BlockConfiguration = (BlockSpecification, [UseClause], [ConfigurationItem])

type GuardExpression = Expression
type BlockLabel     = Label
type BlockStatement = (BlockLabel, (Maybe GuardExpression), BlockHeader, BlockDeclarativePart, BlockStatementPart, BlockLabel)

type BlockStatementPart = ConcurrentStatement

type CaseLabel = Label
type CaseStatement = (Maybe CaseLabel, Expression, CaseStatementAlternative, [CaseStatementAlternative], Maybe CaseLabel)

type CaseStatementAlternative = (Choices, SequenceOfStatements)

type CharacterLiteral = GraphicCharacter

data Choice = CSE SimpleExpression | CSDR DiscreteRange | CSESN SimpleName | Others deriving (Show)

--Cannot be []
type Choices = [Choice]

type ComponentConfiguration = (ComponentSpecification, Maybe BindingIndication, Maybe BlockConfiguration)

type ComponentDeclaration = (Ident , Maybe GenericClause, Maybe PortClause, Maybe SimpleName)

type ComponentInstantiationStmt = (Label, InstantiatedUnit, Maybe GenericMapAspect, Maybe PortMapAspect)

type ComponentSpecification = (InstantiationList, Name)

data CompositeTypeDefinition = CTDA ArrayTypeDefinition | CTDR RecordTypeDefinition deriving (Show)

type ConcurrentAssertionStmt = (Label, Assertion)
type ConcurrentProcedureCallStatement = (Label, ProcedureCall)

data ConcurrentSignalAssignmentStmt = CSAS Label ConditionalSignalAssignment | SSA Label SelectedSignalAssignment deriving (Show)

data ConcurrentStatement = CBlock BlockStatement | CProc ProcessStatement 
                           | CProcC ConcurrentProcedureCallStatement | CAssrtS ConcurrentAssertionStmt
                           | CSig ConcurrentSignalAssignmentStmt | CCmpI ComponentInstantiationStmt deriving (Show)
                           | CGen GenerateStatement -- | CPSL PSLDirective --PSL_PSL_Directive

type Condition = Expression
data ConditionClause = Until Condition deriving (Show)

type ConditionalSignalAssignment = (Target, Options, ConditionalWaveforms)

type ConditionalWaveforms = ([(Waveform,Condition)],(Waveform, Maybe Condition))

type ConfigurationDecl = (Ident, Name, ConfigurationDeclarativePart, BlockConfiguration, Maybe SimpleName)

data ConfigurationDeclarativeItem = CDIU UseClause | CDIA AttributeSpecification | CDGD GroupDeclaration deriving (Show)

type ConfigurationDeclarativePart = [ConfigurationDeclarativeItem]

data ConfigurationItem = CIB BlockConfiguration | CICC ComponentConfiguration deriving (Show)

type ConfigurationSpecification = (ComponentSpecification, BindingIndication)

type ConfigurationDeclaration = (Identifier, Name, ConfigurationDeclarativePart, BlockConfiguration, Bool, SimpleName)

type ConstantDeclaration = (IdentifierList, SubtypeIndication, Maybe Expression)

type ConstrainedArrayDef = (IndexConstraint, SubtypeIndication)

data Constraint = CRange RangeConstraint | CIndex IndexConstraint deriving (Show)

type ContextClause = [ContextItem]

data ContextItem = CtxIL LibraryClause | CtxUC UseClause deriving (Show)

type DecimalLiteral = Double 

data Declaration = DT TypeDeclaration
                  | DST SubTypeDeclaration
                  | DOT ObjectDeclaration
                  | DID InterfaceDeclaration
                  | DAD AliasDeclaration
                  | DCD ComponentDeclaration
                  | DGTD GroupTemplateDeclaration
                  | DGD GroupDeclaration
                  | DED EntityDeclaration
                  | DCFD ConfigurationDecl
                  | DSPD SubProgramDeclaration
                  | DPD PackageDeclaration deriving (Show)

data DelayMechanism = Transport | Inertial (Maybe Expression) deriving (Show)

-- No []
type DesignFile = [DesignUnit]

type DesignUnit = (ContextClause, LibraryUnit)

data Designator = DesIdent Ident | DesOp OperatorSymbol deriving (Show)

data Direction = TO | DOWNTO deriving (Show)

type DisconnectionSpecification = (GuardedSignalSpecification, Expression)

data DiscreteRange = DRST SubtypeIndication | DRR Range deriving (Show)

type ElementAssocation = ([Choices], Expression)

type ElementDeclaration = (IdentifierList, ElementSubtypeDefinition)
type ElementSubtypeDefinition = SubtypeIndication

--data ElementConstraint = ECArray ArrayConstraint | ECRecord RecordConstraint
--data ElementResolution = ERArray ArrayElementResolution | ERRecord RecordResolution


data EntityAspect = Entity Name (Maybe Identifier)
                    | Configuration Name
                    | EAOpen deriving (Show)

data EntityClass = ENTITY 
                   | ARCHITECTURE
                   | CONFIGURATION
                   | PROCEDURE
                   | FUNCTION
                   | PACKAGE
                   | TYPE
                   | SUBTYPE
                   | CONSTANT
                   | SIGNAL
                   | VARIABLE
                   | COMPONENT
                   | LABEL
                   | LITERAL
                   | UNITS
                   | GROUP
                   | FILE deriving (Show)

--The presence of <> indicated by boolean
type EntityClassEntry = (EntityClass, Bool)
--No []
type EntityClassEntryList = [EntityClassEntry]

type EntityDeclaration = (Ident, EntityHeader, EntityDeclarativePart, Maybe EntityStatementPart, Maybe SimpleName)


data EntityDeclarativeItem = EDISD SubProgramDeclaration
                             | EDISB SubProgramBody
                             | EDITD TypeDeclaration
                             | EDISTD SubTypeDeclaration
                             | EDICD  ConstantDeclaration
                             | EDISigD  SignalDeclaration
                             | EDIVD  VariableDeclaration-- SharedVariableDeclaration
                             | EDIFD  FileDeclaration
                             | EDIAD  AliasDeclaration
                             | EDIAtD AttributeDeclaration
                             | EDIAS  AttributeSpecification
                             | EDIDS  DisconnectionSpecification
                             | EDIUC  UseClause
                             | EDIGTD GroupTemplateDeclaration
                             | EDIGD  GroupDeclaration deriving (Show)

type EntityDeclarativePart = [EntityDeclarativeItem]

type EntityDesignator = (EntityTag, Maybe Signature)

type EntityHeader = (Maybe PortClause) -- (Maybe FormalGenericClause, Maybe FormalPortClause)


data EntityNameList = ENLED [EntityDesignator] -- no []
                      | ENLOTHERS
                      | ENALL deriving (Show)

type EntitySpecification = (EntityNameList, EntityClass)

data EntityStatement = EStmtCAS ConcurrentAssertionStmt
                       | EStmtPCPC ConcurrentProcedureCallStatement
                       | EStmtPS ProcessStatement deriving (Show)

type EntityStatementPart = [EntityStatement]

data EntityTag = ETagSN SimpleName
                 | ETagCL CharacterLiteral
                 | ETagOS OperatorSymbol deriving (Show)

data EnumerationLiteral = ELI Ident
                          | ELCL CharacterLiteral deriving (Show)

--no []
type EnumerationTypeDefinition = [EnumerationLiteral]

type ExitStatement = (Maybe Label, Maybe Label, Maybe Condition)

data Exponent = ExpPlus Integer | ExpMinus deriving (Show)

data Expression = ExprAnd Relation [Relation] 
                  | ExprOr Relation [Relation]
                  | ExprXor Relation [Relation]
                  | ExprNand Relation (Maybe Relation)
                  | ExprNor  Relation (Maybe Relation)
                  | ExprXNor Relation [Relation] deriving (Show)

data ExtendedDigit = EDN Char | EDL Letter deriving (Show)

--No []
type ExtendedIdentifier = [GraphicCharacter]

data Factor = FPrim Primary [Primary]
              | FAbs Primary
              | FNot Primary deriving (Show)

type FileDeclaration = (IdentifierList, SubtypeIndication, FileOpenInformation)

type FileLogicalName = Expression

type FileOpenInformation = (Maybe Expression, FileLogicalName)

type FileTypeDefinition = TypeMark

type FloatingTypeDefinition = RangeConstraint

data FormalDesignator = FDName Name | FDPrtName Name | FDPrmName Name deriving (Show)

type FormalParameterList = InterfaceList

data FormalPart = FPD FormalDesignator 
                  | FPFN Name FormalDesignator 
                  | FPTM TypeMark FormalDesignator deriving (Show)

type FullTypeDeclaration = (Ident, TypeDefinition)

type FunctionName = Name

type FunctionCall = (FunctionName, Maybe ActualParameterPart)

type GenerateStatement = (Label, 
                          GenerationScheme, 
                          [BlockDeclarativeItem], 
                          [ConcurrentStatement],
                          Maybe Label)

data GenerationScheme = GSFor ParameterSpecification
                        | GSIf Condition deriving (Show)

type GenericClause = GenericList
type GenericList   = InterfaceList
--type GenericAssociationList = ()
--type GenericList = ()

type GenericMapAspect = AssociationList

type GraphicCharacter = Char

data GroupConstituent = GCName Name | GCChar CharacterLiteral deriving (Show)

--No []
type GroupConstituentList = [GroupConstituent]

type GroupTemplateDeclaration = (Ident, EntityClassEntryList)

type GroupDeclaration = (Ident, Name, GroupConstituentList)

type GuardedSignalSpecification = (SignalList, TypeMark)

type Identifier = Ident

type IdentifierList = [Identifier]

type IfClauseBlock = (Condition,SequenceOfStatements)
type IfStatement = (Label,IfClauseBlock,[IfClauseBlock],Maybe SequenceOfStatements)

type InCompleteTypeDeclaration = Identifier

type IndexConstraint = [DiscreteRange]

data IndexSpecification = ISR DiscreteRange | ISE Expression deriving (Show)

type IndexSubtypeDefinition = (TypeMark, Range)
type IndexedName = (Prefix, [Expression])

data InstantiatedUnit = IUCMP Name
                        | IUENT Name (Maybe Identifier)
                        | IUCNF Name deriving (Show)

data InstantiationList = IList [Label] | ILOTHERS | ILALL deriving (Show)

type IntegerTypeDefinition = RangeConstraint

type InterfaceConstantDeclaration = (IdentifierList, SubtypeIndication, Expression)

data InterfaceDeclaration = IDCD InterfaceConstantDeclaration
                            | IDIS InterfaceSignalDeclaration
                            | IDVD InterfaceVariableDeclaration
                            | IDFD InterfaceFileDeclaration deriving (Show)

type InterfaceElement = InterfaceDeclaration

type InterfaceFileDeclaration = (IdentifierList, SubtypeIndication)

type InterfaceList = [InterfaceElement]

--Bool Bus or not?
type InterfaceSignalDeclaration = (IdentifierList, Maybe Mode, SubtypeIndication, Bool, Maybe Expression)

type InterfaceVariableDeclaration = (IdentifierList, Maybe Mode, SubtypeIndication, Maybe Expression)

data IterationScheme = IWhile Condition | IFor ParameterSpecification deriving (Show)

type Label = Ident
type Letter = Char

type LetterOrDigit = Char

type LibraryClause = LogicalNameList

data LibraryUnit = LUPrim PrimaryUnit | LUSec SecondaryUnit deriving (Show)

data Literal = NLit NumericLiteral 
               | ELit EnumerationLiteral
               | SLit StringLiteral
               | BSLit BitStringLiteral
               | NULL deriving (Show)

type LogicalName = Ident

type LogicalNameList = [LogicalName]

data LogicalOperator = AND | OR | XOR | NAND | NOR | XNOR deriving (Show)

type LoopStatement = (Maybe Label, Maybe IterationScheme, Maybe SequenceOfStatements, Maybe Label)

data MiscellaneousOperator = StarStar | ABS | NOT deriving (Show)

data Mode = IN | OUT | INOUT | BUFFER | LINKAGE deriving (Show)

data MultiplyingOperator = MTimes | MDiv | MMod | MRem  deriving (Show)

data Name = NSN SimpleName
            | NOS OperatorSymbol
            | NSelN SelectedName
            | NIN IndexedName
            | NSlcN SliceName
            | NAN AttributeName deriving (Show)

type NextStatement = (Label, Label, Condition)

type NullStatement = (Maybe Label)

data NumericLiteral = AbsNL AbstractLiteral | PhysNL PhysicalLiteral deriving (Show)

data ObjectDeclaration = ODCD ConstantDeclaration
                         | ODSD SignalDeclaration
                         | ODVD VariableDeclaration
                         | ODFD FileDeclaration deriving (Show)

type OperatorSymbol = StringLiteral

type Options = (Bool, Maybe DelayMechanism)

type PackageBody = (SimpleName, PackageBodyDeclarativePart, Bool, Maybe SimpleName)

data PackageBodyDeclarativeItem = PBSD SubProgramDeclaration
                                  | PBSB SubProgramBody
                                  | PBTD TypeDeclaration
                                  | PBSTD SubTypeDeclaration
                                  | PBCD  ConstantDeclaration
                                  | PBSV  VariableDeclaration
                                  | PBFD  FileDeclaration
                                  | PBAD  AliasDeclaration
                                  | PBUC  UseClause
                                  | PBGTD GroupTemplateDeclaration
                                  | PBGD  GroupDeclaration deriving (Show)

type PackageBodyDeclarativePart = [PackageBodyDeclarativeItem]

type PackageDeclaration = (Identifier, PackageBodyDeclarativePart, Bool, Maybe SimpleName)

type ParameterSpecification = (Identifier, DiscreteRange)

type PhysicalLiteral = (Maybe AbstractLiteral, Name)

type PhysicalTypeDefinition = (RangeConstraint, BaseUnitDeclaration, [SecondaryUnitDeclaration], Maybe SimpleName)

type PortClause = PortList

type PortList = InterfaceList

type PortMapAspect = AssociationList


data Prefix = PName Name | PFCall FunctionCall deriving (Show)

data Primary = PriName Name
               | PriLiteral Literal
               | PriAggr  Aggregate
               | PriFCall FunctionCall
               | PriQual  QualifiedExpression
               | PriTyCon TypeConversion
               | PriAlloc Allocator
               | PriExpr  Expression deriving (Show)

data PrimaryUnit = PUED EntityDeclaration
                   | PUCD ConfigurationDeclaration
                   | PUPD PackageDeclaration deriving (Show)

type ProcedureCall = (Name, Maybe ActualParameterPart)
type ProcedureCallStatement = (Label, ProcedureCall)

data ProcessDeclarativeItem = PDISD SubProgramDeclaration
                              | PDISB SubProgramBody
                              | PDITD TypeDeclaration
                              | PDISTD SubTypeDeclaration
                              | PDICD ConstantDeclaration
                              | PDIVD VariableDeclaration
                              | PDIFD FileDeclaration
                              | PDIAD AliasDeclaration
                              | PDIAttrD AttributeDeclaration
                              | PDIAttrS AttributeSpecification
                              | PDIUC UseClause
                              | PDIGTD GroupTemplateDeclaration
                              | PDIGD GroupDeclaration deriving (Show)

type ProcessDeclarativePart = [ProcessDeclarativeItem]

type ProcessStatement = (Maybe Label, Bool, Maybe SensitivityList, Bool, ProcessDeclarativePart, ProcessStatementPart, Bool, Maybe Label)

type ProcessStatementPart = SequentialStatement

data QualifiedExpression = QETypeME TypeMark Expression | QETypeMA TypeMark Aggregate deriving (Show)

data Range = RangeName AttributeName | Range SimpleExpression Direction SimpleExpression deriving (Show)

type RangeConstraint = (Range,Range)

type RecordTypeDefinition = (ElementDeclaration, [ElementDeclaration], Maybe SimpleName)

type Relation = (ShiftExpression, Maybe (RelationalOperator,ShiftExpression))

data RelationalOperator = EQ | NEQ | LT | LTE | GT | GTE | QEQ | QNEQ | QLT | QLTE | QGT | QGTE deriving (Show)

type ReportStatement = (Maybe Label, Expression, Maybe Expression)

type ReturnStatement = (Maybe Label, Maybe Expression)

data ScalarTypeDefinition = STDE EnumerationTypeDefinition
                            | STDI IntegerTypeDefinition
                            | STDF FloatingTypeDefinition
                            | STDP PhysicalTypeDefinition deriving (Show)

data SecondaryUnit = SUA ArchitectureBody
                     | SUP PackageBody deriving (Show) 

type SecondaryUnitDeclaration = (Identifier, PhysicalLiteral)

type SelectedName = (Prefix, Suffix)

type SelectedSignalAssignment = (Expression, Target, Options, SelectedWaveforms)

--no []
type SelectedWaveforms = [(Waveform, Choices)]

type SensitivityClause = SensitivityList

--no []
type SensitivityList = [Name]

type SequenceOfStatements = [SequentialStatement]

data SequentialStatement = SSW WaitStatement
                           | SSAS AssertionStatement
                           | SSR ReportStatement
                           | SSSA SignalAssignmentStatement
                           | SSVA VariableAssignmentStatement
                           | SSPC ProcedureCallStatement
                           | SSIF IfStatement
                           | SSCS CaseStatement
                           | SSLS LoopStatement
                           | SSNS NextStatement
                           | SSES ExitStatement
                           | SSRS ReturnStatement
                           | SSN NullStatement deriving (Show) 

type SignalAssignmentStatement = (Maybe Label, Target, Maybe DelayMechanism, Waveform)

type ShiftExpression = (SimpleExpression, Maybe (ShiftOperator, SimpleExpression))
                            
data ShiftOperator = SLL | SRL | SLA | SRA | ROL | ROR deriving (Show)

data Sign = Pos | Neg deriving (Show)

type SignalDeclaration = (IdentifierList, SubtypeIndication, Maybe SignalKind, Maybe Expression)

data SignalKind = REGISTER | BUS deriving (Show)

data SignalList = SL [Name]
                  | SLOTHERS
                  | SLALL deriving (Show)

type Signature = Maybe (Maybe [TypeMark], Maybe TypeMark)

type SimpleExpression = ([Sign], Term, [(AddingOperator, Term)])

type SimpleName = Identifier

type SliceName = (Prefix, DiscreteRange)

type StringLiteral = String

data SubProgramKind = SBProcedure | SBFunction deriving (Show)

type SubProgramBody = (SubProgramSpecification, SubProgramDeclarativePart, SubProgramStatementPart, Maybe SubProgramKind, Maybe Designator)

type SubProgramDeclaration = SubProgramSpecification

data SubProgramDeclaritiveItem = SPDISPD SubProgramDeclaration
                                 | SPDISB SubProgramBody
                                 | SPDITD TypeDeclaration
                                 | SPDICD ConstantDeclaration
                                 | SPDIVD VariableDeclaration
                                 | SPDIFD FileDeclaration
                                 | SPDIAD AliasDeclaration
                                 | SPDIAttrD AttributeDeclaration
                                 | SPDIAttrS AttributeSpecification
                                 | SPDIUC UseClause
                                 | SPDIGTD GroupTemplateDeclaration
                                 | SPDIGD GroupDeclaration deriving (Show)

type SubProgramDeclarativePart = [SubProgramDeclaritiveItem]

type SubProgramSpecification = (Designator, Maybe FormalParameterList, Maybe Bool, Designator, Maybe FormalParameterList, TypeMark)

type SubProgramStatementPart = [SequentialStatement]

type SubTypeDeclaration = (Identifier, SubtypeIndication)

type SubtypeIndication = (Maybe Name, TypeMark, Maybe Constraint)

data Suffix = SfxSN SimpleName | All | SfxCL CharacterLiteral | SfxOpSym OperatorSymbol deriving (Show)

data Target = TN Name | TA Aggregate deriving (Show)

type Term = (Factor, [(MultiplyingOperator, Factor)])

type TimeoutClause = Expression

type TypeConversion = (TypeMark, Expression)

data TypeDeclaration = TDF FullTypeDeclaration
                       |  TDI InCompleteTypeDeclaration deriving (Show)

data TypeDefinition = TDefS ScalarTypeDefinition
                      | TDefC CompositeTypeDefinition
                      | TDefA AccessTypeDefinition
                      | TDefF FileTypeDefinition deriving (Show)

data TypeMark = TypeName Name | SubTypeName Name deriving (Show)

type UnconstrainedArrayDef = ([IndexSubtypeDefinition], SubtypeIndication)

type UseClause = [SelectedName]

type VariableAssignmentStatement = (Maybe Label, Target, Expression)

type VariableDeclaration = (Bool, IdentifierList, SubtypeIndication, Maybe Expression)

type WaitStatement = (Maybe Label, Maybe SensitivityClause, Maybe ConditionClause, Maybe TimeoutClause)

data Waveform = WFE [WaveformElement]
                | UNAFFECTED deriving (Show)

data WaveformElement = WFEV Expression (Maybe Expression)
                       | WFENULL (Maybe Expression) deriving (Show)
