{-
 -
 -
 - This module is intended to model a semantic core of HDL for the purposes of
 - interpreting it in ReWire.  The model includes some very basic semantic elements
 - of VHDL.  This includes components which are a combination of entity and 
 - architecture declarations in VHDL.  Components have specified inputs, outputs
 - and a process.  A process has a sensitivity list and a sequence of statements.
 - Statements can have assignments, ITE blocks that have statements in the TE sections,
 - and there is a when_changed construction which is like an ITE but for when a signal
 - has been changed.  When_changed constructs can be run on signals listed in the sensitivty
 - list.
 -}

module SemanticCore where


type Name = String
type Signal = Name

type Input = [Signal]
type Output = [Signal]

type SensitivityList = [Signal]
type Process = (SensitivityList, [Stmt])

data Stmt = Assignment Name Expr 
            | ITE Expr [Stmt] [Stmt]
            | WhenChanged Name [Stmt]

data Expr =   And Expr Expr
            | Or  Expr Expr
            | Not Expr Expr
            | Var Name
            | Lit Val

data Val = High | Low

type Component = (Input,Process,Output)



