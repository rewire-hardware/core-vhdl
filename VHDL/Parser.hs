{-#LANGUAGE NoMonomorphismRestriction #-}

module VHDL.Parser where

import CoreVHDL 
import Text.Parsec hiding (choice, label)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language hiding (emptyDef)
import Text.Parsec.Combinator hiding (choice)
import Control.Monad
import Data.Maybe
import Control.Monad.IO.Class
import Prelude hiding (GT,LT,EQ)
--
reserved_words = [ 
                  "access", "after", "alias", "all", "architecture", 
                  "array", "assert", "assume", "assume_guarantee", "attribute",
                  "begin", "block", "body", "buffer", "bus","case", "component",
                  "configuration", "constant", "context", "cover","default", "disconnect", 
                  "downto", "else", "elsif", "end", "entity", "exit",
                  "fairness", "file", "for", "force", "function","generate", 
                  "generic", "group", "guarded","if", "impure", "in", "inertial", "inout", 
                  "is","label", "library", "linkage", "literal", "loop","map", 
                  "new", "next", "null","of", "on", "open", 
                  "others", "out","package", "parameter", "port", "postponed", 
                  "procedure", "process", "property", "protected", "pure",
                  "range", "record", "register", "reject", "release", "report", 
                  "restrict","restrict_guarantee", "return", "select", 
                  "sequence", "severity", "shared", "signal", 
                  "strong", "subtype", "then", "to", "transport", "type",
                  "unaffected", "units", "until", "use", "variable", "vmode", "vprop", 
                  "vunit", "wait", "when", "while", "with"
                 ]

--testing functions
main = do
        s <- readFile "test1.vhd"
        t <- runParserT vhdl () "" s
        print t


vhdl = many top_decl

top_decl = liftM TEntity (entity) 
           <|> liftM TCtx (context)
           <|> liftM TArch (architecture)
           <|> liftM TPkg (package)
           <|> liftM TPkgBody (package_body)
 --          <|> liftM TCDecl (try configuration_specification)
           <|> liftM TCtxDecl context_declaration
           <?> "Unable to parse top level declaration"
            

--stacked according to precedence
operators = [
               "**", "abs", "not", 
               "*", "/", "mod", "rem", 
               --unary plus and minus go here
               "+", "-", "&", 
               "sll", "srl", "sla", "sra", "rol", "ror", 
                "=", "/=", "<", "<=", ">", ">=", 
               "and", "or", "nand", "nor", "xor", "xnor"
            ]
--Fugly.
emptyDef    = P.LanguageDef
               { P.commentStart   = ""
               , P.commentEnd     = ""
               , P.commentLine    = ""
               , P.nestedComments = True
               , P.identStart     = letter <|> char '_'
               , P.identLetter    = alphaNum <|> oneOf "_'"
               , P.opStart        = P.opLetter emptyDef
               , P.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , P.reservedOpNames= []
               , P.reservedNames  = []
               , P.caseSensitive  = True
               }

vhdlDef :: (Monad m) => GenLanguageDef String u m
vhdlDef = emptyDef { 
                     P.commentLine = "--",
                     --P.reservedOpNames = operators,
                     P.reservedNames = reserved_words
                   }

vhdl_lexer = P.makeTokenParser vhdlDef

whiteSpace= P.whiteSpace vhdl_lexer
lexeme    = P.lexeme vhdl_lexer
symbol    = P.symbol vhdl_lexer
natural   = P.natural vhdl_lexer
parens    = P.parens vhdl_lexer
semi      = P.semi vhdl_lexer
colon     = P.colon vhdl_lexer
comma     = P.comma vhdl_lexer
identifier= P.identifier vhdl_lexer
reserved  = P.reserved vhdl_lexer
reservedOp= P.reservedOp vhdl_lexer
stringLiteral = P.stringLiteral vhdl_lexer
charLiteral = P.charLiteral vhdl_lexer
integer = P.integer vhdl_lexer



--And it begins!

--Contexts
context = liftM CtxUC use_clause <|> liftM CtxLC library_clause <|> liftM CtxCC context_clause
use_clause = liftM UseClause (do
                                 reserved "use"
                                 ids <- selected_name `sepBy1` comma 
                                 semi
                                 return ids)

library_clause = liftM LibraryClause (do 
                                       reserved "library"
                                       ids <- identifier `sepBy1` comma
                                       semi
                                       return ids)
                                       
context_clause = liftM ContextClause (do
                                        reserved "context"
                                        ids <- selected_name `sepBy1` comma
                                        semi
                                        return ids)

--Design Units

entity = do
          --ctx <- context
          reserved "entity"
          id <- identifier
          reserved "is"
          glist <- optionMaybe (do reserved "generic"; gl <- parens generic_list; semi; return gl)
          plist <- optionMaybe (do reserved "port"; pl <- parens port_list; semi; return pl)
          decls <- declarations
          cstmts <- optionMaybe (do reserved "begin"; concurrent_statements)
          reserved "end"
          optional identifier 
          semi
          return $ Entity id glist plist decls cstmts

generic_list = constant_interface_declaration `sepBy1` semi 
port_list = signal_interface_declaration `sepBy1` semi


architecture = do
--                ctx <- context
                reserved "architecture"
                id1 <- identifier
                reserved "of"
                id2 <- identifier
                reserved "is"
                decls <- declarations
                reserved "begin"
                ccs <- concurrent_statements
                --let ccs = CSBlock undefined
                reserved "end"
                optional identifier
                semi
                return ((id1,id2),decls,ccs)

package = do
            --ctx <- context
            reserved "package"
            id <- identifier
            reserved "is"
            decls <- declarations
            reserved "end"
            semi
            return (id, decls)

package_body = do
                --ctx <- context
                reserved "package"
                reserved "body"
                id <- identifier
                reserved "is"
                decls <- declarations
                reserved "end"
                semi
                return (id, decls)

context_declaration = do
                        reserved "context"
                        id <- identifier
                        reserved "is"
                        ctx <- context
                        reserved "end"
                        semi
                        return (id,ctx)

--concurrent statementllall ghc
concurrent_statements = (try concurrent_statement) `endBy` semi
concurrent_statement = liftM CSBlock block_statement
                       <|> liftM CSProc process_statement
                       <|> liftM CSCPC concurrent_procedure_call
                       <|> liftM CSCA concurrent_assertion
                       <|> liftM CSSA concurrent_signal_assignment
                       <|> liftM CSCI component_instance
                       <|> liftM CSGS generate_statement

block_statement = do
                    l <- label
                    colon
                    reserved "block"
                    decls <- declarations
                    reserved "begin"
                    ccs <- concurrent_statements
                    reserved "end"
                    reserved "block"
                    return (l,decls,ccs)

process_statement = do
                      lbl <- optionMaybe $ try (do lbl <- label; colon; return lbl)
                      reserved "process"
                      slist <- optionMaybe $ parens sensitivity_list
                      decls <- declarations
                      reserved "begin"
                      ss <- sequential_statements
                      reserved "end"
                      reserved "process"
                      return (lbl, slist, decls, ss)

sensitivity_list = name `sepBy1` comma

concurrent_procedure_call = do
                              lbl <- optionMaybe $ try (do lbl <- label; colon; return lbl)
                              p <- procedure_call
                              return (lbl, p)

concurrent_assertion = do
                         lbl <- optionMaybe $ try (do lbl <- label; colon; return lbl)
                         a <- assertion
                         return (lbl, a)

concurrent_signal_assignment = liftM2 CSAC (optionMaybe $ try (do lbl <- label; colon; return lbl)) conditional_signal_assignment
                           <|> liftM2 CSAS (optionMaybe $ try (do lbl <- label; colon; return lbl)) selected_signal_assignment

conditional_signal_assignment = do
                                  --liftIO $ print "test"
                                  t <- target
                                  ees <- many exprc
                                  e <- expression
                                  return (t,ees,e)
  where
    exprc = try (do e1 <- expression; reserved "when"; e2 <- expression; reserved "else"; return (e1,e2))


selected_signal_assignment = do
                              reserved "with"
                              e <- expression
                              reserved "select"
                              t <- target
                              symbol "<="
                              cs <- exprc `sepBy1` comma
                              return (e,t,cs)
    where

      exprc = do e <- expression; reserved "when"; c <- choices; return (e,c)


component_instance = do
                        l <- label
                        colon
                        n <- name
                        gmap <- optionMaybe $ (do reserved "generic"; reserved "map"; parens association_list)
                        pmap <- optionMaybe $ (do reserved "port"; reserved "map"; parens association_list)
                        return ((l,n), gmap, pmap)

generate_statement = liftM GSFor for_generate 
                     <|> liftM GSIf if_generate

for_generate = do
                 l <- label
                 colon
                 reserved "for"
                 i <- identifier
                 reserved "in"
                 dr <- discrete_range
                 reserved "generate"
                 ccs <- concurrent_statements
                 reserved "end"
                 reserved "generate"
                 return (l, i, dr, ccs)


if_generate = do
                l <- label
                colon
                reserved "if"
                e <- expression
                reserved "generate"
                ccs <- concurrent_statements
                reserved "end"
                reserved "generate"
                return (l, e, ccs)


--Sequential statements

sequential_statements = (try sequential_statement) `endBy` semi

sequential_statement = liftM SSWait wait_statement 
                       <|> liftM SSA assertion
                       <|> liftM SSSA signal_assignment
                       <|> liftM SSVA signal_assignment
                       <|> liftM SSPC procedure_call
                       <|> liftM SSIS if_statement
                       <|> liftM SSCS case_statement
                       <|> liftM SSFL for_loop
                       <|> liftM SSNS next_statement
                       <|> liftM SSES exit_statement
                       <|> liftM SSRS return_statement
                       <|> liftM SSNull null_statement

wait_statement = do
                   reserved "wait"
                   sl <- optionMaybe (do reserved "on"; sensitivity_list)
                   e  <- optionMaybe (do reserved "until"; expression)
                   return (sl,e)

assertion = do
              reserved "assert"
              e <- expression
              rpt <- optionMaybe (do reserved "report"; expression)
              sev <- optionMaybe (do reserved "severity"; expression) 
              return (e,rpt,sev)

signal_assignment = do
                     t <- target
                     symbol "<="
                     e <- expression
                     return (t,e)

variable_assignment = do
                       t <- target
                       symbol ":="
                       e <- expression
                       return (t,e)

procedure_call = liftM2 (,) identifier (optionMaybe $ parens association_list)

if_statement = do
                 reserved "if"
                 e <- expression
                 reserved "then"
                 ifstmts <- sequential_statements
                 elsifs <- many (do reserved "elsif"; e <- expression; reserved "then"; ss <- sequential_statements; return (e,ss))
                 els <- optionMaybe (do reserved "else"; sequential_statements)
                 reserved "end" ; reserved "if"
                 return ((e,ifstmts),elsifs,els)

case_statement = do
                  reserved "case"
                  e <- expression
                  reserved "is"
                  reserved "when"
                  cs <- choices
                  symbol "=>"
                  stmts <- sequential_statements
                  others <- many case_line
                  reserved "end"; reserved "case"
                  return (e, (cs,stmts) : others)
  where
    case_line = (do reserved "when"; cs <- choices; symbol "=>"; ss <- sequential_statements; return (cs, ss))

for_loop = do
            lbl <- optionMaybe $ try (do l <- label; colon; return l)
            reserved "for"
            i <- identifier
            reserved "in"
            dr <- discrete_range
            reserved "loop"
            ss <- sequential_statements 
            reserved "end"
            reserved "loop"
            return (lbl, i, dr, ss)

next_statement = do
                   reserved "next"
                   lbl <- optionMaybe label
                   e   <- optionMaybe (do reserved "when"; expression)
                   return (lbl, e)

exit_statement = do
                   reserved "exit"
                   lbl <- optionMaybe label
                   e   <- optionMaybe (do reserved "when"; expression)
                   return (lbl, e)

return_statement = try (liftM RSFR function_return) <|> liftM RSPR procedure_return 

function_return = do
                    reserved "return"
                    expression

procedure_return = do
                    reserved "return"
                    return ()
null_statement = do 
                    reserved "null"
                    return ()
--Expressions

--expression operator cleverness 
expr_op = 
            (do reservedOp "and"; return And) <|>
            (do reservedOp "or";  return Or)  <|>
            (do try (reservedOp "xor"); return Xor) <|>
            (do reservedOp "xnor"; return Xnor) <|>
            (do try (reservedOp "nand"); return Nand) <|>
            (do (reservedOp "nor"); return Nor) <?> "Expr op failed to parse."

expression = do
               r1 <- relation 
               --liftIO $ print "STARTDEBUG:"
               --liftIO $ print r1
               --liftIO $ print "ENDDEBUG:"
               parens (do
                          op <- expr_op
                          e1 <- expression 
                          return $ op r1 e1) <|> (return $ ExprR r1) <|>
                 (do
                      op <- expr_op
                      e1 <- expression 
                      return $ op r1 e1) <|> (return $ ExprR r1) <?> "Expression failed to parse."

relational_operator =              
                      (do reservedOp "="; return EQ) <|>
                      (do reservedOp "/=";  return NEQ)  <|>
                      (do try (reservedOp "<="); return LTE) <|>
                      (do reservedOp "<"; return LT) <|>
                      (do try (reservedOp ">="); return GTE) <|>
                      (do (reservedOp ">"); return GT) <?> "Relational operator failed to parse."

relation = do              
             se1 <- shift_expression
             (do
                 op <- relational_operator
                 se2 <- shift_expression
                 return $ op se1 se2) <|> (return $ RelSE se1) <?> "Relation failed to parse."

shift_operator = 
                      (do try (reservedOp "sll"); return SLL) <|>
                      (do try (reservedOp "srl"); return SRL)  <|>
                      (do try (reservedOp "sla"); return SLA) <|>
                      (do reservedOp "sra"; return SRA) <|>
                      (do try (reservedOp "rol"); return ROL) <|>
                      (do reservedOp "ror"; return ROR) <?> "Shift Operator failed to parse."

shift_expression = do
                       se1 <- simple_expression
                       (do
                           op <- shift_operator
                           se2 <- simple_expression
                           return $ op se1 se2) <|> (return $ SESimple se1) <?> "Shift Expression failed to parse"

sign_symbol = (do reservedOp "+"; return Plus) <|> (do reservedOp "-"; return Minus) <?> "Sign Symbol failed to parse"
                
adding_operator = 
                      (do try (reservedOp "+"); return SimpleAdd) <|>
                      (do try (reservedOp "-"); return SimpleSub)  <|>
                      (do try (reservedOp "&"); return SimpleAnd) <?> "Adding operator failed to parse."

simple_expression = do
                      sign <- optionMaybe sign_symbol
                      t1 <- term
                      (do
                           op <- adding_operator
                           t2 <- term 
                           return $ op sign t1 t2) <|> (return $ SimpleExp sign t1) <?> "Simple expression failed to parse."

multiplying_operator = 
                        (do try (reservedOp "*"); return TMult) <|>
                        (do try (reservedOp "/"); return TDiv)  <|>
                        (do try (reservedOp "mod"); return TMod) <|>
                        (do try (reservedOp "rem"); return TRem) <?> "Multiplying Operator failed to parse."

term = do
        f1 <- factor 
        (do
           op <- multiplying_operator
           f2 <- factor 
           return $ op f1 f2) <|> (return $ SingleTerm f1) <?> "Term failed to parse."

factor =
          (do reservedOp "abs"; liftM PrimAbs primary) <|>
          (do reservedOp "not"; liftM PrimNot primary) <|>
          (do p1 <- primary; 
              (do reservedOp "**"; 
                  p2 <- primary; 
                  return $ PrimExp p1 p2) <|> (return $ PrimSingle p1)) <?> "Factor failed to parse."

primary = 
          (liftM PName name_nos) <|>
          (liftM PLit literal) <|>
          (liftM PAgg aggregate) <|>
          (liftM PFun function_call) <|>
          (liftM PQual qualified_expression) <|>
          (liftM PType type_conversion) <|>
          (liftM PExp (parens expression))  <?> "Primary failed to parse."

name = 
          (liftM NID identifier) <|>
          (liftM NSN selected_name) <|>
          (liftM NI indexed_name) <|>
          (liftM NSLN slice_name) <|>
          (liftM NAN attribute_name) <|>
          (liftM NOS operator_symbol) <?> "Name failed to parse."

--FIXME: This is suboptimal. 
name_nos = 
            (liftM NID identifier) <|>
            --(liftM NSN selected_name) <|>
            -- (liftM NI indexed_name) <|>
            --(liftM NSLN slice_name) <|>
            --(liftM NAN attribute_name) <|>
            (liftM NOS operator_symbol) <?> "Namenos failed to parse"

operator_symbol = stringLiteral <?> "operator symbol failed to parse."

selected_name = do
                  --liftIO $ print "Selected."
                  p <- prefix --`endBy` (symbol ".")
                  s <- suffix
                  return (p,s) <?> "Selected Name failed to parse."

prefix = liftM PFN name_nos <|> liftM PFC function_call <?> "Prefix failed to parse."
suffix = liftM SFI identifier <|> liftM SFC stringLiteral <|> liftM SFO operator_symbol <|> (do reserved "all"; return SFALL) <?> "Suffix failed to parse."

indexed_name = do
                 --liftIO $ print "Indexed."
                 p <- prefix
                 exprs <- parens $ expression `sepBy1` (symbol ",")
                 return (p,exprs) <?> "Indexed Name failed to parse."

slice_name = do
                --liftIO $ print "Sliced"
                p <- prefix
                dr <- parens discrete_range
                return (p,dr) <?> "Slice Name Failed to parse."

attribute_name = do
                   --liftIO $ print "Attr"
                   p <- prefix
                   --liftIO $ print "Attr1"
                   symbol "'"
                   i <- identifier
                   --liftIO $ print "Attr2"
                   expr <- optionMaybe $ parens expression
                   return (p, i, expr) <?> "Attribute failed to parse."

choice_expr = (do
                 --liftIO $ print "Choiceexpr"
                 c <- optionMaybe $ try (do c <- choices; symbol "=>"; return c)
                 e <- expression
                 return (c,e)) <?> "Choice Expr failed to parse"

aggregate = parens (do
                     --liftIO $ print "Aggregate"
                     (c,e) <- choice_expr 
                     ces <- many choice_expr
                     return (c,e,ces)) <?> "Aggregate failed to parse."

function_call = do
                  --liftIO $ print "function_call"
                  f <- function_name
                  args <- optionMaybe $ parens association_list 
                  return (f, args) <?> "Function call failed to parse."

function_name = liftM FNID (identifier) <|> liftM FNOS operator_symbol  <?> "Function Name failed to parse."

qualified_expression = do
                         i <- identifier
                         (symbol "'")
                         do (do e <- parens expression; return $ QEIE i e) <|> (do a <- aggregate; return $ QEIA i a) <?> "Qualified Expression failed."

type_conversion = do 
                     i <- identifier
                     e <- parens expression
                     return (i,e) <?> "Type Conversion failed to parse."
                         
choices = choice `sepBy1` (symbol "|") <?> "Choices failed to parse."

choice = liftM CID identifier 
          <|> liftM CSE simple_expression 
          <|> liftM CDR discrete_range 
          <|> (reserved "others" >> return Others) <?> "Choice failed to parse."

association_list = many1 association_element <?> "Association List failed to parse."

ae_name = (do
            n <- name
            reservedOp "=>"
            return n) <?> "ae name failed to parse"

association_element = (do
                        n <- optionMaybe ae_name
                        ap <- actual_part
                        return (n,ap)) <?> "Association Element failed to parse."

actual_part = liftM APE expression <|> (do reserved "open"; return APOpen) <?> "Actual Part failed to parse."

discrete_range = liftM DRS subtype_indication <|> liftM DRR range <?> "Discrete Range failed to parse."

subtype_indication = (do
                       i <- identifier
                       c <- optionMaybe constraint
                       return (i,c)) <?> "Subtype indication failed to parse."

constraint = liftM CSTR range_constraint <|> liftM CSTI index_constraint <?> "Constraint failed to parse."
range_constraint = (do
                    r1 <- range
                    r2 <- range
                    return (r1, r2)) <?> "Range Constraint failed to parse."

index_constraint = (parens $ discrete_range `sepBy1` (symbol ","))  <?> "Index constraint failed to parse."

range = liftM RangeAttr attribute_name <|> liftM3 RangeSE simple_expression direction simple_expression <?> "Range failed to parse"

direction = (do reserved "to"; return To) <|> (do reserved "downto"; return DownTo)

target = liftM TName name <|> liftM TAG aggregate

--Declarations

declarations = (try declaration) `endBy` semi

declaration = liftM DFD function_declaration
              <|> liftM DFB function_body
              <|> liftM DPD procedure_declaration
              <|> liftM DPB procedure_body
              <|> liftM DTD type_declaration
              <|> liftM DSD subtype_declaration
              <|> liftM DCD constant_declaration
              <|> liftM DVD variable_declaration
              <|> liftM DSigD signal_declaration
              <|> liftM DComD component_declaration
              <|> liftM DConfD configuration_specification
                    
function_declaration = do
                        fd <- function_designator
                        il <- optionMaybe $ parens interface_list
                        reserved "return"
                        i <- identifier
                        return (fd, il, i)

function_designator = liftM FDID identifier <|> liftM FDOS operator_symbol

function_body = do
                  fd <- function_designator
                  il <- optionMaybe $ parens interface_list
                  reserved "return"
                  i <- identifier
                  reserved "is"
                  decls <- declarations
                  reserved "begin"
                  stmts <- sequential_statements
                  reserved "end"
                  semi
                  return (fd, il, i, decls, stmts)

procedure_declaration = do 
                          i <- identifier
                          il <- optionMaybe $ parens interface_list
                          return (i,il)

procedure_body = do
                   i <- identifier
                   il <- optionMaybe $ parens interface_list
                   reserved "is"
                   decls <- declarations
                   reserved "begin"
                   stmts <- sequential_statements
                   reserved "end"
                   semi
                   return (i,il,decls,stmts)

interface_list = interface_declaration `sepBy1` semi

interface_declaration = liftM IDC constant_interface_declaration 
                        <|> liftM IDV variable_interface_declaration
                        <|> liftM IDS signal_interface_declaration

decl_assign = do
                symbol ":="
                e <- expression
                return e
                

constant_interface_declaration = do
                                   c <- optionMaybe $ try $ reserved "constant"                                    
                                   il <- identifier_list                                   
                                   colon
                                   m <- optionMaybe $ try $ mode
                                   si <- subtype_indication
                                   e <- optionMaybe $ decl_assign
                                   return (isJust c, il, m, si, e)

variable_interface_declaration = do
                                   c <- optionMaybe $ try $ reserved "variable"                                    
                                   il <- identifier_list                                   
                                   colon
                                   m <- optionMaybe $ try $ mode
                                   si <- subtype_indication
                                   e <- optionMaybe $ decl_assign
                                   return (isJust c, il, m, si, e)
                                   

signal_interface_declaration = do
                                   c <- optionMaybe $ try $ reserved "signal"                                    
                                   il <- identifier_list                                   
                                   colon
                                   m <- optionMaybe $ try $ mode
                                   si <- subtype_indication
                                   e <- optionMaybe $ decl_assign
                                   return (isJust c, il, m, si, e)

identifier_list = identifier `sepBy1` comma

mode = (do reserved "in"; return IN)
       <|> (do reserved "out"; return OUT)
       <|> (do reserved "inout"; return INOUT)
       <|> (do reserved "buffer"; return BUFFER)


type_declaration = do
                      reserved "type"
                      i <- identifier
                      reserved "is"
                      td <- type_definition
                      return (i, td)

type_definition = liftM TDE enumeration_type
                  <|> liftM TDI integer_type
                  <|> liftM TDA array_type
                  <|> liftM TDR record_type

enumeration_type = parens $ enumeration_literal `sepBy1` comma

enumeration_literal = liftM ELID identifier
                      <|> liftM ELC stringLiteral

integer_type = do
                r1 <- range
                r2 <- range
                return (r1,r2)

array_type = do
              reserved "array"
              ac <- parens array_constraint
              reserved "of"
              si <- subtype_indication
              return (ac,si)

array_constraint = (do i <- identifier; r <- range; symbol "<>"; return $ ARRConI i r)
                   <|> liftM ARRConD discrete_range

record_line = do
                i <- identifier_list
                colon
                si <- subtype_indication
                semi
                return (i,si)

record_type = do
                reserved "record"
                rs <- many1 record_line
                reserved "end" ; reserved "record"
                return rs

subtype_declaration = do
                       reserved "subtype"
                       i <- identifier
                       reserved "is"
                       si <- subtype_indication
                       return (i, si)

constant_declaration = do
                        reserved "constant"
                        il <- identifier_list
                        colon
                        si <- subtype_indication
                        e <- decl_assign
                        return (il, si, e)

variable_declaration = do
                        reserved "variable"
                        il <- identifier_list
                        colon
                        si <- subtype_indication
                        e <- optionMaybe decl_assign
                        return (il, si, e)

signal_declaration = do
                        reserved "signal"
                        il <- identifier_list
                        colon
                        si <- subtype_indication
                        e <- optionMaybe decl_assign
                        return (il, si, e)

generic_entry = do 
                  reserved "generic"
                  gl <- parens generic_list
                  semi
                  return gl

port_entry = do
                reserved "port"
                pl <- parens port_list
                semi
                return pl

component_declaration = do
                          symbol "component"
                          i <- identifier
                          gl <- optionMaybe generic_entry
                          pl <- optionMaybe port_entry
                          reserved "end" ; reserved "component"
                          return (i, gl, pl)

configuration_specification = do
                                reserved "component"
                                insts <- instances
                                colon
                                id1 <- identifier
                                reserved "use" ; reserved "entity"
                                sn <- selected_name
                                id2 <- optionMaybe $ parens identifier
                                return (insts, id1, sn, id2)

instances = (do try (reserved "all"); return IAll) 
            <|> (do try (reserved "others"); return IOthers)
            <|> liftM InstI (identifier `sepBy1` comma)


--end placeholders
literal = liftM CharLiteral charLiteral <|> liftM StringLiteral stringLiteral <|> liftM IntegerLiteral integer -- fail "foo" --return (-1) --undefined 
label = identifier
