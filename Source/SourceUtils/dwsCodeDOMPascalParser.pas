{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsCodeDOMPascalParser;

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsTokenTypes,
   dwsCodeDOM, dwsCodeDOMNodes, dwsCodeDOMParser;

type

   TdwsCodeDOMPascalParser = class
      public
         function CreateRules : TdwsParserRules;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cAssignments = [ ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN,
                    ttTIMES_ASSIGN, ttDIVIDE_ASSIGN ];

   cUnaryOperators = [ ttPLUS, ttMINUS, ttNOT ];
   cMultOperators = [ ttTIMES, ttDIVIDE, ttMOD, ttDIV, ttAND,
                      ttCARET, ttAS, ttLESS_LESS, ttGTR_GTR, ttQUESTION_QUESTION,
                      ttSHL, ttSHR, ttSAR ];
   cComparisonOperators = [ ttEQ, ttNOT_EQ, ttEQ_EQ, ttEXCL_EQ, ttEQ_EQ_EQ,
                            ttLESS, ttLESS_EQ, ttGTR, ttGTR_EQ,
                            ttIN, ttIS, ttIMPLEMENTS, ttIMPLIES,
                            ttPLUS_PLUS, ttMINUS_MINUS ];

// ------------------
// ------------------ TdwsCodeDOMPascalParser ------------------
// ------------------

// Create
//
function TdwsCodeDOMPascalParser.CreateRules : TdwsParserRules;
begin
   Result := TdwsParserRules.Create;

   var expression := Result.NewRuleAlternative('expression');

   var expr_list := Result.NewRuleNode('expr_list', TdwsCodeDOMNode)
      .AddSubRule(expression)
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifRestart ]);

   var reference := Result.NewRuleNode('reference', TdwsCodeDOMReference)
      .AddMatchName
      .AddMatchTokenType(ttDOT, [ rifOptional, rifRestart ])
   ;

   var name_list := Result.NewRuleNode('name_list', TdwsCodeDOMNameList)
      .AddMatchName
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifRestart ])
   ;

   var var_decl := Result.NewRuleNode('var_decl', TdwsCodeDOMVarDeclaration)
      .AddSubRule(name_list)
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(reference)
   ;

   var var_infer  := Result.NewRuleNode('var_infer', TdwsCodeDOMVarDeclaration)
      .AddMatchName
      .AddMatchTokenType(ttASSIGN)
      .AddSubRule(expression)
   ;

   var var_section_line := Result.NewRuleAlternative('var_section_line')
      .AddSubRule(var_decl)
      .AddSubRule(var_infer)
   ;

//   var varsection := Result.NewRuleNode('varsection', TdwsCodeDOMVarSection)
//      .AddMatchTokenType(ttVAR)
//      .AddSubRule(varsectionline)
//   ;

   var var_inline := Result.NewRuleNode('var_inline', TdwsCodeDOMVarSection)
      .AddMatchTokenType(ttVAR)
      .AddSubRule(var_section_line)
   ;

   var literal := Result.NewRuleAlternative('literal')
      .AddMatchTokenTypes([ ttStrVal, ttIntVal, ttFloatVal, ttTRUE, ttFALSE, ttNIL ])
   ;

   var parenthesis := Result.NewRuleNode('parenthesis', TdwsCodeDOMExpression)
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(expression)
      .AddMatchTokenType(ttBRIGHT)
   ;

   var call := Result.NewRuleNode('call', TdwsCodeDOMCall)
      .AddSubRule(reference)
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(expr_list, [ rifOptional ])
      .AddMatchTokenType(ttBRIGHT)
   ;

   var unary  := Result.NewRuleNode('unary', TdwsCodeDOMUnaryOperator)
      .AddMatchTokenTypes(cUnaryOperators)
      .AddSubRule(expression)
   ;

   var if_then_else_expr := Result.NewRuleNode('if_then_else_expr', TdwsCodeDOMIfThenElseExpr)
      .AddMatchTokenType(ttIF)
      .AddSubRule(expression)
      .AddMatchTokenType(ttTHEN)
      .AddSubRule(expression)
      .AddMatchTokenType(ttELSE, [ rifEndIfNotPresent ])
      .AddSubRule(expression)
   ;

   var term := Result.NewRuleAlternative('term')
      .AddSubRule(parenthesis)
      .AddSubRule(literal)
      .AddSubRule(if_then_else_expr)
      .AddSubRule(call)
      .AddSubRule(unary)
      .AddSubRule(reference)
   ;

   var exprmult := Result.NewRuleAlternative('expr_mult')
      .AddSubRule(Result.NewRuleNode('expr_mult_op', TdwsCodeDOMBinaryOperator)
         .AddSubRule(term)
         .AddMatchTokenTypes(cMultOperators)
         .AddSubRule(term)
      ).AddSubRule(term)
   ;

   var expradd := Result.NewRuleAlternative('expr_add')
      .AddSubRule(Result.NewRuleNode('expr_add_op', TdwsCodeDOMBinaryOperator)
         .AddSubRule(exprmult)
         .AddMatchTokenTypes([ ttPLUS, ttMINUS, ttOR, ttXOR, ttNOT ])
         .AddSubRule(exprmult)
      ).AddSubRule(term)
   ;

   var exprcmp := Result.NewRuleNode('expr_cmp', TdwsCodeDOMBinaryOperator)
      .AddSubRule(expradd)
      .AddMatchTokenTypes(cComparisonOperators)
      .AddSubRule(expradd)
   ;

   expression
      .AddSubRule(exprcmp)
      .AddSubRule(expradd)
      .AddSubRule(exprmult)
   ;

   var assignment := Result.NewRuleNode('assignment', TdwsCodeDOMAssignment)
      .AddSubRule(reference)
      .AddMatchTokenTypes(cAssignments)
      .AddSubRule(expression)
   ;

   var statement := Result.NewRuleAlternative('statement');
   var statementList := Result.NewRuleNode('statement_list', TdwsCodeDOMStatementList, [ prfRoot, prfReplaceBySingleChild ]);

   var if_then_else_stmt := Result.NewRuleNode('if_then_else_stmt', TdwsCodeDOMIfThenElseStmt)
      .AddMatchTokenType(ttIF)
      .AddSubRule(expression)
      .AddMatchTokenType(ttTHEN)
      .AddSubRule(statement)
      .AddMatchTokenType(ttELSE, [ rifEndIfNotPresent ])
      .AddSubRule(statement)
   ;

   var repeat_until := Result.NewRuleNode('repeat_until', TdwsCodeDOMRepeat)
      .AddMatchTokenType(ttREPEAT, [ rifSkipSnippet ])
      .AddSubRule(statementlist)
      .AddMatchTokenType(ttUNTIL)
      .AddSubRule(expression)
   ;

   var while_do := Result.NewRuleNode('while_do', TdwsCodeDOMRepeat)
      .AddMatchTokenType(ttWHILE)
      .AddSubRule(expression)
      .AddMatchTokenType(ttDO)
      .AddSubRule(statement)
   ;

   var block := Result.NewRuleNode('block', TdwsCodeDOMBlock)
      .AddMatchTokenType(ttBEGIN, [ rifSkipSnippet ])
      .AddSubRule(statementList)
      .AddMatchTokenType(ttEND, [ rifSkipSnippet ])
   ;

   var parameter_decl := Result.NewRuleNode('parameter_decl', TdwsCodeDOMParameterDecl)
      .AddMatchTokenTypes([ ttCONST, ttVAR, ttLAZY ], [ rifOptional ])
      .AddSubRule(name_list)
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(reference)
   ;
   var parameter_decl_list := Result.NewRuleNode('parameter_decl_list', TdwsCodeDOMParameterDeclList)
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(parameter_decl)
      .AddMatchTokenType(ttSEMI, [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttBRIGHT)
   ;

   var function_decl := Result.NewRuleNode('function_decl', TdwsCodeDOMFunctionDecl)
      .AddMatchTokenType(ttCLASS, [ rifOptional ])
      .AddMatchTokenTypes([ ttPROCEDURE, ttFUNCTION, ttMETHOD ])
      .AddSubRule(reference)
      .AddSubRule(parameter_decl_list, [ rifOptional ])
      .AddMatchTokenType(ttCOLON, [ rifEndIfNotPresent ])
      .AddSubRule(reference)
   ;

   var function_impl := Result.NewRuleNode('function_impl', TdwsCodeDOMFunctionImpl)
      .AddSubRule(function_decl)
      .AddMatchTokenType(ttSEMI)
      .AddSubRule(block)
   ;

   var class_type_fwd := Result.NewRuleNode('class_type_fwd', TdwsCodeDOMClassFwd)
      .AddMatchName
      .AddMatchTokenType(ttEQ)
      .AddMatchTokenTypes([ ttPARTIAL, ttSTATIC ], [ rifOptional ])
      .AddMatchTokenType(ttCLASS)
      .AddMatchTokenType(ttSTATIC, [ rifOptional ])
      .AddMatchTokenTypes([ ttABSTRACT, ttSEALED ], [ rifOptional ])
      .AddMatchTokenType(ttEXTERNAL, [ rifOptional ])
      .AddMatchTokenType(ttPARTIAL, [ rifOptional ])
   ;
   var class_type_inh := Result.NewRuleNode('class_type_inh', TdwsCodeDOMClassInh)
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(reference)
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttBRIGHT)
   ;
   var class_type_decl := Result.NewRuleNode('class_type_decl', TdwsCodeDOMClassDecl)
      .AddSubRule(class_type_fwd)
      .AddSubRule(class_type_inh, [ rifOptional ])
      .AddMatchTokenType(ttEND)
   ;

   var type_decl := Result.NewRuleAlternative('type_decl')
      .AddSubRule(class_type_decl)
      .AddSubRule(class_type_fwd)
   ;

   var type_decl_inline := Result.NewRuleNode('type_inline', TdwsCodeDOMTypeSection)
      .AddMatchTokenType(ttTYPE)
      .AddSubRule(type_decl)
   ;

   statement
      .AddSubRule(if_then_else_stmt)
      .AddSubRule(repeat_until)
      .AddSubRule(while_do)
      .AddSubRule(block)
      .AddSubRule(var_inline)
      .AddSubRule(function_impl)
      .AddSubRule(type_decl_inline)
      .AddSubRule(assignment)
      .AddSubRule(expression)
   ;

   statementList
      .AddSubRule(statement, [ rifEndIfNotPresent ])
      .AddMatchTokenType(ttSEMI, [ rifOptional, rifRestart ])
   ;

end;

end.
