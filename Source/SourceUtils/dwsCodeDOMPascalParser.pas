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
   dwsCodeDOM, dwsCodeDOMNodes, dwsCodeDOMParser,
   dwsPascalTokenizer;

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
   cAddOperators = [ ttPLUS, ttMINUS, ttOR, ttXOR, ttNOT ];
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
   Result.ReservedTokens := cPascalReservedNames;
   Result.SymbolTokens := cPascalSymbolTokens;


   var switch := Result.NewRuleNode('switch', TdwsCodeDOMSwitch, [ prfComment, prfRoot ])
      .AddMatchTokenType(ttSWITCH)
      .AddMatchAnyExceptTokenTypes([ ttCRIGHT ], [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttCRIGHT)
   ;
   var comment := Result.NewRuleNode('comment', TdwsCodeDOMComment, [ prfComment, prfRoot ])
      .AddMatchTokenType(ttCOMMENT)
   ;

   var expression := Result.NewRuleAlternative('expression');

   var tuple := Result.NewRuleNode('tuple', TdwsCodeDOMTuple)
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

   var range := Result.NewRuleNode('range', TdwsCodeDOMRange)
      .AddSubRule(expression)
      .AddMatchTokenType(ttDOTDOT)
      .AddSubRule(expression)
   ;

   var type_decl_type := Result.NewRuleAlternative('type_decl_type');

   var var_decl := Result.NewRuleNode('var_decl', TdwsCodeDOMVarDeclaration)
      .AddSubRule(name_list)
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(type_decl_type)
      .AddMatchTokenTypes([ ttEQ, ttASSIGN ], [ rifEndIfNotPresent ])
      .AddSubRule(expression)
   ;

   var var_infer  := Result.NewRuleNode('var_infer', TdwsCodeDOMVarDeclaration)
      .AddMatchName
      .AddMatchTokenTypes([ ttASSIGN, ttEQ ])
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

   var const_inline := Result.NewRuleNode('const_inline', TdwsCodeDOMConstSection)
      .AddMatchTokenType(ttCONST)
      .AddSubRule(var_section_line)
   ;

   var literal_str := Result.NewRuleNode('literal_str', TdwsCodeDOMLiteralStr,  [ prfReplaceBySingleChild ])
      .AddMatchTokenType(ttStrVal)
      .AddMatchTokenType(ttStrVal, [ rifOptional, rifGoToStep1 ])
   ;

   var literal := Result.NewRuleAlternative('literal')
      .AddMatchTokenTypes([ ttIntVal, ttFloatVal, ttTRUE, ttFALSE, ttNIL ])
      .AddSubRule(literal_str)
   ;

   var parenthesis := Result.NewRuleNode('parenthesis', TdwsCodeDOMParenthesis)
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(expression)
      .AddMatchTokenType(ttBRIGHT)
   ;

   var case_of_alternative_case := Result.NewRuleAlternative('case_of_alternative_case')
      .AddSubRule(range)
      .AddSubRule(expression)
   ;
   var case_of_alternative_cases := Result.NewRuleNode('case_of_alternative_cases', TdwsCodeDOMCaseOfAlternativeCases)
      .AddSubRule(case_of_alternative_case)
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifRestart ])
   ;

   var brackets := Result.NewRuleNode('brackets', TdwsCodeDOMBrackets)
      .AddMatchTokenType(ttALEFT)
      .AddSubRule(case_of_alternative_cases)
      .AddMatchTokenType(ttARIGHT)
   ;

   var call := Result.NewRuleNode('call', TdwsCodeDOMCall)
      .AddSubRule(reference)
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(tuple, [ rifOptional ])
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

   var term := Result.NewRuleNode('term', TdwsCodeDOMIndexed, [ prfReplaceBySingleChild ])
      .AddSubRule(Result.NewRuleAlternative('term')
         .AddSubRule(if_then_else_expr)
         .AddSubRule(parenthesis)
         .AddSubRule(brackets)
         .AddSubRule(unary)
         .AddSubRule(literal)
         .AddSubRule(call)
         .AddSubRule(reference)
      ).AddMatchTokenType(ttALEFT, [ rifEndIfNotPresent ])
      .AddSubRule(tuple)
      .AddMatchTokenType(ttARIGHT)
   ;

//   var exprmult := Result.NewRuleAlternative('expr_mult')
//      .AddSubRule(Result.NewRuleNode('expr_mult_op', TdwsCodeDOMBinaryOperator)
//         .AddSubRule(term)
//         .AddMatchTokenTypes(cMultOperators)
//         .AddSubRule(term)
//      ).AddSubRule(term)
//   ;
//   var expradd := Result.NewRuleAlternative('expr_add')
//      .AddSubRule(Result.NewRuleNode('expr_add_op', TdwsCodeDOMBinaryOperator)
//         .AddSubRule(exprmult)
//         .AddMatchTokenTypes([ ttPLUS, ttMINUS, ttOR, ttXOR, ttNOT ])
//         .AddSubRule(exprmult)
//      ).AddSubRule(exprmult)
//   ;

//   var exprcmp := Result.NewRuleNode('expr_cmp', TdwsCodeDOMBinaryOperator)
//      .AddSubRule(expradd)
//      .AddMatchTokenTypes(cComparisonOperators)
//      .AddSubRule(expradd)
//   ;
//

   var exprmult := Result.NewRuleNode('expr_mult', TdwsCodeDOMBinaryOperator, [ prfReplaceBySingleChild ]);
   exprmult
      .AddSubRule(term)
      .AddMatchTokenTypes(cMultOperators, [ rifEndIfNotPresent ])
      .AddSubRule(exprmult)
   ;

   var expradd := Result.NewRuleNode('expr_add', TdwsCodeDOMBinaryOperator, [ prfReplaceBySingleChild ]);
   expradd
      .AddSubRule(exprmult)
      .AddMatchTokenTypes(cAddOperators, [ rifEndIfNotPresent ])
      .AddSubRule(expradd)
   ;

   var exprcmp := Result.NewRuleNode('expr_cmp', TdwsCodeDOMBinaryOperator, [ prfReplaceBySingleChild ])
      .AddSubRule(expradd)
      .AddMatchTokenTypes(cComparisonOperators, [ rifEndIfNotPresent ])
      .AddSubRule(expradd)
   ;

   expression
      .AddSubRule(exprcmp)
//      .AddSubRule(expradd)
//      .AddSubRule(exprmult)
   ;

   var assignment := Result.NewRuleNode('assignment', TdwsCodeDOMAssignment)
      .AddSubRule(term)
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
      .AddMatchTokenType(ttREPEAT)
      .AddSubRule(statementlist)
      .AddMatchTokenType(ttUNTIL)
      .AddSubRule(expression)
   ;

   var while_do := Result.NewRuleNode('while_do', TdwsCodeDOMWhile)
      .AddMatchTokenType(ttWHILE)
      .AddSubRule(expression)
      .AddMatchTokenType(ttDO)
      .AddSubRule(statement)
   ;

   var block := Result.NewRuleNode('block', TdwsCodeDOMBeginEnd)
      .AddMatchTokenType(ttBEGIN)
      .AddSubRule(statementList, [ rifOptional ])
      .AddMatchTokenType(ttEND)
   ;

   var case_of_alternative := Result.NewRuleNode('case_of_alternative', TdwsCodeDOMCaseOfAlternative)
      .AddSubRule(case_of_alternative_cases)
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(statement)
   ;
   var case_of_alternatives := Result.NewRuleNode('case_of_alternatives', TdwsCodeDOMCaseOfAlternatives)
      .AddSubRule(case_of_alternative,  [ rifEndIfNotPresent ])
      .AddMatchTokenType(ttSEMI, [ rifOptional, rifRestart ])
   ;

   var case_of := Result.NewRuleNode('case_of', TdwsCodeDOMCaseOf)
      .AddMatchTokenType(ttCASE)
      .AddSubRule(expression)
      .AddMatchTokenType(ttOF)
      .AddSubRule(case_of_alternatives)
      .AddSubRule(Result.NewRuleNode('case_of_else', TdwsCodeDOMNode)
         .AddMatchTokenType(ttELSE)
         .AddSubRule(statementList)
         , [ rifOptional, rifMergeChildren ])
      .AddMatchTokenType(ttEND)
   ;

   var for_loop := Result.NewRuleNode('for_loop', TdwsCodeDOMForLoop)
      .AddMatchTokenType(ttFOR)
      .AddMatchTokenType(ttVAR, [ rifOptional ])
      .AddMatchName
      .AddMatchTokenType(ttASSIGN)
      .AddSubRule(expression)
      .AddMatchTokenTypes([ ttTO, ttDOWNTO ])
      .AddSubRule(expression)
      .AddSubRule(Result.NewRuleNode('for_loop_step', TdwsCodeDOMForLoopStep)
         .AddMatchName('step')
         .AddSubRule(expression)
         , [ rifOptional ])
      .AddMatchTokenType(ttDO)
      .AddSubRule(statement, [ rifOptional ]);

   var for_in := Result.NewRuleNode('for_in', TdwsCodeDOMForIn)
      .AddMatchTokenType(ttFOR)
      .AddMatchTokenType(ttVAR, [ rifOptional ])
      .AddMatchName
      .AddMatchTokenType(ttIN)
      .AddSubRule(expression)
      .AddMatchTokenType(ttDO)
      .AddSubRule(statement, [ rifOptional ]);

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

   var qualifier := Result.NewRuleNode('qualifier', TdwsCodeDOMFunctionQualifier)
      .AddMatchTokenType(ttSEMI)
      .AddMatchTokenTypes([
         ttVIRTUAL, ttABSTRACT, ttOVERRIDE,
         ttOVERLOAD, ttREINTRODUCE, ttFORWARD,
         ttSAFECALL, ttSTDCALL, ttCDECL, ttREGISTER, ttPASCAL
         ])
   ;

   var function_decl := Result.NewRuleNode('function_decl', TdwsCodeDOMFunctionDecl)
      .AddMatchTokenType(ttCLASS, [ rifOptional ])
      .AddMatchTokenTypes([ ttPROCEDURE, ttFUNCTION, ttMETHOD ])
      .AddSubRule(reference)
      .AddSubRule(parameter_decl_list, [ rifOptional ])
      .AddSubRule(Result.NewRuleNode('function_return_decl', TdwsCodeDOMFunctionReturnDecl)
         .AddMatchTokenType(ttCOLON)
         .AddSubRule(reference)
         , [ rifOptional ])
      .AddSubRule(qualifier, [ rifOptional, rifRepeat ])
   ;

   var function_impl := Result.NewRuleNode('function_impl', TdwsCodeDOMFunctionImpl)
      .AddSubRule(function_decl)
      .AddMatchTokenType(ttSEMI)
      .AddSubRule(block)
   ;

   var property_decl := Result.NewRuleNode('property_decl', TdwsCodeDOMPropertyDecl)
      .AddMatchTokenType(ttCLASS, [ rifOptional ])
      .AddMatchTokenType(ttPROPERTY)
      .AddMatchName
      .AddSubRule(Result.NewRuleNode('property_index_decl', TdwsCodeDOMPropertyArrayDecl)
         .AddMatchTokenType(ttALEFT)
         .AddSubRule(parameter_decl)
         .AddMatchTokenType(ttSEMI, [ rifOptional, rifGoToStep1 ])
         .AddMatchTokenType(ttARIGHT)
         , [ rifOptional ])
      .AddMatchTokenType(ttCOLON, [ rifEndIfNotPresent ])
      .AddSubRule(reference)
      .AddSubRule(Result.NewRuleNode('property_read_decl', TdwsCodeDOMPropertyReadDecl)
         .AddMatchTokenType(ttREAD)
         .AddSubRule(expression)
         , [ rifOptional ])
      .AddSubRule(Result.NewRuleNode('property_write_decl', TdwsCodeDOMPropertyWriteDecl)
         .AddMatchTokenType(ttWRITE)
         .AddSubRule(expression)
         , [ rifOptional ])
   ;

   var type_inner_decl := Result.NewRuleNode('type_inner_decl', TdwsCodeDOMNode)
      .AddSubRule(Result.NewRuleAlternative('type_inner_decl_alt')
         .AddSubRule(property_decl)
         .AddSubRule(function_impl)
         .AddSubRule(function_decl)
         .AddSubRule(var_decl)
         .AddSubRule(block)
         , [ rifOptional ])
      .AddMatchTokenType(ttSEMI, [ rifOptional, rifRestart ])
   ;

   var type_visibility_section := Result.NewRuleNode('class_type_visib_section', TdwsCodeDOMTypeVisibilitySection)
      .AddMatchTokenTypes([ ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED ])
      .AddSubRule(type_inner_decl)
   ;

   var class_type_fwd := Result.NewRuleNode('class_type_fwd', TdwsCodeDOMClassFwd)
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

   var class_type_body := Result.NewRuleNode('class_type_body', TdwsCodeDOMClassBody)
      .AddSubRule(type_inner_decl, [ rifOptional ])
      .AddSubRule(type_visibility_section, [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttEND)
   ;

   var class_type_decl := Result.NewRuleNode('class_type_decl', TdwsCodeDOMClassDecl)
      .AddSubRule(class_type_fwd)
      .AddSubRule(Result.NewRuleAlternative('class_body_alt')
         .AddSubRule(Result.NewRuleNode('class_inh_body', TdwsCodeDOMNode)
            .AddSubRule(class_type_inh)
            .AddSubRule(class_type_body)
         )
         .AddSubRule(class_type_inh)
         .AddSubRule(class_type_body)
      )
   ;

   var class_of_decl := Result.NewRuleNode('class_of_decl', TdwsCodeDOMClassOfDecl)
      .AddMatchTokenType(ttCLASS)
      .AddMatchTokenType(ttOF)
      .AddSubRule(reference)
   ;

   var interface_type_decl := Result.NewRuleNode('interface_type_decl', TdwsCodeDOMInterfaceDecl)
      .AddMatchTokenType(ttINTERFACE)
      .AddMatchTokenType(ttEND)
   ;

   var array_type_range_num := Result.NewRuleNode('array_type_range_num', TdwsCodeDOMArrayRangeNum)
      .AddMatchTokenType(ttALEFT)
      .AddSubRule(range)
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttARIGHT)
   ;
   var array_type_range_type := Result.NewRuleNode('array_type_range_type', TdwsCodeDOMArrayRangeType)
      .AddMatchTokenType(ttALEFT)
      .AddSubRule(reference)
      .AddMatchTokenType(ttARIGHT)
   ;

   var array_type_decl := Result.NewRuleNode('array_type_decl', TdwsCodeDOMArrayDecl)
      .AddMatchTokenType(ttARRAY)
      .AddSubRule(Result.NewRuleAlternative('array_type_range')
         .AddSubRule(array_type_range_type)
         .AddSubRule(array_type_range_num)
      , [ rifOptional ])
      .AddMatchTokenType(ttOF)
      .AddSubRule(type_decl_type)
   ;

   var enum_type_elements := Result.NewRuleNode('enum_type_elements', TdwsCodeDOMEnumElements)
      .AddMatchName
      .AddSubRule(Result.NewRuleNode('enum_type_element_value', TdwsCodeDOMEnumElementValue)
         .AddMatchTokenType(ttEQ)
         .AddSubRule(expression),
         [ rifOptional ])
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifRestart ])
   ;
   var enum_type_decl := Result.NewRuleNode('enum_type_decl', TdwsCodeDOMEnumDecl)
      .AddMatchTokenTypes([ ttENUM, ttFLAGS ], [ rifOptional ])
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(enum_type_elements)
      .AddMatchTokenType(ttBRIGHT)
   ;

   type_decl_type
      .AddSubRule(class_of_decl)
      .AddSubRule(class_type_decl)
      .AddSubRule(class_type_fwd)
      .AddSubRule(interface_type_decl)
      .AddSubRule(array_type_decl)
      .AddSubRule(enum_type_decl)
      .AddSubRule(reference)
   ;
   var type_decl := Result.NewRuleNode('type_decl', TdwsCodeDOMTypeDecl)
      .AddMatchName
      .AddMatchTokenType(ttEQ)
      .AddSubRule(type_decl_type)
   ;

   var type_decl_inline := Result.NewRuleNode('type_inline', TdwsCodeDOMTypeSection)
      .AddMatchTokenType(ttTYPE)
      .AddSubRule(type_decl)
   ;

   var instructions := Result.NewRuleAlternative('instructions')
      .AddSubRule(Result.NewRuleNode('flow_control', TdwsCodeDOMInstruction)
         .AddMatchTokenTypes([ ttCONTINUE, ttBREAK ])
      ).AddSubRule(Result.NewRuleNode('exit', TdwsCodeDOMInstruction)
         .AddMatchTokenType(ttEXIT)
         .AddMatchTokenType(ttBLEFT, [ rifEndIfNotPresent ])
         .AddSubRule(expression, [ rifOptional ])
         .AddMatchTokenType(ttBRIGHT)
      ).AddSubRule(Result.NewRuleNode('raise', TdwsCodeDOMInstruction)
         .AddMatchTokenType(ttRaise)
         .AddSubRule(expression, [ rifOptional ])
      )
   ;

   var on_clause := Result.NewRuleNode('on_clause', TdwsCodeDOMExceptOnClause)
      .AddMatchTokenType(ttON)
      .AddMatchName
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(reference)
      .AddMatchTokenType(ttDO)
      .AddSubRule(statement, [ rifOptional ])
   ;

   var try_except_finally := Result.NewRuleNode('try', TdwsCodeDOMTryExceptFinally)
      .AddMatchTokenType(ttTRY)
      .AddSubRule(statementList, [ rifOptional ])
      .AddMatchTokenTypes([ ttEXCEPT, ttFINALLY ])
      .AddSubRule(statementList, [ rifOptional ])
      .AddMatchTokenType(ttEND)
   ;

   statement
      .AddSubRule(switch)
      .AddSubRule(comment)
      .AddSubRule(if_then_else_stmt)
      .AddSubRule(repeat_until)
      .AddSubRule(while_do)
      .AddSubRule(case_of)
      .AddSubRule(for_loop)
      .AddSubRule(for_in)
      .AddSubRule(block)
      .AddSubRule(var_inline)
      .AddSubRule(const_inline)
      .AddSubRule(function_impl)
      .AddSubRule(function_decl)
      .AddSubRule(type_decl_inline)
      .AddSubRule(instructions)
      .AddSubRule(try_except_finally)
      .AddSubRule(on_clause)
      .AddSubRule(assignment)
      .AddSubRule(expression)
   ;

   statementList
      .AddSubRule(statement, [ rifEndIfNotPresent ])
      .AddMatchTokenType(ttSEMI, [ rifOptional, rifRestart ])
   ;

end;

end.
