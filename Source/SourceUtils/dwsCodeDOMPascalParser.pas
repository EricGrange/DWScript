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
   System.Classes, System.SysUtils,
   dwsUtils, dwsTokenTypes,
   dwsCodeDOM, dwsCodeDOMNodes, dwsCodeDOMParser,
   dwsPascalTokenizer;

type

   TdwsCodeDOMPascalParser = class
      public
         function CreateRules : TdwsParserRules;
   end;

const
   cAssignments = [ ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN,
                    ttTIMES_ASSIGN, ttDIVIDE_ASSIGN ];

   cUnaryOperators = [ ttPLUS, ttMINUS, ttNOT, ttAT, ttOLD, ttPLUS_PLUS, ttMINUS_MINUS ];
   cMultOperators = [ ttTIMES, ttDIVIDE, ttMOD, ttDIV, ttAND,
                      ttCARET, ttAS, ttLESS_LESS, ttGTR_GTR, ttQUESTION_QUESTION,
                      ttSHL, ttSHR, ttSAR ];
   cAddOperators = [ ttPLUS, ttMINUS, ttOR, ttXOR, ttNOT ];
   cComparisonOperators = [ ttEQ, ttNOT_EQ, ttEQ_EQ, ttEXCL_EQ, ttEQ_EQ_EQ,
                            ttLESS, ttLESS_EQ, ttGTR, ttGTR_EQ,
                            ttIN, ttIS, ttIMPLEMENTS, ttIMPLIES,
                            ttPLUS_PLUS, ttMINUS_MINUS ];

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

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

   var statement := Result.NewRuleAlternative('statement');
   var statement_list := Result.NewRuleNode('statement_list', TdwsCodeDOMStatementList, [ prfReplaceBySingleChild ]);

   var tuple := Result.NewRuleNode('tuple', TdwsCodeDOMTuple)
      .AddSubRule(expression)
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifRestart ]);

   var named_tuple := Result.NewRuleNode('named_tuple', TdwsCodeDOMNamedTuple)
      .AddMatchName
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(expression)
      .AddMatchTokenType(ttSEMI, [ rifOptional, rifRestart ])
   ;

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

   var deprecated_qualifier := Result.NewRuleNode('deprecated_qualifier', TdwsCodeDOMDeprecatedQualifier)
      .AddMatchTokenType(ttDEPRECATED)
      .AddMatchTokenType(ttStrVal, [ rifOptional ])
   ;
   var external_qualifier := Result.NewRuleNode('external_qualifier', TdwsCodeDOMExternalQualifier)
      .AddMatchTokenType(ttEXTERNAL)
      .AddMatchTokenType(ttStrVal, [ rifOptional ])
      .AddMatchTokenType(ttPROPERTY, [ rifOptional ])
   ;
   var helper_qualifier := Result.NewRuleNode('helper_qualifier', TdwsCodeDOMHelperQualifier)
      .AddMatchTokenType(ttHELPER)
      .AddMatchName('', [ rifOptional ])
   ;
   var qualifier := Result.NewRuleNode('qualifier', TdwsCodeDOMFunctionQualifier)
      .AddMatchTokenType(ttSEMI)
      .AddSubRule(Result.NewRuleAlternative('qualifier_alt')
         .AddMatchTokenTypes([
            ttVIRTUAL, ttABSTRACT, ttOVERRIDE, ttSTATIC,
            ttOVERLOAD, ttREINTRODUCE, ttFORWARD, ttEMPTY, ttDEFAULT,
            ttSAFECALL, ttSTDCALL, ttCDECL, ttREGISTER, ttPASCAL, ttINLINE
            ])
         .AddSubRule(external_qualifier)
         .AddSubRule(helper_qualifier)
      )
   ;
   var var_qualifier := Result.NewRuleNode('var_qualifier', TdwsCodeDOMVarQualifier)
      .AddMatchTokenType(ttSEMI)
      .AddSubRule(external_qualifier)
   ;

   var type_decl_type := Result.NewRuleAlternative('type_decl_type');


   var generic_constraint := Result.NewRuleNode('generic_contraint', TdwsCodeDOMTypeGenericConstraint)
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(Result.NewRuleAlternative('generic_contraint_alt')
         .AddMatchTokenType(ttRECORD)
         .AddMatchTokenType(ttCLASS)
         .AddMatchName
      )
   ;
   var generic_parameters := Result.NewRuleNode('generic_parameters', TdwsCodeDOMTypeGenericParameters)
      .AddMatchTokenType(ttLESS, [ rifEndIfNotPresent ])
      .AddSubRule(name_list)
      .AddSubRule(generic_constraint, [ rifOptional ])
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttGTR)
   ;

   var type_name := Result.NewRuleNode('type_name', TdwsCodeDOMTypeGeneric, [ prfReplaceBySingleChild ])
      .AddSubRule(reference)
      .AddSubRule(generic_parameters, [ rifEndIfNotPresent ])
      .AddMatchTokenType(ttDOT, [ rifOptional, rifRestart ])
   ;

   var var_decl := Result.NewRuleNode('var_decl', TdwsCodeDOMVarDeclaration)
      .AddSubRule(name_list)
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(type_decl_type)
      .AddSubRule(Result.NewRuleNode('var_decl_assign', TdwsCodeDOMNode)
         .AddMatchTokenTypes([ ttEQ, ttASSIGN ])
         .AddSubRule(expression)
         , [ rifOptional ])
      .AddSubRule(var_qualifier, [ rifOptional, rifRepeat ])
   ;

   var var_infer  := Result.NewRuleNode('var_infer', TdwsCodeDOMVarDeclaration)
      .AddMatchName
      .AddMatchTokenTypes([ ttASSIGN, ttEQ ])
      .AddSubRule(expression)
      .AddSubRule(var_qualifier, [ rifOptional, rifRepeat ])
   ;

   var var_decl_or_infer := Result.NewRuleAlternative('var_decl_or_infer')
      .AddSubRule(var_decl)
      .AddSubRule(var_infer)
   ;

   var var_section_line := Result.NewRuleNode('var_section_line', TdwsCodeDOMVarSection)
      .AddSubRule(var_decl_or_infer).AddMatchTokenType(ttSEMI)
   ;

   var var_section := Result.NewRuleNode('var_section', TdwsCodeDOMVarSection)
      .AddMatchTokenType(ttVAR)
      .AddSubRule(var_section_line)
      .AddSubRule(var_section_line, [ rifOptional, rifRepeat ])
   ;

   var var_inline := Result.NewRuleNode('var_inline', TdwsCodeDOMVarSection)
      .AddMatchTokenType(ttVAR)
      .AddSubRule(var_decl_or_infer)
   ;

   var const_decl := Result.NewRuleNode('const_decl', TdwsCodeDOMConstDeclaration)
      .AddMatchName
      .AddSubRule(Result.NewRuleNode('const_type_decl', TdwsCodeDOMConstDeclarationType)
         .AddMatchTokenType(ttCOLON)
         .AddSubRule(type_decl_type)
         , [ rifOptional ])
      .AddMatchTokenTypes([ ttEQ, ttASSIGN ], [ rifEndIfNotPresent ])
      .AddSubRule(expression)
      .AddSubRule(deprecated_qualifier, [ rifOptional ])
   ;

   var const_inline := Result.NewRuleNode('const_inline', TdwsCodeDOMConstSection)
      .AddMatchTokenType(ttCONST)
      .AddSubRule(const_decl)
   ;

   var const_section_line := Result.NewRuleNode('const_section_line', TdwsCodeDOMConstSection)
      .AddSubRule(const_decl)
      .AddMatchTokenType(ttSEMI)
   ;

   var const_section := Result.NewRuleNode('const_section', TdwsCodeDOMConstSection)
      .AddMatchTokenType(ttCONST)
      .AddSubRule(const_section_line)
      .AddSubRule(const_section_line, [ rifOptional, rifRepeat ])
   ;

   var resourcestring_line := Result.NewRuleNode('resourcestring', TdwsCodeDOMResourceString)
      .AddMatchName
      .AddMatchTokenType(ttEQ)
      .AddMatchTokenType(ttStrVal)
   ;

   var resourcestring_section := Result.NewRuleNode('resourcestring_section', TdwsCodeDOMResourceStringSection)
      .AddMatchTokenType(ttRESOURCESTRING)
      .AddSubRule(resourcestring_line, [ rifOptional ])
      .AddMatchTokenType(ttSEMI, [ rifOptional, rifGoToStep1 ])
   ;

   var resourcestring_inline := Result.NewRuleNode('resourcestring_inline', TdwsCodeDOMResourceString)
      .AddMatchTokenType(ttRESOURCESTRING)
      .AddSubRule(resourcestring_line)
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
      .AddSubRule(Result.NewRuleAlternative('parenthesis_alt')
         .AddSubRule(named_tuple)
         .AddSubRule(tuple)
      ).AddMatchTokenType(ttBRIGHT)
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
      .AddSubRule(case_of_alternative_cases, [ rifOptional, rifMergeChildren ])
      .AddMatchTokenType(ttARIGHT)
   ;

   var unary  := Result.NewRuleNode('unary', TdwsCodeDOMUnaryOperator)
      .AddMatchTokenTypes(cUnaryOperators)
      .AddSubRule(expression)
   ;

   var new  := Result.NewRuleNode('new', TdwsCodeDOMNew)
      .AddMatchTokenType(ttNEW)
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

   var parameters := Result.NewRuleNode('parameters', TdwsCodeDOMParameters)
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(tuple, [ rifOptional, rifMergeChildren ])
      .AddMatchTokenType(ttBRIGHT)
   ;
   var indexes := Result.NewRuleNode('indexes', TdwsCodeDOMIndexes)
      .AddMatchTokenType(ttALEFT)
      .AddSubRule(tuple, [ rifMergeChildren ])
      .AddMatchTokenType(ttARIGHT)
   ;

   var call_inherited := Result.NewRuleNode('call_inherited', TdwsCodeDOMCallInherited)
      .AddMatchTokenType(ttINHERITED)
      .AddSubRule(reference, [ rifOptional ])
      .AddSubRule(parameters, [ rifOptional, rifRepeat, rifMergeChildren ])
   ;

   var lambda_parameters := Result.NewRuleNode('lambda_parameters', TdwsCodeDOMLambdaParameters)
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(Result.NewRuleAlternative('lambda_parameters_alt')
         .AddSubRule(var_decl)
         .AddMatchName
      )
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttBRIGHT)
   ;

   var lambda := Result.NewRuleNode('lambda', TdwsCodeDOMLambda)
      .AddMatchTokenType(ttLAMBDA)
      .AddSubRule(lambda_parameters, [ rifOptional ])
      .AddSubRule(Result.NewRuleAlternative('lambda_alt')
         .AddSubRule(Result.NewRuleNode('lambda_expr', TdwsCodeDOMLambdaExpression)
            .AddMatchTokenType(ttEQ_GTR)
            .AddSubRule(expression)
         ).AddSubRule(Result.NewRuleNode('lambda_stmt', TdwsCodeDOMLambdaStatements)
            .AddSubRule(statement_list)
            .AddMatchTokenType(ttEND)
         )
      )
   ;

   var record_type_decl := Result.NewRuleNode('record_type_decl', TdwsCodeDOMRecordDecl);

   var term_root := Result.NewRuleAlternative('term_root')
      .AddSubRule(parenthesis)
      .AddSubRule(brackets)
      .AddSubRule(unary)
      .AddSubRule(new)
      .AddSubRule(literal)
      .AddSubRule(if_then_else_expr)
      .AddSubRule(call_inherited)
      .AddSubRule(lambda)
      .AddSubRule(record_type_decl)
      .AddSubRule(type_name)
      .AddSubRule(reference)
   ;

   var composite := Result.NewRuleWrap('composite');

   var call := Result.NewRuleNode('call', TdwsCodeDOMCall)
      .AddSubRule(parameters)
   ;
   var field := Result.NewRuleNode('field', TdwsCodeDOMField)
      .AddMatchTokenType(ttDOT)
      .AddSubRule(composite)
   ;
   var index := Result.NewRuleNode('index', TdwsCodeDOMIndex)
      .AddSubRule(indexes)
   ;

   composite
      .AddSubRule(term_root)
      .AddSubRule(call, [ rifRepeat ])
      .AddSubRule(field, [ rifRepeat ])
      .AddSubRule(index, [ rifRepeat ])
   ;

   var term := Result.NewRuleNode('term', TdwsCodeDOMTerm, [ prfReplaceBySingleChild ])
      .AddSubRule(composite)
   ;

   var exprmult := Result.NewRuleNode('expr_mult', TdwsCodeDOMBinaryOperator, [ prfReplaceBySingleChild ]);
   exprmult
      .AddSubRule(term)
      .AddMatchTokenTypes(cMultOperators, [ rifEndIfNotPresent ])
      .AddSubRule(exprmult)
   ;

   var expradd := Result.NewRuleNode('expr_add', TdwsCodeDOMBinaryOperator, [ prfReplaceBySingleChild ]);
   expradd
      .AddSubRule(exprmult)
      .AddSubRule(Result.NewRuleAlternative('expr_add_alt')
         .AddMatchTokenTypePair(ttNOT, ttIN)
         .AddMatchTokenTypes(cAddOperators)
      , [ rifEndIfNotPresent ])
      .AddSubRule(expradd)
   ;

   var exprcmp := Result.NewRuleNode('expr_cmp', TdwsCodeDOMBinaryOperator, [ prfReplaceBySingleChild ]);
   exprcmp
      .AddSubRule(expradd)
      .AddMatchTokenTypes(cComparisonOperators, [ rifEndIfNotPresent ])
      .AddSubRule(exprcmp)
   ;

   expression
      .AddSubRule(exprcmp)
   ;

   var assignment := Result.NewRuleNode('assignment', TdwsCodeDOMAssignment)
      .AddSubRule(term)
      .AddMatchTokenTypes(cAssignments)
      .AddSubRule(expression)
   ;

   var implementation_section_node := Result.NewRuleAlternative('implementation_section_node');

   var if_then_else_stmt := Result.NewRuleNode('if_then_else_stmt', TdwsCodeDOMIfThenElseStmt)
      .AddMatchTokenType(ttIF)
      .AddSubRule(expression)
      .AddMatchTokenType(ttTHEN)
      .AddSubRule(statement, [ rifOptional ])
      .AddMatchTokenType(ttELSE, [ rifEndIfNotPresent ])
      .AddSubRule(statement, [ rifOptional ])
   ;

   var repeat_until := Result.NewRuleNode('repeat_until', TdwsCodeDOMRepeat)
      .AddMatchTokenType(ttREPEAT)
      .AddSubRule(statement_list)
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
      .AddSubRule(statement_list, [ rifOptional ])
      .AddMatchTokenType(ttEND)
   ;

   var asm_block := Result.NewRuleNode('asm', TdwsCodeDOMAsmBlock)
      .AddMatchTokenType(ttASM)
      .AddMatchAnyExceptTokenTypes([ ttEND ], [ rifOptional, rifRepeat ])
      .AddMatchTokenType(ttEND)
   ;

   var with_block := Result.NewRuleNode('with', TdwsCodeDOMWith)
      .AddMatchTokenType(ttWITH)
      .AddSubRule(var_decl_or_infer)
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttDO)
      .AddSubRule(statement, [ rifOptional ])
   ;

   var case_of_alternative := Result.NewRuleNode('case_of_alternative', TdwsCodeDOMCaseOfAlternative)
      .AddSubRule(case_of_alternative_cases)
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(statement, [ rifOptional ])
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
         .AddSubRule(statement_list)
         , [ rifOptional, rifMergeChildren ])
      .AddMatchTokenType(ttEND)
   ;

   var for_step := Result.NewRuleNode('for_loop_step', TdwsCodeDOMForLoopStep)
      .AddMatchName('step')
      .AddSubRule(expression)
   ;

   var for_loop := Result.NewRuleNode('for_loop', TdwsCodeDOMForLoop)
      .AddMatchTokenType(ttFOR)
      .AddMatchTokenType(ttVAR, [ rifOptional ])
      .AddMatchName
      .AddMatchTokenType(ttASSIGN)
      .AddSubRule(expression)
      .AddMatchTokenTypes([ ttTO, ttDOWNTO ])
      .AddSubRule(expression)
      .AddSubRule(for_step, [ rifOptional ])
      .AddMatchTokenType(ttDO)
      .AddSubRule(statement, [ rifOptional ]);

   var for_in := Result.NewRuleNode('for_in', TdwsCodeDOMForIn)
      .AddMatchTokenType(ttFOR)
      .AddMatchTokenType(ttVAR, [ rifOptional ])
      .AddMatchName
      .AddMatchTokenType(ttIN)
      .AddSubRule(expression)
      .AddSubRule(for_step, [ rifOptional ])
      .AddMatchTokenType(ttDO)
      .AddSubRule(statement, [ rifOptional ]);

   var parameter_decl := Result.NewRuleNode('parameter_decl', TdwsCodeDOMParameterDecl)
      .AddMatchTokenTypes([ ttCONST, ttVAR, ttLAZY ], [ rifOptional ])
      .AddSubRule(name_list)
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(type_decl_type)
      .AddMatchTokenType(ttEQ, [ rifEndIfNotPresent ])
      .AddSubRule(expression)
   ;
   var parameter_decl_list := Result.NewRuleNode('parameter_decl_list', TdwsCodeDOMParameterDeclList)
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(parameter_decl, [ rifOptional ])
      .AddMatchTokenType(ttSEMI, [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttBRIGHT)
   ;

   var contract_description := Result.NewRuleNode('contract_description', TdwsCodeDOMContractDescription)
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(expression)
   ;
   var contract_clause := Result.NewRuleNode('contract_clause', TdwsCodeDOMContractClause)
      .AddSubRule(expression)
      .AddSubRule(contract_description, [ rifOptional ])
      .AddMatchTokenType(ttSEMI)
   ;
   var require_section := Result.NewRuleNode('require', TdwsCodeDOMContractRequire)
      .AddMatchTokenType(ttREQUIRE)
      .AddSubRule(contract_clause)
      .AddSubRule(contract_clause, [ rifOptional, rifRepeat ])
   ;
   var ensure_section := Result.NewRuleNode('ensure', TdwsCodeDOMContractEnsure)
      .AddMatchTokenType(ttENSURE)
      .AddSubRule(contract_clause)
      .AddSubRule(contract_clause, [ rifOptional, rifRepeat ])
   ;

   var function_block := Result.NewRuleNode('function_block', TdwsCodeDOMFunctionBlock)
      .AddMatchTokenType(ttBEGIN)
      .AddSubRule(statement_list, [ rifOptional ])
      .AddSubRule(ensure_section, [ rifOptional ])
      .AddMatchTokenType(ttEND)
   ;

   var function_decl := Result.NewRuleNode('function_decl', TdwsCodeDOMFunctionDecl)
      .AddMatchTokenType(ttCLASS, [ rifOptional ])
      .AddMatchTokenTypes([ ttPROCEDURE, ttFUNCTION, ttMETHOD, ttCONSTRUCTOR, ttDESTRUCTOR ])
      .AddSubRule(type_name)
      .AddSubRule(parameter_decl_list, [ rifOptional ])
      .AddSubRule(Result.NewRuleNode('function_return_decl', TdwsCodeDOMFunctionReturnDecl)
         .AddMatchTokenType(ttCOLON)
         .AddSubRule(type_decl_type)
         , [ rifOptional ])
      .AddSubRule(qualifier, [ rifOptional, rifRepeat ])
   ;

   var function_type_decl := Result.NewRuleNode('function_type_decl', TdwsCodeDOMFunctionDecl)
      .AddMatchTokenType(ttCLASS, [ rifOptional ])
      .AddMatchTokenTypes([ ttPROCEDURE, ttFUNCTION, ttMETHOD, ttCONSTRUCTOR, ttDESTRUCTOR ])
      .AddSubRule(parameter_decl_list, [ rifOptional ])
      .AddSubRule(Result.NewRuleNode('function_return_decl', TdwsCodeDOMFunctionReturnDecl)
         .AddMatchTokenType(ttCOLON)
         .AddSubRule(type_decl_type)
         , [ rifOptional ])
      .AddMatchTokenTypePair(ttOF, ttOBJECT, [ rifOptional ])
      .AddSubRule(qualifier, [ rifOptional, rifRepeat ])
   ;

   var function_impl := Result.NewRuleNode('function_impl', TdwsCodeDOMFunctionImpl)
      .AddSubRule(function_decl)
      .AddSubRule(qualifier, [ rifOptional, rifRepeat ])
      .AddMatchTokenType(ttSEMI)
      .AddSubRule(require_section, [ rifOptional ])
      .AddSubRule(implementation_section_node, [ rifOptional, rifRepeat ])
      .AddSubRule(require_section, [ rifOptional ])
      .AddSubRule(function_block)
   ;

   var property_write_decl := Result.NewRuleNode('property_write_decl', TdwsCodeDOMPropertyWriteDecl)
      .AddMatchTokenType(ttWRITE)
      .AddSubRule(Result.NewRuleAlternative('property_write_decl_alt')
         .AddSubRule(Result.NewRuleNode('property_write_statement', TdwsCodeDOMPropertyWriteStmt)
            .AddMatchTokenType(ttBLEFT)
            .AddSubRule(statement)
            .AddMatchTokenType(ttBRIGHT)
         )
         .AddSubRule(reference)
      )
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
         , [ rifOptional ]
      ).AddMatchTokenType(ttCOLON, [ rifEndIfNotPresent ])
      .AddSubRule(type_decl_type)
      .AddMatchTokenTypePair(ttINDEX, ttIntVal, [ rifOptional ])
      .AddSubRule(Result.NewRuleNode('property_read_decl', TdwsCodeDOMPropertyReadDecl)
         .AddMatchTokenType(ttREAD)
         .AddSubRule(expression)
         , [ rifOptional ]
      ).AddSubRule(property_write_decl, [ rifOptional ])
      .AddMatchTokenType(ttREINTRODUCE, [ rifOptional ])
      .AddMatchTokenTypePair(ttSEMI, ttDEFAULT, [ rifOptional ])
   ;

   var class_var_decl := Result.NewRuleNode('class_var_decl', TdwsCodeDOMClassVar)
      .AddMatchTokenType(ttCLASS)
      .AddMatchTokenType(ttVAR)
      .AddSubRule(var_decl_or_infer)
   ;
   var class_const_decl := Result.NewRuleNode('class_const_decl', TdwsCodeDOMClassConst)
      .AddMatchTokenType(ttCLASS, [ rifOptional ])
      .AddMatchTokenType(ttCONST)
      .AddSubRule(const_decl)
   ;

   var class_operator_decl := Result.NewRuleNode('class_operator', TdwsCodeDOMClassOperator)
      .AddMatchTokenType(ttCLASS)
      .AddMatchTokenType(ttOPERATOR)
      .AddMatchTokenTypes(cAddOperators + cMultOperators + cComparisonOperators + cAssignments)
      .AddSubRule(type_decl_type)
      .AddMatchTokenType(ttUSES)
      .AddMatchName
   ;

   var type_decl_inline := Result.NewRuleNode('type_inline', TdwsCodeDOMTypeInline);

   var field_decl := Result.NewRuleNode('field_decl', TdwsCodeDOMFieldDeclaration)
      .AddSubRule(name_list)
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(type_decl_type)
      .AddSubRule(Result.NewRuleNode('field_decl_assign', TdwsCodeDOMNode)
         .AddMatchTokenTypes([ ttEQ, ttASSIGN ])
         .AddSubRule(expression)
         , [ rifOptional ])
      .AddSubRule(Result.NewRuleNode('field_decl_external', TdwsCodeDOMNode)
         .AddMatchTokenType(ttSEMI)
         .AddSubRule(external_qualifier)
         , [ rifOptional ])
      .AddSubRule(Result.NewRuleNode('var_qualifier_alt', TdwsCodeDOMNode)
         .AddMatchTokenType(ttSEMI)
         .AddMatchTokenTypePairPeek(ttREADONLY, ttSEMI)
         , [ rifOptional ])
   ;

   var type_inner_decl := Result.NewRuleNode('type_inner_decl', TdwsCodeDOMTypeInnerDecl)
      .AddSubRule(Result.NewRuleAlternative('type_inner_decl_alt')
         .AddSubRule(property_decl)
         .AddSubRule(function_impl)
         .AddSubRule(function_decl)
         .AddSubRule(class_var_decl)
         .AddSubRule(class_const_decl)
         .AddSubRule(class_operator_decl)
         .AddSubRule(field_decl)
         .AddSubRule(var_infer)
         .AddSubRule(type_decl_inline)
         , [ rifOptional ])
      .AddMatchTokenType(ttSEMI, [ rifOptional, rifRestart ])
   ;

   var type_visibility_section := Result.NewRuleNode('class_type_visib_section', TdwsCodeDOMTypeVisibilitySection)
      .AddMatchTokenTypes([ ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED ])
      .AddSubRule(type_inner_decl, [ rifOptional ])
   ;

   var class_type_fwd := Result.NewRuleNode('class_type_fwd', TdwsCodeDOMClassFwd)
      .AddMatchTokenTypes([ ttPARTIAL, ttSTATIC ], [ rifOptional ])
      .AddMatchTokenType(ttCLASS)
      .AddMatchTokenType(ttSTATIC, [ rifOptional ])
      .AddMatchTokenTypes([ ttABSTRACT, ttSEALED ], [ rifOptional ])
      .AddSubRule(external_qualifier, [ rifOptional ])
      .AddMatchTokenType(ttPARTIAL, [ rifOptional ])
   ;
   var class_type_inh := Result.NewRuleNode('class_type_inh', TdwsCodeDOMClassInh)
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(type_name)
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
      .AddMatchTokenTypePair(ttCLASS, ttOF)
      .AddSubRule(reference)
   ;

   var interface_decl_inh := Result.NewRuleNode('interface_decl_inh', TdwsCodeDOMClassInh)
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(reference)
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttBRIGHT)
   ;
   var interface_decl_body := Result.NewRuleNode('interface_decl_body', TdwsCodeDOMInterfaceDeclBody)
      .AddSubRule(Result.NewRuleAlternative('interface_decl_body_alt')
         .AddSubRule(property_decl)
         .AddSubRule(function_impl)
         .AddSubRule(function_decl)
         , [ rifOptional ]
      ).AddMatchTokenType(ttSEMI, [ rifOptional, rifRestart ])
   ;

   var interface_type_decl := Result.NewRuleNode('interface_type_decl', TdwsCodeDOMInterfaceDecl)
      .AddMatchTokenType(ttINTERFACE)
      .AddSubRule(interface_decl_inh, [ rifOptional ])
      .AddSubRule(interface_decl_body, [ rifOptional ])
      .AddMatchTokenType(ttEND)
   ;

   record_type_decl
      .AddMatchTokenType(ttRECORD)
      .AddSubRule(type_inner_decl, [ rifOptional ])
      .AddSubRule(type_visibility_section, [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttEND)
   ;

   var array_type_range := Result.NewRuleNode('array_type_range_num', TdwsCodeDOMArrayRange)
      .AddMatchTokenType(ttALEFT)
      .AddSubRule(Result.NewRuleAlternative('array_type_range_alt')
         .AddSubRule(range)
         .AddSubRule(reference)
      )
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttARIGHT)
   ;

   var array_type_decl := Result.NewRuleNode('array_type_decl', TdwsCodeDOMArrayDecl)
      .AddMatchTokenType(ttARRAY)
      .AddSubRule(array_type_range, [ rifOptional ])
      .AddMatchTokenType(ttOF)
      .AddSubRule(Result.NewRuleAlternative('array_element_type')
         .AddMatchTokenType(ttCONST)
         .AddSubRule(type_decl_type)
      )
   ;

   var enum_type_elements := Result.NewRuleNode('enum_type_elements', TdwsCodeDOMEnumElements)
      .AddMatchName
      .AddSubRule(deprecated_qualifier, [ rifOptional ])
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

   var set_type_decl := Result.NewRuleNode('set_type_decl', TdwsCodeDOMSetDecl)
      .AddMatchTokenTypePair(ttSET, ttOF)
      .AddSubRule(type_decl_type)
   ;

   var helper_decl_body := Result.NewRuleNode('helper_decl_body', TdwsCodeDOMHelperBody)
      .AddSubRule(Result.NewRuleAlternative('helper_body_alt')
         .AddSubRule(property_decl)
         .AddSubRule(function_impl)
         .AddSubRule(function_decl)
         .AddSubRule(class_var_decl)
         .AddSubRule(class_const_decl)
         , [ rifOptional ]
      ).AddMatchTokenType(ttSEMI, [ rifOptional, rifRestart ])
   ;

   var helper_decl := Result.NewRuleNode('helper_decl', TdwsCodeDOMHelperDecl)
      .AddMatchTokenType(ttSTRICT, [ rifOptional ])
      .AddMatchTokenTypes([ ttCLASS, ttRECORD ], [ rifOptional ])
      .AddMatchTokenTypePair(ttHELPER, ttFOR)
      .AddSubRule(type_decl_type)
      .AddSubRule(helper_decl_body, [ rifOptional ])
      .AddMatchTokenType(ttEND)
   ;

   type_decl_type
      .AddSubRule(helper_decl)
      .AddSubRule(class_of_decl)
      .AddSubRule(class_type_decl)
      .AddSubRule(class_type_fwd)
      .AddSubRule(interface_type_decl)
      .AddSubRule(record_type_decl)
      .AddSubRule(array_type_decl)
      .AddSubRule(enum_type_decl)
      .AddSubRule(set_type_decl)
      .AddSubRule(function_type_decl)
      .AddSubRule(type_name)
   ;
   var type_decl := Result.NewRuleNode('type_decl', TdwsCodeDOMTypeDecl)
      .AddSubRule(type_name)
      .AddMatchTokenType(ttEQ)
      .AddSubRule(type_decl_type)
   ;

   type_decl_inline
      .AddMatchTokenType(ttTYPE)
      .AddSubRule(type_decl)
   ;

   var type_section := Result.NewRuleNode('type_section', TdwsCodeDOMTypeSection)
      .AddMatchTokenType(ttTYPE)
      .AddSubRule(type_decl, [ rifOptional ])
      .AddMatchTokenType(ttSEMI, [ rifOptional, rifGoToStep1 ])
   ;

   var instructions := Result.NewRuleAlternative('instructions')
      .AddSubRule(Result.NewRuleNode('flow_control', TdwsCodeDOMInstruction)
         .AddMatchTokenTypes([ ttCONTINUE, ttBREAK ])
      ).AddSubRule(Result.NewRuleNode('exit', TdwsCodeDOMInstruction)
         .AddMatchTokenType(ttEXIT)
         .AddSubRule(expression, [ rifOptional ])
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

   var try_except_finally := Result.NewRuleNode('try', TdwsCodeDOMTry)
      .AddMatchTokenType(ttTRY)
      .AddSubRule(statement_list, [ rifOptional ])
      .AddSubRule(Result.NewRuleNode('try_except', TdwsCodeDOMTryExcept)
         .AddMatchTokenType(ttEXCEPT)
         .AddSubRule(statement_list, [ rifOptional ])
         .AddSubRule(Result.NewRuleNode('try_else', TdwsCodeDOMTryExceptElse)
            .AddMatchTokenType(ttELSE)
            .AddSubRule(statement_list)
         , [ rifOptional ]
      ), [ rifOptional ])
      .AddSubRule(Result.NewRuleNode('try_finally', TdwsCodeDOMTryFinally)
         .AddMatchTokenType(ttFINALLY)
         .AddSubRule(statement_list, [ rifOptional ])
         , [ rifOptional ])
      .AddMatchTokenType(ttEND)
   ;

   var operator_decl_parameters := Result.NewRuleNode('operator_decl', TdwsCodeDOMOperatorDecl)
      .AddMatchTokenType(ttBLEFT)
      .AddSubRule(type_decl_type)
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifGoToStep1 ])
      .AddMatchTokenType(ttBRIGHT)
   ;

   var operator_decl := Result.NewRuleNode('operator_decl', TdwsCodeDOMOperatorDecl)
      .AddMatchTokenType(ttOPERATOR)
      .AddMatchTokenTypes(
           cAddOperators + cMultOperators + cComparisonOperators
         + cAssignments
         + [ ttIMPLICIT ]
      )
      .AddSubRule(operator_decl_parameters)
      .AddMatchTokenType(ttCOLON)
      .AddSubRule(type_decl_type)
      .AddMatchTokenType(ttUSES)
      .AddSubRule(reference)
   ;

   var uses_clause := Result.NewRuleNode('uses', TdwsCodeDOMUses)
      .AddMatchTokenType(ttUSES)
      .AddSubRule(reference)
      .AddMatchTokenType(ttCOMMA, [ rifOptional, rifGoToStep1 ])
   ;

   statement
      .AddSubRule(switch)
      .AddSubRule(if_then_else_stmt)
      .AddSubRule(repeat_until)
      .AddSubRule(while_do)
      .AddSubRule(case_of)
      .AddSubRule(for_loop)
      .AddSubRule(for_in)
      .AddSubRule(block)
      .AddSubRule(asm_block)
      .AddSubRule(with_block)
      .AddSubRule(var_inline)
      .AddSubRule(const_inline)
      .AddSubRule(resourcestring_inline)
      .AddSubRule(function_impl)
      .AddSubRule(function_decl)
      .AddSubRule(type_decl_inline)
      .AddSubRule(instructions)
      .AddSubRule(try_except_finally)
      .AddSubRule(on_clause)
      .AddSubRule(uses_clause)
      .AddSubRule(assignment)
      .AddSubRule(call_inherited)
      .AddSubRule(operator_decl)
      .AddSubRule(expression)
   ;

   statement_list
      .AddSubRule(statement, [ rifEndIfNotPresent ])
      .AddMatchTokenType(ttSEMI, [ rifOptional, rifRestart ])
   ;

   var uses_section_clause := Result.NewRuleNode('uses_section', TdwsCodeDOMUses)
      .AddSubRule(uses_clause, [ rifMergeChildren ])
      .AddMatchTokenType(ttSEMI, [ rifOptional ])
   ;

   var interface_section := Result.NewRuleNode('interface_section', TdwsCodeDOMMainInterface)
      .AddMatchTokenType(ttINTERFACE)
      .AddSubRule(Result.NewRuleAlternative('interface_section_alt')
         .AddSubRule(type_section)
         .AddSubRule(var_section)
         .AddSubRule(const_section)
         .AddSubRule(resourcestring_section)
         .AddSubRule(uses_section_clause)
         .AddSubRule(function_decl)
      , [ rifOptional, rifRepeat ])
      .AddMatchTokenType(ttSEMI, [ rifOptional, rifGoToStep1 ])
   ;

   implementation_section_node
      .AddSubRule(type_section)
      .AddSubRule(var_section)
      .AddSubRule(const_section)
      .AddSubRule(resourcestring_section)
      .AddSubRule(uses_section_clause)
      .AddSubRule(Result.NewRuleNode('implementation_func', TdwsCodeDOMMainImplementation)
         .AddSubRule(Result.NewRuleAlternative('implementation_func_alt')
            .AddSubRule(function_impl)
            .AddSubRule(function_decl)
         ).AddMatchTokenType(ttSEMI, [ rifOptional ])
      )
   ;
   var implementation_section := Result.NewRuleNode('implementation_section', TdwsCodeDOMMainImplementation)
      .AddMatchTokenType(ttIMPLEMENTATION)
      .AddSubRule(implementation_section_node, [ rifOptional, rifRepeat ])
   ;

   var initialization_section := Result.NewRuleNode('initialization_section', TdwsCodeDOMMainImplementation)
      .AddMatchTokenType(ttINITIALIZATION)
      .AddSubRule(statement_list, [ rifOptional ])
   ;

   var finalization_section := Result.NewRuleNode('finalization_section', TdwsCodeDOMMainImplementation)
      .AddMatchTokenType(ttFINALIZATION)
      .AddSubRule(statement_list, [ rifOptional ])
   ;

   var unit_namespace := Result.NewRuleNode('unit_namespace', TdwsCodeDOMUniteNamespace)
      .AddMatchTokenTypePair(ttUNIT, ttNAMESPACE)
      .AddSubRule(reference)
      .AddSubRule(deprecated_qualifier, [ rifOptional ])
      .AddMatchTokenType(ttSEMI)
      .AddSubRule(uses_section_clause, [ rifOptional, rifRepeat ])
   ;

   var unit_or_library := Result.NewRuleNode('unit_or_library', TdwsCodeDOMMain)
      .AddMatchTokenTypes([ ttUNIT, ttLIBRARY ])
      .AddSubRule(reference)
      .AddSubRule(deprecated_qualifier, [ rifOptional ])
      .AddMatchTokenType(ttSEMI)
      .AddSubRule(interface_section, [ rifOptional ])
      .AddSubRule(implementation_section, [ rifOptional ])
      .AddSubRule(initialization_section, [ rifOptional ])
      .AddSubRule(finalization_section, [ rifOptional ])
      .AddMatchTokenType(ttEND, [ rifOptional ])
      .AddMatchTokenTypes([ ttDOT, ttSEMI ], [ rifOptional ])
   ;

   var program_main := Result.NewRuleNode('program', TdwsCodeDOMMain)
      .AddMatchTokenType(ttPROGRAM)
      .AddSubRule(reference)
      .AddMatchTokenType(ttSEMI)
      .AddSubRule(implementation_section_node, [ rifOptional, rifRepeat ])
      .AddSubRule(statement_list, [ rifOptional ])
      .AddMatchTokenTypes([ ttDOT, ttSEMI ], [ rifOptional ])
   ;

   var script_main := Result.NewRuleNode('script', TdwsCodeDOMMain, [ prfReplaceBySingleChild ])
      .AddSubRule(statement_list)
      .AddMatchTokenTypes([ ttDOT, ttSEMI ], [ rifOptional ]);

   var root := Result.NewRuleAlternative('root', [ prfRoot ])
      .AddSubRule(unit_namespace)
      .AddSubRule(unit_or_library)
      .AddSubRule(program_main)
      .AddSubRule(script_main)
   ;

end;

end.
