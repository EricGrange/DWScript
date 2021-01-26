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
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsTokenTypes;

interface

{$I dws.inc}

type

   TTokenType =
     (
     ttNone, ttStrVal, ttIntVal, ttFloatVal, ttNAME, ttSWITCH,
     ttLAZY, ttVAR, ttCONST, ttRESOURCESTRING,
     ttTYPE, ttRECORD, ttARRAY, ttSET, ttDOT, ttDOTDOT, ttOF, ttENUM, ttFLAGS,
     ttTRY, ttEXCEPT, ttRAISE, ttFINALLY, ttON,
     ttREAD, ttWRITE, ttPROPERTY, ttDESCRIPTION,
     ttFUNCTION, ttPROCEDURE, ttCONSTRUCTOR, ttDESTRUCTOR, ttMETHOD, ttLAMBDA,
     ttASYNC, ttAWAIT,
     ttOPERATOR,
     ttCLASS, ttNIL, ttIS, ttAS, ttIMPLEMENTS, ttINDEX, ttOBJECT,
     ttVIRTUAL, ttOVERRIDE, ttREINTRODUCE, ttINHERITED, ttFINAL, ttNEW,
     ttABSTRACT, ttSEALED, ttSTATIC, ttPARTIAL, ttDEPRECATED, ttOVERLOAD,
     ttEXTERNAL, ttEXPORT, ttFORWARD, ttINLINE, ttEMPTY, ttIN,
     ttENSURE, ttREQUIRE, ttINVARIANTS, ttOLD,
     ttINTERFACE, ttIMPLEMENTATION, ttINITIALIZATION, ttFINALIZATION,
     ttHELPER, ttSTRICT,
     ttASM, ttBEGIN, ttEND, ttBREAK, ttCONTINUE, ttEXIT,
     ttIF, ttTHEN, ttELSE, ttWITH, ttWHILE, ttREPEAT, ttUNTIL, ttFOR, ttTO, ttDOWNTO, ttDO,
     ttCASE,
     ttTRUE, ttFALSE,
     ttAND, ttOR, ttXOR, ttDIV, ttMOD, ttNOT, ttSHL, ttSHR, ttSAR,
     ttPLUS, ttMINUS, ttIMPLIES, ttIMPLICIT,
     ttTIMES, ttDIVIDE, ttPERCENT, ttCARET, ttAT, ttTILDE,
     ttDOLLAR, ttEXCLAMATION, ttEXCL_EQ,
     ttQUESTION, ttQUESTION_QUESTION, ttQUESTION_DOT,
     ttEQ, ttNOT_EQ, ttGTR, ttGTR_EQ, ttLESS, ttLESS_EQ, ttEQ_GTR, ttEQ_EQ, ttEQ_EQ_EQ,
     ttLESS_LESS, ttGTR_GTR, ttPIPE, ttPIPE_PIPE, ttAMP, ttAMP_AMP,
     ttSEMI, ttCOMMA, ttCOLON,
     ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN, ttDIVIDE_ASSIGN,
     ttPERCENT_ASSIGN, ttCARET_ASSIGN, ttAT_ASSIGN, ttTILDE_ASSIGN,
     ttPLUS_PLUS, ttMINUS_MINUS, ttTIMES_TIMES,
     ttBLEFT, ttBRIGHT, ttALEFT, ttARIGHT, ttCLEFT, ttCRIGHT,
     ttDEFAULT,
     ttUSES, ttUNIT, ttNAMESPACE,
     ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED,
     ttPROGRAM, ttLIBRARY,

     // Tokens for compatibility to Delphi
     ttREGISTER, ttPASCAL, ttCDECL, ttSAFECALL, ttSTDCALL, ttFASTCALL, ttREFERENCE
     );

   TTokenTypes = set of TTokenType;

const
   cTokenStrings : array [TTokenType] of String = (
     '', 'UnicodeString Literal', 'Integer Literal', 'Float Literal', 'name', 'switch',
     'lazy', 'var', 'const', 'resourcestring',
     'type', 'record', 'array', 'set', '.', '..', 'of', 'enum', 'flags',
     'try', 'except', 'raise', 'finally', 'on',
     'read', 'write', 'property', 'description',
     'function', 'procedure', 'constructor', 'destructor', 'method', 'lambda',
     'async', 'await',
     'operator',
     'class', 'nil', 'is', 'as', 'implements', 'index', 'object',
     'virtual', 'override', 'reintroduce', 'inherited', 'final', 'new',
     'abstract', 'sealed', 'static', 'partial', 'deprecated', 'overload',
     'external', 'export', 'forward', 'inline', 'empty', 'in',
     'ensure', 'require', 'invariants', 'old',
     'interface', 'implementation', 'initialization', 'finalization',
     'helper', 'strict',
     'asm', 'begin', 'end', 'break', 'continue', 'exit',
     'if', 'then', 'else', 'with', 'while', 'repeat', 'until', 'for', 'to', 'downto', 'do',
     'case',
     'True', 'False',
     'and', 'or', 'xor', 'div', 'mod', 'not', 'shl', 'shr', 'sar',
     '+', '-', 'implies', 'implicit',
     '*', '/', '%', '^', '@', '~',
     '$', '!', '!=',
     '?', '??', '?.',
     '=', '<>', '>', '>=', '<', '<=', '=>', '==', '===',
     '<<', '>>', '|', '||', '&', '&&',
     ';', ',', ':',
     ':=', '+=', '-=', '*=', '/=',
     '%=', '^=', '@=', '~=',
     '++', '--', '**',
     '(', ')', '[', ']', '{', '}',
     'default', 'uses', 'unit', 'namespace',
     'private', 'protected', 'public', 'published',
     'program', 'library',
     'register', 'pascal', 'cdecl', 'safecall', 'stdcall', 'fastcall', 'reference'
     );

function TokenTypesToString(const tt : TTokenTypes) : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// TokenTypesToString
//
function TokenTypesToString(const tt : TTokenTypes) : String;
var
   t : TTokenType;
begin
   for t in tt do begin
      if Result<>'' then
         Result:=Result+' or ';
      case t of
         ttIntVal, ttStrVal, ttFloatVal :
            Result:=Result+cTokenStrings[t];
      else
         Result:=Result+'"'+cTokenStrings[t]+'"';
      end;
   end;
end;

end.
