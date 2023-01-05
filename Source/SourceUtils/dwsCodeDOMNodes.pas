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
unit dwsCodeDOMNodes;

{$I ../dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsCodeDOM, dwsTokenTypes;

type

   TdwsCodeDOMSwitch = class (TdwsCodeDOMNode);

   TdwsCodeDOMComment = class (TdwsCodeDOMNode);

   TdwsCodeDOMSection = class (TdwsCodeDOMNode);

   TdwsCodeDOMTypeSection = class (TdwsCodeDOMSection)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMVarSection = class (TdwsCodeDOMSection)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMConstSection = class (TdwsCodeDOMSection)
   end;

   TdwsCodeDOMMain = class (TdwsCodeDOMNode)
      private
         FMainType : TTokenType;

      public
         function StatementsSection : Boolean; override;

         property MainType : TTokenType read FMainType;
   end;

   TdwsCodeDOMMainInterface = class (TdwsCodeDOMMain);
   TdwsCodeDOMMainImplementation = class (TdwsCodeDOMMain);
   TdwsCodeDOMMainInitialization = class (TdwsCodeDOMMain);
   TdwsCodeDOMMainFinalization = class (TdwsCodeDOMMain);

   TdwsCodeDOMStatement = class (TdwsCodeDOMNode)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMStatementList = class (TdwsCodeDOMStatement)
   end;

   TdwsCodeDOMInstruction = class (TdwsCodeDOMStatement);

   TdwsCodeDOMAssignment = class (TdwsCodeDOMStatement);

   TdwsCodeDOMTryExceptFinally = class (TdwsCodeDOMStatement);
   TdwsCodeDOMTryExceptElse = class (TdwsCodeDOMNode);

   TdwsCodeDOMExceptOnClause = class (TdwsCodeDOMStatement);

   TdwsCodeDOMNop = class (TdwsCodeDOMStatement)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMBeginEnd = class (TdwsCodeDOMStatementList)
      protected
         procedure Prepare; override;
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMAsmBlock = class (TdwsCodeDOMBeginEnd);

   TdwsCodeDOMRepeat = class (TdwsCodeDOMStatement)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMWhile = class (TdwsCodeDOMStatement)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMIfThenElseStmt = class (TdwsCodeDOMStatement)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMCaseOf = class (TdwsCodeDOMStatement)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;
   TdwsCodeDOMCaseOfAlternative = class (TdwsCodeDOMNode);
   TdwsCodeDOMCaseOfAlternatives = class (TdwsCodeDOMNode);
   TdwsCodeDOMCaseOfAlternativeCase = class (TdwsCodeDOMNode);
   TdwsCodeDOMCaseOfAlternativeCaseRange = class (TdwsCodeDOMNode);
   TdwsCodeDOMCaseOfAlternativeCases = class (TdwsCodeDOMNode);

   TdwsCodeDOMFor = class (TdwsCodeDOMStatement)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMForLoop = class (TdwsCodeDOMFor);
   TdwsCodeDOMForLoopStep = class (TdwsCodeDOMNode);

   TdwsCodeDOMForIn = class (TdwsCodeDOMFor);

   TdwsCodeDOMNameList = class (TdwsCodeDOMNode);

   TdwsCodeDOMVarDeclaration = class (TdwsCodeDOMStatement);
   TdwsCodeDOMConstDeclaration = class (TdwsCodeDOMStatement);
   TdwsCodeDOMConstDeclarationType = class (TdwsCodeDOMStatement);

   TdwsCodeDOMExpression = class (TdwsCodeDOMNode);

   TdwsCodeDOMTuple = class (TdwsCodeDOMNode);
   TdwsCodeDOMNamedTuple = class (TdwsCodeDOMNode);

   TdwsCodeDOMLiteral = class (TdwsCodeDOMExpression);

   TdwsCodeDOMLiteralStr = class (TdwsCodeDOMExpression)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMParenthesis = class (TdwsCodeDOMExpression);
   TdwsCodeDOMBrackets = class (TdwsCodeDOMExpression);
   TdwsCodeDOMBracketsElements = class (TdwsCodeDOMNode);

   TdwsCodeDOMReference = class (TdwsCodeDOMExpression);

   TdwsCodeDOMRange = class (TdwsCodeDOMNode);

   TdwsCodeDOMCall = class (TdwsCodeDOMExpression)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;
   TdwsCodeDOMCallInherited = class (TdwsCodeDOMCall);

   TdwsCodeDOMOperator = class (TdwsCodeDOMExpression)
      private
         FOperatorType : TTokenType;

      public
         property OperatorType : TTokenType read FOperatorType write FOperatorType;
   end;

   TdwsCodeDOMBinaryOperator = class (TdwsCodeDOMOperator)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMUnaryOperator = class (TdwsCodeDOMOperator)
      protected
         procedure Prepare; override;
         procedure WritePropertiesToOutline(wobs : TWriteOnlyBlockStream; indent : Integer); override;
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMNew = class (TdwsCodeDOMUnaryOperator);

   TdwsCodeDOMTerm = class (TdwsCodeDOMExpression);

   TdwsCodeDOMIndexed = class (TdwsCodeDOMExpression);

   TdwsCodeDOMDotted = class (TdwsCodeDOMBinaryOperator);

   TdwsCodeDOMIfThenElseExpr = class (TdwsCodeDOMExpression)
      private
         FHasElse : Boolean;

      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;

         property HasElse : Boolean read FHasElse write FHasElse;
   end;

   TdwsCodeDOMDeprecatedQualifier = class (TdwsCodeDOMNode);
   TdwsCodeDOMExternalQualifier = class (TdwsCodeDOMNode);

   TdwsCodeDOMContractDescription = class (TdwsCodeDOMNode);
   TdwsCodeDOMContractClause = class (TdwsCodeDOMNode);
   TdwsCodeDOMContractClauses = class (TdwsCodeDOMNode);
   TdwsCodeDOMContractRequire = class (TdwsCodeDOMContractClauses);
   TdwsCodeDOMContractEnsure = class (TdwsCodeDOMContractClauses);

   TdwsCodeDOMParameterDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMParameterDeclList = class (TdwsCodeDOMNode);

   TdwsCodeDOMFunctionDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMFunctionReturnDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMFunctionQualifier = class (TdwsCodeDOMNode);
   TdwsCodeDOMFunctionImpl = class (TdwsCodeDOMNode);

   TdwsCodeDOMTypeDecl = class (TdwsCodeDOMNode);

   TdwsCodeDOMFunctionTypeDecl = class (TdwsCodeDOMTypeDecl);

   TdwsCodeDOMClassInh = class (TdwsCodeDOMNode);
   TdwsCodeDOMClassFwd = class (TdwsCodeDOMTypeDecl);
   TdwsCodeDOMClassDecl = class (TdwsCodeDOMTypeDecl);

   TdwsCodeDOMClassBody = class (TdwsCodeDOMNode);
   TdwsCodeDOMClassConst = class (TdwsCodeDOMNode);
   TdwsCodeDOMClassVar = class (TdwsCodeDOMNode);
   TdwsCodeDOMClassOperator = class (TdwsCodeDOMNode);

   TdwsCodeDOMTypeVisibilitySection = class (TdwsCodeDOMNode);

   TdwsCodeDOMClassOfDecl = class (TdwsCodeDOMTypeDecl);

   TdwsCodeDOMPropertyDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMPropertyArrayDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMPropertyReadDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMPropertyWriteDecl = class (TdwsCodeDOMNode);

   TdwsCodeDOMInterfaceDecl = class (TdwsCodeDOMTypeDecl);

   TdwsCodeDOMRecordDecl = class (TdwsCodeDOMTypeDecl);

   TdwsCodeDOMArrayDecl = class (TdwsCodeDOMTypeDecl);
   TdwsCodeDOMArrayRange = class (TdwsCodeDOMNode);
   TdwsCodeDOMArrayRangeNum = class (TdwsCodeDOMArrayRange);
   TdwsCodeDOMArrayRangeType = class (TdwsCodeDOMArrayRange);

   TdwsCodeDOMEnumDecl = class (TdwsCodeDOMTypeDecl);
   TdwsCodeDOMEnumElements = class (TdwsCodeDOMNode);
   TdwsCodeDOMEnumElementValue = class (TdwsCodeDOMNode);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsCodeDOMStatement ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMStatement.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   inherited;
//   if ChildCount = 0 then Exit;
//
//   for var i := 0 to ChildCount-2 do
//      Child[i].WriteToOutput(output);
//
//   var lastChild := Child[ChildCount-1];
//   if (lastChild is TdwsCodeDOMSnippet) and (TdwsCodeDOMSnippet(lastChild).TokenType = ttCOMMENT) then begin
//      lastChild.WriteToOutput(output);
//      output.WriteNewLine;
//   end else begin
//      lastChild.WriteToOutput(output);
//      output.WriteNewLine;
//   end;
end;

// ------------------
// ------------------ TdwsCodeDOMMain ------------------
// ------------------

// StatementsSection
//
function TdwsCodeDOMMain.StatementsSection : Boolean;
begin
   Result := not (MainType in [ ttINTERFACE, ttIMPLEMENTATION ]);
end;

// ------------------
// ------------------ TdwsCodeDOMTypeSection ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMTypeSection.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   inherited;
//   output.WriteTokenString(ttTYPE);
//
//   var inInterface := ParentOfClass(TdwsCodeDOMMainInterface) <> nil;
//
//   output.IncIndent;
//   if inInterface then
//      output.WriteNewLine;
//
//   inherited WriteToOutput(output);
//   output.DecIndent;
end;

// ------------------
// ------------------ TdwsCodeDOMVarSection ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMVarSection.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   inherited;
//   output.WriteString('var');
//
//   var indent := Parent is TdwsCodeDOMSection;
//
//   if indent then
//      output.IncIndentNewLine;
//
//   inherited WriteToOutput(output);
//   output.WriteNewLine;
//
//   if indent then
//      output.DecIndent;
end;

// ------------------
// ------------------ TdwsCodeDOMLiteralStr ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMLiteralStr.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   for var i := 0 to ChildCount-1 do begin
      Child[i].WriteToOutput(output);
      output.SkipSpace;
   end;
end;

// ------------------
// ------------------ TdwsCodeDOMBinaryOperator ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMBinaryOperator.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   inherited;
//   var i := 0;
//   if HasLeftOperand then begin
//      Child[0].WriteToOutput(output);
//      Inc(i);
//   end;
//   output.WriteString(cTokenStrings[OperatorType]);
//   if not HasLeftOperand then
//      output.SkipSpace;
//   for i := i to ChildCount-1 do
//      Child[i].WriteToOutput(output);
end;

// ------------------
// ------------------ TdwsCodeDOMBeginEnd ------------------
// ------------------

// Prepare
//
procedure TdwsCodeDOMBeginEnd.Prepare;
begin
   inherited;
   if ChildCount <> 1 then Exit;
   if Child[0].ClassType = TdwsCodeDOMStatementList then begin
      var list := Child[0];
      while list.ChildCount > 0 do
         AddChild(list.Extract(0));
      Extract(0).Free;
   end;
end;

// WriteToOutput
//
procedure TdwsCodeDOMBeginEnd.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   var i := 0;
   output
      .WriteChild(Self, i)
      .IncIndentNewLine
      .WriteChildrenBeforeTokens(Self, i, [ ttENSURE, ttEND ])
      .DecIndentNewLine;
   if ChildIsTokenType(i, ttENSURE) then begin
      output
         .WriteChild(Self, i)
         .IncIndentNewLine
         .WriteChildrenBeforeToken(Self, i, ttEND)
         .DecIndentNewLine;
   end;
   output.WriteChildren(Self, i);
end;

// ------------------
// ------------------ TdwsCodeDOMRepeat ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMRepeat.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   var i := 0;
   output
      .WriteChild(Self, i)
      .IncIndentNewLine
      .WriteChildrenBeforeToken(Self, i, ttUNTIL)
      .DecIndentNewLine
      .WriteChildren(Self, i);
end;

// ------------------
// ------------------ TdwsCodeDOMWhile ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMWhile.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   var i := 0;
   output.WriteChildrenUntilToken(Self, i, ttDO);
   var indent := not ChildIsOfClass(i, TdwsCodeDOMBeginEnd);
   if indent then
      output.IncIndentNewLine;
   output.WriteChildren(Self, i);
   if indent then
      output.DecIndent;
end;

// ------------------
// ------------------ TdwsCodeDOMNop ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMNop.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   // nothing
end;

// ------------------
// ------------------ TdwsCodeDOMIfThenElseStmt ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMIfThenElseStmt.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   var i := 0;
   output.WriteChildrenUntilToken(Self, i, ttTHEN);
   var indent := not ChildIsOfClass(i, TdwsCodeDOMBeginEnd);
   if indent then
      output.IncIndentNewLine;
   output.WriteChildrenBeforeToken(Self, i, ttELSE);
   if indent then
      output.DecIndentNewLine;
   output.WriteChildren(Self, i);
end;

// ------------------
// ------------------ TdwsCodeDOMCaseOf ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMCaseOf.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   var i := 0;
   output
      .WriteChildrenUntilToken(Self, i, ttOF)
      .IncIndentNewLine
      .WriteChildrenBeforeTokens(Self, i, [ ttELSE, ttEND ]);
   if ChildIsTokenType(i, ttELSE) then begin
      output
         .DecIndentNewLine
         .WriteChild(Self, i)
         .IncIndentNewLine;
   end;
   output
      .WriteChildrenBeforeToken(Self, i, ttEND)
      .DecIndentNewLine
      .WriteChildren(Self, i);
end;

// ------------------
// ------------------ TdwsCodeDOMFor ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMFor.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   var i := 0;
   output.WriteChildrenUntilToken(Self, i, ttDO);
   var indent := not ChildIsOfClass(i, TdwsCodeDOMBeginEnd);
   if indent then
      output.IncIndentNewLine;
   output.WriteChildren(Self, i);
   if indent then
      output.DecIndent;
end;

// ------------------
// ------------------ TdwsCodeDOMIfThenElseExpr ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMIfThenElseExpr.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   inherited;
//   output.WriteTokenString(ttIF);
//
//   var i := 0;
//   WriteChildrenUntilClass(output, i, TdwsCodeDOMExpression);
//
//   output.WriteTokenString(ttTHEN);
//   WriteChildrenUntilClass(output, i, TdwsCodeDOMStatement);
//
//   if HasElse then begin
//      output.WriteTokenString(ttELSE);
//      WriteChildrenUntilClass(output, i, TdwsCodeDOMStatement);
//   end;
end;

// ------------------
// ------------------ TdwsCodeDOMCall ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMCall.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   inherited;
//   Child[0].WriteToOutput(output);
//
//   output.WriteTokenString(ttBLEFT);
//   output.IncIndent;
//
//   var i := 1;
//   while i < ChildCount do begin
//      if i > 1 then
//         output.WriteTokenString(ttCOMMA);
//      WriteChildrenUntilClass(output, i, TdwsCodeDOMExpression);
//   end;
//
//   output.DecIndent;
//   output.WriteTokenString(ttBRIGHT);
end;

// ------------------
// ------------------ TdwsCodeDOMUnaryOperator ------------------
// ------------------

// Prepare
//
procedure TdwsCodeDOMUnaryOperator.Prepare;
begin
   inherited;
   OperatorType := (Child[0] as TdwsCodeDOMSnippet).TokenType;
end;

// WritePropertiesToOutline
//
procedure TdwsCodeDOMUnaryOperator.WritePropertiesToOutline(wobs : TWriteOnlyBlockStream; indent : Integer);
begin
   wobs.WriteIndent(indent);
   wobs.WriteString('UnaryOperator ');
   wobs.WriteString(cTokenStrings[OperatorType]);
   wobs.WriteCRLF;
end;

// WriteToOutput
//
procedure TdwsCodeDOMUnaryOperator.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   var i := 0;
   output.WriteChild(Self, i);
   if not (OperatorType in cWordTokenTypes) then
      output.SkipSpace;
   output.WriteChildren(Self, i);
end;

end.
