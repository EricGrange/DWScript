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

   TdwsCodeDOMComment = class (TdwsCodeDOMNode)
      public
         function PreLine : Boolean;
   end;

   TdwsCodeDOMSection = class (TdwsCodeDOMNode);

   TdwsCodeDOMTypeSection = class (TdwsCodeDOMSection)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;
   TdwsCodeDOMTypeInline = class (TdwsCodeDOMSection)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMVarSection = class (TdwsCodeDOMSection)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMConstSection = class (TdwsCodeDOMSection);

   TdwsCodeDOMResourceStringSection = class (TdwsCodeDOMSection);

   TdwsCodeDOMUses = class (TdwsCodeDOMNode)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMMain = class (TdwsCodeDOMNode)
      private
         FMainType : TTokenType;

      public
         function StatementsSection : Boolean; override;

         property MainType : TTokenType read FMainType;
   end;

   TdwsCodeDOMUniteNamespace = class (TdwsCodeDOMMain);

   TdwsCodeDOMMainInterface = class (TdwsCodeDOMMain);
   TdwsCodeDOMMainImplementation = class (TdwsCodeDOMMain);
   TdwsCodeDOMMainInitialization = class (TdwsCodeDOMMain);
   TdwsCodeDOMMainFinalization = class (TdwsCodeDOMMain);

   TdwsCodeDOMStatement = class (TdwsCodeDOMNode);

   TdwsCodeDOMStatementList = class (TdwsCodeDOMStatement);

   TdwsCodeDOMInstruction = class (TdwsCodeDOMStatement);

   TdwsCodeDOMAssignment = class (TdwsCodeDOMStatement);

   TdwsCodeDOMTry = class (TdwsCodeDOMStatement);
   TdwsCodeDOMTryExcept = class (TdwsCodeDOMStatement);
   TdwsCodeDOMTryExceptElse = class (TdwsCodeDOMNode);
   TdwsCodeDOMTryFinally = class (TdwsCodeDOMStatement);

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

   TdwsCodeDOMFunctionBlock = class (TdwsCodeDOMBeginEnd);

   TdwsCodeDOMAsmBlock = class (TdwsCodeDOMBeginEnd);

   TdwsCodeDOMWith = class (TdwsCodeDOMStatement);

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
   TdwsCodeDOMResourceString = class (TdwsCodeDOMStatement);

   TdwsCodeDOMExpression = class (TdwsCodeDOMNode);

   TdwsCodeDOMTuple = class (TdwsCodeDOMNode);
   TdwsCodeDOMNamedTuple = class (TdwsCodeDOMNode);

   TdwsCodeDOMLiteral = class (TdwsCodeDOMExpression);

   TdwsCodeDOMLiteralStr = class (TdwsCodeDOMExpression)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMParenthesis = class (TdwsCodeDOMExpression);
   TdwsCodeDOMBrackets = class (TdwsCodeDOMExpression)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;
   TdwsCodeDOMBracketsElements = class (TdwsCodeDOMNode);

   TdwsCodeDOMReference = class (TdwsCodeDOMExpression);

   TdwsCodeDOMRange = class (TdwsCodeDOMNode);

   TdwsCodeDOMCall = class (TdwsCodeDOMExpression);
   TdwsCodeDOMCallInherited = class (TdwsCodeDOMCall);

   TdwsCodeDOMField = class (TdwsCodeDOMExpression);
   TdwsCodeDOMIndex = class (TdwsCodeDOMExpression);

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

   TdwsCodeDOMIfThenElseExpr = class (TdwsCodeDOMExpression)
      private
         FHasElse : Boolean;

      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;

         property HasElse : Boolean read FHasElse write FHasElse;
   end;

   TdwsCodeDOMDeprecatedQualifier = class (TdwsCodeDOMNode);
   TdwsCodeDOMExternalQualifier = class (TdwsCodeDOMNode);
   TdwsCodeDOMHelperQualifier = class (TdwsCodeDOMNode);

   TdwsCodeDOMContractDescription = class (TdwsCodeDOMNode);
   TdwsCodeDOMContractClause = class (TdwsCodeDOMNode);
   TdwsCodeDOMContractClauses = class (TdwsCodeDOMNode);
   TdwsCodeDOMContractRequire = class (TdwsCodeDOMContractClauses);
   TdwsCodeDOMContractEnsure = class (TdwsCodeDOMContractClauses);

   TdwsCodeDOMParameterDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMParameterDeclList = class (TdwsCodeDOMNode)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMParameters = class (TdwsCodeDOMNode)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMIndexes = class (TdwsCodeDOMNode)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMFunctionDecl = class (TdwsCodeDOMNode)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;
   TdwsCodeDOMFunctionReturnDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMFunctionQualifier = class (TdwsCodeDOMNode);
   TdwsCodeDOMFunctionImpl = class (TdwsCodeDOMNode)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMLambda = class (TdwsCodeDOMExpression);
   TdwsCodeDOMLambdaParameters = class (TdwsCodeDOMParameters);
   TdwsCodeDOMLambdaExpression = class (TdwsCodeDOMNode);
   TdwsCodeDOMLambdaStatements = class (TdwsCodeDOMStatementList);

   TdwsCodeDOMTypeDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMTypeGeneric = class (TdwsCodeDOMNode);
   TdwsCodeDOMTypeGenericParameters = class (TdwsCodeDOMNode);
   TdwsCodeDOMTypeGenericConstraint = class (TdwsCodeDOMNode);

   TdwsCodeDOMFunctionTypeDecl = class (TdwsCodeDOMTypeDecl);

   TdwsCodeDOMClassInh = class (TdwsCodeDOMNode);
   TdwsCodeDOMClassFwd = class (TdwsCodeDOMTypeDecl);
   TdwsCodeDOMClassDecl = class (TdwsCodeDOMTypeDecl);

   TdwsCodeDOMClassBody = class (TdwsCodeDOMNode)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMClassConst = class (TdwsCodeDOMNode);
   TdwsCodeDOMClassVar = class (TdwsCodeDOMNode);
   TdwsCodeDOMClassOperator = class (TdwsCodeDOMNode);

   TdwsCodeDOMTypeInnerDecl = class (TdwsCodeDOMNode)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMTypeVisibilitySection = class (TdwsCodeDOMNode)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMClassOfDecl = class (TdwsCodeDOMTypeDecl);

   TdwsCodeDOMPropertyDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMPropertyArrayDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMPropertyReadDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMPropertyWriteDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMPropertyWriteStmt = class (TdwsCodeDOMNode);

   TdwsCodeDOMInterfaceDecl = class (TdwsCodeDOMTypeDecl);
   TdwsCodeDOMInterfaceDeclInh = class (TdwsCodeDOMNode);
   TdwsCodeDOMInterfaceDeclBody = class (TdwsCodeDOMNode);

   TdwsCodeDOMRecordDecl = class (TdwsCodeDOMTypeDecl);

   TdwsCodeDOMOperatorDecl = class (TdwsCodeDOMNode);
   TdwsCodeDOMOperatorDeclParameters = class (TdwsCodeDOMNode);

   TdwsCodeDOMArrayDecl = class (TdwsCodeDOMTypeDecl);
   TdwsCodeDOMArrayRange = class (TdwsCodeDOMNode);
   TdwsCodeDOMArrayRangeNum = class (TdwsCodeDOMArrayRange);
   TdwsCodeDOMArrayRangeType = class (TdwsCodeDOMArrayRange);

   TdwsCodeDOMEnumDecl = class (TdwsCodeDOMTypeDecl);
   TdwsCodeDOMEnumElements = class (TdwsCodeDOMNode);
   TdwsCodeDOMEnumElementValue = class (TdwsCodeDOMNode);

   TdwsCodeDOMSetDecl = class (TdwsCodeDOMTypeDecl);

   TdwsCodeDOMHelperDecl = class (TdwsCodeDOMTypeDecl);
   TdwsCodeDOMHelperBody = class (TdwsCodeDOMNode);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsCodeDOMPascalParser;

procedure OutputSeparatedChildren(
   output : TdwsCodeDOMOutput; node : TdwsCodeDOMNode;
   beginToken, separatorToken, endToken : TTokenType;
   startIndex : Integer = 0
);
begin
   var subOutput := TdwsCodeDOMOutput.Create;
   try
      var i := startIndex;
      subOutput.WriteChildrenBeforeToken(node, i, endToken);
      if subOutput.Line > 1 then begin
         // preformatted table
         i := startIndex;
         if beginToken <> ttNone then
            output.WriteChildrenUntilToken(node, i, beginToken);
         output
            .IncIndentNewLine
            .WriteChildrenBeforeToken(node, i, endToken);
         if endToken <> ttNone then
            output.DecIndentNewLine
         else output.DecIndent;
      end else  if subOutput.ColMax + output.Col + Length(cTokenStrings[endToken]) + 1 >= output.MaxToleranceColumn then begin
         // reformat
         i := startIndex;
         if beginToken <> ttNone then
            output.WriteChildrenUntilToken(node, i, beginToken);
         output.IncIndentNewLine;
         var wroteAtLeastOne := False;
         while (i < node.ChildCount) and not node.ChildIsTokenType(i, endToken)  do begin
            var beforeState := output.SaveState;
            var beforeI := i;
            output.WriteChildrenBeforeTokens(node, i, [ separatorToken, endToken ]);
            if node.ChildIsTokenType(i, endToken) then break;
            output.WriteChild(node, i);
            if wroteAtLeastOne and (output.Col >= output.MaxDesiredColumn) then begin
               output.RestoreState(beforeState);
               i := beforeI;
               output.WriteNewLine;
            end else wroteAtLeastOne := True;
         end;
         if endToken <> ttNone then
            output.DecIndentNewLine
         else output.DecIndent;
      end else begin
         // small enough, output inline
         i := startIndex;
      end;
      output.WriteChildren(node, i);
   finally
      subOutput.Free;
   end;

end;

// ------------------
// ------------------ TdwsCodeDOMComment ------------------
// ------------------

// PreLine
//
function TdwsCodeDOMComment.PreLine : Boolean;
begin
   Result :=     (ChildCount > 0) and ChildIsOfClass(0, TdwsCodeDOMSnippet)
             and TdwsCodeDOMSnippet(Child[0]).PreLine;
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
// ------------------ TdwsCodeDOMUses ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMUses.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   OutputSeparatedChildren(output, Self, ttUSES, ttCOMMA, ttNone);
end;

// ------------------
// ------------------ TdwsCodeDOMTypeSection ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMTypeSection.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   var i := 0;
   output
      .WriteChild(Self, i)
      .WriteNewLine
      .IncIndentNewLine
      .WriteChildren(Self, i)
      .DecIndent;
end;

// ------------------
// ------------------ TdwsCodeDOMTypeInline ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMTypeInline.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   var i := 0;
   output
      .WriteChild(Self, i)
      .IncIndentNewLine
      .WriteChildren(Self, i)
      .DecIndent;
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
      if ChildIsTokenType(i, ttELSE) then
         output.DecIndentNewLine
      else output.DecIndent;
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

// ------------------
// ------------------ TdwsCodeDOMParameterDeclList ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMParameterDeclList.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   output.SkipSpace;
   inherited;
end;

// ------------------
// ------------------ TdwsCodeDOMParameters ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMParameters.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   output.SkipSpace;
   OutputSeparatedChildren(output, Self, ttBLEFT, ttCOMMA, ttBRIGHT);
end;

// ------------------
// ------------------ TdwsCodeDOMIndexes ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMIndexes.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   output.SkipSpace;
   OutputSeparatedChildren(output, Self, ttALEFT, ttCOMMA, ttARIGHT);
end;

// ------------------
// ------------------ TdwsCodeDOMClassBody ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMClassBody.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   var firstVisibilitySection := True;
   var i := 0;
   output.IncIndentNewLine;

   while (i < ChildCount) and not ChildIsTokenType(i, ttEND) do begin
      if ChildIsOfClass(i, TdwsCodeDOMTypeVisibilitySection) then begin
         if firstVisibilitySection then
            firstVisibilitySection := False
         else output.WritePreLine;
      end;
      output.WriteChild(Self, i);
   end;

   output
      .DecIndentNewLine
      .WriteChildren(Self, i)
      .SkipExtraLineAfterNextNewLine;
end;

// ------------------
// ------------------ TdwsCodeDOMTypeInnerDecl ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMTypeInnerDecl.WriteToOutput(output : TdwsCodeDOMOutput);
type
   TLineCategory = ( catNone, catMethod, catProperty, catOthers );
begin
   var i := 0;
   var prevCat := catNone;
   while i < ChildCount do begin

      var c := Child[i];
      var cat := prevCat;
      if c is TdwsCodeDOMPropertyDecl then
         cat := catProperty
      else if c is TdwsCodeDOMFunctionDecl then
         cat := catMethod
      else if c is TdwsCodeDOMField then
         cat := catOthers;
      if cat <> prevCat then begin
         if prevCat <> catNone then
            output.WritePreLine;
         prevCat := cat;
      end;

      output.WriteChild(Self, i);
   end;
end;

// ------------------
// ------------------ TdwsCodeDOMTypeVisibilitySection ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMTypeVisibilitySection.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   var i := 0;
   output
      .WriteChild(Self, i)
      .IncIndentNewLine
      .WriteChildren(Self, i)
      .DecIndentNewLine
end;

// ------------------
// ------------------ TdwsCodeDOMBrackets ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMBrackets.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   OutputSeparatedChildren(output, Self, ttALEFT, ttCOMMA, ttARIGHT);
end;

// ------------------
// ------------------ TdwsCodeDOMFunctionImpl ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMFunctionImpl.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   inherited;
   output.SkipExtraLineAfterNextNewLine;
end;

// ------------------
// ------------------ TdwsCodeDOMFunctionDecl ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMFunctionDecl.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   var i := 0;
   while i < ChildCount do begin
      output.SkipNewLine;
      output.WriteChild(Self, i);
   end;
   output.DiscardSkipNewLine;
end;

end.
