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

   TdwsCodeDOMSection = class (TdwsCodeDOMNode);

   TdwsCodeDOMTypeSection = class (TdwsCodeDOMSection)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMVarSection = class (TdwsCodeDOMSection)
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

   TdwsCodeDOMAssignment = class (TdwsCodeDOMStatement)
   end;

   TdwsCodeDOMNop = class (TdwsCodeDOMStatement)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMBlock = class (TdwsCodeDOMStatementList)
      protected
         procedure Prepare; override;
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMRepeat = class (TdwsCodeDOMStatement)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMWhile = class (TdwsCodeDOMStatement)
      public
   end;

   TdwsCodeDOMIfThenElseStmt = class (TdwsCodeDOMStatement)
      private
         FHasElse : Boolean;

      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;

         property HasElse : Boolean read FHasElse write FHasElse;
   end;

   TdwsCodeDOMNameList = class (TdwsCodeDOMNode)
   end;

   TdwsCodeDOMVarDeclaration = class (TdwsCodeDOMStatement)
   end;

   TdwsCodeDOMExpression = class (TdwsCodeDOMNode)
   end;

   TdwsCodeDOMLiteral = class (TdwsCodeDOMExpression)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

   TdwsCodeDOMReference = class (TdwsCodeDOMExpression)
   end;

   TdwsCodeDOMCall = class (TdwsCodeDOMExpression)
      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;
   end;

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

   TdwsCodeDOMIfThenElseExpr = class (TdwsCodeDOMExpression)
      private
         FHasElse : Boolean;

      public
         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;

         property HasElse : Boolean read FHasElse write FHasElse;
   end;

   TdwsCodeDOMParameterDecl = class (TdwsCodeDOMNode)
   end;

   TdwsCodeDOMParameterDeclList = class (TdwsCodeDOMNode)
   end;

   TdwsCodeDOMFunctionDecl = class (TdwsCodeDOMNode)
   end;

   TdwsCodeDOMFunctionImpl = class (TdwsCodeDOMNode)
   end;

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
// ------------------ TdwsCodeDOMLiteral ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMLiteral.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   inherited;
//   output.WriteString(AsString);
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
// ------------------ TdwsCodeDOMBlock ------------------
// ------------------

// Prepare
//
procedure TdwsCodeDOMBlock.Prepare;
begin
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
procedure TdwsCodeDOMBlock.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   output.WriteString(cTokenStrings[ttBEGIN]);
   output.IncIndentNewLine;
   inherited;
   output.DecIndentNewLine;
   output.WriteString(cTokenStrings[ttEND]);
end;

// ------------------
// ------------------ TdwsCodeDOMRepeat ------------------
// ------------------

// WriteToOutput
//
procedure TdwsCodeDOMRepeat.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   output.WriteString(cTokenStrings[ttREPEAT]);
   output.IncIndentNewLine;
   var i := 0;
   WriteChildrenUntilToken(output, i, ttUNTIL);
   output.DecIndentNewLine;
   WriteChildren(output, i);
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
   OperatorType := ExtractTokenType(0);
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
   output.WriteTokenString(OperatorType);
   if not (OperatorType in cWordTokenTypes) then
      output.SkipSpace;
   inherited;
end;

end.