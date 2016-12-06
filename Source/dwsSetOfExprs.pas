{**************************************************************************}
{                                                                          }
{    This Source Code Form is subject to the terms of the Mozilla Public   }
{    License, v. 2.0. If a copy of the MPL was not distributed with this   }
{     file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                          }
{    Software distributed under the License is distributed on an           }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express           }
{    or implied. See the License for the specific language                 }
{    governing rights and limitations under the License.                   }
{                                                                          }
{    Copyright Eric Grange / Creative IT                                   }
{                                                                          }
{**************************************************************************}
unit dwsSetOfExprs;

{$I dws.inc}

interface

uses
   dwsUtils, dwsErrors, dwsDataContext,
   dwsSymbols, dwsExprs, dwsScriptSource;

type

   // include/exclude
   TSetOfFunctionExpr = class(TNoResultExpr)
      private
         FBaseExpr : TDataExpr;
         FOperand : TTypedExpr;
         FSetType : TSetOfSymbol;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         procedure Perform(const dc : IDataContext; value : Integer); virtual; abstract;

      public
         constructor Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                            aBase : TDataExpr; operand : TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property BaseExpr : TDataExpr read FBaseExpr;
         property Operand : TTypedExpr read FOperand write FOperand;
         property SetType : TSetOfSymbol read FSetType write FSetType;
   end;

   TSetOfIncludeExpr = class(TSetOfFunctionExpr)
      protected
         procedure Perform(const dc : IDataContext; value : Integer); override;
   end;

   TSetOfExcludeExpr = class(TSetOfFunctionExpr)
      protected
         procedure Perform(const dc : IDataContext; value : Integer); override;
   end;

   TSetOfInExpr = class(TBooleanBinOpExpr)
      private
         FSetType : TSetOfSymbol;

      public
         constructor Create(prog: TdwsProgram; const aScriptPos : TScriptPos;
                            aLeft : TTypedExpr; aRight : TDataExpr); reintroduce;
         class function CreateOptimal(prog: TdwsProgram; const aScriptPos : TScriptPos;
                                      aLeft : TTypedExpr; aRight : TDataExpr) : TSetOfInExpr; static;

         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;

         property SetType : TSetOfSymbol read FSetType write FSetType;
   end;

   TSetOfSmallInExpr = class(TSetOfInExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

implementation

// ------------------
// ------------------ TSetOfFunctionExpr ------------------
// ------------------

// Create
//
constructor TSetOfFunctionExpr.Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                                    aBase : TDataExpr; operand : TTypedExpr);
begin
   inherited Create(scriptPos);
   FBaseExpr:=aBase;
   FOperand:=operand;
   FSetType:=TSetOfSymbol(FBaseExpr.Typ);
end;

// Destroy
//
destructor TSetOfFunctionExpr.Destroy;
begin
   inherited;
   FBaseExpr.Free;
   FOperand.Free;
end;

// EvalNoResult
//
procedure TSetOfFunctionExpr.EvalNoResult(exec : TdwsExecution);
var
   v : Integer;
begin
   v:=Operand.EvalAsInteger(exec);
   if Cardinal(v-FSetType.MinValue)<Cardinal(FSetType.CountValue) then
      Perform(BaseExpr.DataPtr[exec], v);
end;

// GetSubExpr
//
function TSetOfFunctionExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FBaseExpr
   else Result:=FOperand;
end;

// GetSubExprCount
//
function TSetOfFunctionExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TSetOfIncludeExpr ------------------
// ------------------

// Perform
//
procedure TSetOfIncludeExpr.Perform(const dc : IDataContext; value : Integer);
var
   offset : Integer;
   mask : Int64;
begin
   offset:=FSetType.ValueToOffsetMask(value, mask);
   dc.AsInteger[offset]:=dc.AsInteger[offset] or mask;
end;

// ------------------
// ------------------ TSetOfExcludeExpr ------------------
// ------------------

// Perform
//
procedure TSetOfExcludeExpr.Perform(const dc : IDataContext; value : Integer);
var
   offset : Integer;
   mask : Int64;
begin
   offset:=FSetType.ValueToOffsetMask(value, mask);
   dc.AsInteger[offset]:=dc.AsInteger[offset] and not mask;
end;

// ------------------
// ------------------ TSetOfInExpr ------------------
// ------------------

// Create
//
constructor TSetOfInExpr.Create(prog: TdwsProgram; const aScriptPos : TScriptPos;
                                aLeft : TTypedExpr; aRight : TDataExpr);
begin
   inherited Create(prog, aScriptPos, aLeft, aRight);
   FSetType:=TSetOfSymbol(aRight.Typ);
end;

// CreateOptimal
//
class function TSetOfInExpr.CreateOptimal(prog: TdwsProgram; const aScriptPos : TScriptPos;
                                          aLeft : TTypedExpr; aRight : TDataExpr) : TSetOfInExpr;
begin
   if (aRight.Typ as TSetOfSymbol).CountValue<=32 then
      Result:=TSetOfSmallInExpr.Create(prog, aScriptPos, aLeft, aRight)
   else Result:=TSetOfInExpr.Create(prog, aScriptPos, aLeft, aRight);
end;

// EvalAsBoolean
//
function TSetOfInExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   value, offset : Integer;
   mask : Int64;
begin
   value:=Left.EvalAsInteger(exec)-FSetType.MinValue;
   if Cardinal(value)>=Cardinal(FSetType.CountValue) then
      Result:=False
   else begin
      offset:=FSetType.ValueToOffsetMask(value, mask);
      Result:=(TDataExpr(Right).DataPtr[exec].AsInteger[offset] and mask)<>0;
   end;
end;

// ------------------
// ------------------ TSetOfSmallInExpr ------------------
// ------------------

// EvalAsBoolean
//
function TSetOfSmallInExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   value : Integer;
begin
   value:=Left.EvalAsInteger(exec)-FSetType.MinValue;
   if Cardinal(value)>=Cardinal(FSetType.CountValue) then
      Result:=False
   else begin
      Result:=(Right.EvalAsInteger(exec) and (1 shl value))<>0;
   end;
end;

end.
