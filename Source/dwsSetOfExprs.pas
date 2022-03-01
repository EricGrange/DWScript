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
   dwsUtils, dwsErrors, dwsDataContext, dwsCompilerContext, dwsStack,
   dwsSymbols, dwsExprs, dwsScriptSource, dwsTokenTypes;

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
         constructor Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
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
         constructor Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                            aLeft : TTypedExpr; aRight : TDataExpr); reintroduce;
         class function CreateOptimal(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                                      aLeft : TTypedExpr; aRight : TDataExpr) : TSetOfInExpr; static;

         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;

         property SetType : TSetOfSymbol read FSetType write FSetType;
   end;

   TSetOfSmallInExpr = class(TSetOfInExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TSetOfBooleanBinOpExpr = class(TBooleanBinOpExpr)
      private
         FSetType : TSetOfSymbol;

      public
         constructor Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                            const anOp : TTokenType; aLeft, aRight : TDataExpr); reintroduce;

         property SetType : TSetOfSymbol read FSetType;
   end;

   TSetOfEqualExpr = class(TSetOfBooleanBinOpExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TSetOfLeftContainedInRightExpr = class(TSetOfBooleanBinOpExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TSetOfBinOpExpr = class(TDataExpr)
      private
         FLeft, FRight : TDataExpr;
         FResultAddr : Integer;
         FOp : TTokenType;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         class function Perform(const aLeft, aRight : Int64) : Int64; virtual; abstract;

      public
         constructor Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                            const anOp : TTokenType; aLeft, aRight : TDataExpr);
         destructor Destroy; override;

         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;

         property Op : TTokenType read FOp;
         property Left : TDataExpr read FLeft;
         property Right : TDataExpr read FRight;
         property ResultAddr : Integer read FResultAddr;
   end;

   TSetOfAddExpr = class(TSetOfBinOpExpr)
      protected
         class function Perform(const aLeft, aRight : Int64) : Int64; override;
   end;

   TSetOfSubExpr = class(TSetOfBinOpExpr)
      protected
         class function Perform(const aLeft, aRight : Int64) : Int64; override;
   end;

   TSetOfMultExpr = class(TSetOfBinOpExpr)
      protected
         class function Perform(const aLeft, aRight : Int64) : Int64; override;
   end;

implementation

// ------------------
// ------------------ TSetOfFunctionExpr ------------------
// ------------------

// Create
//
constructor TSetOfFunctionExpr.Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
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
constructor TSetOfInExpr.Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                                aLeft : TTypedExpr; aRight : TDataExpr);
begin
   inherited Create(context, aScriptPos, ttIN, aLeft, aRight);
   FSetType:=TSetOfSymbol(aRight.Typ);
end;

// CreateOptimal
//
class function TSetOfInExpr.CreateOptimal(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                                          aLeft : TTypedExpr; aRight : TDataExpr) : TSetOfInExpr;
begin
   if (aRight.Typ as TSetOfSymbol).CountValue<=32 then
      Result := TSetOfSmallInExpr.Create(context, aScriptPos, aLeft, aRight)
   else Result := TSetOfInExpr.Create(context, aScriptPos, aLeft, aRight);
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

// ------------------
// ------------------ TSetOfBooleanBinOpExpr ------------------
// ------------------

// Create
//
constructor TSetOfBooleanBinOpExpr.Create(
      context : TdwsCompilerContext; const aScriptPos : TScriptPos;
      const anOp : TTokenType; aLeft, aRight : TDataExpr);
begin
   inherited Create(context, aScriptPos, anOp, aLeft, aRight);
   FSetType := aLeft.Typ.UnAliasedType as TSetOfSymbol;
end;

// ------------------
// ------------------ TSetOfEqualExpr ------------------
// ------------------

// EvalAsBoolean
//
function TSetOfEqualExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   i : Integer;
   leftDC, rightDC : IDataContext;
begin
   leftDC := (Left as TDataExpr).DataPtr[exec];
   rightDC := (Right as TDataExpr).DataPtr[exec];
   for i := 0 to SetType.Size-1 do begin
      if rightDC.AsInteger[i] <> leftDC.AsInteger[i] then Exit(False);
   end;
   Result := True;
end;

// ------------------
// ------------------ TSetOfLeftContainedInRightExpr ------------------
// ------------------

// EvalAsBoolean
//
function TSetOfLeftContainedInRightExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   i : Integer;
   leftDC, rightDC : IDataContext;
   leftItem : Int64;
begin
   leftDC := (Left as TDataExpr).DataPtr[exec];
   rightDC := (Right as TDataExpr).DataPtr[exec];
   for i := 0 to SetType.Size-1 do begin
      leftItem := leftDC.AsInteger[i];
      if (leftItem and rightDC.AsInteger[i]) <> leftItem then Exit(False);
   end;
   Result := True;
end;

// ------------------
// ------------------ TSetOfBinOpExpr ------------------
// ------------------

// Create
//
constructor TSetOfBinOpExpr.Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                                   const anOp : TTokenType; aLeft, aRight : TDataExpr);
begin
   inherited Create(aScriptPos, (aLeft.Typ.UnAliasedType as TSetOfSymbol));
   FOp := anOp;
   FLeft := aLeft;
   FRight := aRight;
   FResultAddr := context.GetTempAddr(FTyp.Size);
end;

// Destroy
//
destructor TSetOfBinOpExpr.Destroy;
begin
   inherited;
   FLeft.Free;
   FRight.Free;
end;

// EvalAsVariant
//
procedure TSetOfBinOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   VarCopySafe(result, EvalAsInteger(exec));
end;

// EvalAsInteger
//
function TSetOfBinOpExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   leftV, rightV : Int64;
begin
   leftV := FLeft.EvalAsInteger(exec);
   rightV := FRight.EvalAsInteger(exec);
   Result := Perform(leftV, rightV);
end;

// GetDataPtr
//
procedure TSetOfBinOpExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
var
   i : Integer;
   leftDC, rightDC : IDataContext;
begin
   leftDC := FLeft.DataPtr[exec];
   rightDC := FRight.DataPtr[exec];
   exec.DataContext_CreateBase(FResultAddr, result);
   for i := 0 to Typ.Size-1 do
      result.AsInteger[i] := Perform(leftDC.AsInteger[i], rightDC.AsInteger[i]);
end;

// GetSubExpr
//
function TSetOfBinOpExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i = 0 then
      Result := Left
   else Result := Right;
end;

// GetSubExprCount
//
function TSetOfBinOpExpr.GetSubExprCount : Integer;
begin
   Result := 2;
end;

// ------------------
// ------------------ TSetOfAddExpr ------------------
// ------------------

// Perform
//
class function TSetOfAddExpr.Perform(const aLeft, aRight : Int64) : Int64;
begin
   result := aLeft or aRight;
end;

// ------------------
// ------------------ TSetOfSubExpr ------------------
// ------------------

// Perform
//
class function TSetOfSubExpr.Perform(const aLeft, aRight : Int64) : Int64;
begin
   Result := aLeft and not aRight;
end;

// ------------------
// ------------------ TSetOfMultExpr ------------------
// ------------------

// Perform
//
class function TSetOfMultExpr.Perform(const aLeft, aRight : Int64) : Int64;
begin
   Result := aLeft and aRight;
end;

end.
