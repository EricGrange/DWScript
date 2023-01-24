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
unit dwsAssociativeArrays;

{$I dws.inc}

interface

uses
   Classes, SysUtils, System.Variants,
   dwsSymbols, dwsUtils, dwsDataContext, dwsJSON, dwsExprs;

type

   TScriptAssociativeArrayHashCodes = array of Cardinal;

   TScriptAssociativeArray = class sealed (TDataContext, IScriptAssociativeArray)
      private
         FElementTyp, FKeyTyp : TTypeSymbol;
         FElementSize, FKeySize : Integer;

         FCount : NativeInt;
         FCapacity, FGrowth : NativeInt;
         FHashCodes : TScriptAssociativeArrayHashCodes;
         FKeys : IDataContext;
         FCreateKeyOnAccess : Boolean;

      protected
         procedure Grow;
         function LinearFind(const item : IDataContext; var index : Integer) : Boolean;
         function LinearValueFind(const keyValue : Variant; var index : Integer) : Boolean;

         procedure IndexExprToKeyAndHashCode(exec : TdwsExecution; index : TTypedExpr;
                                             out key : IDataContext; out hashCode : Cardinal);

         procedure DebugIntegrityCheck;

      public
         class function CreateNew(keyTyp, elemTyp : TTypeSymbol) : TScriptAssociativeArray; static;

         function ScriptTypeName : String; override;

         procedure GetDataPtr(exec : TdwsExecution; index : TTypedExpr; var result : IDataContext);
         procedure GetDataAsVariant(exec : TdwsExecution; const keyValue : Variant; var result : Variant); overload;
         function GetDataAsBoolean(exec : TdwsExecution; index : TTypedExpr) : Boolean; overload;
         function GetDataAsBoolean(exec : TdwsExecution; const keyValue : Variant) : Boolean; overload;
         function GetDataAsInteger(exec : TdwsExecution; index : TTypedExpr) : Int64; overload;
         function GetDataAsInteger(exec : TdwsExecution; const keyValue : Variant) : Int64; overload;
         procedure GetDataAsString(exec : TdwsExecution; index : TTypedExpr; var result : String); overload;
         procedure GetDataAsString(exec : TdwsExecution; const keyValue : Variant; var result : String); overload;

         procedure ReplaceValue(exec : TdwsExecution; index, value : TTypedExpr); overload;
         procedure ReplaceValue(exec : TdwsExecution; const key, value : Variant); overload;

         function ContainsKey(exec : TdwsExecution; index : TTypedExpr) : Boolean;

         function Delete(exec : TdwsExecution; index : TTypedExpr) : Boolean;

         procedure Clear;

         function ReadBucket(index : Integer; var key : TData; var value : IDataContext) : Boolean;

         function Count : NativeInt;
         function Capacity : NativeInt;

         procedure CopyKeys(const dest : IScriptDynArray);

         property Keys : IDataContext read FKeys;
         property KeyType : TTypeSymbol read FKeyTyp;
         property ElementType : TTypeSymbol read FElementTyp;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsStack;

// ------------------
// ------------------ TScriptAssociativeIntegerArray ------------------
// ------------------

// TScriptAssociativeInteger_InitData
//
procedure TScriptAssociativeArray_InitData(typ : TTypeSymbol; const resultDC : IDataContext; offset : NativeInt);
var
   a : TAssociativeArraySymbol;
begin
   a := (typ as TAssociativeArraySymbol);
   resultDC.AsInterface[offset] := IScriptAssociativeArray(TScriptAssociativeArray.CreateNew(a.KeyType, a.Typ));
end;

// CreateNew
//
class function TScriptAssociativeArray.CreateNew(keyTyp, elemTyp : TTypeSymbol) : TScriptAssociativeArray;
var
   size : Integer;
   ct : TClass;
begin
   Result := TScriptAssociativeArray.Create;

   if keyTyp <> nil then
      size := keyTyp.Size
   else size := 0;
   Result.FKeyTyp := keyTyp;
   Result.FKeySize := size;
   Result.FKeys := TDataContext.CreateStandalone(0);

   if elemTyp <> nil then begin
      size := elemTyp.Size;
      ct := elemTyp.UnAliasedType.ClassType;
      Result.FCreateKeyOnAccess := (ct = TDynamicArraySymbol) or (ct = TAssociativeArraySymbol) or (size > 1)
   end else size := 0;
   Result.FElementTyp := elemTyp;
   Result.FElementSize := size;
end;

// ScriptTypeName
//
function TScriptAssociativeArray.ScriptTypeName : String;
begin
   Result := 'array [' + KeyType.Caption + '] of ' + ElementType.Caption;
end;

// Grow
//
procedure TScriptAssociativeArray.Grow;
var
   i, j, n : Integer;
   oldHashCodes : TScriptAssociativeArrayHashCodes;
   oldKeys, oldData : TData;
begin
   if FCapacity=0 then
      FCapacity:=32
   else FCapacity:=FCapacity*2;
   FGrowth:=(FCapacity*11) div 16;

   oldHashCodes:=FHashCodes;
   oldKeys := FKeys.AsPData^;
   oldData := AsPData^;

   FHashCodes:=nil;
   SetLength(FHashCodes, FCapacity);

   FKeys := TDataContext.CreateStandalone(FCapacity*FKeySize);

   ClearData;
   SetDataLength(FCapacity*FElementSize);

   n:=FCapacity-1;
   for i:=0 to High(oldHashCodes) do begin
      if oldHashCodes[i]=0 then continue;
      j:=(oldHashCodes[i] and (FCapacity-1));
      while FHashCodes[j]<>0 do
         j:=(j+1) and n;
      FHashCodes[j]:=oldHashCodes[i];
      DWSCopyData(oldKeys, i*FKeySize, FKeys.AsPData^, j*FKeySize, FKeySize);
      DWSCopyData(oldData, i*FElementSize, AsPData^, j*FElementSize, FElementSize);
   end;
end;

// LinearFind
//
function TScriptAssociativeArray.LinearFind(const item : IDataContext; var index : Integer) : Boolean;
begin
   repeat
      if FHashCodes[index]=0 then
         Exit(False)
      else if item.SameData(0, FKeys, index*FKeySize, FKeySize) then
         Exit(True);
      index:=(index+1) and (FCapacity-1);
   until False;
end;

// LinearValueFind
//
function TScriptAssociativeArray.LinearValueFind(const keyValue : Variant; var index : Integer) : Boolean;
begin
   repeat
      if FHashCodes[index]=0 then
         Exit(False)
      else if DWSSameVariant(keyValue, FKeys[index]) then
         Exit(True);
      index:=(index+1) and (FCapacity-1);
   until False;
end;

// IndexExprToKeyAndHashCode
//
procedure TScriptAssociativeArray.IndexExprToKeyAndHashCode(exec : TdwsExecution;
         index : TTypedExpr; out key : IDataContext; out hashCode : Cardinal);
var
   v : Variant;
begin
   if FKeySize>1 then
      (index as TDataExpr).GetDataPtr(exec, key)
   else begin
      index.EvalAsVariant(exec, v);
      exec.DataContext_CreateValue(v, key);
   end;
   hashCode:=key.HashCode(FKeySize);
end;

// DebugIntegrityCheck
//
procedure TScriptAssociativeArray.DebugIntegrityCheck;
var
   i : Integer;
begin
   for i := 0 to High(FHashCodes) do begin
      if FHashCodes[i] <> 0 then
         Assert(TVarData(AsVariant[i*FElementSize]).VType <> 0);
   end;
end;

// ReplaceValue
//
procedure TScriptAssociativeArray.ReplaceValue(exec : TdwsExecution; index, value : TTypedExpr);

   procedure WriteDataExpr(index : Integer);
   begin
      WriteData(index*FElementSize, (value as TDataExpr).DataPtr[exec], 0, FElementSize)
   end;

var
   i : Integer;
   hashCode : Cardinal;
   key : IDataContext;
begin
   if FCount>=FGrowth then Grow;

   IndexExprToKeyAndHashCode(exec, index, key, hashCode);
   i:=(hashCode and (FCapacity-1));
   if not LinearFind(key, i) then begin
      FHashCodes[i]:=hashCode;
      FKeys.WriteData(i*FKeySize, key,0, FKeySize);
      Inc(FCount);
   end;
   if FElementSize > 1 then
      WriteDataExpr(i)
   else value.EvalAsVariant(exec, DirectData[Addr + i]);
end;

// ReplaceValue
//
procedure TScriptAssociativeArray.ReplaceValue(exec : TdwsExecution; const key, value : Variant);
var
   i : Integer;
   hashCode : Cardinal;
begin
   if FCount >= FGrowth then Grow;

   hashCode := DWSHashCode(key);
   i := (hashCode and (FCapacity-1));
   if not LinearValueFind(key, i) then begin
      FHashCodes[i] := hashCode;
      FKeys[i] := key;
      Inc(FCount);
   end;
   AsVariant[i] := value;
end;

// GetDataPtr
//
procedure TScriptAssociativeArray.GetDataPtr(exec : TdwsExecution; index : TTypedExpr; var result : IDataContext);
var
   i : Integer;
   hashCode : Cardinal;
   key : IDataContext;
begin
   if FCreateKeyOnAccess then
      if FCount >= FGrowth then Grow;

   if (FCount > 0) or FCreateKeyOnAccess then begin
      IndexExprToKeyAndHashCode(exec, index, key, hashCode);
      i:=(hashCode and (FCapacity-1));
      if LinearFind(key, i) then begin
         CreateOffset(i*FElementSize, result);
         Exit;
      end;
   end;

   if FCreateKeyOnAccess then begin

      CreateOffset(i*FElementSize, result);
      FElementTyp.InitDataContext(result, 0);

      FHashCodes[i] := hashCode;
      FKeys.WriteData(i*FKeySize, key, 0, FKeySize);
      Inc(FCount);

   end else begin

      exec.DataContext_CreateEmpty(FElementSize, result);
      FElementTyp.InitDataContext(result, 0);

   end;
end;

// GetDataAsVariant
//
procedure TScriptAssociativeArray.GetDataAsVariant(exec : TdwsExecution; const keyValue : Variant; var result : Variant);
var
   hashCode : Cardinal;
   i : Integer;
   dc : IDataContext;
begin
   if FCreateKeyOnAccess then
      if FCount >= FGrowth then Grow;

   if (FCount > 0) or FCreateKeyOnAccess then begin
      hashCode := DWSHashCode(keyValue);
      i:=(hashCode and (FCapacity-1));
      if LinearValueFind(keyValue, i) then begin
         EvalAsVariant(i, result);
         Exit;
      end;
   end else hashcode := 0;

   dc := TDataContext.CreateStandalone(1);
   FElementTyp.InitDataContext(dc, 0);
   dc.EvalAsVariant(0, result);

   if FCreateKeyOnAccess then begin
      FHashCodes[i] := hashCode;
      FKeys[i] := keyValue;
      Inc(FCount);
      AsVariant[i] := result;
   end;
end;

// GetDataAsBoolean
//
function TScriptAssociativeArray.GetDataAsBoolean(exec : TdwsExecution; index : TTypedExpr) : Boolean;
var
   i : Integer;
   hashCode : Cardinal;
   key : IDataContext;
begin
   if FCount>0 then begin
      IndexExprToKeyAndHashCode(exec, index, key, hashCode);
      i:=(hashCode and (FCapacity-1));
      if LinearFind(key, i) then begin
         Result := AsBoolean[i];
         Exit;
      end;
   end;
   Result := False;
end;

// GetDataAsBoolean
//
function TScriptAssociativeArray.GetDataAsBoolean(exec : TdwsExecution; const keyValue : Variant) : Boolean;
var
   hashCode : Cardinal;
   i : Integer;
begin
   if FCount>0 then begin
      hashCode := DWSHashCode(keyValue);
      i:=(hashCode and (FCapacity-1));
      if LinearValueFind(keyValue, i) then begin
         Result := AsBoolean[i];
         Exit;
      end;
   end;
   Result := False;
end;

// GetDataAsInteger
//
function TScriptAssociativeArray.GetDataAsInteger(exec : TdwsExecution; index : TTypedExpr) : Int64;
var
   i : Integer;
   hashCode : Cardinal;
   key : IDataContext;
begin
   if FCount>0 then begin
      IndexExprToKeyAndHashCode(exec, index, key, hashCode);
      i:=(hashCode and (FCapacity-1));
      if LinearFind(key, i) then begin
         Result := AsInteger[i];
         Exit;
      end;
   end;
   Result := 0;
end;

// GetDataAsInteger
//
function TScriptAssociativeArray.GetDataAsInteger(exec : TdwsExecution; const keyValue : Variant) : Int64;
var
   hashCode : Cardinal;
   i : Integer;
begin
   if FCount>0 then begin
      hashCode := DWSHashCode(keyValue);
      i:=(hashCode and (FCapacity-1));
      if LinearValueFind(keyValue, i) then begin
         Result := AsInteger[i];
         Exit;
      end;
   end;
   Result := 0;
end;

// GetDataAsString
//
procedure TScriptAssociativeArray.GetDataAsString(exec : TdwsExecution; index : TTypedExpr; var result : String);
var
   i : Integer;
   hashCode : Cardinal;
   key : IDataContext;
begin
   if FCount>0 then begin
      IndexExprToKeyAndHashCode(exec, index, key, hashCode);
      i:=(hashCode and (FCapacity-1));
      if LinearFind(key, i) then begin
         EvalAsString(i, result);
         Exit;
      end;
   end;
   FElementTyp.InitString(result);
end;

// GetDataAsString
//
procedure TScriptAssociativeArray.GetDataAsString(exec : TdwsExecution; const keyValue : Variant; var result : String);
var
   hashCode : Cardinal;
   i : Integer;
begin
   if FCount>0 then begin
      hashCode := DWSHashCode(keyValue);
      i:=(hashCode and (FCapacity-1));
      if LinearValueFind(keyValue, i) then begin
         EvalAsString(i, result);
         Exit;
      end;
   end;
   FElementTyp.InitString(result);
end;

// ContainsKey
//
function TScriptAssociativeArray.ContainsKey(exec : TdwsExecution; index : TTypedExpr) : Boolean;
var
   i : Integer;
   hashCode : Cardinal;
   key : IDataContext;
begin
   if FCount <= 0 then Exit(False);

   IndexExprToKeyAndHashCode(exec, index, key, hashCode);
   i := (hashCode and (FCapacity-1));
   Result := LinearFind(key, i);
end;

// Delete
//
function TScriptAssociativeArray.Delete(exec : TdwsExecution; index : TTypedExpr) : Boolean;
var
   i, k, gap : Integer;
   hashCode : Cardinal;
   key : IDataContext;
begin
   if FCount = 0 then Exit(False);

   IndexExprToKeyAndHashCode(exec, index, key, hashCode);
   i:=(hashCode and (FCapacity-1));
   if not LinearFind(key, i) then Exit(False);

   gap := i;
   repeat
      i := (i+1) and (FCapacity-1);

      if FHashCodes[i] = 0 then
         Break;

      k := FHashCodes[i] and (FCapacity - 1);

      if ((gap >= k) or (k > i)) and ((i >= gap) or (k <= gap)) and ((i >= gap) or (k > i)) then begin
         InternalCopyData(i*FElementSize, gap*FElementSize, FElementSize);
         DWSCopyData(FKeys.AsPData^, i*FKeySize, gap*FKeySize, FKeySize);
         FHashCodes[gap] := FHashCodes[i];
         FHashCodes[i] := 0;
         gap := i;
      end;
   until False;

   FHashCodes[gap] := 0;
   FKeyTyp.InitDataContext(FKeys, gap*FKeySize);
   FElementTyp.InitDataContext(Self, gap*FElementSize);
   Dec(FCount);

   Result := True;

   //DebugIntegrityCheck;
end;

// Clear
//
procedure TScriptAssociativeArray.Clear;
begin
   SetDataLength(0);
   FKeys := TDataContext.CreateStandalone(0);
   FHashCodes:=nil;
   FCount:=0;
   FCapacity:=0;
   FGrowth:=0;
end;

// ReadBucket
//
function TScriptAssociativeArray.ReadBucket(index : Integer; var key : TData; var value : IDataContext) : Boolean;
begin
   if Cardinal(index) >= Cardinal(FCapacity) then Exit(False);
   if FHashCodes[index] = 0 then Exit(False);

   DWSCopyData(FKeys.AsPData^, index*FKeySize, key, 0, FKeySize);
   CreateOffset(index*FElementSize, value);
   Result := True;
end;

// Count
//
function TScriptAssociativeArray.Count : NativeInt;
begin
   Result := FCount;
end;

// Capacity
//
function TScriptAssociativeArray.Capacity : NativeInt;
begin
   Result := FCapacity;
end;

// CopyKeys
//
procedure TScriptAssociativeArray.CopyKeys(const dest : IScriptDynArray);
var
   i, k : Integer;
begin
   dest.ArrayLength := FCount;
   k := 0;
   if FKeySize > 1 then begin
      for i := 0 to FCapacity-1 do begin
         if FHashCodes[i] <> 0 then begin
            dest.WriteData(k*FKeySize, FKeys, i*FKeySize, FKeySize);
            Inc(k, FKeySize);
         end;
      end;
   end else begin
      for i := 0 to FCapacity-1 do begin
         if FHashCodes[i] <> 0 then begin
            dest.AsVariant[k] := FKeys.AsVariant[i];
            Inc(k);
         end;
      end;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TAssociativeArraySymbol.SetInitAssociativeArrayProc(TScriptAssociativeArray_InitData);

end.
