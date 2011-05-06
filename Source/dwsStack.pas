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
{$I dws.inc}
unit dwsStack;

interface

uses Variants, Classes, SysUtils, dwsStrings, dwsUtils;

type

   TData = array of Variant;
   PData = ^TData;
   TDataArray = array [0..0] of Variant;
   PDataArray = ^TDataArray;
   PIUnknown = ^IUnknown;

   TStackParameters = record
      MaxLevel : Integer;
      ChunkSize : Integer;
      MaxByteSize : Integer;
      MaxRecursionDepth : Integer;
   end;

   {$IFDEF VER200}
   // D2009 workaround for:
   // [DCC Fatal Error] dwsCoreExprs.pas(5327): F2051 Unit dwsCoreExprs was compiled with a different version of dwsStack.TSimpleStack<System.Integer>
   TSimpleStackIntegerDummy = TSimpleStack<Integer>;
   {$ENDIF}

   // TStackMixIn
   //
   TStack = ^TStackMixIn;
   TStackMixIn = record
      private
         FBasePointer : Integer;
         FBaseData : PDataArray;
         FBpStore : array of TSimpleStack<Integer>;
         FParams : TStackParameters;
         FMaxSize : Integer;
         FSize : Integer;
         FStackPointer : Integer;

         function GetFrameSize : Integer;

         procedure ClearBpStore;

         procedure GrowTo(desiredSize : Integer);

         procedure SetBasePointer(newBp : Integer); inline;

      public
         Data: TData;

         procedure Initialize(const params : TStackParameters);
         procedure Finalize;

         procedure Push(Delta: Integer); inline;
         procedure Pop(Delta: Integer);

         procedure WriteData(sourceAddr, destAddr, size: Integer; const sourceData: TData);
         procedure ReadData(sourceAddr, destAddr, size: Integer; destData: TData);
         procedure CopyData(sourceAddr, destAddr, size: Integer);

         procedure ClearData(addr, size : Integer); inline;

         procedure WriteValue(DestAddr: Integer; const Value: Variant);
         procedure WriteIntValue(DestAddr: Integer; const Value: Int64); overload; inline;
         procedure WriteIntValue_BaseRelative(DestAddr: Integer; const Value: Int64); overload; inline;
         procedure WriteIntValue_BaseRelative(DestAddr: Integer; const pValue: PInt64); overload; inline;
         procedure WriteFloatValue(DestAddr: Integer; const Value: Double); inline;
         procedure WriteFloatValue_BaseRelative(DestAddr: Integer; const Value: Double); inline;
         procedure WriteStrValue(DestAddr: Integer; const Value: String); inline;
         procedure WriteBoolValue(DestAddr: Integer; const Value: Boolean); inline;
         procedure WriteInterfaceValue(DestAddr: Integer; const intf: IUnknown);

         function  SetStrChar(DestAddr: Integer; index : Integer; c : Char) : Boolean;

         procedure ReadValue(sourceAddr : Integer; var result : Variant); inline;
         function  ReadIntValue(SourceAddr: Integer): Int64; inline;
         function  ReadIntValue_BaseRelative(SourceAddr: Integer): Int64; inline;
         function  ReadIntAsFloatValue_BaseRelative(SourceAddr: Integer) : Double; inline;
         function  ReadFloatValue(SourceAddr: Integer) : Double; inline;
         function  ReadFloatValue_BaseRelative(SourceAddr: Integer) : Double; inline;
         procedure ReadStrValue(SourceAddr: Integer; var Result : String);
         function  ReadBoolValue(SourceAddr: Integer): Boolean;
         procedure ReadInterfaceValue(SourceAddr: Integer; var Result : IUnknown);

         function  PointerToIntValue(addr : Integer) : PInt64;
         function  PointerToFloatValue_BaseRelative(addr : Integer) : PDouble;//inline;
         function  PointerToInterfaceValue(addr : Integer) : PIUnknown;

         procedure IncIntValue_BaseRelative(destAddr : Integer; const value : Int64); inline;
         procedure AppendStringValue_BaseRelative(destAddr : Integer; const value : String);

         procedure PushBp(Level, Bp: Integer); inline;
         function  GetSavedBp(Level: Integer): Integer; inline;
         procedure PopBp(Level : Integer); inline;

         function  SwitchFrame(level : Integer) : Integer; inline;
         procedure RestoreFrame(level, oldBasePointer: Integer); inline;
         procedure Reset;

         property BasePointer: Integer read FBasePointer write SetBasePointer;
         property FrameSize: Integer read GetFrameSize;
         property MaxSize: Integer read FMaxSize write FMaxSize;
         property StackPointer: Integer read FStackPointer;
         property MaxRecursionDepth : Integer read FParams.MaxRecursionDepth write FParams.MaxRecursionDepth;
   end;

   EStackException = class(Exception);

procedure DWSCopyData(const SourceData: TData; SourceAddr: Integer;
                   DestData: TData; DestAddr: Integer; Size: Integer);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// DWSCopyData
//
procedure DWSCopyData(const SourceData: TData; SourceAddr: Integer;
                      DestData: TData; DestAddr: Integer; Size: Integer);
begin
   while Size > 0 do begin
      VarCopy(DestData[DestAddr], SourceData[SourceAddr]);
      Inc(SourceAddr);
      Inc(DestAddr);
      Dec(Size);
   end;
end;

// ------------------
// ------------------ TStackMixIn ------------------
// ------------------

// Initialize
//
procedure TStackMixIn.Initialize(const params : TStackParameters);
begin
   FParams:=params;
   FMaxSize:=params.MaxByteSize div SizeOf(Variant);
end;

// Destroy
//
procedure TStackMixIn.Finalize;
begin
   ClearBpStore;
end;

// ClearBpStore
//
procedure TStackMixIn.ClearBpStore;
var
   i : Integer;
begin
   for i:=0 to High(FBpStore) do
      FBpStore[i].Free;
end;

procedure TStackMixIn.CopyData(SourceAddr, DestAddr, Size: Integer);
begin
  while Size > 0 do
  begin
    VarCopy(Data[DestAddr], Data[SourceAddr]);
    Inc(SourceAddr);
    Inc(DestAddr);
    Dec(Size);
  end;
end;

// ClearData
//
procedure TStackMixIn.ClearData(addr, size : Integer);
var
   p : PVariant;
   i : Integer;
begin
   p:=@Data[addr];
   for i:=1 to size do begin
      VarClear(p^);
      Inc(p);
   end;
end;

function TStackMixIn.GetFrameSize: Integer;
begin
  Result := FStackPointer - FBasePointer;
end;

procedure TStackMixIn.Pop(delta : Integer);
var
   x, sp : Integer;
   v : PVariant;
begin
   sp:=FStackPointer;
   v:=@Data[sp];
   sp:=sp-delta;
   for x:=1 to delta do begin
      Dec(v);
      VarClear(v^);
   end;
   FStackPointer:=sp;
end;

// GrowTo
//
procedure TStackMixIn.GrowTo(desiredSize : Integer);
begin
   if desiredSize > FMaxSize then
      raise EStackException.CreateFmt(RTE_MaximalDatasizeExceeded, [FMaxSize]);
   FSize := ((desiredSize) div FParams.ChunkSize + 1) * FParams.ChunkSize;
   if FSize > FMaxSize then
      FSize := FMaxSize;
   SetLength(Data, FSize);
   BasePointer:=BasePointer;
end;

// SetBasePointer
//
procedure TStackMixIn.SetBasePointer(newBp : Integer);
begin
   FBasePointer:=newBp;
   FBaseData:=@Data[FBasePointer];
end;

// Push
//
procedure TStackMixIn.Push(Delta: Integer);
var
   sp : Integer;
begin
   sp := FStackPointer + Delta;

   // Increase stack size if necessary
   if sp > FSize then
      GrowTo(sp);

   FStackPointer := sp;
end;

procedure TStackMixIn.Reset;
var
   i : Integer;
begin
   Data := nil;
   FSize := 0;
   FStackPointer := 0;
   FBasePointer := 0;
   FBaseData:=nil;
   ClearBpStore;
   SetLength(FBpStore, FParams.MaxLevel + 1);
   for i:=0 to High(FBpStore) do begin
      FBpStore[i]:=TSimpleStack<Integer>.Create;
      FBpStore[i].Push(0);
   end;
end;

// PushBp
//
procedure TStackMixIn.PushBp(Level, Bp: Integer);
begin
   Assert(Cardinal(Level)<=Cardinal(FParams.MaxLevel));
   FBpStore[Level].Push(Bp);
end;

// GetSavedBp
//
function TStackMixIn.GetSavedBp(Level: Integer): Integer;
begin
   Assert(Cardinal(Level)<=Cardinal(FParams.MaxLevel));
   Result:=FBpStore[Level].Peek;
end;

// PopBp
//
procedure TStackMixIn.PopBp(Level : Integer);
begin
   FBpStore[Level].Pop;
end;

// SwitchFrame
//
function TStackMixIn.SwitchFrame(level : Integer) : Integer;
begin
   Result:=FBasePointer;
   BasePointer:=FStackPointer;
   PushBP(level, Result);
end;

// RestoreFrame
//
procedure TStackMixIn.RestoreFrame(level, oldBasePointer : Integer);
begin
   FStackPointer:=BasePointer;
   BasePointer:=oldBasePointer;
   PopBp(level);
end;

procedure TStackMixIn.ReadData(SourceAddr, DestAddr, Size: Integer; DestData: TData);
begin
  while Size > 0 do
  begin
    VarCopy(DestData[DestAddr], Data[SourceAddr]);
    Inc(SourceAddr);
    Inc(DestAddr);
    Dec(Size);
  end;
end;

// ReadValue
//
procedure TStackMixIn.ReadValue(sourceAddr : Integer; var result : Variant);
begin
   result:=Data[sourceAddr];
end;

// ReadIntValue
//
function TStackMixIn.ReadIntValue(SourceAddr: Integer): Int64;
var
   varData : PVarData;
begin
   varData:=@Data[SourceAddr];
   if varData.VType=varInt64 then
      Result:=varData.VInt64
   else Result:=PVariant(varData)^;
end;

// ReadIntValue
//
function TStackMixIn.ReadIntValue_BaseRelative(SourceAddr: Integer): Int64;
var
   varData : PVarData;
begin
   varData:=@FBaseData[SourceAddr];
   if varData.VType=varInt64 then
      Result:=varData.VInt64
   else Result:=PVariant(varData)^;
end;

// ReadIntAsFloatValue_BaseRelative
//
function TStackMixIn.ReadIntAsFloatValue_BaseRelative(SourceAddr: Integer) : Double;
var
   varData : PVarData;
begin
   varData:=@FBaseData[SourceAddr];
   Assert(varData.VType=varInt64);
   Result:=varData.VInt64;
end;

// ReadFloatValue
//
function TStackMixIn.ReadFloatValue(SourceAddr: Integer) : Double;
var
   varData : PVarData;
begin
   varData:=@Data[SourceAddr];
   Assert(varData.VType=varDouble);
   Result:=varData.VDouble;
end;

// ReadFloatValue_BaseRelative
//
function TStackMixIn.ReadFloatValue_BaseRelative(SourceAddr: Integer) : Double;
var
   varData : PVarData;
begin
   varData:=@FBaseData[SourceAddr];
   Assert(varData.VType=varDouble);
   Result:=varData.VDouble;
end;

// ReadStrValue
//
procedure TStackMixIn.ReadStrValue(SourceAddr: Integer; var Result : String);
var
   varData : PVarData;
begin
   varData:=@Data[SourceAddr];
   if varData.VType=varUString then
      Result:=String(varData.VUString)
   else Result:=PVariant(varData)^;
end;

// ReadBoolValue
//
function TStackMixIn.ReadBoolValue(SourceAddr: Integer): Boolean;
var
   varData : PVarData;
begin
   varData:=@Data[SourceAddr];
   if varData.VType=varBoolean then
      Result:=varData.VBoolean
   else Result:=PVariant(varData)^;
end;

// ReadInterfaceValue
//
procedure TStackMixIn.ReadInterfaceValue(SourceAddr: Integer; var Result : IUnknown);
var
   varData : PVarData;
begin
   varData:=@Data[SourceAddr];
   if varData.VType=varUnknown then
      Result:=IUnknown(varData.VUnknown)
   else Result:=PVariant(varData)^;
end;

// PointerToIntValue
//
function TStackMixIn.PointerToIntValue(addr : Integer) : PInt64;
var
   varData : PVarData;
begin
   varData:=@Data[addr];
   Assert(varData.VType=varInt64);
   Result:=@varData.VInt64;
end;

// PointerToFloatValue_BaseRelative
//
function TStackMixIn.PointerToFloatValue_BaseRelative(addr : Integer) : PDouble;
var
   varData : PVarData;
begin
   varData:=@FBaseData[addr];
   Assert(varData.VType=varDouble);
   Result:=@varData.VDouble;
end;

// PointerToInterfaceValue
//
function TStackMixIn.PointerToInterfaceValue(addr : Integer) : PIUnknown;
var
   varData : PVarData;
begin
   varData:=@Data[addr];
   Assert(varData.VType=varUnknown);
   Result:=@varData.VUnknown;
end;

// IncIntValue_BaseRelative
//
procedure TStackMixIn.IncIntValue_BaseRelative(destAddr: Integer; const value: Int64);
var
   varData : PVarData;
begin
   varData:=@FBaseData[destAddr];
   Assert(varData.VType=varInt64);
   varData.VInt64:=varData.VInt64+value
end;

// AppendStringValue_BaseRelative
//
procedure TStackMixIn.AppendStringValue_BaseRelative(destAddr : Integer; const value : String);

   procedure Fallback(varData : PVarData);
   begin
      PVariant(varData)^:=PVariant(varData)^+value;
   end;

var
   varData : PVarData;
begin
   varData:=@FBaseData[destAddr];
   if varData.VType=varUString then
      String(varData.VUString):=String(varData.VUString)+value
   else Fallback(varData);
end;

// WriteData
//
procedure TStackMixIn.WriteData(SourceAddr, DestAddr, Size: Integer; const SourceData: TData);
begin
   while Size>0 do begin
      Data[DestAddr]:=SourceData[SourceAddr];
      Inc(SourceAddr);
      Inc(DestAddr);
      Dec(Size);
   end;
end;

// WriteValue
//
procedure TStackMixIn.WriteValue(DestAddr: Integer; const Value: Variant);
begin
  VarCopy(Data[DestAddr], Value);
end;

// WriteIntValue
//
procedure TStackMixIn.WriteIntValue(DestAddr: Integer; const Value: Int64);
var
   varData : PVarData;
begin
   varData:=@Data[DestAddr];
   if varData.VType=varInt64 then
      varData.VInt64:=Value
   else PVariant(varData)^:=Value;
end;

// WriteIntValue_BaseRelative
//
procedure TStackMixIn.WriteIntValue_BaseRelative(DestAddr: Integer; const Value: Int64);
var
   varData : PVarData;
begin
   varData:=@FBaseData[DestAddr];
   if varData.VType=varInt64 then
      varData.VInt64:=Value
   else PVariant(varData)^:=Value;
end;

// WriteIntValue_BaseRelative
//
procedure TStackMixIn.WriteIntValue_BaseRelative(DestAddr: Integer; const pValue: PInt64);
var
   varData : PVarData;
begin
   varData:=@FBaseData[DestAddr];
   if varData.VType=varInt64 then
      varData.VInt64:=pValue^
   else PVariant(varData)^:=pValue^;
end;

// WriteFloatValue
//
procedure TStackMixIn.WriteFloatValue(DestAddr: Integer; const value : Double);
var
   varData : PVarData;
begin
   varData:=@Data[DestAddr];
   if varData.VType=varDouble then
      varData.VDouble:=Value
   else PVariant(varData)^:=Value;
end;

// WriteFloatValue_BaseRelative
//
procedure TStackMixIn.WriteFloatValue_BaseRelative(DestAddr: Integer; const value : Double);
var
   varData : PVarData;
begin
   varData:=@FBaseData[DestAddr];
   if varData.VType=varDouble then
      varData.VDouble:=Value
   else PVariant(varData)^:=Value;
end;

// WriteStrValue
//
procedure TStackMixIn.WriteStrValue(DestAddr: Integer; const Value: String);
var
   varData : PVarData;
begin
   varData:=@Data[DestAddr];
   if varData.VType=varUString then
      String(varData.VUString):=Value
   else PVariant(varData)^:=Value;
end;

// WriteBoolValue
//
procedure TStackMixIn.WriteBoolValue(DestAddr: Integer; const Value: Boolean);
var
   varData : PVarData;
begin
   varData:=@Data[DestAddr];
   if varData.VType=varBoolean then
      varData.VBoolean:=Value
   else PVariant(varData)^:=Value;
end;

// WriteInterfaceValue
//
procedure TStackMixIn.WriteInterfaceValue(DestAddr: Integer; const intf: IUnknown);
var
   varData : PVarData;
begin
   varData:=@Data[DestAddr];
   if varData.VType=varUnknown then
      PUnknown(@varData.VUnknown)^:=intf
   else PVariant(varData)^:=intf;
end;

// SetStrChar
//
function TStackMixIn.SetStrChar(DestAddr: Integer; index : Integer; c : Char) : Boolean;
var
   varData : PVarData;
begin
   varData:=@Data[DestAddr];
   if varData.VType=varUString then
      if index>Length(String(varData.VUString)) then
         Exit(False)
      else String(varData.VUString)[index]:=c
   else PVariant(varData)^[index]:=c;
   Result:=True;
end;

end.
