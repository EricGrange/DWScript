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
unit dwsStack;

{$I dws.inc}

interface

uses
   Variants, Classes, SysUtils,
   dwsStrings, dwsUtils, dwsXPlatform, dwsDataContext;

type

   TStackParameters = record
      MaxLevel : Integer;
      ChunkSize : Integer;
      MaxByteSize : Integer;
      MaxRecursionDepth : Integer;
      MaxExceptionDepth : Integer;
   end;

   // TStackMixIn
   //
   TStack = ^TStackMixIn;
   TStackMixIn = record
      private
         FBaseData : PDataArray;
         FBasePointer : Integer;
         FBpStore : array of TSimpleIntegerStack;
         FParams : TStackParameters;
         FMaxSize : Integer;
         FSize : Integer;
         FStackPointer : Integer;
         FDataPtrPool : IDataContextPool;

         function GetFrameSize : Integer;

         procedure ClearBpStore;

         procedure GrowTo(desiredSize : Integer);

         procedure SetBasePointer(newBp : Integer); inline;

      public
         Data : TData;

         procedure Initialize(const params : TStackParameters);
         procedure Finalize;

         procedure Push(delta : Integer); inline;
         procedure Pop(delta : Integer); inline;

         procedure WriteData(sourceAddr, destAddr, size: Integer; const sourceData: TData);
         procedure ReadData(sourceAddr, destAddr, size: Integer; destData: TData);
         procedure CopyData(sourceAddr, destAddr, size: Integer);

         procedure ClearData(addr, size : Integer); inline;

         procedure WriteValue(destAddr: Integer; const Value: Variant);
         procedure WriteIntValue(destAddr: Integer; const Value: Int64); inline;
         procedure WriteIntValue_BaseRelative(destAddr: Integer; const Value: Int64); inline;
         procedure WriteFloatValue(destAddr: Integer; const Value: Double); inline;
         procedure WriteFloatValue_BaseRelative(destAddr: Integer; const Value: Double); inline;
         procedure WriteStrValue(destAddr: Integer; const Value: UnicodeString); inline;
         procedure WriteStrValue_BaseRelative(destAddr: Integer; const Value: UnicodeString); inline;
         procedure WriteBoolValue(destAddr: Integer; const Value: Boolean); inline;
         procedure WriteInterfaceValue(destAddr: Integer; const intf: IUnknown);

         function  SetStrChar(DestAddr: Integer; index : Integer; c : WideChar) : Boolean;

         procedure ReadValue(sourceAddr : Integer; var result : Variant); inline;
         function  ReadIntValue(SourceAddr: Integer): Int64; inline;
         function  ReadIntValue_BaseRelative(SourceAddr: Integer) : Int64; inline;
         function  ReadIntAsFloatValue_BaseRelative(SourceAddr: Integer) : Double; inline;
         function  ReadFloatValue(SourceAddr: Integer) : Double; //inline;
         function  ReadFloatValue_BaseRelative(SourceAddr: Integer) : Double; inline;
         procedure ReadStrValue(SourceAddr: Integer; var Result : UnicodeString);
         function  ReadBoolValue(SourceAddr: Integer): Boolean;
         procedure ReadInterfaceValue(SourceAddr: Integer; var Result : IUnknown);

         function  PointerToIntValue(addr : Integer) : PInt64;
         function  PointerToFloatValue_BaseRelative(addr : Integer) : PDouble;
         function  PointerToStringValue_BaseRelative(addr : Integer) : PUnicodeString;// inline;
         function  PointerToInterfaceValue_BaseRelative(addr : Integer) : PIUnknown;

         procedure InitDataPtr(var dataPtr : IDataContext; addr : Integer); inline;
         procedure InitDataPtrLevel(var dataPtr : IDataContext; level, addr : Integer); inline;
         function  CreateDataContext(const data : TData; addr : Integer) : TDataContext; inline;

         procedure IncIntValue_BaseRelative(destAddr : Integer; const value : Int64); inline;
         procedure AppendStringValue_BaseRelative(destAddr : Integer; const value : UnicodeString);

         // D2010 compiler crashes when inlining those
         procedure PushBp(Level, Bp: Integer); {$IFDEF DELPHI_XE_PLUS} inline; {$ENDIF}
         function  GetSavedBp(Level: Integer): Integer; {$IFDEF DELPHI_XE_PLUS} inline; {$ENDIF}
         procedure PopBp(Level : Integer); {$IFDEF DELPHI_XE_PLUS} inline; {$ENDIF}

         procedure FixBaseStack(newSize : Integer);

         function  SwitchFrame(level : Integer) : Integer; inline;
         procedure RestoreFrame(level, oldBasePointer: Integer); inline;
         procedure Reset;

         property BasePointer: Integer read FBasePointer write SetBasePointer;
         property BaseData: PDataArray read FBaseData;
         property FrameSize: Integer read GetFrameSize;
         property MaxSize: Integer read FMaxSize write FMaxSize;
         property StackPointer: Integer read FStackPointer;
         property MaxRecursionDepth : Integer read FParams.MaxRecursionDepth write FParams.MaxRecursionDepth;
         property MaxExceptionDepth : Integer read FParams.MaxExceptionDepth write FParams.MaxExceptionDepth;
   end;

   EScriptStackException = class(Exception);
   EScriptStackOverflow = class(EScriptStackException);
   EScriptExceptionOverflow = class(EScriptStackException);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R-}

// ------------------
// ------------------ TStackMixIn ------------------
// ------------------

// Initialize
//
procedure TStackMixIn.Initialize(const params : TStackParameters);
begin
   FParams:=params;
   FMaxSize:=params.MaxByteSize div SizeOf(Variant);
   FDataPtrPool:=TDataContextPool.Create;
end;

// Destroy
//
procedure TStackMixIn.Finalize;
begin
   ClearBpStore;
   FDataPtrPool.Cleanup;
   FDataPtrPool:=nil;
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
   while Size > 0 do begin
      VarCopySafe(Data[DestAddr], Data[SourceAddr]);
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
      VarClearSafe(p^);
      Inc(p);
   end;
end;

function TStackMixIn.GetFrameSize: Integer;
begin
  Result := FStackPointer - FBasePointer;
end;

// GrowTo
//
procedure TStackMixIn.GrowTo(desiredSize : Integer);
begin
   if desiredSize > FMaxSize then
      raise EScriptStackException.CreateFmt(RTE_MaximalDatasizeExceeded, [FMaxSize]);
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
procedure TStackMixIn.Push(delta : Integer);
var
   sp : Integer;
begin
   sp:=FStackPointer+delta;
   FStackPointer:=sp;

   // Increase stack size if necessary
   if sp>FSize then
      GrowTo(sp);
end;

// Pop
//
procedure TStackMixIn.Pop(delta : Integer);
var
   i : Integer;
   v : PVariant;
begin
   if delta=0 then Exit;
   v:=@Data[FStackPointer];
   for i:=1 to delta do begin
      Dec(v);
      VarClearSafe(v^);
   end;
   Dec(FStackPointer, delta);
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

// FixBaseStack
//
procedure TStackMixIn.FixBaseStack(newSize : Integer);
begin
   Assert(BasePointer=0);
   Assert(FBpStore[0].Count=2);
   GrowTo(newSize);
   FStackPointer:=newSize;
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

procedure TStackMixIn.Reset;
var
   i : Integer;
begin
   Data:=nil;
   FSize:=0;
   FStackPointer:=0;
   FBasePointer:=0;
   FBaseData:=nil;
   ClearBpStore;
   SetLength(FBpStore, FParams.MaxLevel + 1);
   for i:=0 to High(FBpStore) do begin
      FBpStore[i]:=TSimpleIntegerStack.Allocate;
      FBpStore[i].Push(0);
   end;
   FDataPtrPool.Cleanup;
end;

procedure TStackMixIn.ReadData(SourceAddr, DestAddr, Size: Integer; DestData: TData);
begin
  while Size > 0 do
  begin
    VarCopySafe(DestData[DestAddr], Data[SourceAddr]);
    Inc(SourceAddr);
    Inc(DestAddr);
    Dec(Size);
  end;
end;

// ReadValue
//
procedure TStackMixIn.ReadValue(sourceAddr : Integer; var result : Variant);
begin
   VarCopySafe(result, Data[sourceAddr]);
end;

// ReadIntValue
//
function TStackMixIn.ReadIntValue(SourceAddr: Integer): Int64;
var
   varData : PVarData;
begin
   varData:=@Data[SourceAddr];
   Assert(varData.VType=varInt64);
   Result:=varData.VInt64
end;

// ReadIntValue
//
function TStackMixIn.ReadIntValue_BaseRelative(SourceAddr: Integer) : Int64;
var
   varData : PVarData;
begin
   varData:=@FBaseData[SourceAddr];
   Assert(varData.VType=varInt64);
   Result:=varData.VInt64;
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
   if varData.VType=varDouble then
      Result:=varData.VDouble
   else if varData.VType=varInt64 then
      Result:=varData.VInt64
   else Result:=PVariant(varData)^;
end;

// ReadFloatValue_BaseRelative
//
function TStackMixIn.ReadFloatValue_BaseRelative(SourceAddr: Integer) : Double;
var
   varData : PVarData;
begin
   varData:=@FBaseData[SourceAddr];
   if varData.VType=varDouble then
      Result:=varData.VDouble
   else if varData.VType=varInt64 then
      Result:=varData.VInt64
   else Result:=PVariant(varData)^;
end;

// ReadStrValue
//
procedure TStackMixIn.ReadStrValue(SourceAddr: Integer; var Result : UnicodeString);
var
   varData : PVarData;
begin
   varData:=@Data[SourceAddr];
   if varData.VType=varUString then
      Result:=UnicodeString(varData.VString)
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

// PointerToStringValue_BaseRelative
//
function TStackMixIn.PointerToStringValue_BaseRelative(addr : Integer) : PUnicodeString;
var
   varData : PVarData;
begin
   varData:=@FBaseData[addr];
   Assert(varData.VType=varUString);
   Result:=@varData.VString;
end;

// PointerToInterfaceValue_BaseRelative
//
function TStackMixIn.PointerToInterfaceValue_BaseRelative(addr : Integer) : PIUnknown;
var
   varData : PVarData;
begin
   varData:=@FBaseData[addr];
   Assert(varData.VType=varUnknown);
   Result:=@varData.VUnknown;
end;

// InitDataPtr
//
procedure TStackMixIn.InitDataPtr(var dataPtr : IDataContext; addr : Integer);
begin
   dataPtr:=FDataPtrPool.Create(Data, BasePointer+addr);
end;

// InitDataPtrLevel
//
procedure TStackMixIn.InitDataPtrLevel(var dataPtr : IDataContext; level, addr : Integer);
begin
   dataPtr:=FDataPtrPool.Create(Data, GetSavedBp(level)+addr);
end;

// CreateDataContext
//
function TStackMixIn.CreateDataContext(const data : TData; addr : Integer) : TDataContext;
begin
   Result:=FDataPtrPool.Create(data, addr);
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
procedure TStackMixIn.AppendStringValue_BaseRelative(destAddr : Integer; const value : UnicodeString);
var
   varData : PVarData;
begin
   varData:=@FBaseData[destAddr];
   Assert(varData.VType=varUString);
   UnicodeString(varData.VString):=UnicodeString(varData.VString)+value
end;

// WriteData
//
procedure TStackMixIn.WriteData(SourceAddr, DestAddr, Size: Integer; const SourceData: TData);
begin
   while Size>0 do begin
      VarCopySafe(Data[DestAddr], SourceData[SourceAddr]);
      Inc(SourceAddr);
      Inc(DestAddr);
      Dec(Size);
   end;
end;

// WriteValue
//
procedure TStackMixIn.WriteValue(DestAddr: Integer; const Value: Variant);
begin
   VarCopySafe(Data[destAddr], Value);
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
   else VarCopySafe(PVariant(varData)^, Value);
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
   else VarCopySafe(PVariant(varData)^, Value);
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
   else VarCopySafe(PVariant(varData)^, Value);
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
   else VarCopySafe(PVariant(varData)^, Value);
end;

// WriteStrValue
//
procedure TStackMixIn.WriteStrValue(DestAddr: Integer; const Value: UnicodeString);
var
   varData : PVarData;
begin
   varData:=@Data[DestAddr];
   if varData.VType=varUString then
      UnicodeString(varData.VString):=Value
   else VarCopySafe(PVariant(varData)^, Value);
end;

// WriteStrValue_BaseRelative
//
procedure TStackMixIn.WriteStrValue_BaseRelative(DestAddr: Integer; const Value: UnicodeString);
var
   varData : PVarData;
begin
   varData:=@FBaseData[DestAddr];
   if varData.VType=varUString then
      UnicodeString(varData.VString):=Value
   else VarCopySafe(PVariant(varData)^, Value);
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
   else VarCopySafe(PVariant(varData)^, Value);
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
   else VarCopySafe(PVariant(varData)^, intf);
end;

// SetStrChar
//
function TStackMixIn.SetStrChar(DestAddr: Integer; index : Integer; c : WideChar) : Boolean;
var
   varData : PVarData;
begin
   varData:=@Data[DestAddr];
   if varData.VType=varUString then
      if index>Length(UnicodeString(varData.VString)) then
         Exit(False)
      else UnicodeString(varData.VString)[index]:=c
   else PVariant(varData)^[index]:=c;
   Result:=True;
end;

end.
