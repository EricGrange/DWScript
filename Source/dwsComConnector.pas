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
unit dwsComConnector;

{$I dws.inc}

interface

uses
   Windows, Variants, Classes, SysUtils, SysConst,
   ComObj, ComConst, ActiveX, AxCtrls,
   dwsComp, dwsSymbols, dwsDataContext,
   dwsExprs, dwsStrings, dwsFunctions, dwsStack,
   dwsOperators, dwsUtils;

const
   COM_ConnectorCaption = 'COM Connector 1.0';
   COM_UnitName = 'COM';

type
   TdwsComConnector = class(TdwsAbstractStaticUnit, IUnknown, IConnector)
      private
         function ConnectorCaption: String;
         function ConnectorName: String;
         function GetUnit(const UnitName: String): IConnectorType;

      protected
         function GetUnitName: String; override;
         procedure AddUnitSymbols(Table: TSymbolTable; operators : TOperators); override;

      published
         property StaticSymbols;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
  MaxDispArgs = 64;

// DwsOleCheck
//
procedure DwsOleCheck(Result: HResult);

   procedure RaiseOleError;
   begin
      raise EOleSysError.Create(Format('OLE Error %.8x (%s)',
                                       [Cardinal(Result), SysErrorMessage(Cardinal(Result))]),
                                Result, 0);
   end;

begin
   if not Succeeded(Result) then
      RaiseOleError;
end;

type
  TCreateOleObjectFunc = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

  TGetActiveOleObjectFunc = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

  TClassIDToProgIDFunc = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

  TOleInt32Func = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

  TOleInt64Func = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

  TOleDateFunc = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

   TComConnectorType = class(TInterfacedSelfObject, IUnknown, IConnectorType, IConnectorEnumerator)
      private
         FTable : TSymbolTable;

      protected
         { IConnectorType }
         function ConnectorCaption: String;
         function AcceptsParams(const params: TConnectorParamArray) : Boolean;
         function HasMethod(const MethodName: String; const Params: TConnectorParamArray;
                            var TypSym: TTypeSymbol): IConnectorCall;
         function HasMember(const MemberName: String; var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorMember;
         function HasIndex(const propName: String; const Params: TConnectorParamArray;
                           var typSym: TTypeSymbol; IsWrite: Boolean): IConnectorCall;
         function HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;

         function NewEnumerator(const base : Variant; const args : TConnectorArgs) : IUnknown;
         function Step(const enumerator : IInterface; var data : TData) : Boolean;

      public
         constructor Create(Table: TSymbolTable);
   end;

   TComConnectorCall = class(TInterfacedSelfObject, IUnknown, IConnectorCall)
      private
         FMethodName : WideString;
         FPMethodName : PWideString;
         FMethodType : Cardinal;

      protected
         function Call(const Base: Variant; const args : TConnectorArgs) : TData;
         function NeedDirectReference : Boolean;

      public
         constructor Create(const MethodName: String; const Params: TConnectorParamArray;
                            MethodType: Cardinal = DISPATCH_METHOD);
  end;

   TComConnectorMember = class(TInterfacedSelfObject, IUnknown, IConnectorMember)
      private
         FMemberName : WideString;
         FPMemberName : PWideString;

      protected
         function GetDispID(const Disp: IDispatch) : Integer;
         function Read(const Base: Variant): TData;
         procedure Write(const Base: Variant; const Data: TData);

      public
         constructor Create(const memberName : String);
   end;

   TComVariantArraySymbol = class(TConnectorSymbol)
      public
         constructor Create(const name : String; const connectorType: IConnectorType; Typ: TTypeSymbol);

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         procedure InitData(const Dat: TData; Offset: Integer); override;
   end;

   IComVariantArrayLength = interface(IConnectorMember)
   end;
   IComVariantArrayHighBound = interface(IConnectorMember)
   end;
   IComVariantArrayLowBound = interface(IConnectorMember)
   end;
   IComVariantArrayDimCount = interface(IConnectorMember)
   end;
   IComVariantArrayReadIndex = interface(IConnectorCall)
   end;
   IComVariantArrayWriteIndex = interface(IConnectorCall)
   end;
   IComVariantArrayLengthCall = interface(IConnectorCall)
   end;
   IComVariantArrayHighBoundCall = interface(IConnectorCall)
   end;
   IComVariantArrayLowBoundCall = interface(IConnectorCall)
   end;

   TComVariantArrayEnumerator = class (TInterfacedSelfObject)
      private
         FArray : Variant;
         FIndex : Integer;

      public
         constructor Create(const a : Variant);

         function Next(var v : Variant) : Boolean;
   end;

   TComVariantArrayType = class (TInterfacedSelfObject, IUnknown, IConnectorType,
                                 IComVariantArrayReadIndex, IComVariantArrayWriteIndex,
                                 IComVariantArrayLength, IComVariantArrayDimCount,
                                 IComVariantArrayHighBound, IComVariantArrayLowBound,
                                 IComVariantArrayLowBoundCall, IComVariantArrayHighBoundCall,
                                 IComVariantArrayLengthCall,
                                 IConnectorEnumerator)
      private
         FTable: TSymbolTable;

      protected
         function ReadLength(const Base: Variant): TData; overload;
         function ReadLowBound(const Base: Variant): TData; overload;
         function ReadHighBound(const Base: Variant): TData; overload;
         function ReadDimCount(const Base: Variant): TData;
         procedure WriteHighBound(const Base: Variant; const Data: TData);
         function ReadIndex(const Base: Variant; const Args: TConnectorArgs): TData;
         function WriteIndex(const Base: Variant; const Args: TConnectorArgs): TData;
         function ReadLength(const Base: Variant; const Args: TConnectorArgs): TData; overload;
         function ReadLowBound(const Base: Variant; const Args: TConnectorArgs): TData; overload;
         function ReadHighBound(const Base: Variant; const Args: TConnectorArgs): TData; overload;

         function NewEnumerator(const base : Variant; const args : TConnectorArgs) : IUnknown;
         function Step(const enumerator : IInterface; var data : TData) : Boolean;

         function NeedDirectReference : Boolean;

         { IConnectorType }
         function ConnectorCaption: String;
         function AcceptsParams(const params: TConnectorParamArray) : Boolean;
         function HasMethod(const MethodName: String; const Params: TConnectorParamArray;
                            var TypSym: TTypeSymbol): IConnectorCall;
         function HasMember(const MemberName: String; var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorMember;
         function HasIndex(const PropName: String; const Params: TConnectorParamArray;
                         var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorCall;
         function HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;

         { IConnectorCall }
         function IComVariantArrayReadIndex.Call = ReadIndex;
         function IComVariantArrayWriteIndex.Call = WriteIndex;
         function IComVariantArrayLowBoundCall.Call = ReadLowBound;
         function IComVariantArrayHighBoundCall.Call = ReadHighBound;
         function IComVariantArrayLengthCall.Call = ReadLength;

         { IConnectorMember }
         function IComVariantArrayLength.Read = ReadLength;
         function IComVariantArrayHighBound.Read = ReadHighBound;
         function IComVariantArrayLowBound.Read = ReadLowBound;
         function IComVariantArrayDimCount.Read = ReadDimCount;
         procedure IComVariantArrayHighBound.Write = WriteHighBound;
         procedure Write(const Base: Variant; const Data: TData);

         { IConnectorEnumerator }
         function IConnectorEnumerator.NewEnumerator = NewEnumerator;
         function IConnectorEnumerator.Step = Step;

      public
         constructor Create(Table: TSymbolTable);
   end;

// ------------------
// ------------------ TdwsComConnector ------------------
// ------------------

function TdwsComConnector.ConnectorCaption: String;
begin
  Result := COM_ConnectorCaption;
end;

function TdwsComConnector.ConnectorName: String;
begin
  Result := COM_UnitName;
end;

function TdwsComConnector.GetUnit(const UnitName: String): IConnectorType;
begin
  raise Exception.Create('Not supported');
end;

function TdwsComConnector.GetUnitName: String;
begin
  Result := COM_UnitName;
end;

procedure TdwsComConnector.AddUnitSymbols(Table: TSymbolTable; operators : TOperators);
var
  v: Variant;
  VariantSym: TTypeSymbol;
  ComVariantSym: TTypeSymbol;
begin
  VariantSym := Table.FindTypeSymbol('Variant', cvMagic);

  // Datatype of com-objects
  ComVariantSym := TConnectorSymbol.Create('ComVariant', TComConnectorType.Create(Table));
  Table.AddSymbol(ComVariantSym);
  Table.AddSymbol(TAliasSymbol.Create('OleVariant', ComVariantSym));

  // Optional parameter for dispatch interfaces with unnamed arguments
  v := 0;
  PVarData(@v).VType := varError;
  Table.AddSymbol(TConstSymbol.Create('ComOpt', VariantSym, v));

  // Function to create a new COM-Object
  TCreateOleObjectFunc.Create(Table, 'CreateOleObject', ['ClassName', SYS_STRING], 'ComVariant');

  TClassIDToProgIDFunc.Create(Table, 'ClassIDToProgID', ['ClassID', SYS_STRING], 'String');
  TGetActiveOleObjectFunc.Create(Table, 'GetActiveOleObject', ['ClassName', SYS_STRING], 'ComVariant');

  TOleInt32Func.Create(Table, 'OleInt32', ['v', SYS_INTEGER], 'ComVariant');
  TOleInt64Func.Create(Table, 'OleInt64', ['v', SYS_INTEGER], 'ComVariant');
  TOleDateFunc.Create(Table, 'OleDate', ['v', SYS_FLOAT], 'ComVariant');

  Table.AddSymbol(TComVariantArraySymbol.Create('ComVariantArray', TComVariantArrayType.Create(Table), VariantSym));
end;

// ------------------
// ------------------ TCreateOleObjectFunc ------------------
// ------------------

procedure TCreateOleObjectFunc.Execute(info : TProgramInfo);
begin
   Info.ResultAsVariant := CreateOleObject(Info.ParamAsString[0]);
end;

// ------------------
// ------------------ TClassIDToProgIDFunc ------------------
// ------------------

procedure TClassIDToProgIDFunc.Execute(info : TProgramInfo);
var
   guid : TGUID;
begin
   guid := StringToGUID(Info.ParamAsString[0]);
   Info.ResultAsString := ClassIDToProgID(guid);
end;

// ------------------
// ------------------ TGetActiveOleObjectFunc ------------------
// ------------------

procedure TGetActiveOleObjectFunc.Execute(info : TProgramInfo);
begin
   Info.ResultAsVariant := GetActiveOleObject(Info.ParamAsString[0]);
end;

// ------------------
// ------------------ TOleInt32Func ------------------
// ------------------

procedure TOleInt32Func.Execute(info : TProgramInfo);
begin
   Info.ResultAsVariant := Int32(Info.ParamAsInteger[0]);
end;

// ------------------
// ------------------ TOleInt64Func ------------------
// ------------------

procedure TOleInt64Func.Execute(info : TProgramInfo);
begin
  Info.ResultAsVariant := Info.ParamAsInteger[0];
end;

// ------------------
// ------------------ TOleDateFunc ------------------
// ------------------

procedure TOleDateFunc.Execute(info : TProgramInfo);
begin
  Info.ResultAsVariant := VarFromDateTime(Info.ParamAsFloat[0]);
end;

// ------------------
// ------------------ TComConnectorType ------------------
// ------------------

function TComConnectorType.ConnectorCaption: String;
begin
  Result := COM_ConnectorCaption;
end;

constructor TComConnectorType.Create(Table: TSymbolTable);
begin
  FTable := Table;
end;

function TComConnectorType.HasIndex(const PropName: String; const Params: TConnectorParamArray;
                                    var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorCall;
var
   methType : Cardinal;
begin
   TypSym := FTable.FindTypeSymbol('ComVariant', cvMagic);
   if IsWrite then
      methType := DISPATCH_PROPERTYPUT
   else methType := DISPATCH_PROPERTYGET;
   Result := TComConnectorCall.Create(PropName, Params, methType);
end;

// HasEnumerator
//
function TComConnectorType.HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;
begin
   typSym := FTable.FindTypeSymbol('ComVariant', cvMagic);
   Result := IConnectorEnumerator(Self);
end;

// NewEnumerator
//
function TComConnectorType.NewEnumerator(const base : Variant; const args : TConnectorArgs) : IUnknown;
begin
   Result := IUnknown(base._NewEnum) as IEnumVariant;
end;

// Step
//
function TComConnectorType.Step(const enumerator : IInterface; var data : TData) : Boolean;
var
   fetched : LongWord;
   ov : OleVariant;
begin
   if IEnumVariant(enumerator).Next(1, ov, fetched)=0 then begin
      Result:=(fetched=1);
      if Result then
         data[0]:=ov;
   end else Result:=False;
end;

function TComConnectorType.HasMember(Const MemberName: String;
  var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorMember;
begin
  TypSym := FTable.FindTypeSymbol('ComVariant', cvMagic);
  Result := TComConnectorMember.Create(MemberName);
end;

// AcceptsParams
//
function TComConnectorType.AcceptsParams(const params: TConnectorParamArray) : Boolean;
var
   x: Integer;
   typ : TTypeSymbol;
begin
   for x := 0 to Length(Params) - 1 do begin
      typ:=Params[x].TypSym;
      if (typ=nil) or (typ.Size>1) then
         Exit(False);
      if typ is TArraySymbol then
         if not (typ is TDynamicArraySymbol) then
            Exit(False);
      if typ.IsFuncSymbol then
         Exit(False);
   end;
   Result:=True;
end;

function TComConnectorType.HasMethod(Const MethodName: String;
  const Params: TConnectorParamArray; var TypSym: TTypeSymbol): IConnectorCall;
begin
  TypSym := FTable.FindTypeSymbol('ComVariant', cvMagic);
  Result := TComConnectorCall.Create(MethodName, Params);
end;

// RaiseOleError
//
procedure RaiseOleError(err : HResult; const excepInfo : TExcepInfo);
begin
   raise EOleError.CreateFmt('OLE Error %x (%s) from %s, %s',
                             [err, SysErrorMessage(err),
                              excepInfo.bstrSource, excepInfo.bstrDescription]);
end;

type
   POleParams = ^TOleParams;
   TOleParams = array[0..MaxDispArgs - 1] of PVarData;
   TStringDesc = record
      BStr : PWideChar;
      PStr : PString;
   end;
   PStringDesc = ^TStringDesc;

// DispatchInvoke
//
function DispatchInvoke(const dispatch: IDispatch; invKind, namedArgCount : Integer;
                        dispIDs: PDispIDList; const connArgs : TConnectorArgs;
                        PResult: PVariant): HResult;
var
   i, argType, strCount : Integer;
   dispParams : TDispParams;
   strings : array [0 .. MaxDispArgs-1] of TStringDesc;
   argPtr : PVariantArg;
   args : array [0 .. MaxDispArgs-1] of TVariantArg;
   param : PVarData;
   dispID : Integer;
   excepInfo : TExcepInfo;
begin
   strCount := 0;
   Result := S_OK;

   // Fill in the dispParams struct
   FillChar(strings, MaxDispArgs*SizeOf(TStringDesc), 0);
   FillChar(args, MaxDispArgs*SizeOf(TVariantArg), 0);
   try
      argPtr:=@args[0];
      for i:=High(connArgs) downto 0 do begin
        param:=@connArgs[i][0];
        argType:=param.VType and varTypeMask;

        if (param.VType and varArray) <> 0 then begin

            argPtr.vt     := VT_ARRAY Or argType;
            argPtr.parray := PSafeArray(param.VArray);

         end else begin

            case argType of
               varInteger : begin
                  argPtr.vt := VT_I4 or VT_BYREF;
                  argPtr.plVal := @param.VInteger;
               end;
               varInt64 : begin
                  argPtr.vt := VT_I8 or VT_BYREF;
                  argPtr.plVal := @param.VInt64;
               end;
               varDouble : begin
                  argPtr.vt := VT_R8 or VT_BYREF;
                  argPtr.pdblVal := @param.VDouble;
               end;
               varBoolean : begin
                  argPtr.vt := VT_BOOL or VT_BYREF;
                  argPtr.pbool := @param.VBoolean;
               end;
               varDate : begin
                  argPtr.vt := VT_DATE or VT_BYREF;
                  argPtr.pdate := @param.VDate;
               end;
               varString : begin
                  // Transform Delphi-strings to OLE-strings
                  strings[strCount].BStr := StringToOleStr(AnsiString(param.VString));
                  strings[strCount].PStr := @param.VString;
                  argPtr.vt := VT_BSTR or VT_BYREF;
                  argPtr.pbstrVal := @strings[strCount].BStr;
                  Inc(strCount);
               end;
               varUString : begin
                  // Transform Delphi-strings to OLE-strings
                  strings[strCount].BStr := StringToOleStr(String(param.VUString));
                  strings[strCount].PStr := @param.VUString;
                  argPtr.vt := VT_BSTR or VT_BYREF;
                  argPtr.pbstrVal := @strings[strCount].BStr;
                  Inc(strCount);
               end;
               varOleStr : begin
                  argPtr.vt := VT_BSTR or VT_BYREF;
                  argPtr.pbstrVal := @param.VOleStr;
               end;
               varDispatch : begin
                  argPtr.vt := VT_DISPATCH or VT_BYREF;
                  argPtr.pdispVal := @param.VDispatch;
               end;
               varError : begin
                  argPtr.vt := VT_ERROR;
                  argPtr.scode := DISP_E_PARAMNOTFOUND;
               end;
               varVariant, varEmpty, varNull : begin
                  argPtr.vt := varVariant or VT_BYREF;
                  argPtr.pvarVal := PVariant(param);
               end;
            else
               raise Exception.CreateFmt('Unsupported data type (%d) for DWScript COM Connector!',
                                         [argType]);
            end;
         end;
         Inc(argPtr);
      end;
      dispParams.rgvarg := @args;
      dispParams.cArgs := Length(connArgs);

      dispID := dispIDs[0];

      if InvKind = DISPATCH_PROPERTYPUT then begin

         if (Args[0].vt and varTypeMask) = varDispatch then
            InvKind := DISPATCH_PROPERTYPUTREF;
         dispParams.rgdispidNamedArgs := dispIDs;
         dispParams.cNamedArgs := namedArgCount + 1;
         dispIDs[0] := DISPID_PROPERTYPUT;

      end else begin

         dispParams.rgdispidNamedArgs := @dispIDs[1];
         dispParams.cNamedArgs := namedArgCount;

      end;

      FillChar(excepInfo, SizeOf(excepInfo), 0);

      // Invoke COM Method
      Result := dispatch.Invoke(dispID, GUID_NULL, 0, InvKind, dispParams,
                                PResult, @excepInfo, nil);

      if Result = S_OK then begin
         for i := strCount - 1 downto 0 do begin
            if strings[i].PStr <> nil then
               OleStrToStrVar(strings[i].BStr, strings[i].PStr^);
         end;
      end else begin
         RaiseOleError(Result, excepInfo);
      end;

   finally
      for i := strCount - 1 downto 0 do
         SysFreeString(strings[i].BStr);
   end;
end;

// DispatchGetPropOrCall
//
function DispatchGetPropOrCall(const disp : IDispatch; dispID : Integer) : OleVariant;
var
   excepInfo : TExcepInfo;
   dispParams : TDispParams;
   err : HResult;
begin
   FillChar(DispParams, SizeOf(DispParams), 0);
   err:=disp.Invoke(dispID, GUID_NULL, 0, DISPATCH_PROPERTYGET or DISPATCH_METHOD,
                      dispParams, @Result, @excepInfo, nil);
   if err<>S_OK then
      RaiseOleError(err, excepInfo);
end;

// DispatchSetProp
//
procedure DispatchSetProp(const disp : IDispatch; dispID : Integer;
                          const value : OleVariant);
const
   dispIDNamedArgs : Longint = DISPID_PROPERTYPUT;
var
   excepInfo : TExcepInfo;
   dispParams : TDispParams;
   err : HResult;
begin
   dispParams.rgvarg:=@value;
   dispParams.rgdispidNamedArgs:=@dispIDNamedArgs;
   dispParams.cArgs:=1;
   dispParams.cNamedArgs:=1;
   err:=disp.Invoke(dispId, GUID_NULL, 0, DISPATCH_PROPERTYPUT, dispParams,
                    nil, @excepInfo, nil);
   if err<>S_OK then
      RaiseOleError(err, excepInfo);
end;

// ------------------
// ------------------ TComConnectorCall ------------------
// ------------------

// Create
//
constructor TComConnectorCall.Create(const MethodName: String;
                     const Params: TConnectorParamArray; MethodType: Cardinal);
begin
   FMethodName:=MethodName;
   FPMethodName:=PWideString(FMethodName);
   FMethodType:=MethodType;
end;

// Call
//
function TComConnectorCall.Call(const Base: Variant; const args : TConnectorArgs) : TData;
var
   disp : IDispatch;
   dispID : Integer;
begin
   disp:=Base;
   if disp=nil then
      raise EScriptError.Create(CPE_NilConnectorCall);

   if FMethodName='' then
      dispID:=0 // default method or property
   else DwsOleCheck(disp.GetIDsOfNames(GUID_NULL, @FPMethodName, 1, LOCALE_SYSTEM_DEFAULT, @dispID));

   SetLength(Result, 1);
   DwsOleCheck(DispatchInvoke(disp, FMethodType, 0, @dispID, args, @Result[0]));
end;

// NeedDirectReference
//
function TComConnectorCall.NeedDirectReference : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TComConnectorMember ------------------
// ------------------

// Create
//
constructor TComConnectorMember.Create(const memberName : String);
begin
   FMemberName:=memberName;
   FPMemberName:=PWideString(FMemberName);
end;

// GetDispID
//
function TComConnectorMember.GetDispID(const Disp: IDispatch) : Integer;
begin
   Result:=0;
   DwsOleCheck(disp.GetIDsOfNames(GUID_NULL, @FPMemberName, 1, LOCALE_SYSTEM_DEFAULT, @Result));
end;

// Read
//
function TComConnectorMember.Read(const Base: Variant): TData;
var
   disp : IDispatch;
begin
   disp:=Base;
   if disp=nil then
      raise EScriptError.Create(CPE_NilConnectorRead);

   SetLength(Result, 1);
   Result[0] := DispatchGetPropOrCall(disp, GetDispId(disp));
end;

// Write
//
procedure TComConnectorMember.Write(const Base: Variant; const Data: TData);
var
   disp : IDispatch;
begin
   disp:=Base;
   if disp=nil then
      raise EScriptError.Create(CPE_NilConnectorWrite);

   DispatchSetProp(disp, GetDispID(disp), Data[0]);
end;

// ------------------
// ------------------ TComVariantArrayType ------------------
// ------------------

function TComVariantArrayType.ReadIndex(const Base: Variant;
  const Args: TConnectorArgs): TData;
var
  Indices: array of Integer;
  x, ArgCount: Integer;
begin
  ArgCount := Length(Args);
  SetLength(Result, 1);
  SetLength(Indices, ArgCount);
  for x := 0 to ArgCount - 1 do
    Indices[x] := Args[x][0];
  VarCopy(Result[0], VarArrayGet(Base, Indices));
end;

function TComVariantArrayType.WriteIndex(const Base: Variant;
  const Args: TConnectorArgs): TData;
var
  BaseRef: PVariant;
  x, ArgCount: Integer;
  Indices: array of Integer;
begin
  ArgCount := Length(Args) - 1;
  SetLength(Indices, ArgCount);
  for x := 0 to ArgCount - 1 do
    Indices[x] := Args[x][0];
  BaseRef := @Base;//VarArrayRef(Base); // need var-ref
  VarArrayPut(Baseref^, Args[ArgCount][0], Indices) ;
end;

function TComVariantArrayType.ConnectorCaption: String;
begin
  Result := 'ComVariantArray';
end;

constructor TComVariantArrayType.Create(Table: TSymbolTable);
begin
  inherited Create;
  FTable := Table;
end;

function TComVariantArrayType.HasIndex(Const PropName: String; const Params: TConnectorParamArray;
  var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorCall;
var
  SymInteger: TTypeSymbol;
  SymVariant: TTypeSymbol;
  x, l: Integer;
begin
  Result := nil;

  SymVariant := FTable.FindTypeSymbol(SYS_VARIANT, cvMagic);
  SymInteger := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);

  l := Length(Params);
  if IsWrite then
  begin
    Dec(l); // Last Parameter is Put-Value
    if not SymVariant.IsCompatible(Params[l].TypSym) then
      Exit;
  end;

  // Check Integer Indices
  x := 0;
  while (x < l) and SymInteger.IsCompatible(Params[x].TypSym) do
    Inc(x);

  if x < l then
    Exit;

  if IsWrite then
  begin
    TypSym := nil;
    Result := IComVariantArrayWriteIndex(Self);
  end
  else
  begin
    TypSym := SymVariant;
    Result := IComVariantArrayReadIndex(Self);
  end;
end;

// HasEnumerator
//
function TComVariantArrayType.HasEnumerator(var typSym: TTypeSymbol) : IConnectorEnumerator;
begin
   typSym := FTable.FindTypeSymbol(SYS_VARIANT, cvMagic);
   Result := IConnectorEnumerator(Self);
end;

function TComVariantArrayType.HasMember(const MemberName: String;
  var typSym: TTypeSymbol; IsWrite: Boolean): IConnectorMember;
begin
  if UnicodeSameText(MemberName, 'high') then
  begin
    Result := IComVariantArrayHighBound(Self);
    typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
  end
  else if IsWrite then
    Result := nil
  else
  begin
    if UnicodeSameText(MemberName, 'length') then
    begin
      Result := IComVariantArrayLength(Self);
      typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
    end
    else if UnicodeSameText(MemberName, 'low') then
    begin
      Result := IComVariantArrayLowBound(Self);
      typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
    end
    else if UnicodeSameText(MemberName, 'dimcount') then
    begin
      Result := IComVariantArrayDimCount(Self);
      typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
    end
    else
      Result := nil;
  end;
end;

// AcceptsParams
//
function TComVariantArrayType.AcceptsParams(const params: TConnectorParamArray) : Boolean;
begin
  Result:=    (Length(params) in [1, 2])
          and FTable.FindTypeSymbol(SYS_INTEGER, cvMagic).IsCompatible(params[0].typSym);
end;

function TComVariantArrayType.HasMethod(Const methodName: String;
  const params: TConnectorParamArray; var typSym: TTypeSymbol): IConnectorCall;
begin
   if UnicodeSameText(methodName, 'length') then begin
      Result := IComVariantArrayLengthCall(Self);
      typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
   end else if UnicodeSameText(methodName, 'low') then begin
      Result := IComVariantArrayLowBoundCall(Self);
      typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
   end else if UnicodeSameText(methodName, 'high') then begin
      Result := IComVariantArrayHighBoundCall(Self);
      typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
   end else Result := nil;
end;

function TComVariantArrayType.ReadHighBound(const Base: Variant): TData;
begin
  SetLength(Result, 1);
  Result[0] := VarArrayHighBound(Base, 1);
end;

// NewEnumerator
//
function TComVariantArrayType.NewEnumerator(const base : Variant; const args : TConnectorArgs) : IUnknown;
begin
   Result:=TComVariantArrayEnumerator.Create(base);
end;

// Step
//
function TComVariantArrayType.Step(const enumerator : IInterface; var data : TData) : Boolean;
begin
   Result:=((enumerator as IGetSelf).GetSelf as TComVariantArrayEnumerator).Next(data[0]);
end;

// NeedDirectReference
//
function TComVariantArrayType.NeedDirectReference : Boolean;
begin
   Result:=True;
end;

function TComVariantArrayType.ReadLength(const Base: Variant): TData;
begin
  SetLength(Result, 1);
  Result[0] := VarArrayHighBound(Base, 1) - VarArrayLowBound(Base, 1) + 1;
end;

function TComVariantArrayType.ReadLowBound(const Base: Variant): TData;
begin
  SetLength(Result, 1);
  Result[0] := VarArrayLowBound(Base, 1);
end;

procedure TComVariantArrayType.Write(const Base: Variant; const Data: TData);
begin
  Assert(False); // we should never com here
end;

function TComVariantArrayType.ReadDimCount(const Base: Variant): TData;
begin
  SetLength(Result, 1);
  Result[0] := VarArrayDimCount(Base);
end;

procedure TComVariantArrayType.WriteHighBound(const Base: Variant; const Data: TData);
var
  BaseRef: Variant;
  x: Integer;
begin
  x := Data[0];
  BaseRef := VarArrayRef(Base);
  VarArrayRedim(BaseRef, x);
end;

function TComVariantArrayType.ReadHighBound(const Base: Variant;
  const Args: TConnectorArgs): TData;
begin
  SetLength(Result, 1);
  Result[0] := VarArrayHighBound(Base, Args[0][0]);
end;

function TComVariantArrayType.ReadLength(const Base: Variant;
  const Args: TConnectorArgs): TData;
var
  x: Integer;
begin
  x := Args[0][0];
  SetLength(Result, 1);
  Result[0] := VarArrayHighBound(Base, x) - VarArrayLowBound(Base, x) + 1;
end;

function TComVariantArrayType.ReadLowBound(const Base: Variant;
  const Args: TConnectorArgs): TData;
begin
  SetLength(Result, 1);
  Result[0] := VarArrayLowBound(Base, Args[0][0]);
end;

{ TComVariantArraySymbol }

function TComVariantArraySymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
  // only accept comvariantarray or std-arrays or variants
  Result :=    (Self = TypSym)
            or (typSym.IsBaseType and (typSym.BaseType is TBaseVariantSymbol))
            or (typSym.IsBaseType and Typ.IsCompatible(typSym.Typ));
end;

constructor TComVariantArraySymbol.Create(const Name: String;
  const ConnectorType: IConnectorType; Typ: TTypeSymbol);
begin
  inherited Create(Name, ConnectorType);
  Self.Typ := Typ;
end;

procedure TComVariantArraySymbol.InitData(const Dat: TData; Offset: Integer);
begin
  Dat[Offset] := VarArrayCreate([0, -1], varVariant); // empty array
end;

{ TComVariantArrayEnumerator }

// Create
//
constructor TComVariantArrayEnumerator.Create(const a : Variant);
begin
   inherited Create;
   FArray:=a;
   FIndex:=0;
end;

// Next
//
function TComVariantArrayEnumerator.Next(var v : Variant) : Boolean;
begin
   if FIndex<=VarArrayHighBound(FArray, 1) then begin

      v:=FArray[FIndex];
      Inc(FIndex);
      Result:=True;

   end else Result:=False;
end;


end.

