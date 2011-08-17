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
unit dwsComConnector;

interface

uses Windows, Variants, Classes, SysUtils, SysConst, dwsComp, dwsSymbols,
   dwsExprs, dwsStrings, dwsFunctions, dwsStack, ComObj, ComConst, ActiveX,
   AxCtrls, dwsOperators;

const
  COM_ConnectorCaption = 'COM Connector 1.0';
  COM_UnitName = 'COM';

type
  TdwsComConnector = class(TdwsAbstractStaticUnit, IUnknown, IConnector)
  private
    function ConnectorCaption: string;
    function ConnectorName: string;
    function GetUnit(const UnitName: string): IConnectorType;
  protected
    function GetUnitName: string; override;
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

  TComConnectorType = class(TInterfacedObject, IUnknown, IConnectorType)
  private
    FTable: TSymbolTable;
  protected
    { IConnectorType }
    function ConnectorCaption: string;
    function AcceptsParams(const params: TConnectorParamArray) : Boolean;
    function HasMethod(Const MethodName: string; const Params: TConnectorParamArray;
                       var TypSym: TTypeSymbol): IConnectorCall;
    function HasMember(Const MemberName: string; var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorMember;
    function HasIndex(const PropName: string; const Params: TConnectorParamArray;
                      var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorCall;
  public
    constructor Create(Table: TSymbolTable);
  end;

  TComConnectorCall = class(TInterfacedObject, IUnknown, IConnectorCall)
  private
    FDispId: TDispId;
    FIsInitialized: Boolean;
    FMethodName: WideString;
    FMethodType: Cardinal;
  protected
    function Call(Const Base: Variant; Args: TConnectorArgs): TData;
  public
    constructor Create(const MethodName: string; const Params: TConnectorParamArray;
      MethodType: Cardinal = DISPATCH_METHOD);
  end;

  TComConnectorMember = class(TInterfacedObject, IUnknown, IConnectorMember)
  protected
    FDispId: TDispId;
    FIsInitialized: Boolean;
    FMemberName: WideString;
    procedure GetDispId(Disp: IDispatch);
    function Read(const Base: Variant): TData;
    procedure Write(const Base: Variant; const Data: TData);
  public
    constructor Create(MemberName: string);
  end;

  TComVariantArraySymbol = class(TConnectorSymbol)
  public
    constructor Create(const Name: string; ConnectorType: IConnectorType; Typ: TTypeSymbol);
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

  TComVariantArrayType = class(TInterfacedObject, IUnknown, IConnectorType,
      IComVariantArrayReadIndex, IComVariantArrayWriteIndex,
      IComVariantArrayLength, IComVariantArrayDimCount,
      IComVariantArrayHighBound, IComVariantArrayLowBound,
      IComVariantArrayLowBoundCall, IComVariantArrayHighBoundCall,
      IComVariantArrayLengthCall)
  private
    FTable: TSymbolTable;
  protected
    function ReadLength(const Base: Variant): TData; overload;
    function ReadLowBound(const Base: Variant): TData; overload;
    function ReadHighBound(const Base: Variant): TData; overload;
    function ReadDimCount(const Base: Variant): TData;
    procedure WriteHighBound(const Base: Variant; const Data: TData);
    function ReadIndex(const Base: Variant; Args: TConnectorArgs): TData;
    function WriteIndex(const Base: Variant; Args: TConnectorArgs): TData;
    function ReadLength(const Base: Variant; Args: TConnectorArgs): TData; overload;
    function ReadLowBound(const Base: Variant; Args: TConnectorArgs): TData; overload;
    function ReadHighBound(const Base: Variant; Args: TConnectorArgs): TData; overload;
    { IConnectorType }
    function ConnectorCaption: string;
    function AcceptsParams(const params: TConnectorParamArray) : Boolean;
    function HasMethod(Const MethodName: string; const Params: TConnectorParamArray;
                       var TypSym: TTypeSymbol): IConnectorCall;
    function HasMember(Const MemberName: string; var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorMember;
    function HasIndex(Const PropName: string; const Params: TConnectorParamArray;
                      var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorCall;
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
  public
    constructor Create(Table: TSymbolTable);
  end;

{ TdwsComConnector }

function TdwsComConnector.ConnectorCaption: string;
begin
  Result := COM_ConnectorCaption;
end;

function TdwsComConnector.ConnectorName: string;
begin
  Result := COM_UnitName;
end;

function TdwsComConnector.GetUnit(const UnitName: string): IConnectorType;
begin
  raise Exception.Create('Not supported');
end;

function TdwsComConnector.GetUnitName: string;
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
  Table.AddSymbol(TAliasSymbol.Create('OleVariant',ComVariantSym));

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

{ TCreateOleObjectFunc }

procedure TCreateOleObjectFunc.Execute(info : TProgramInfo);
begin
  Info.ResultAsVariant := CreateOleObject(Info.ValueAsString['ClassName']);
end;

{ TClassIDToProgIDFunc }

procedure TClassIDToProgIDFunc.Execute(info : TProgramInfo);
var
   guid : TGUID;
begin
   guid := StringToGUID(Info.ValueAsString['ClassID']);
   Info.ResultAsString := ClassIDToProgID(guid);
end;

{ TGetActiveOleObjectFunc }

procedure TGetActiveOleObjectFunc.Execute(info : TProgramInfo);
begin
  Info.ResultAsVariant := GetActiveOleObject(Info.ValueAsString['ClassName']);
end;

{ TOleInt32Func }

procedure TOleInt32Func.Execute(info : TProgramInfo);
begin
  Info.ResultAsVariant := Int32(Info.ValueAsInteger['v']);
end;

{ TOleInt64Func }

procedure TOleInt64Func.Execute(info : TProgramInfo);
begin
  Info.ResultAsVariant := Info.ValueAsInteger['v'];
end;

{ TOleDateFunc }

procedure TOleDateFunc.Execute(info : TProgramInfo);
begin
  Info.ResultAsVariant := VarFromDateTime(Info.ValueAsFloat['v']);
end;

{ TComConnectorSymbol }

function TComConnectorType.ConnectorCaption: string;
begin
  Result := COM_ConnectorCaption;
end;

constructor TComConnectorType.Create(Table: TSymbolTable);
begin
  FTable := Table;
end;

function TComConnectorType.HasIndex(Const PropName: string; const Params: TConnectorParamArray;
  var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorCall;
var
  methType: Cardinal;
begin
   TypSym := FTable.FindTypeSymbol('ComVariant', cvMagic);
   if IsWrite then
      methType := DISPATCH_PROPERTYPUT
   else methType := DISPATCH_PROPERTYGET;
   Result := TComConnectorCall.Create(PropName, Params, methType);
end;

function TComConnectorType.HasMember(Const MemberName: string;
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
      if typ.Size > 1 then
         Exit(False);
      if typ is TArraySymbol then
         if not (typ is TDynamicArraySymbol) then
            Exit(False);
   end;
   Result:=True;
end;

function TComConnectorType.HasMethod(Const MethodName: string;
  const Params: TConnectorParamArray; var TypSym: TTypeSymbol): IConnectorCall;
begin
  TypSym := FTable.FindTypeSymbol('ComVariant', cvMagic);
  Result := TComConnectorCall.Create(MethodName, Params);
end;

const
  MaxDispArgs = 64;

type
  POleParams = ^TOleParams;
  TOleParams = array[0..MaxDispArgs - 1] of PVariant;

function DispatchInvoke(const Dispatch: IDispatch; InvKind, ArgCount, NamedArgCount: Byte;
  DispIDs: PDispIDList; PParams: POleParams; PResult: PVariant): HResult;
type
  TStringDesc = record
    BStr: PWideChar;
    PStr: PString;
  end;
var
  x, argType, strCount: Integer;
  dispParams: TDispParams;
  excepInfo: TExcepInfo;
  strings: array[0..MaxDispArgs - 1] of TStringDesc;
  argPtr: PVariantArg;
  args: array[0..MaxDispArgs - 1] of TVariantArg;
  DispID: Integer;
begin
  strCount := 0;
  Result := S_OK;

  // Fill in the dispParams struct
  try
    if ArgCount <> 0 then
    begin
      for x := 0 to ArgCount - 1 do
      begin
        argPtr := @args[ArgCount - x - 1];
        argType := PVarData(PParams[x]).VType and varTypeMask;
        If PVarData(PParams[x]).VType And varArray <> 0 Then Begin
          argPtr.vt     := VT_ARRAY Or argType;
          argPtr.parray := PSafeArray(PVarData(PParams[x]).VArray);
        End Else case argType of
          varInteger:
            begin
              argPtr.vt := VT_I4 or VT_BYREF;
              argPtr.plVal := @TVarData(PParams[x]^).VInteger;
            end;
          varInt64:
            begin
              argPtr.vt := VT_I8 or VT_BYREF;
              argPtr.plVal := @TVarData(PParams[x]^).VInt64;
            end;
          varDouble:
            begin
              argPtr.vt := VT_R8 or VT_BYREF;
              argPtr.pdblVal := @TVarData(PParams[x]^).VDouble;
            end;
          varBoolean:
            begin
              argPtr.vt := VT_BOOL or VT_BYREF;
              argPtr.pbool := @TVarData(PParams[x]^).VBoolean;
            end;
          varDate:
            begin
              argPtr.vt := VT_DATE or VT_BYREF;
              argPtr.pdate := @TVarData(PParams[x]^).VDate;
            end;
          varString:
            begin
              // Transform Delphi-strings to OLE-strings
              with strings[strCount] do
              begin
                BStr := StringToOleStr(AnsiString(PVarData(PParams[x]).VString));
                PStr := @(PVarData(PParams[x]).VString);
                argPtr.vt := VT_BSTR or VT_BYREF;
                argPtr.pbstrVal := @BStr;
              end;
              Inc(strCount);
            end;
          varUString:
            begin
              // Transform Delphi-strings to OLE-strings
              with strings[strCount] do
              begin
                BStr := StringToOleStr(string(PVarData(PParams[x]).VUString));
                PStr := @(PVarData(PParams[x]).VUString);
                argPtr.vt := VT_BSTR or VT_BYREF;
                argPtr.pbstrVal := @BStr;
              end;
              Inc(strCount);
            end;
          varOleStr:
            begin
              argPtr.vt := VT_BSTR or VT_BYREF;
              argPtr.pbstrVal := @TVarData(PParams[x]^).VOleStr;
            end;
          varDispatch:
            begin
              argPtr.vt := VT_DISPATCH or VT_BYREF;
              argPtr.pdispVal := @TVarData(PParams[x]^).VDispatch;
            end;
          varError:
            begin
              argPtr.vt := VT_ERROR;
              argPtr.scode := DISP_E_PARAMNOTFOUND;
            end;
          varVariant, varEmpty, varNull:
            begin
              argPtr.vt := varVariant or VT_BYREF;
              argPtr.pvarVal := PParams[x];
            end;
        else
          raise Exception.CreateFmt('Invalid data type (%d) for DWS Com-Wrapper!', [argType]);
        end;
      end;
    end;
    DispParams.rgvarg := @args;
    DispParams.cArgs := ArgCount;

    DispID := DispIDs[0];

    if InvKind = DISPATCH_PROPERTYPUT then
    begin
      if Args[0].vt and varTypeMask = varDispatch then
        InvKind := DISPATCH_PROPERTYPUTREF;
      DispParams.rgdispidNamedArgs := DispIDs; // = @DispIDs[0]
      DispParams.cNamedArgs := NamedArgCount + 1;
      DispIDs[0] := DISPID_PROPERTYPUT;
    end
    else
    begin
      DispParams.rgdispidNamedArgs := @DispIDs[1];
      DispParams.cNamedArgs := NamedArgCount;
    end;

    try
      // Invoke COM Method
      Result := Dispatch.Invoke(DispID, GUID_NULL, 0, InvKind, dispParams,
        PResult, @excepInfo, nil);
    finally
      DispIDs[0] := DispID; // reset
    end;

    if Result = 0 then
    begin
      for x := strCount - 1 downto 0 do
        with strings[x] do
          if PStr <> nil then
            OleStrToStrVar(BStr, PStr^);
    end;

  finally
    for x := strCount - 1 downto 0 do
      SysFreeString(strings[x].BStr);
  end;
end;

{ TComConnectorCall }

function TComConnectorCall.Call(const Base: Variant; Args: TConnectorArgs): TData;
const
   maxOleArgs = 64;
var
   x: Integer;
   paramData: array[0..maxOleArgs - 1] of Pointer;
   disp: IDispatch;
   pMethodName: PWideChar;
begin
   for x := 0 to Length(Args) - 1 do
      paramData[x] := @Args[x][0];

   disp := Base;
   if disp=nil then
      raise EOleError.Create(CPE_NilConnectorCall);

   if not FIsInitialized then begin
      pMethodName := PWideChar(FMethodName);
      // Get DISPID of this method
      DwsOleCheck(disp.GetIDsOfNames(GUID_NULL, @pMethodName, 1, LOCALE_SYSTEM_DEFAULT, @FDispId));

      FIsInitialized := True;
   end;

   SetLength(Result, 1);
   DwsOleCheck(DispatchInvoke(disp, FMethodType, Length(Args), 0, @FDispId, @paramData, @Result[0]));
end;

constructor TComConnectorCall.Create(const MethodName: string;
                     const Params: TConnectorParamArray; MethodType: Cardinal);
begin
  FMethodName := MethodName;
  FMethodType := MethodType;
end;

{ TComConnectorMember }

constructor TComConnectorMember.Create(MemberName: string);
begin
  FMemberName := MemberName;
end;

procedure TComConnectorMember.GetDispId(Disp: IDispatch);
var
  pMemberName: PWideChar;
begin
  pMemberName := PWideChar(FMemberName);
  DwsOleCheck(disp.GetIDsOfNames(GUID_NULL, @pMemberName, 1, LOCALE_SYSTEM_DEFAULT, @FDispId));
  FIsInitialized := True;
end;

function TComConnectorMember.Read(const Base: Variant): TData;
begin
  if not FIsInitialized then
    GetDispId(Base);
  SetLength(Result, 1);
  Result[0] := GetDispatchPropValue(IDispatch(Base), FDispID);
end;

procedure TComConnectorMember.Write(const Base: Variant; const Data: TData);
begin
  if not FIsInitialized then
    GetDispId(Base);
  SetDispatchPropValue(IDispatch(Base), FDispId, Data[0]);
end;

{ TComVariantArrayType }

function TComVariantArrayType.ReadIndex(const Base: Variant;
  Args: TConnectorArgs): TData;
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
  Args: TConnectorArgs): TData;
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

function TComVariantArrayType.ConnectorCaption: string;
begin
  Result := 'ComVariantArray';
end;

constructor TComVariantArrayType.Create(Table: TSymbolTable);
begin
  inherited Create;
  FTable := Table;
end;

function TComVariantArrayType.HasIndex(Const PropName: string; const Params: TConnectorParamArray;
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

function TComVariantArrayType.HasMember(const MemberName: string;
  var typSym: TTypeSymbol; IsWrite: Boolean): IConnectorMember;
begin
  if SameText(MemberName, 'high') then
  begin
    Result := IComVariantArrayHighBound(Self);
    typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
  end
  else if IsWrite then
    Result := nil
  else
  begin
    if SameText(MemberName, 'length') then
    begin
      Result := IComVariantArrayLength(Self);
      typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
    end
    else if SameText(MemberName, 'low') then
    begin
      Result := IComVariantArrayLowBound(Self);
      typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
    end
    else if SameText(MemberName, 'dimcount') then
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

function TComVariantArrayType.HasMethod(Const methodName: string;
  const params: TConnectorParamArray; var typSym: TTypeSymbol): IConnectorCall;
begin
   if SameText(methodName, 'length') then begin
      Result := IComVariantArrayLengthCall(Self);
      typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
   end else if SameText(methodName, 'low') then begin
      Result := IComVariantArrayLowBoundCall(Self);
      typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
   end else if SameText(methodName, 'high') then begin
      Result := IComVariantArrayHighBoundCall(Self);
      typSym := FTable.FindTypeSymbol(SYS_INTEGER, cvMagic);
   end else Result := nil;
end;

function TComVariantArrayType.ReadHighBound(const Base: Variant): TData;
begin
  SetLength(Result, 1);
  Result[0] := VarArrayHighBound(Base, 1);
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
  Args: TConnectorArgs): TData;
begin
  SetLength(Result, 1);
  Result[0] := VarArrayHighBound(Base, Args[0][0]);
end;

function TComVariantArrayType.ReadLength(const Base: Variant;
  Args: TConnectorArgs): TData;
var
  x: Integer;
begin
  x := Args[0][0];
  SetLength(Result, 1);
  Result[0] := VarArrayHighBound(Base, x) - VarArrayLowBound(Base, x) + 1;
end;

function TComVariantArrayType.ReadLowBound(const Base: Variant;
  Args: TConnectorArgs): TData;
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

constructor TComVariantArraySymbol.Create(const Name: string;
  ConnectorType: IConnectorType; Typ: TTypeSymbol);
begin
  inherited Create(Name, ConnectorType);
  Self.Typ := Typ;
end;

procedure TComVariantArraySymbol.InitData(const Dat: TData; Offset: Integer);
begin
  Dat[Offset] := VarArrayCreate([0, -1], varVariant); // empty array
end;

end.

