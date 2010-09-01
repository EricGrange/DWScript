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

uses Windows, Variants, Classes, SysUtils, SysConst, dwsComp, dwsSymbols, dwsExprs;

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
    procedure AddUnitSymbols(Table: TSymbolTable); override;
  published
    property StaticSymbols;
  end;

implementation

uses
  dwsStrings, dwsFunctions, dwsStack, ComObj, ComConst, ActiveX, AxCtrls;

type
  TCreateOleObjectFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  // thy Adds
  TGetActiveOleObjectFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TClassIDToProgIDFunc = class(TInternalFunction)
    procedure Execute; override;
  end;
  // thy Adds end //

  TComConnectorType = class(TInterfacedObject, IUnknown, IConnectorType)
  private
    FTable: TSymbolTable;
  protected
    { IConnectorType }
    function ConnectorCaption: string;
    function HasMethod(Const MethodName: string; const Params: TConnectorParamArray;
                       var TypSym:  TSymbol): IConnectorCall;
    function HasMember(Const MemberName: string; var TypSym: TSymbol; IsWrite: Boolean): IConnectorMember;
    function HasIndex(Const PropName: string; const Params: TConnectorParamArray; var TypSym: TSymbol; IsWrite: Boolean): IConnectorCall;
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
    constructor Create(const Name: string; ConnectorType: IConnectorType; Typ: TSymbol);
    function IsCompatible(TypSym: TSymbol): Boolean; override;
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
    function HasMethod(Const MethodName: string; const Params: TConnectorParamArray; var TypSym:
      TSymbol): IConnectorCall;
    function HasMember(Const MemberName: string; var TypSym: TSymbol; IsWrite: Boolean): IConnectorMember;
    function HasIndex(Const PropName: string; const Params: TConnectorParamArray; var TypSym: TSymbol; IsWrite: Boolean): IConnectorCall;
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

{$IFNDEF DELPHI6up}

// this code was taken from Delphi 5 unit "System.pas"

const
  oleaut = 'oleaut32.dll';

const
  reVarNotArray = 19;
  reVarArrayBounds = 20;

procedure Error(errorCode: Byte);
var
  Msg: string;
begin
  case errorCode of
    reVarNotArray: Msg := SVarInvalid;
    reVarArrayBounds: Msg := SVarArrayBounds;
  end;
  raise Exception.Create(Msg);
end;

function SafeArrayGetElement(VarArray: PVarArray; Indices,
  Data: Pointer): Integer; stdcall;
  external oleaut name 'SafeArrayGetElement';

function SafeArrayPtrOfIndex(VarArray: PVarArray; Indices: Pointer;
  var pvData: Pointer): HResult; stdcall;
  external oleaut name 'SafeArrayPtrOfIndex';

function SafeArrayPutElement(VarArray: PVarArray; Indices,
  Data: Pointer): Integer; stdcall;
  external oleaut name 'SafeArrayPutElement';

procedure VarStringToOleStr(var Dest: Variant; const Source: Variant);
var
  OleStrPtr: PWideChar;
begin
  OleStrPtr := StringToOleStr(string(TVarData(Source).VString));
  VarClear(Dest);
  TVarData(Dest).VType := varOleStr;
  TVarData(Dest).VOleStr := OleStrPtr;
end;

function GetVarArray(const A: Variant): PVarArray;
begin
  if TVarData(A).VType and varArray = 0 then
    Error(reVarNotArray);
  if TVarData(A).VType and varByRef <> 0 then
    Result := PVarArray(TVarData(A).VPointer^)
  else
    Result := TVarData(A).VArray;
end;

function _VarArrayGet(var A: Variant; IndexCount: Integer;
  Indices: Integer): Variant; cdecl;
var
  VarArrayPtr: PVarArray;
  VarType: Integer;
  P: Pointer;
begin
  if TVarData(A).VType and varArray = 0 then
    Error(reVarNotArray);
  VarArrayPtr := GetVarArray(A);
  if VarArrayPtr^.DimCount <> IndexCount then
    Error(reVarArrayBounds);
  VarType := TVarData(A).VType and varTypeMask;
  VarClear(Result);
  if VarType = varVariant then
  begin
    if SafeArrayPtrOfIndex(VarArrayPtr, @Indices, P) <> 0 then
      Error(reVarArrayBounds);
    Result := PVariant(P)^;
  end
  else
  begin
    if SafeArrayGetElement(VarArrayPtr, @Indices,
      @TVarData(Result).VPointer) <> 0 then
      Error(reVarArrayBounds);
    TVarData(Result).VType := VarType;
  end;
end;

procedure _VarArrayPut(var A: Variant; const Value: Variant;
  IndexCount: Integer; Indices: Integer); cdecl;
type
  TAnyPutArrayProc = procedure(var A: Variant; const Value: Variant; Index: Integer);
var
  VarArrayPtr: PVarArray;
  VarType: Integer;
  P: Pointer;
  Temp: TVarData;
begin
  if TVarData(A).VType and varArray = 0 then
    Error(reVarNotArray);
  VarArrayPtr := GetVarArray(A);
  if VarArrayPtr^.DimCount <> IndexCount then
    Error(reVarArrayBounds);
  VarType := TVarData(A).VType and varTypeMask;
  if (VarType = varVariant) and (not VarIsStr(Value)) then
  begin
    if SafeArrayPtrOfIndex(VarArrayPtr, @Indices, P) <> 0 then
      Error(reVarArrayBounds);
    PVariant(P)^ := Value;
  end
  else
  begin
    Temp.VType := varEmpty;
    try
      if VarType = varVariant then
      begin
        VarStringToOleStr(Variant(Temp), Value);
        P := @Temp;
      end
      else
      begin
        VarCast(Variant(Temp), Value, VarType);
        case VarType of
          varOleStr, varDispatch, varUnknown:
            P := Temp.VPointer;
        else
          P := @Temp.VPointer;
        end;
      end;
      if SafeArrayPutElement(VarArrayPtr, @Indices, P) <> 0 then
        ; // Error(reVarArrayBounds);
    finally
      VarClear(Variant(Temp));
    end;
  end;
end;

function VarArrayGet(const A: Variant; const Indices: array of Integer): Variant;
asm
        {     ->EAX     Pointer to A            }
        {       EDX     Pointer to Indices      }
        {       ECX     High bound of Indices   }
        {       [EBP+8] Pointer to Result       }

        PUSH    EBX

        MOV     EBX,ECX
        INC     EBX
        JLE     @@endLoop
@@loop:
        PUSH    [EDX+ECX*4].Integer
        DEC     ECX
        JNS     @@loop
@@endLoop:
        PUSH    EBX
        PUSH    EAX
        MOV     EAX,[EBP+8]
        PUSH    EAX
        CALL    _VarArrayGet
        LEA     ESP,[ESP+EBX*4+3*4]

        POP     EBX
end;

procedure VarArrayPut(var A: Variant; const Value: Variant; const Indices: array of Integer);
asm
        {     ->EAX     Pointer to A            }
        {       EDX     Pointer to Value        }
        {       ECX     Pointer to Indices      }
        {       [EBP+8] High bound of Indices   }

        PUSH    EBX

        MOV     EBX,[EBP+8]

        TEST    EBX,EBX
        JS      @@endLoop
@@loop:
        PUSH    [ECX+EBX*4].Integer
        DEC     EBX
        JNS     @@loop
@@endLoop:
        MOV     EBX,[EBP+8]
        INC     EBX
        PUSH    EBX
        PUSH    EDX
        PUSH    EAX
        CALL    _VarArrayPut
        LEA     ESP,[ESP+EBX*4+3*4]

        POP     EBX
end;

{$ENDIF}

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

procedure TdwsComConnector.AddUnitSymbols(Table: TSymbolTable);
var
  v: Variant;
  VariantSym: TSymbol;
  ComVariantSym: TTypeSymbol;
begin
  VariantSym := Table.FindSymbol('Variant');

  // Datatype of com-objects
  ComVariantSym := TConnectorSymbol.Create('ComVariant',
    TComConnectorType.Create(Table));
  Table.AddSymbol(ComVariantSym);
  Table.AddSymbol(TAliasSymbol.Create('OleVariant',ComVariantSym));

  // Optional parameter for dispatch interfaces with unnamed arguments
  v := 0;
  PVarData(@v).VType := varError;
  Table.AddSymbol(TConstSymbol.Create('ComOpt', VariantSym, v));

  // Function to create a new COM-Object
  TCreateOleObjectFunc.Create(Table, 'CreateOleObject', ['ClassName', SYS_STRING],
    'ComVariant');

  // thy Adds
  TClassIDToProgIDFunc.Create(Table, 'ClassIDToProgID', ['ClassID', SYS_STRING], 'String');

  TGetActiveOleObjectFunc.Create(Table, 'GetActiveOleObject', ['ClassName', SYS_STRING],
    'ComVariant');
  // thy Adds end //


  Table.AddSymbol(TComVariantArraySymbol.Create('ComVariantArray',
    TComVariantArrayType.Create(Table), VariantSym));
end;

{ TCreateOleObjectFunc }

procedure TCreateOleObjectFunc.Execute;
begin
  Info.ResultAsVariant := CreateOleObject(Info.ValueAsString['ClassName']);
end;

// thy Adds
procedure TClassIDToProgIDFunc.Execute;
var
   guid : TGUID;
begin
   guid := StringToGUID(Info.ValueAsString['ClassID']);
   Info.ResultAsString := ClassIDToProgID(guid);
end;

procedure TGetActiveOleObjectFunc.Execute;
begin
  Info.ResultAsVariant := GetActiveOleObject(Info.ValueAsString['ClassName']);
end;
// thy Adds end //

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
  var TypSym: TSymbol; IsWrite: Boolean): IConnectorCall;
var
  x: Integer;
  isValid: Boolean;
  MethType: Cardinal;
begin
  isValid := True;
  for x := 0 to Length(Params) - 1 do
  begin
    if Params[x].TypSym.Size > 1 then
    begin
      isValid := False;
      Break;
    end;
  end;

  TypSym := FTable.FindSymbol('ComVariant');
  if isValid then
  begin
    if IsWrite then
      MethType := DISPATCH_PROPERTYPUT
    else
      MethType := DISPATCH_PROPERTYGET;
    Result := TComConnectorCall.Create(PropName, Params, MethType);
  end
  else
    Result := nil;
end;

function TComConnectorType.HasMember(Const MemberName: string;
  var TypSym: TSymbol; IsWrite: Boolean): IConnectorMember;
begin
  TypSym := FTable.FindSymbol('ComVariant');
  Result := TComConnectorMember.Create(MemberName);
end;

function TComConnectorType.HasMethod(Const MethodName: string;
  const Params: TConnectorParamArray; var TypSym: TSymbol): IConnectorCall;
var
  x: Integer;
  isValid: Boolean;
begin
  isValid := True;
  for x := 0 to Length(Params) - 1 do
  begin
    if Params[x].TypSym.Size > 1 then
    begin
      isValid := False;
      Break;
    end;
  end;

  TypSym := FTable.FindSymbol('ComVariant');
  if isValid then
  begin
    Result := TComConnectorCall.Create(MethodName, Params);
  end
  else
    Result := nil;
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
        case argType of
          varInteger:
            begin
              argPtr.vt := VT_I4 or VT_BYREF;
              argPtr.plVal := @TVarData(PParams[x]^).VInteger;
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
          raise Exception.CreateFmt('Invalid data type (%d) for DWSII Com-Wrapper!', [argType]);
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

  if not FIsInitialized then
  begin
    pMethodName := PWideChar(FMethodName);
    // Get DISPID of this method
    OleCheck(disp.GetIDsOfNames(GUID_NULL, @pMethodName, 1, LOCALE_SYSTEM_DEFAULT, @FDispId));
    FIsInitialized := True;
  end;

  SetLength(Result, 1);
  OleCheck(DispatchInvoke(disp, FMethodType,
    Length(Args), 0, @FDispId, @paramData, @Result[0]));
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
  OleCheck(disp.GetIDsOfNames(GUID_NULL, @pMemberName, 1, LOCALE_SYSTEM_DEFAULT, @FDispId));
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
  var TypSym: TSymbol; IsWrite: Boolean): IConnectorCall;
var
  SymInteger: TSymbol;
  SymVariant: TSymbol;
  x, l: Integer;
begin
  Result := nil;

  SymVariant := FTable.FindSymbol(SYS_VARIANT);
  SymInteger := FTable.FindSymbol(SYS_INTEGER);

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
  var TypSym: TSymbol; IsWrite: Boolean): IConnectorMember;
begin
  if SameText(MemberName, 'high') then
  begin
    Result := IComVariantArrayHighBound(Self);
    TypSym := FTable.FindSymbol(SYS_INTEGER);
  end
  else if IsWrite then
    Result := nil
  else
  begin
    if SameText(MemberName, 'length') then
    begin
      Result := IComVariantArrayLength(Self);
      TypSym := FTable.FindSymbol(SYS_INTEGER);
    end
    else if SameText(MemberName, 'low') then
    begin
      Result := IComVariantArrayLowBound(Self);
      TypSym := FTable.FindSymbol(SYS_INTEGER);
    end
    else if SameText(MemberName, 'dimcount') then
    begin
      Result := IComVariantArrayDimCount(Self);
      TypSym := FTable.FindSymbol(SYS_INTEGER);
    end
    else
      Result := nil;
  end;
end;

function TComVariantArrayType.HasMethod(Const MethodName: string;
  const Params: TConnectorParamArray; var TypSym: TSymbol): IConnectorCall;
begin
  if (Length(Params) = 1) and
    FTable.FindSymbol(SYS_INTEGER).IsCompatible(Params[0].TypSym) then
  begin
    if SameText(MethodName, 'length') then
    begin
      Result := IComVariantArrayLengthCall(Self);
      TypSym := FTable.FindSymbol(SYS_INTEGER);
    end
    else if SameText(MethodName, 'low') then
    begin
      Result := IComVariantArrayLowBoundCall(Self);
      TypSym := FTable.FindSymbol(SYS_INTEGER);
    end
    else if SameText(MethodName, 'high') then
    begin
      Result := IComVariantArrayHighBoundCall(Self);
      TypSym := FTable.FindSymbol(SYS_INTEGER);
    end
    else
      Result := nil;
  end
  else
    Result := nil;
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

function TComVariantArraySymbol.IsCompatible(TypSym: TSymbol): Boolean;
begin
  // only accept comvariantarray or std-arrays or variants
  Result := (Self = TypSym) or
    (TypSym is TBaseSymbol) and (TBaseSymbol(TypSym).Id = TypVariantID) or
    (typSym is TArraySymbol) and Typ.IsCompatible(typSym.Typ);
end;

constructor TComVariantArraySymbol.Create(const Name: string;
  ConnectorType: IConnectorType; Typ: TSymbol);
begin
  inherited Create(Name, ConnectorType);
  Self.Typ := Typ;
end;

procedure TComVariantArraySymbol.InitData(const Dat: TData; Offset: Integer);
begin
  Dat[Offset] := VarArrayCreate([0, -1], varVariant); // empty array
end;

end.

