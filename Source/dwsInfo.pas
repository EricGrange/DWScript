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
unit dwsInfo;

{$I dws.inc}

interface

uses Classes, SysUtils, Variants, dwsSymbols, dwsStack, dwsExprs, dwsFunctions,
   dwsStrings, dwsErrors, dwsUtils, dwsXPlatform;

type

   IDataMaster = interface
      ['{8D534D17-4C6B-11D5-8DCB-0000216D9E86}']
      function GetCaption : String;
      function GetSize : Integer;
      procedure Read(exec : TdwsExecution; const data : TData);
      procedure Write(exec : TdwsExecution; const data : TData);
      property Caption : String read GetCaption;
      property Size : Integer read GetSize;
   end;

   // private implementation of IInfo
   TInfo = class(TInterfacedObject, IUnknown, IInfo)
      protected
         FExec : TdwsProgramExecution;
         FChild : IInfo;
         FData : TData;
         FOffset : Integer;
         FProgramInfo : TProgramInfo;
         FDataMaster : IDataMaster;
         FTypeSym : TSymbol;

         function GetData : TData; virtual;
         function GetExternalObject : TObject; virtual;
         function GetMember(const s : String) : IInfo; virtual;
         function GetFieldMemberNames : TStrings; virtual;
         function GetMethod(const s : String) : IInfo; virtual;
         function GetScriptObj : IScriptObj; virtual;
         function GetParameter(const s : String) : IInfo; virtual;
         function GetTypeSym : TSymbol;
         function GetValue : Variant; virtual;
         function GetValueAsString : String; virtual;
         function GetValueAsDataString : RawByteString; virtual;
         function GetValueAsInteger : Int64; virtual;
         function GetValueAsBoolean : Boolean; virtual;
         function GetValueAsFloat : Double; virtual;
         function GetInherited: IInfo; virtual;
         procedure SetData(const Value: TData); virtual;
         procedure SetExternalObject(ExtObject: TObject); virtual;
         procedure SetValue(const Value: Variant); virtual;

      public
         constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
                            const Data: TData; Offset: Integer; const DataMaster: IDataMaster = nil);

         function Call : IInfo; overload; virtual;
         function Call(const params : array of Variant) : IInfo; overload; virtual;
         function Element(const indices : array of Integer) : IInfo; virtual;
         function GetConstructor(const methName : String; extObject : TObject) : IInfo; virtual;

         property Data : TData read FData write FData;
         property Offset : Integer read FOffset write FOffset;

         class procedure SetChild(out result : IInfo; programInfo : TProgramInfo; childTypeSym : TSymbol;
                                  const childData : TData; childOffset : Integer; const childDataMaster : IDataMaster = nil);
      end;

  TInfoConst = class(TInfo)
  private
    FData: TData;
  public
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Value: Variant);
    function GetValue: Variant; override;
    function GetData : TData; override;
  end;

   TInfoData = class(TInfo)
      protected
         function GetValue: Variant; override;
         function GetValueAsString : String; override;
         function GetValueAsInteger : Int64; override;
         function GetValueAsFloat : Double; override;
         function GetData : TData; override;
         function GetScriptObj: IScriptObj; override;
         procedure SetData(const Value: TData); override;
         procedure SetValue(const Value: Variant); override;
   end;

  TInfoClass = class(TInfoData)
    FScriptObj: IScriptObj;
    function GetConstructor(const MethName: String; ExtObject: TObject): IInfo; override;
    function GetMethod(const s: String): IInfo; override;
    function GetValueAsString : String; override;
    function GetScriptObj: IScriptObj; override;
    function GetInherited: IInfo; override;
  end;

  TInfoClassObj = class(TInfoClass)
    FMembersCache : TStrings;
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
                       const Data: TData; Offset: Integer;
                       const DataMaster: IDataMaster = nil);
    destructor Destroy; override;
    function GetMember(const s : String): IInfo; override;
    function GetFieldMemberNames : TStrings; override;
    function GetExternalObject: TObject; override;
    procedure SetExternalObject(ExtObject: TObject); override;
  end;

   TTempParam = class(TParamSymbol)
      private
         FData : TData;
         FIsVarParam : Boolean;
      public
         constructor Create(paramSym : TSymbol);
         property Data : TData read FData;
         property IsVarParam : Boolean read FIsVarParam;
   end;

   TInfoClassOf = class(TInfoClass)
   end;

   TInfoRecord = class(TInfoData)
      FMembersCache : TStrings;
      destructor Destroy; override;
      function GetMember(const s: String): IInfo; override;
      function GetFieldMemberNames : TStrings; override;
   end;

   TInfoStaticArray = class(TInfoData)
      function Element(const Indices: array of Integer): IInfo; override;
      function GetMember(const s: String): IInfo; override;
      function GetValueAsString : String; override;
   end;

   TInfoDynamicArrayBase = class (TInfoData)
      protected
         function SelfDynArray : TScriptDynamicArray;
   end;

   TInfoDynamicArrayLength = class (TInfoDynamicArrayBase)
      private
         FDelta : Integer;

      public
         constructor Create(ProgramInfo: TProgramInfo; const Data: TData; Offset, Delta: Integer);
         function GetValue : Variant; override;
         function GetValueAsInteger : Int64; override;
         procedure SetValue(const Value: Variant); override;
   end;

   TInfoDynamicArray = class(TInfoDynamicArrayBase)
      protected
         function GetScriptObj : IScriptObj; override;

      public
         function Element(const Indices: array of Integer): IInfo; override;
         function GetMember(const s: String): IInfo; override;
         function GetValueAsString : String; override;
         function GetData : TData; override;
         procedure SetData(const Value: TData); override;
   end;

   TInfoOpenArray = class(TInfoStaticArray)
      function Element(const Indices: array of Integer): IInfo; override;
      function GetMember(const s: String): IInfo; override;
   end;

   TInfoFunc = class(TInfo)
      protected
         FClassSym: TClassSymbol;
         FExternalObject: TObject;
         FScriptObj: IScriptObj;
         FParams: TSymbolTable;
         FParamSize: Integer;
         FResult: TData;
         FTempParams: TSymbolTable;
         FTempParamSize: Integer;
         FUsesTempParams: Boolean;
         FForceStatic: Boolean;

         function CreateTempFuncExpr : TFuncExpr;
         procedure InitTempParams;
         function GetParameter(const s: String): IInfo; override;
         function GetExternalObject: TObject; override;
         procedure SetExternalObject(ExtObject: TObject); override;
         function GetInherited: IInfo; override;

      public
         constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
                            const Data: TData; Offset: Integer;
                            const DataMaster: IDataMaster;
                            const ScriptObj: IScriptObj;
                            ClassSym: TClassSymbol; ForceStatic: Boolean = False);
         destructor Destroy; override;
         function Call: IInfo; overload; override;
         function Call(const Params: array of Variant): IInfo; overload; override;
   end;

   TInfoProperty = class(TInfo)
      private
         FScriptObj: IScriptObj;
         FPropSym: TPropertySymbol;
         FTempParams: TSymbolTable;
         procedure AssignIndices(const Func: IInfo; FuncParams: TSymbolTable);
      protected
         procedure InitTempParams;
         function GetParameter(const s: String): IInfo; override;
         function GetValue: Variant; override;
         function GetData : TData; override;
         procedure SetData(const Value: TData); override;
         procedure SetValue(const Value: Variant); override;
      public
         constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
                            const Data: TData; Offset: Integer;
                            PropSym: TPropertySymbol; const ScriptObj: IScriptObj);
         destructor Destroy; override;
   end;

   TInfoConnector = class(TInfoData)
      function GetMethod(const s: String): IInfo; override;
      function GetMember(const s: String): IInfo; override;
   end;

   TInfoConnectorCall = class(TInfo)
      protected
         FName: String;
         FConnectorType: IConnectorType;
      public
         constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
                            const Data: TData; Offset: Integer;
                            const ConnectorType: IConnectorType; const Name: String);
         function Call(const Params: array of Variant): IInfo; overload; override;
   end;

   TDataMaster = class(TInterfacedObject, IUnknown, IDataMaster)
      private
         FCaller: TdwsProgramExecution;
         FSym: TSymbol;
         function GetCaption: String;
         function GetSize: Integer;
      public
         constructor Create(Caller: TdwsProgramExecution; Sym: TSymbol);
         procedure Read(exec : TdwsExecution; const Data: TData); virtual;
         procedure Write(exec : TdwsExecution; const Data: TData); virtual;
   end;

   TExternalVarDataMaster = class(TDataMaster)
      public
         procedure Read(exec : TdwsExecution; const Data: TData); override;
         procedure Write(exec : TdwsExecution; const Data: TData); override;
   end;

   TConnectorMemberDataMaster = class(TDataMaster)
      private
         FBaseValue: Variant;
         FName: String;

      public
         constructor Create(Caller: TdwsProgramExecution; Sym: TSymbol; const BaseValue: Variant; const Name: String);
         procedure Read(exec : TdwsExecution; const Data: TData); override;
         procedure Write(exec : TdwsExecution; const Data: TData); override;
   end;

   TPrintFunction = class(TInternalFunction)
      public
         procedure Execute(info : TProgramInfo); override;
   end;

   TPrintLnFunction = class(TInternalFunction)
      public
         procedure Execute(info : TProgramInfo); override;
   end;

   TdwsDefaultResultType = class(TdwsResultType)
      public
         procedure AddResultSymbols(SymbolTable: TSymbolTable); override;
         function CreateProgResult: TdwsResult; override;
   end;

procedure CreateInfoOnSymbol(var result : IInfo; programInfo : TProgramInfo; typeSym : TSymbol;
                             const data : TData; offset : Integer);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsCoreExprs;

// CreateInfoOnSymbol
//
procedure CreateInfoOnSymbol(var result : IInfo; programInfo : TProgramInfo; typeSym : TSymbol;
                             const data : TData; offset : Integer);
begin
   TInfo.SetChild(result, programInfo, typeSym, data, offset, nil);
end;

// ------------------
// ------------------ TdwsDefaultResultType ------------------
// ------------------

// CreateProgResult
//
function TdwsDefaultResultType.CreateProgResult: TdwsResult;
begin
   Result:=TdwsDefaultResult.Create(Self);
end;

// AddResultSymbols
//
procedure TdwsDefaultResultType.AddResultSymbols(SymbolTable: TSymbolTable);
begin
   inherited;
   TPrintFunction.Create(SymbolTable, 'Print',  ['v', 'Variant'], '', []);
   TPrintLnFunction.Create(SymbolTable, 'PrintLn', ['v', 'Variant'], '', []);
end;

// ------------------
// ------------------ TPrintFunction ------------------
// ------------------

procedure TPrintFunction.Execute(info : TProgramInfo);
begin
   info.Execution.Result.AddString(info.ValueAsString['v']);
end;

// ------------------
// ------------------ TPrintLnFunction ------------------
// ------------------

procedure TPrintLnFunction.Execute(info : TProgramInfo);
var
   result : TdwsResult;
begin
   result:=info.Execution.Result;
   result.AddString(Info.ValueAsString['v']);
   result.AddString(#13#10);
end;

// ------------------
// ------------------ TInfo ------------------
// ------------------

constructor TInfo.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Data: TData; Offset: Integer; const DataMaster: IDataMaster = nil);
begin
  FProgramInfo := ProgramInfo;
  if Assigned(ProgramInfo) then
    FExec := ProgramInfo.Execution;
  FTypeSym := TypeSym;
  FData := Data;
  FOffset := Offset;
  FDataMaster := DataMaster;
end;

function TInfo.Call(const Params: array of Variant): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Call', FTypeSym.Caption]);
end;

function TInfo.Call: IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Call', FTypeSym.Caption]);
end;

function TInfo.Element(const Indices: array of Integer): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Element', FTypeSym.Caption]);
end;

function TInfo.GetConstructor(const MethName: String; ExtObject: TObject): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['GetConstructor', FTypeSym.Caption]);
end;

function TInfo.GetData : TData;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Data', FTypeSym.Caption]);
end;

function TInfo.GetExternalObject: TObject;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['ExternalObject', FTypeSym.Caption]);
end;

// GetMember
//
function TInfo.GetMember(const s : String) : IInfo;
begin
   raise Exception.CreateFmt(RTE_InvalidOp, ['Member', FTypeSym.Caption]);
end;

// GetFieldMemberNames
//
function TInfo.GetFieldMemberNames : TStrings;
begin
   Result:=nil;
end;

function TInfo.GetMethod(const s: String): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Method', FTypeSym.Caption]);
end;

function TInfo.GetScriptObj: IScriptObj;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Obj', FTypeSym.Caption]);
end;

function TInfo.GetParameter(const s: String): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Parameter', FTypeSym.Caption]);
end;

function TInfo.GetTypeSym: TSymbol;
begin
  Result := FTypeSym;
end;

function TInfo.GetValue: Variant;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Value', FTypeSym.Caption]);
end;

function TInfo.GetValueAsString : String;
begin
   Result:=GetValue;
end;

// GetValueAsDataString
//
function TInfo.GetValueAsDataString : RawByteString;
begin
   Result:=ScriptStringToRawByteString(GetValueAsString);
end;

// GetValueAsInteger
//
function TInfo.GetValueAsInteger : Int64;
begin
   Result:=GetValue;
end;

// GetValueAsBoolean
//
function TInfo.GetValueAsBoolean : Boolean;
begin
   Result:=GetValue;
end;

// GetValueAsFloat
//
function TInfo.GetValueAsFloat : Double;
begin
   Result:=GetValue;
end;

class procedure TInfo.SetChild(out Result : IInfo; ProgramInfo: TProgramInfo;
  ChildTypeSym: TSymbol; const ChildData: TData; ChildOffset: Integer;
  const ChildDataMaster: IDataMaster = nil);
var
   baseType : TTypeSymbol;
begin
   Assert(Assigned(ChildTypeSym));
   baseType := ChildTypeSym.baseType;

   if    (baseType is TBaseSymbol)
      or (baseType is TEnumerationSymbol)
      or (baseType is TConnectorSymbol) then
         Result := TInfoData.Create(ProgramInfo, ChildTypeSym, ChildData, ChildOffset,
                                    ChildDataMaster)
   else if ChildTypeSym is TFuncSymbol then
      Result := TInfoFunc.Create(ProgramInfo, ChildTypeSym, ChildData, ChildOffset,
                                 ChildDataMaster, nil, nil)
   else if baseType is TRecordSymbol then
      Result := TInfoRecord.Create(ProgramInfo, ChildTypeSym, ChildData,
                                   ChildOffset, ChildDataMaster)
   else if baseType is TStaticArraySymbol then begin
      if baseType is TOpenArraySymbol then begin
         Result := TInfoOpenArray.Create(ProgramInfo, ChildTypeSym, ChildData,
                                         ChildOffset, ChildDataMaster);
      end else begin
         Result := TInfoStaticArray.Create(ProgramInfo, ChildTypeSym, ChildData,
                                           ChildOffset, ChildDataMaster);
      end;
   end else if baseType is TDynamicArraySymbol then
      Result := TInfoDynamicArray.Create(ProgramInfo, ChildTypeSym, ChildData,
                                         ChildOffset, ChildDataMaster)
   else if baseType is TClassSymbol then
      Result := TInfoClassObj.Create(ProgramInfo, ChildTypeSym, ChildData,
                                     ChildOffset, ChildDataMaster)
   else if baseType is TClassOfSymbol then
      Result := TInfoClassOf.Create(ProgramInfo, ChildTypeSym, ChildData,
                                    ChildOffset, ChildDataMaster)
   else Assert(False); // Shouldn't be ever executed
end;

procedure TInfo.SetData(const Value: TData);
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['SetData', FTypeSym.Caption]);
end;

procedure TInfo.SetExternalObject(ExtObject: TObject);
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['SetExternalObject', FTypeSym.Caption]);
end;

procedure TInfo.SetValue(const Value: Variant);
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['SetValue', FTypeSym.Caption]);
end;

function TInfo.GetInherited: IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['GetInherited', FTypeSym.Caption]);
end;

{ TInfoData }

function TInfoData.GetData;
begin
  if Assigned(FDataMaster) then
    FDataMaster.Read(FExec, FData);

  SetLength(Result, FTypeSym.Size);
  DWSCopyData(FData, FOffset, Result, 0, FTypeSym.Size);
end;

// GetScriptObj
//
function TInfoData.GetScriptObj : IScriptObj;
var
   v : Variant;
begin
   if FTypeSym.Size=1 then begin
      v:=GetValue;
      if VarType(v)=varUnknown then begin
         Result:=IScriptObj(IUnknown(v));
         Exit;
      end;
   end;
   Result:=inherited GetScriptObj;
end;

function TInfoData.GetValue : Variant;
begin
   if Assigned(FDataMaster) then
      FDataMaster.Read(FExec, FData);
   if Assigned(FTypeSym) and (FTypeSym.Size = 1) then
      Result := FData[FOffset]
   else Result:=FTypeSym.Name;//raise Exception.CreateFmt(RTE_CanNotReadComplexType, [FTypeSym.Caption]);
end;

// GetValueAsString
//
function TInfoData.GetValueAsString : String;
var
   varData : PVarData;
begin
   if (FDataMaster=nil) and (FTypeSym<>nil) and (FTypeSym.Size=1) then begin
      varData:=@FData[FOffset];
      {$ifdef FPC}
      if varData.VType=varString then
         Result:=String(varData.VString)
      {$else}
      if varData.VType=varUString then
         Result:=String(varData.VUString)
      {$endif}
      else Result:=PVariant(varData)^;
   end else Result:=inherited GetValueAsString;
end;

// GetValueAsInteger
//
function TInfoData.GetValueAsInteger : Int64;
var
   varData : PVarData;
begin
   if (FDataMaster=nil) and (FTypeSym<>nil) and (FTypeSym.Size=1) then begin
      varData:=@FData[FOffset];
      if varData.VType=varInt64 then
         Result:=varData.VInt64
      else Result:=PVariant(varData)^;
   end else Result:=inherited GetValueAsInteger;
end;

// GetValueAsFloat
//
function TInfoData.GetValueAsFloat : Double;
var
   varData : PVarData;
begin
   if (FDataMaster=nil) and (FTypeSym<>nil) and (FTypeSym.Size=1) then begin
      varData:=@FData[FOffset];
      if varData.VType=varDouble then
         Result:=varData.VDouble
      else Result:=PVariant(varData)^;
   end else Result:=inherited GetValueAsFloat;
end;

procedure TInfoData.SetData(const Value: TData);
begin
  if Length(Value) <> FTypeSym.Size then
    raise Exception.CreateFmt(RTE_InvalidInputDataSize, [Length(Value), FTypeSym.Size]);
  DWSCopyData(Value, 0, FData, FOffset, FTypeSym.Size);

  if Assigned(FDataMaster) then
  begin
    if FTypeSym.Size = FDataMaster.Size then
      FDataMaster.Write(FExec, FData)
    else
      raise Exception.CreateFmt(RTE_CanOnlyWriteBlocks, [FDataMaster.Caption, FTypeSym.Caption]);
  end;
end;

procedure TInfoData.SetValue(const Value: Variant);
begin
  if Assigned(FTypeSym) and (FTypeSym.Size = 1) then
    FData[FOffset] := Value
  else
    raise Exception.CreateFmt(RTE_CanNotSetValueForType, [FTypeSym.Caption]);

  if Assigned(FDataMaster) then
    FDataMaster.Write(FExec, FData);
end;

{ TInfoClass }

function TInfoClass.GetConstructor(const MethName: String;
  ExtObject: TObject): IInfo;
begin
  Result := GetMethod(MethName);
  Result.ExternalObject := ExtObject;
end;

function TInfoClass.GetInherited: IInfo;
begin
  SetChild(Result, FProgramInfo,(FTypeSym as TClassSymbol).Parent,FData,
           FOffset,FDataMaster);
end;

function TInfoClass.GetMethod(const s: String): IInfo;
var
  sym: TSymbol;
begin
  if not (FTypeSym is TClassSymbol) then
    raise Exception.CreateFmt(RTE_NoClassNoMethod, [FTypeSym.Caption, s]);

  sym := TClassSymbol(FTypeSym).Members.FindSymbol(s, cvMagic);

  if not (sym is TMethodSymbol) then
    sym := nil;

  if not Assigned(sym) then
    raise Exception.CreateFmt(RTE_MethodNotFoundInClass, [s, FTypeSym.Caption]);

  Result := TInfoFunc.Create(FProgramInfo, sym, nil, 0, nil, FScriptObj, TClassSymbol(FTypeSym));
end;

function TInfoClass.GetScriptObj: IScriptObj;
begin
  Result := FScriptObj;
end;

// GetValueAsString
//
function TInfoClass.GetValueAsString : String;
begin
   if FScriptObj=nil then
      Result:='(nil)'
   else if FScriptObj.Destroyed then begin
      if FScriptObj.ClassSym<>nil then
         Result:='destroyed '+FScriptObj.ClassSym.Name
      else Result:='destroyed object';
   end else Result:=FScriptObj.ClassSym.Name;
end;

{ TInfoClassObj }

constructor TInfoClassObj.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Data: TData; Offset: Integer; const DataMaster: IDataMaster);
begin
  inherited;
  if VarType(Data[Offset]) = varUnknown then
    FScriptObj := IScriptObj(IUnknown(Data[Offset]))
  else
    FScriptObj := nil;
end;

destructor TInfoClassObj.Destroy;
begin
   FMembersCache.Free;
   inherited;
end;

// GetMember
//
function TInfoClassObj.GetMember(const s : String) : IInfo;
var
   member : TSymbol;
begin
   member:=FScriptObj.ClassSym.Members.FindSymbol(s, cvMagic);
   if member=nil then
      raise Exception.CreateFmt(RTE_NoMemberOfClass, [s, FTypeSym.Caption]);

   if member is TFieldSymbol then
      SetChild(Result, FProgramInfo, member.Typ, FScriptObj.Data, TFieldSymbol(member).Offset)
   else if member is TPropertySymbol then
      Result:=TInfoProperty.Create(FProgramInfo, member.Typ, nil, 0, TPropertySymbol(member), FScriptObj)
   else raise Exception.CreateFmt(RTE_UnsupportedMemberOfClass, [member.ClassName]);
end;

// GetFieldMemberNames
//
function TInfoClassObj.GetFieldMemberNames : TStrings;

   procedure CollectMembers(members : TMembersSymbolTable);
   var
      i : Integer;
      symTable : TSymbolTable;
      sym : TSymbol;
   begin
      for i:=0 to members.ParentCount-1 do begin
         symTable:=members.Parents[i];
         if symTable is TMembersSymbolTable then
            CollectMembers(TMembersSymbolTable(symTable));
      end;
      for i:=0 to members.Count-1 do begin
         sym:=members.Symbols[i];
         if sym is TFieldSymbol then
            FMembersCache.AddObject(sym.Name, sym);
      end;
   end;

begin
   if FMembersCache=nil then
      FMembersCache:=TStringList.Create
   else FMembersCache.Clear;
   if (FScriptObj<>nil) and (not FScriptObj.Destroyed) then
      CollectMembers(FScriptObj.ClassSym.Members);
   Result:=FMembersCache;
end;

// GetExternalObject
//
function TInfoClassObj.GetExternalObject: TObject;
begin
   if (FScriptObj<>nil) and (not FScriptObj.Destroyed) then
      Result:=FScriptObj.ExternalObject
   else Result:=nil;
end;

// SetExternalObject
//
procedure TInfoClassObj.SetExternalObject(ExtObject: TObject);
begin
   Assert(FScriptObj<>nil);
   Assert(not FScriptObj.Destroyed);
   FScriptObj.ExternalObject:=ExtObject;
end;

{ TTempParam }

constructor TTempParam.Create(ParamSym: TSymbol);
begin
  inherited Create(ParamSym.Name, ParamSym.Typ);
  FIsVarParam := (ParamSym.ClassType=TVarParamSymbol);
  SetLength(FData, Size);
  ParamSym.Typ.InitData(FData, 0);
end;

{ TInfoFunc }

constructor TInfoFunc.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
   const Data: TData; Offset: Integer; const DataMaster: IDataMaster;
   const ScriptObj: IScriptObj; ClassSym: TClassSymbol; ForceStatic: Boolean);
begin
   inherited Create(ProgramInfo, TypeSym, Data, Offset, DataMaster);
   FScriptObj := ScriptObj;
   FClassSym := ClassSym;
   FParams := TFuncSymbol(FTypeSym).Params;
   FParamSize := TFuncSymbol(FTypeSym).ParamSize;
   FTempParams := TUnsortedSymbolTable.Create;
   FForceStatic := ForceStatic;

   if Assigned(TFuncSymbol(FTypeSym).Typ) then
      SetLength(FResult, TFuncSymbol(FTypeSym).Typ.Size)
   else if (FTypeSym is TMethodSymbol) and (TMethodSymbol(FTypeSym).Kind = fkConstructor) then
      SetLength(FResult, 1);
end;

destructor TInfoFunc.Destroy;
begin
   FTempParams.Free;
   inherited;
end;

// Call
//
function TInfoFunc.Call: IInfo;
var
   x : Integer;
   tp : TTempParam;
   funcExpr : TFuncExpr;
   resultAddr : Integer;
   resultData : TData;
begin
   resultData := nil;
   if not FUsesTempParams then
      InitTempParams;

   // Write the var-params as local variables to the stack.
   for x := 0 to FTempParams.Count - 1 do begin
      tp := TTempParam(FTempParams[x]);
      if tp.IsVarParam then begin
         FExec.Stack.Push(tp.Size);
         FExec.Stack.WriteData(0, FExec.Stack.StackPointer-tp.Size, tp.Size, tp.Data);
      end;
   end;

   try

      // Simulate the params of the functions as local variables
      FExec.Stack.Push(FParamSize);
      try
         funcExpr:=CreateTempFuncExpr;
         FExec.ExternalObject:=FExternalObject;
         try

            for x := 0 to FTempParams.Count - 1 do begin
               tp := TTempParam(FTempParams[x]);
               if tp.IsVarParam then begin
                  funcExpr.AddArg(TVarExpr.Create(FExec.Prog, tp));
               end else begin
                  funcExpr.AddArg(TConstExpr.CreateTyped(FExec.Prog, tp.Typ, tp.Data));
               end;
            end;
            funcExpr.Initialize(FExec.Prog);
            if Assigned(funcExpr.Typ) then begin
               if funcExpr.Typ.Size > 1 then begin
                  // Allocate space on the stack to store the Result value
                  FExec.Stack.Push(funcExpr.Typ.Size);
                  try
                     // Result-space is just behind the temporary-params
                     // (Calculated relative to the basepointer of the caller!)
                     funcExpr.SetResultAddr(FExec.CurrentProg, FExec, FExec.Stack.StackPointer-funcExpr.Typ.Size);

                     // Execute function.
                     // Result is stored on the stack
                     resultData := funcExpr.GetData(FExec);
                     resultAddr := funcExpr.GetAddr(FExec);

                     // Copy Result
                     DWSCopyData(resultData, resultAddr, FResult, 0, funcExpr.Typ.Size);
                  finally
                     FExec.Stack.Pop(funcExpr.Typ.Size);
                  end;
               end else funcExpr.EvalAsVariant(FExec, FResult[0]);
               SetChild(Result, FProgramInfo, funcExpr.Typ, FResult, 0);
            end else begin
               // Execute as procedure
               funcExpr.EvalNoResult(FExec);
               Result := nil;
            end;
         finally
            FExec.ExternalObject:=nil;
            funcExpr.Free;
         end;
      finally
         FExec.Stack.Pop(FParamSize);
      end;
   finally
      // Copy back the Result of var-parameters
      for x := FTempParams.Count - 1 downto 0 do begin
         tp := TTempParam(FTempParams[x]);
         if tp.IsVarParam then begin
            FExec.Stack.ReadData(FExec.Stack.Stackpointer - tp.Size, 0, tp.Size, tp.Data);
            FExec.Stack.Pop(tp.Size);
         end;
      end;
   end;
end;

// Call
//
function TInfoFunc.Call(const Params: array of Variant): IInfo;
var
   x : Integer;
   funcSym : TFuncSymbol;
   dataSym : TDataSymbol;
   funcExpr : TFuncExpr;
   resultAddr : Integer;
   resultData : TData;
begin
   resultData := nil;
   funcSym := TFuncSymbol(FTypeSym);

   if Length(Params) <> funcSym.Params.Count then
      raise Exception.CreateFmt(RTE_InvalidNumberOfParams, [Length(Params),
         funcSym.Params.Count, FTypeSym.Caption]);

   funcExpr:=CreateTempFuncExpr;

   FExec.ExternalObject:=FExternalObject;
   try
      // Add arguments to the expression
      for x := Low(Params) to High(Params) do begin
         dataSym := TDataSymbol(FParams[x]);

         if dataSym.Size > 1 then
            raise Exception.CreateFmt(RTE_UseParameter,
                                      [dataSym.Caption, funcSym.Caption]);

         funcExpr.AddArg(TConstExpr.Create(FExec.Prog, dataSym.Typ, Params[x]));
      end;
      funcExpr.Initialize(FExec.Prog);
      if Assigned(funcExpr.Typ) then begin
         if funcExpr.Typ.Size > 1 then begin
            // Allocate space on the stack to store the Result value
            FExec.Stack.Push(funcExpr.Typ.Size);
            try
               // Result-space is just behind the temporary-params
               funcExpr.SetResultAddr(FExec.CurrentProg, FExec, FExec.Stack.StackPointer-FParamSize);

               // Execute function.
               // Result is stored on the stack
               resultData := funcExpr.GetData(FExec);
               resultAddr := funcExpr.GetAddr(FExec);

               // Copy Result
               for x := 0 to funcExpr.Typ.Size - 1 do
                  FResult[x] := resultData[resultAddr + x];
            finally
               FExec.Stack.Pop(funcExpr.Typ.Size);
            end;
         end else funcExpr.EvalAsVariant(FExec, FResult[0]);
         SetChild(Result, FProgramInfo, funcExpr.Typ, FResult, 0);
      end else begin
         funcExpr.EvalNoResult(FExec);
      end;
   finally
      FExec.ExternalObject:=nil;
      funcExpr.Free;
   end;
end;

// GetParameter
//
function TInfoFunc.GetParameter(const s: String): IInfo;
var
   tp: TTempParam;
begin
   if not FUsesTempParams then
      InitTempParams;

   tp := TTempParam(FTempParams.FindSymbol(s, cvMagic));

   if Assigned(tp) then
      SetChild(Result, FProgramInfo, tp.Typ, tp.FData, 0)
   else raise Exception.CreateFmt(RTE_NoParameterFound, [s, FTypeSym.Caption]);
end;

// InitTempParams
//
procedure TInfoFunc.InitTempParams;
var
   x : Integer;
   tp : TTempParam;
begin
   FTempParamSize := 0;
   for x := 0 to FParams.Count - 1 do begin
      tp := TTempParam.Create(FParams[x]);
      FTempParams.AddSymbol(tp);
      if tp.FIsVarParam then begin
         tp.StackAddr := FTempParamSize + FExec.Stack.FrameSize;
         Inc(FTempParamSize, tp.Size);
      end;
   end;
   FUsesTempParams := True;
end;

// GetExternalObject
//
function TInfoFunc.GetExternalObject: TObject;
begin
   Result := FExternalObject;
end;

// SetExternalObject
//
procedure TInfoFunc.SetExternalObject(ExtObject: TObject);
begin
   FExternalObject := ExtObject;
end;

function TInfoFunc.GetInherited: IInfo;
begin
  if FTypeSym is TMethodSymbol then
    result := TInfoFunc.Create(FProgramInfo,TMethodSymbol(FTypeSym).ParentMeth,
      FData,FOffset,FDataMaster,FScriptObj,FClassSym.Parent,True)
  else
    result := inherited GetInherited;
end;

// CreateTempFuncExpr
//
function TInfoFunc.CreateTempFuncExpr : TFuncExpr;
begin
   if FData<>nil then begin
      Result:=TFuncPtrExpr.Create(FExec.Prog, cNullPos,
                                  TConstExpr.Create(FExec.Prog, TFuncSymbol(FTypeSym), FData[FOffset]));
   end else begin
      Result:=CreateFuncExpr(FExec.Prog, TFuncSymbol(FTypeSym), FScriptObj,
                             FClassSym, FForceStatic);
   end;
end;

{ TInfoRecord }

// Destroy
//
destructor TInfoRecord.Destroy;
begin
   FMembersCache.Free;
   inherited;
end;

function TInfoRecord.GetMember(const s: String): IInfo;
var
  sym: TSymbol;
begin
  sym := TRecordSymbol(FTypeSym).Members.FindLocal(s);

  if not (sym is TFieldSymbol) then
    raise Exception.CreateFmt(RTE_NoRecordMemberFound, [s, FTypeSym.Caption]);

  SetChild(Result, FProgramInfo, sym.Typ, FData,
           FOffset + TFieldSymbol(sym).Offset, FDataMaster);
end;

// GetFieldMemberNames
//
function TInfoRecord.GetFieldMemberNames : TStrings;
var
   i : Integer;
   symTable : TSymbolTable;
   member : TSymbol;
begin
   if FMembersCache=nil then begin
      FMembersCache:=TStringList.Create;
      symTable:=TRecordSymbol(FTypeSym).Members;
      for i:=0 to symTable.Count-1 do begin
         member:=symTable[i];
         if member is TFieldSymbol then
            FMembersCache.AddObject(member.Name, member);
      end;
   end;
   Result:=FMembersCache;
end;

{ TInfoStaticArray }

function TInfoStaticArray.Element(const Indices: array of Integer): IInfo;
var
  x: Integer;
  elemTyp: TSymbol;
  arrTyp: TStaticArraySymbol;
  elemOff, elemIdx: Integer;
begin
  elemTyp := FTypeSym;
  elemOff := FOffset;
  for x := 0 to Length(Indices) - 1 do
  begin
    if Assigned(elemTyp) and (elemTyp.BaseType is TStaticArraySymbol) then
      arrTyp := TStaticArraySymbol(elemTyp.BaseType)
    else
      raise Exception.Create(RTE_TooManyIndices);

    if Indices[x] > arrTyp.HighBound then
      raise Exception.CreateFmt(RTE_ArrayUpperBoundExceeded, [x]);

    if Indices[x] < arrTyp.LowBound then
      raise Exception.CreateFmt(RTE_ArrayLowerBoundExceeded, [x]);

    elemTyp := arrTyp.Typ;
    elemIdx := Indices[x] - arrTyp.LowBound;
    elemOff := elemOff + elemIdx * elemTyp.Size;
  end;

  SetChild(Result, FProgramInfo, elemTyp, FData, elemOff, FDataMaster);
end;

function TInfoStaticArray.GetMember(const s: String): IInfo;
var
  h, l: Integer;
begin
  h := TStaticArraySymbol(FTypeSym).HighBound;
  l := TStaticArraySymbol(FTypeSym).LowBound;
  if UnicodeSameText('length', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, h - l + 1)
  else if UnicodeSameText('low', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, l)
  else if UnicodeSameText('high', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, h)
  else
    raise Exception.CreateFmt(RTE_NoMemberOfArray, [s, FTypeSym.Caption]);
end;

// GetValueAsString
//
function TInfoStaticArray.GetValueAsString : String;
begin
   Result:=FTypeSym.Description;
end;

// ------------------
// ------------------ TInfoDynamicArrayBase ------------------
// ------------------

// SelfDynArray
//
function TInfoDynamicArrayBase.SelfDynArray : TScriptDynamicArray;
var
   obj : IScriptObj;
begin
   obj:=IScriptObj(IUnknown(FData[FOffset]));
   if obj<>nil then
      Result:=obj.InternalObject as TScriptDynamicArray
   else Result:=nil;
end;

// ------------------
// ------------------ TInfoDynamicArrayLength ------------------
// ------------------

// Create
//
constructor TInfoDynamicArrayLength.Create(ProgramInfo: TProgramInfo; const Data: TData; Offset, Delta: Integer);
begin
   inherited Create(ProgramInfo, ProgramInfo.Execution.Prog.TypInteger, Data, Offset);
   FDelta:=Delta;
end;

// GetValue
//
function TInfoDynamicArrayLength.GetValue: Variant;
begin
   Result:=GetValueAsInteger;
end;

// GetValueAsInteger
//
function TInfoDynamicArrayLength.GetValueAsInteger : Int64;
begin
   Result:=SelfDynArray.Length+FDelta;
end;

// SetValue
//
procedure TInfoDynamicArrayLength.SetValue(const Value: Variant);
begin
   SelfDynArray.Length:=Value-FDelta;
end;

// ------------------
// ------------------ TInfoDynamicArray ------------------
// ------------------

// Element
//
function TInfoDynamicArray.Element(const Indices: array of Integer): IInfo;
var
   x : Integer;
   elemTyp : TSymbol;
   elemOff : Integer;
   dynArray : TScriptDynamicArray;
begin
   dynArray:=SelfDynArray;

   elemTyp := FTypeSym;
   elemOff := 0;
   if Length(Indices)=0 then
      raise Exception.Create(RTE_TooFewIndices);

   for x := 0 to High(Indices) do begin
      if Assigned(elemTyp) and (elemTyp.BaseType is TDynamicArraySymbol) then
         elemTyp := elemTyp.BaseType.Typ
      else raise Exception.Create(RTE_TooManyIndices);

      if Indices[x]>=dynArray.Length then
         raise Exception.CreateFmt(RTE_ArrayUpperBoundExceeded,[x]);

      if Indices[x]<0 then
         raise Exception.CreateFmt(RTE_ArrayLowerBoundExceeded,[x]);

      elemOff := Indices[x] * SelfDynArray.ElementSize;

      if x<High(Indices) then
         dynArray := IScriptObj(IUnknown(dynArray.Data[elemOff])).InternalObject as TScriptDynamicArray;
   end;

   SetChild(Result, FProgramInfo, elemTyp, dynArray.Data, elemOff, FDataMaster);
end;

// GetMember
//
function TInfoDynamicArray.GetMember(const s: String): IInfo;
begin
   if UnicodeSameText('length', s) then
      Result:=TInfoDynamicArrayLength.Create(FProgramInfo, Data, Offset, 0)
   else if UnicodeSameText('low', s) then
      Result:=TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, 0)
   else if UnicodeSameText('high', s) then
      Result:=TInfoDynamicArrayLength.Create(FProgramInfo, Data, Offset, -1)
   else raise Exception.CreateFmt(RTE_NoMemberOfArray, [s, FTypeSym.Caption]);
end;

// GetValueAsString
//
function TInfoDynamicArray.GetValueAsString : String;
begin
   Result:=FTypeSym.Description;
end;

// GetData
//
function TInfoDynamicArray.GetData : TData;
var
   dynArray : TScriptDynamicArray;
begin
   dynArray:=SelfDynArray;
   Result:=dynArray.Data;
end;

// SetData
//
procedure TInfoDynamicArray.SetData(const Value: TData);
var
   dynArray : TScriptDynamicArray;
begin
   dynArray:=SelfDynArray;
   dynArray.Data:=value;
end;

// GetScriptObj
//
function TInfoDynamicArray.GetScriptObj : IScriptObj;
begin
   Result:=IScriptObj(IUnknown(FData[FOffset]));
end;

// ------------------
// ------------------ TInfoOpenArray ------------------
// ------------------

// GetMember
//
function TInfoOpenArray.GetMember(const s: String): IInfo;
begin
   if UnicodeSameText('length', s) then
      Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, Length(FData))
   else if UnicodeSameText('low', s) then
      Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, 0)
   else if UnicodeSameText('high', s) then
      Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, High(FData))
   else
      raise Exception.CreateFmt(RTE_NoMemberOfClass, [s, FTypeSym.Caption]);
end;

// Element
//
function TInfoOpenArray.Element(const Indices: array of Integer): IInfo;
var
   elemTyp : TSymbol;
   elemOff, elemIdx : Integer;
begin
   if Length(Indices)>1 then
      raise Exception.Create(RTE_TooManyIndices);

   elemIdx := Indices[0];

   if elemIdx > High(FData) then
      raise Exception.CreateFmt(RTE_ArrayUpperBoundExceeded, [elemIdx]);

   if elemIdx < 0 then
      raise Exception.CreateFmt(RTE_ArrayLowerBoundExceeded, [elemIdx]);

   elemTyp := FExec.Prog.TypVariant;
   elemOff := elemIdx * elemTyp.Size;

   SetChild(Result, FProgramInfo, elemTyp, FData, elemOff, FDataMaster);
end;

{ TInfoConst }

constructor TInfoConst.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Value: Variant);
begin
  inherited Create(ProgramInfo, TypeSym, FData, 0);
  SetLength(FData, TypeSym.Size);
  VarCopy(FData[0], Value);
end;

function TInfoConst.GetData : TData;
begin
  Result := FData;
end;

function TInfoConst.GetValue: Variant;
begin
  Result := FData[0];
end;

// ------------------
// ------------------ TInfoProperty ------------------
// ------------------

constructor TInfoProperty.Create(ProgramInfo: TProgramInfo;
  TypeSym: TSymbol; const Data: TData; Offset: Integer;
  PropSym: TPropertySymbol; const ScriptObj: IScriptObj);
begin
  inherited Create(ProgramInfo,TypeSym,Data,Offset);
  FPropSym := PropSym;
  FScriptObj := ScriptObj;
end;

destructor TInfoProperty.Destroy;
begin
  FTempParams.Free;
  inherited;
end;

function TInfoProperty.GetParameter(const s: String): IInfo;
var
  tp: TTempParam;
begin
  if not Assigned(FTempParams) then
    InitTempParams;

  tp := TTempParam(FTempParams.FindSymbol(s, cvMagic));

  if Assigned(tp) then
    SetChild(Result, FProgramInfo, tp.Typ, tp.FData, 0)
  else
    raise Exception.CreateFmt(RTE_NoIndexFound, [s, FPropSym.Name]);
end;

procedure TInfoProperty.InitTempParams;
var
  x: Integer;
  tp: TTempParam;
begin
  FTempParams := TSymbolTable.Create;
  for x := 0 to FPropSym.ArrayIndices.Count - 1 do
  begin
    tp := TTempParam.Create(FPropSym.ArrayIndices[x]);
    FTempParams.AddSymbol(tp);
  end;
end;

function TInfoProperty.GetValue: Variant;
begin
  result := IInfo(Self).Data[0];
end;

procedure TInfoProperty.AssignIndices(const Func: IInfo; FuncParams: TSymbolTable);
var
  paramName: String;
  x: Integer;
  destParam: IInfo;
begin
  if Assigned(FTempParams) then
    for x := 0 to FTempParams.Count - 1 do
    begin
      paramName := FuncParams[x].Name;
      destParam := Func.Parameter[paramName];
      destParam.Data := TTempParam(FTempParams[x]).Data;
    end;
end;

function TInfoProperty.GetData : TData;
var
  func : IInfo;
begin
  if FPropSym.ReadSym is TFuncSymbol then begin
    func := TInfoFunc.Create(FProgramInfo,FPropSym.ReadSym,FData,FOffset,
      FDataMaster,FScriptObj,FScriptObj.ClassSym);
    AssignIndices(func,TFuncSymbol(FPropSym.ReadSym).Params);
    result := func.Call.Data;
  end
  else if FPropSym.ReadSym is TFieldSymbol then
  begin
    SetChild(func, FProgramInfo,FPropSym.ReadSym.Typ,FScriptObj.Data,
      TFieldSymbol(FPropSym.ReadSym).Offset);
    result := func.Data;
{
    fieldSym := TFieldSymbol(FPropSym.ReadSym); // var fieldSym : TFieldSymbol;
    SetLength(result,fieldSym.Typ.Size);
    CopyData(FScriptObj.Data,fieldSym.Offset,result,0,fieldSym.Typ.Size);
}
  end
  else
    raise Exception.Create(CPE_WriteOnlyProperty);
end;

procedure TInfoProperty.SetData(const Value: TData);
var
  func: IInfo;
  paramName: String;
  params: TSymbolTable;
begin
  if FPropSym.WriteSym is TFuncSymbol then
  begin
    func := TInfoFunc.Create(FProgramInfo,FPropSym.WriteSym,FData,FOffset,
      FDataMaster,FScriptObj,FScriptObj.ClassSym);

    params := TFuncSymbol(FPropSym.WriteSym).Params;
    AssignIndices(func,params);

    paramName := params[params.Count - 1].Name;
    func.Parameter[paramName].Data := Value;

    func.Call;
  end
  else if FPropSym.WriteSym is TFieldSymbol then
  begin
    SetChild(func, FProgramInfo,FPropSym.WriteSym.Typ,FScriptObj.Data,
      TFieldSymbol(FPropSym.WriteSym).Offset);
    func.Data := Value;
  end
  else
    raise Exception.Create(CPE_ReadOnlyProperty);
end;

procedure TInfoProperty.SetValue(const Value: Variant);
var dat: TData;
begin
  SetLength(dat,1);
  dat[0] := Value;
  IInfo(Self).Data := dat;
end;

{ TInfoConnector }

function TInfoConnector.GetMember(const s: String): IInfo;
begin
  TInfo.SetChild(Result, FProgramInfo, FTypeSym, FData, FOffset,
    TConnectorMemberDataMaster.Create(FExec, FTypeSym, s, FData[FOffset]));
end;

function TInfoConnector.GetMethod(const s: String): IInfo;
begin
  Result := TInfoConnectorCall.Create(FProgramInfo, FTypeSym,
    FData, FOffset, TConnectorSymbol(FTypeSym).ConnectorType, s);
end;

{ TInfoConnectorCall }

constructor TInfoConnectorCall.Create(ProgramInfo: TProgramInfo;
  TypeSym: TSymbol; const Data: TData; Offset: Integer;
  const ConnectorType: IConnectorType; const Name: String);
begin
  inherited Create(ProgramInfo, TypeSym, Data, Offset);
  FConnectorType := ConnectorType;
  FName := Name;
end;

function TInfoConnectorCall.Call(const Params: array of Variant): IInfo;
var
  x: Integer;
  expr: TConnectorCallExpr;
  resultData: TData;
begin
  expr := TConnectorCallExpr.Create(FExec.Prog, cNullPos, FName,
    TConstExpr.Create(FExec.Prog, FExec.Prog.TypVariant, FData[FOffset]));

  try
    for x := 0 to Length(Params) - 1 do
      expr.AddArg(TConstExpr.Create(FExec.Prog, FExec.Prog.TypVariant, Params[x]));

    if expr.AssignConnectorSym(FExec.Prog, FConnectorType) then
    begin
      if Assigned(expr.Typ) then
      begin
        SetLength(resultData, 1);
        expr.EvalAsVariant(FExec, resultData[0]);
        TInfo.SetChild(Result, FProgramInfo, expr.Typ, resultData, 0);
      end
      else
      begin
        resultData := nil;
        expr.EvalNoResult(FExec);
        Result := nil;
      end;
    end
    else
      raise Exception.CreateFmt(RTE_ConnectorCallFailed, [FName]);
  finally
    expr.Free;
  end;
end;

{ TDataMaster }

constructor TDataMaster.Create(Caller: TdwsProgramExecution; Sym: TSymbol);
begin
  FCaller := Caller;
  FSym := Sym;
end;

function TDataMaster.GetCaption: String;
begin
  Result := FSym.Caption;
end;

function TDataMaster.GetSize: Integer;
begin
  Result := FSym.Size;
end;

procedure TDataMaster.Read(exec : TdwsExecution; const Data: TData);
begin
end;

procedure TDataMaster.Write(exec : TdwsExecution; const Data: TData);
begin
end;

{ TExternalVarDataMaster }

// Read
//
procedure TExternalVarDataMaster.Read(exec : TdwsExecution; const Data: TData);
var
   x : Integer;
   resultData : TData;
   resultAddr : Integer;
   funcExpr : TFuncExpr;
   prog : TdwsProgram;
begin
   resultData := nil;
   // Read an external var
   if TExternalVarSymbol(FSym).ReadFunc<>nil then begin
      funcExpr := CreateFuncExpr(FCaller.Prog, TExternalVarSymbol(FSym).ReadFunc, nil, nil);
      try
         prog:=(exec as TdwsProgramExecution).Prog;
         funcExpr.Initialize(prog);
         if funcExpr.Typ.Size > 1 then begin // !! > 1 untested !!
            funcExpr.SetResultAddr(prog, exec, FCaller.Stack.FrameSize);
            // Allocate space on the stack to store the Result value
            FCaller.Stack.Push(funcExpr.Typ.Size);
            try
               // Execute function.
               resultData := funcExpr.GetData(exec);
               resultAddr := funcExpr.GetAddr(exec);
               // Copy Result
               for x := 0 to funcExpr.Typ.Size - 1 do
                  Data[x] := resultData[resultAddr + x];
            finally
               FCaller.Stack.Pop(funcExpr.Typ.Size);
            end;
         end else funcExpr.EvalAsVariant(exec, Data[0]);
      finally
         funcExpr.Free;
      end;
   end;
end;

// Write
//
procedure TExternalVarDataMaster.Write(exec : TdwsExecution; const Data: TData);
var
   funcExpr : TFuncExpr;
begin
   if TExternalVarSymbol(FSym).WriteFunc<>nil then begin
      funcExpr := CreateFuncExpr(FCaller.Prog, TExternalVarSymbol(FSym).WriteFunc, nil, nil);
      try
         funcExpr.AddArg(TConstExpr.CreateTyped(FCaller.Prog, FSym.Typ, Data));
         funcExpr.AddPushExprs((exec as TdwsProgramExecution).Prog);
         funcExpr.EvalNoResult(exec);
      finally
         funcExpr.Free;
      end;
   end;
end;

{ TConnectorMemberDataMaster }

// Create
//
constructor TConnectorMemberDataMaster.Create(Caller: TdwsProgramExecution;
   Sym: TSymbol; const BaseValue: Variant; const Name: String);
begin
   inherited Create(Caller, Sym);
   FName := Name;
end;

procedure TConnectorMemberDataMaster.Read(exec : TdwsExecution; const Data: TData);
var
  readExpr: TConnectorReadExpr;
  dataSource: TData;
begin
  dataSource := nil;
  readExpr := TConnectorReadExpr.Create(FCaller.Prog, cNullPos, FName,
    TConstExpr.Create(FCaller.Prog, FCaller.Prog.TypVariant, FBaseValue));

  if readExpr.AssignConnectorSym(TConnectorSymbol(FSym).ConnectorType) then
  begin
    dataSource := readExpr.Data[exec];
    DWSCopyData(dataSource, 0, Data, 0, readExpr.Typ.Size);
  end
  else
    raise Exception.Create(RTE_ConnectorReadError);
end;

procedure TConnectorMemberDataMaster.Write(exec : TdwsExecution; const Data: TData);
var
  writeExpr: TConnectorWriteExpr;
begin
  writeExpr := TConnectorWriteExpr.Create(FCaller.Prog, cNullPos, FName,
    TConstExpr.Create(FCaller.Prog, FCaller.Prog.TypVariant, FBaseValue),
    TConstExpr.Create(FCaller.Prog, FCaller.Prog.TypVariant, Data));

  if writeExpr.AssignConnectorSym(TdwsProgramExecution(exec).Prog, TConnectorSymbol(FSym).ConnectorType) then
    writeExpr.EvalNoResult(exec)
  else
    raise Exception.Create(RTE_ConnectorWriteError);
end;

end.
