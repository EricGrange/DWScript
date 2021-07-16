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
unit dwsInfoClasses;

{$I dws.inc}

interface

uses
   Classes, SysUtils, Variants,
   dwsUtils, dwsXPlatform, dwsDataContext, dwsInfo, dwsScriptSource,
   dwsSymbols, dwsConnectorSymbols, dwsStack, dwsExprs, dwsFunctions, dwsConstExprs,
   dwsConnectorExprs, dwsConvExprs, dwsMethodExprs, dwsResultFunctions,
   dwsStrings, dwsErrors, dwsCompilerUtils, dwsCompilerContext, dwsJSON, dwsJSONScript;

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
   TInfo = class (TInterfacedObject, IUnknown, IInfo)
      protected
         FExec : TdwsProgramExecution;
         FChild : IInfo;
         FDataPtr : IDataContext;
         FProgramInfo : TProgramInfo;
         FDataMaster : IDataMaster;
         FTypeSym : TSymbol;

         function GetData : TData; virtual;
         function GetExternalObject : TObject; virtual;
         function GetMember(const s : String) : IInfo; virtual;
         function GetFieldMemberNames : TStrings; virtual;
         function GetMethod(const s : String) : IInfo; virtual;
         function GetScriptObj : IScriptObj; virtual;
         function GetScriptDynArray: IScriptDynArray; virtual;
         function GetParameter(const s : String) : IInfo; virtual;
         function GetTypeSym : TSymbol;
         function GetValue : Variant; virtual;
         function GetValueIsEmpty : Boolean; virtual;
         function GetValueAsString : String; virtual;
         function GetValueAsDataString : RawByteString; virtual;
         function GetValueAsInteger : Int64; virtual;
         function GetValueAsBoolean : Boolean; virtual;
         function GetValueAsFloat : Double; virtual;
         function GetInherited: IInfo; virtual;
         function GetExec : IdwsExecution;
         procedure SetData(const Value: TData); virtual;
         procedure SetExternalObject(ExtObject: TObject); virtual;
         procedure SetValue(const Value: Variant); virtual;
         procedure SetValueAsInteger(const value : Int64); virtual;
         procedure SetValueAsString(const value : String); virtual;
         procedure SetValueAsDataString(const value : RawByteString); virtual;
         procedure WriteToJSON(writer : TdwsJSONWriter); virtual;

      public
         constructor Create(programInfo : TProgramInfo; typeSym : TSymbol;
                            const dataPtr : IDataContext; const dataMaster : IDataMaster = nil);

         function Call : IInfo; overload; virtual;
         function Call(const params : array of Variant) : IInfo; overload; virtual;
         function Element(const indices : array of Integer) : IInfo; virtual;
         function GetConstructor(const methName : String; extObject : TObject) : IInfo; virtual;

         property DataPtr : IDataContext read FDataPtr write FDataPtr;
         property ProgramInfo : TProgramInfo read FProgramInfo;

         class procedure SetChild(out result : IInfo; programInfo : TProgramInfo; childTypeSym : TSymbol;
                                  const childDataPtr : IDataContext; const childDataMaster : IDataMaster = nil);
      end;

  TInfoConst = class(TInfo)
  private
    FData: TData;
  public
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Value: Variant);
    function GetValue: Variant; override;
    function GetData : TData; override;
  end;

   TInfoData = class (TInfo)
      protected
         function GetValue: Variant; override;
         function GetValueAsString : String; override;
         function GetValueAsInteger : Int64; override;
         function GetValueAsFloat : Double; override;
         function GetData : TData; override;
         function GetScriptObj: IScriptObj; override;
         function GetScriptDynArray: IScriptDynArray; override;
         procedure SetData(const Value: TData); override;
         procedure SetValue(const Value: Variant); override;
         procedure SetValueAsInteger(const Value: Int64); override;

         procedure WriteToJSON(writer : TdwsJSONWriter); override;
   end;

  TInfoClass = class(TInfoData)
    FScriptObj: IScriptObj;
    function GetConstructor(const MethName: String; ExtObject: TObject): IInfo; override;
    function GetMethod(const s: String): IInfo; override;
    function GetValueIsEmpty : Boolean; override;
    function GetValueAsString : String; override;
    function GetScriptObj: IScriptObj; override;
    function GetInherited: IInfo; override;
  end;

  TInfoClassObj = class(TInfoClass)
    FMembersCache : TStrings;
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
                       const DataPtr: IDataContext;
                       const DataMaster: IDataMaster = nil);
    destructor Destroy; override;
    function GetMember(const s : String): IInfo; override;
    function GetFieldMemberNames : TStrings; override;
    function GetExternalObject: TObject; override;
    procedure SetExternalObject(ExtObject: TObject); override;
  end;

  TInfoInterfaceObj = class(TInfoData)
    // Todo
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
         function SelfDynArray : IScriptDynArray;
   end;

   TInfoDynamicArrayLength = class (TInfoDynamicArrayBase)
      private
         FDelta : Integer;

      public
         constructor Create(ProgramInfo: TProgramInfo; const DataPtr: IDataContext; Delta: Integer);
         function GetValue : Variant; override;
         function GetValueAsInteger : Int64; override;
         procedure SetValue(const Value: Variant); override;
         procedure SetValueAsInteger(const Value: Int64); override;
   end;

   TInfoDynamicArray = class(TInfoDynamicArrayBase)
      protected
         function GetScriptObj : IScriptObj; override;
         function GetScriptDynArray: IScriptDynArray; override;

      public
         function Element(const indices : array of Integer) : IInfo; override;
         function GetMember(const s: String): IInfo; override;
         function GetValueAsString : String; override;
         function GetData : TData; override;
         procedure SetData(const Value: TData); override;
   end;

   TInfoOpenArray = class(TInfoStaticArray)
      function Element(const Indices: array of Integer): IInfo; override;
      function GetMember(const s: String): IInfo; override;
   end;

   TInfoAssociativeArrayBase = class (TInfoData)
      protected
         function SelfAssocArray : IScriptAssociativeArray;
   end;

   TInfoAssociativeArrayLength = class (TInfoAssociativeArrayBase)
      public
         function GetValue : Variant; override;
         function GetValueAsInteger : Int64; override;
         procedure SetValue(const Value: Variant); override;
         procedure SetValueAsInteger(const Value: Int64); override;
   end;

   TInfoAssociativeArray = class(TInfoAssociativeArrayBase)
      public
         function GetMember(const s: String): IInfo; override;
   end;

   TInfoFunc = class(TInfoData)
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

         function CreateTempFuncExpr(context : TdwsCompilerContext) : TFuncExprBase;
         procedure InitTempParams;
         function GetParameter(const s: String): IInfo; override;
         function GetExternalObject: TObject; override;
         procedure SetExternalObject(ExtObject: TObject); override;
         function GetInherited: IInfo; override;

      public
         constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
                            const DataPtr: IDataContext;
                            const DataMaster: IDataMaster;
                            const ScriptObj: IScriptObj;
                            ClassSym: TClassSymbol; ForceStatic: Boolean = False);
         destructor Destroy; override;
         function GetValueIsEmpty : Boolean; override;
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
                            const DataPtr: IDataContext;
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
                            const DataPtr: IDataContext;
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
         procedure Read(exec : TdwsExecution; const data : TData); virtual; abstract;
         procedure Write(exec : TdwsExecution; const data : TData); virtual; abstract;
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

uses dwsCoreExprs, dwsArrayElementContext;

// CreateInfoOnSymbol
//
procedure CreateInfoOnSymbol(var result : IInfo; programInfo : TProgramInfo; typeSym : TSymbol;
                             const data : TData; offset : Integer);
var
   dataPtr : IDataContext;
begin
   programInfo.Execution.DataContext_Create(data, offset, dataPtr);
   TInfo.SetChild(result, programInfo, typeSym, dataPtr, nil);
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
   RegisterStandardResultFunctions(SymbolTable);
end;

// ------------------
// ------------------ TInfo ------------------
// ------------------

constructor TInfo.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const DataPtr: IDataContext; const DataMaster: IDataMaster = nil);
begin
  FProgramInfo := ProgramInfo;
  if Assigned(ProgramInfo) then
    FExec := ProgramInfo.Execution;
  FTypeSym := TypeSym;
  FDataPtr := DataPtr;
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

// GetScriptDynArray
//
function TInfo.GetScriptDynArray: IScriptDynArray;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['DynArray', FTypeSym.Caption]);
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

// GetValueIsEmpty
//
function TInfo.GetValueIsEmpty : Boolean;
begin
   Result := VarIsEmpty(GetValue);
end;

function TInfo.GetValueAsString : String;
begin
   VariantToString(GetValue, Result);
end;

// GetValueAsDataString
//
function TInfo.GetValueAsDataString : RawByteString;
begin
   ScriptStringToRawByteString(GetValueAsString, Result);
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

class procedure TInfo.SetChild(out result : IInfo; programInfo: TProgramInfo;
  childTypeSym: TSymbol; const childDataPtr: IDataContext;
  const childDataMaster: IDataMaster = nil);
var
   baseType : TTypeSymbol;
   baseTypeClass : TClass;
begin
   Assert(Assigned(childTypeSym));
   baseType := childTypeSym.baseType;
   baseTypeClass := baseType.ClassType;

   if    (baseType is TBaseSymbol)
      or (baseTypeClass=TEnumerationSymbol)
      or (baseTypeClass=TSetOfSymbol) then
         result := TInfoData.Create(programInfo, childTypeSym, childDataPtr,
                                    childDataMaster)
   else if childTypeSym.AsFuncSymbol<>nil then
      result := TInfoFunc.Create(programInfo, childTypeSym, childDataPtr,
                                 childDataMaster, nil, nil)
   else if baseTypeClass=TRecordSymbol then
      result := TInfoRecord.Create(programInfo, childTypeSym, childDataPtr,
                                   childDataMaster)
   else if baseType is TStaticArraySymbol then begin
      if baseType is TOpenArraySymbol then begin
         result := TInfoOpenArray.Create(programInfo, childTypeSym, childDataPtr,
                                          childDataMaster);
      end else begin
         result := TInfoStaticArray.Create(programInfo, childTypeSym, childDataPtr,
                                            childDataMaster);
      end;
   end else if baseTypeClass=TDynamicArraySymbol then
      result := TInfoDynamicArray.Create(programInfo, childTypeSym, childDataPtr,
                                          childDataMaster)
   else if baseTypeClass=TClassSymbol then
      result := TInfoClassObj.Create(programInfo, childTypeSym, childDataPtr,
                                      childDataMaster)
   else if baseTypeClass=TClassOfSymbol then
      result := TInfoClassOf.Create(programInfo, childTypeSym, childDataPtr,
                                     childDataMaster)
   else if baseTypeClass = TAssociativeArraySymbol then
      Result := TInfoAssociativeArray.Create(programInfo, childTypeSym, childDataPtr,
                                             childDataMaster)
   else if baseType is TConnectorSymbol then
      result := TInfoData.Create(programInfo, childTypeSym, childDataPtr,
                                    ChildDataMaster)
   else if baseType is TInterfaceSymbol then
      Result := TInfoInterfaceObj.Create(ProgramInfo, ChildTypeSym, childDataPtr,
                                     ChildDataMaster)
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

// SetValueAsInteger
//
procedure TInfo.SetValueAsInteger(const value : Int64);
begin
   SetValue(value);
end;

// SetValueAsString
//
procedure TInfo.SetValueAsString(const value : String);
begin
   SetValue(value);
end;

// SetValueAsDataString
//
procedure TInfo.SetValueAsDataString(const value : RawByteString);
begin
   SetValue(RawByteStringToScriptString(value));
end;

// WriteToJSON
//
procedure TInfo.WriteToJSON(writer : TdwsJSONWriter);
begin
   JSONScript.StringifyVariant(ProgramInfo.Execution, writer, GetValue);
end;

// GetInherited
//
function TInfo.GetInherited: IInfo;
begin
   raise Exception.CreateFmt(RTE_InvalidOp, ['GetInherited', FTypeSym.Caption]);
end;

// GetExec
//
function TInfo.GetExec : IdwsExecution;
begin
   Result:=FProgramInfo.Execution;
end;

{ TInfoData }

function TInfoData.GetData : TData;
begin
  if Assigned(FDataMaster) then
    FDataMaster.Read(FExec, FDataPtr.AsPData^);

  if FDataPtr.DataLength<>0 then begin
     SetLength(Result, FTypeSym.Size);
     FDataPtr.CopyData(Result, 0, FTypeSym.Size);
  end;
end;

// GetScriptObj
//
function TInfoData.GetScriptObj : IScriptObj;
var
   v : Variant;
begin
   if FTypeSym.Size=1 then begin
      v:=GetValue;
      if VariantType(v)=varUnknown then begin
         Result:=IScriptObj(IUnknown(v));
         Exit;
      end;
   end;
   Result:=inherited GetScriptObj;
end;

// GetScriptDynArray
//
function TInfoData.GetScriptDynArray: IScriptDynArray;
var
   v : Variant;
begin
   if FTypeSym.Size=1 then begin
      v:=GetValue;
      if VariantType(v)=varUnknown then begin
         Result:=IScriptDynArray(IUnknown(v));
         Exit;
      end;
   end;
   Result:=inherited GetScriptDynArray;
end;

function TInfoData.GetValue : Variant;
begin
   if Assigned(FDataMaster) then
      FDataMaster.Read(FExec, FDataPtr.AsPData^);
   if Assigned(FTypeSym) and (FTypeSym.Size = 1) then
      FDataPtr.EvalAsVariant(0, Result)
   else Result:=FTypeSym.Name;//raise Exception.CreateFmt(RTE_CanNotReadComplexType, [FTypeSym.Caption]);
end;

// GetValueAsString
//
function TInfoData.GetValueAsString : String;
begin
   if (FDataMaster=nil) and (FTypeSym<>nil) and (FTypeSym.Size=1) then
      FDataPtr.EvalAsString(0, Result)
   else Result := inherited GetValueAsString;
end;

// GetValueAsInteger
//
function TInfoData.GetValueAsInteger : Int64;
begin
   if (FDataMaster=nil) and (FTypeSym<>nil) and (FTypeSym.Size=1) then
      Result := FDataPtr.AsInteger[0]
   else Result := inherited GetValueAsInteger;
end;

// GetValueAsFloat
//
function TInfoData.GetValueAsFloat : Double;
begin
   if (FDataMaster=nil) and (FTypeSym<>nil) and (FTypeSym.Size=1) then
      Result := FDataPtr.AsFloat[0]
   else Result := inherited GetValueAsFloat;
end;

procedure TInfoData.SetData(const Value: TData);
begin
  if Length(Value) <> FTypeSym.Size then
    raise Exception.CreateFmt(RTE_InvalidInputDataSize, [Length(Value), FTypeSym.Size]);
  FDataPtr.WriteData(Value, 0, FTypeSym.Size);

  if Assigned(FDataMaster) then
  begin
    if FTypeSym.Size = FDataMaster.Size then
      FDataMaster.Write(FExec, FDataPtr.AsPData^)
    else
      raise Exception.CreateFmt(RTE_CanOnlyWriteBlocks, [FDataMaster.Caption, FTypeSym.Caption]);
  end;
end;

// SetValue
//
procedure TInfoData.SetValue(const Value: Variant);
begin
   if Assigned(FTypeSym) and (FTypeSym.Size = 1) then
      case VariantType(Value) of
         varInt64, varDouble, varBoolean, {$ifdef FPC}varString{$else}varUString{$endif}, varUnknown :
            FDataPtr[0] := Value;
      else
         if VariantIsFloat(Value) then
            FDataPtr[0] := VariantToFloat(Value)
         else if VariantIsOrdinal(Value) then
            FDataPtr[0] := VariantToInt64(Value)
         else if VariantIsString(Value) then
            FDataPtr[0] := VariantToString(Value)
         else FDataPtr[0] := Value;
      end
   else raise Exception.CreateFmt(RTE_CanNotSetValueForType, [FTypeSym.Caption]);

   if Assigned(FDataMaster) then
      FDataMaster.Write(FExec, FDataPtr.AsPData^);
end;

// SetValueAsInteger
//
procedure TInfoData.SetValueAsInteger(const Value: Int64);
begin
   FDataPtr[0]:=Value;

   if Assigned(FDataMaster) then
      FDataMaster.Write(FExec, FDataPtr.AsPData^);
end;

// WriteToJSON
//
procedure TInfoData.WriteToJSON(writer : TdwsJSONWriter);
begin
   JSONScript.StringifySymbol(ProgramInfo.Execution, writer, GetTypeSym, FDataPtr);
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
  SetChild(Result, FProgramInfo,(FTypeSym as TClassSymbol).Parent,
           FDataPtr, FDataMaster);
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

  Result := TInfoFunc.Create(FProgramInfo, sym, FProgramInfo.Execution.DataContext_Nil,
                             nil, FScriptObj, TClassSymbol(FTypeSym));
end;

// GetValueIsEmpty
//
function TInfoClass.GetValueIsEmpty : Boolean;
begin
   Result := (FScriptObj = nil);
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
   end else begin
      if FScriptObj.ClassSym<>nil then
         Result:=FScriptObj.ClassSym.Name
      else Result:='N/A';
   end;
end;

{ TInfoClassObj }

constructor TInfoClassObj.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const DataPtr: IDataContext; const DataMaster: IDataMaster);
begin
   inherited;
   DataPtr.EvalAsInterface(0, IUnknown(FScriptObj));
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
   locData : IDataContext;
begin
   member:=FScriptObj.ClassSym.Members.FindSymbol(s, cvMagic);
   if member=nil then
      raise Exception.CreateFmt(RTE_NoMemberOfClass, [s, FTypeSym.Caption]);

   if member is TFieldSymbol then begin
      FProgramInfo.Execution.DataContext_CreateOffset(FScriptObj, TFieldSymbol(member).Offset, locData);
      SetChild(Result, FProgramInfo, member.Typ, locData);
   end else if member is TPropertySymbol then
      Result:=TInfoProperty.Create(FProgramInfo, member.Typ, FProgramInfo.Execution.DataContext_Nil,
                                   TPropertySymbol(member), FScriptObj)
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
   if (FScriptObj<>nil) and (not FScriptObj.Destroyed) and (FScriptObj.ClassSym<>nil) then
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
   const DataPtr: IDataContext; const DataMaster: IDataMaster;
   const ScriptObj: IScriptObj; ClassSym: TClassSymbol; ForceStatic: Boolean);
var
   funcSym : TFuncSymbol;
begin
   inherited Create(ProgramInfo, TypeSym, DataPtr, DataMaster);
   FScriptObj := ScriptObj;
   FClassSym := ClassSym;
   funcSym := FTypeSym.AsFuncSymbol;
   FParams := funcSym.Params;
   FParamSize := funcSym.ParamSize;
   FTempParams := TUnsortedSymbolTable.Create;
   FForceStatic := ForceStatic;

   if Assigned(funcSym.Typ) then
      SetLength(FResult, funcSym.Typ.Size)
   else if (funcSym is TMethodSymbol) and (TMethodSymbol(funcSym).Kind = fkConstructor) then
      SetLength(FResult, 1);
end;

destructor TInfoFunc.Destroy;
begin
   FTempParams.Free;
   inherited;
end;

// GetValueIsEmpty
//
function TInfoFunc.GetValueIsEmpty : Boolean;
begin
   Result := (FDataPtr.DataLength = 0) or FDataPtr.IsEmpty(0);
end;

// Call
//
function TInfoFunc.Call : IInfo;
var
   x : Integer;
   tp : TTempParam;
   funcExpr : TFuncExprBase;
   resultDataPtr : IDataContext;
begin
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
         funcExpr:=CreateTempFuncExpr(FExec.CompilerContext);
         FExec.ExternalObject:=FExternalObject;
         try

            for x := 0 to FTempParams.Count - 1 do begin
               tp := TTempParam(FTempParams[x]);
               if tp.IsVarParam then begin
                  funcExpr.AddArg(TVarExpr.Create(cNullPos, tp));
               end else begin
                  funcExpr.AddArg(TConstExpr.Create(cNullPos, tp.Typ, tp.Data, 0));
               end;
            end;
            funcExpr.Initialize(FExec.CompilerContext);
            if Assigned(funcExpr.Typ) then begin
               if funcExpr.Typ.Size > 1 then begin
                  // Allocate space on the stack to store the Result value
                  FExec.Stack.Push(funcExpr.Typ.Size);
                  try
                     // Result-space is just behind the temporary-params
                     // (Calculated relative to the basepointer of the caller!)
                     funcExpr.SetResultAddr(FExec.Stack.StackPointer-funcExpr.Typ.Size);

                     // Execute function.
                     // Result is stored on the stack
                     funcExpr.GetDataPtr(FExec, resultDataPtr);

                     // Copy Result
                     resultDataPtr.CopyData(FResult, 0, funcExpr.Typ.Size);
                  finally
                     FExec.Stack.Pop(funcExpr.Typ.Size);
                  end;
               end else begin
                  funcExpr.EvalAsVariant(FExec, FResult[0]);
               end;
               FProgramInfo.Execution.DataContext_Create(FResult, 0, resultDataPtr);
               SetChild(Result, FProgramInfo, funcExpr.Typ, resultDataPtr);
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
   funcExpr : TFuncExprBase;
   resultDataPtr : IDataContext;
begin
   funcSym := FTypeSym.AsFuncSymbol;

   if Length(Params) <> funcSym.Params.Count then
      raise Exception.CreateFmt(RTE_InvalidNumberOfParams, [Length(Params),
         funcSym.Params.Count, FTypeSym.Caption]);

   funcExpr:=CreateTempFuncExpr(FExec.CompilerContext);

   FExec.ExternalObject:=FExternalObject;
   try
      // Add arguments to the expression
      for x := Low(Params) to High(Params) do begin
         dataSym := TDataSymbol(FParams[x]);

         if dataSym.Size > 1 then
            raise Exception.CreateFmt(RTE_UseParameter,
                                      [dataSym.Caption, funcSym.Caption]);

         funcExpr.AddArg(TConstExpr.Create(cNullPos, dataSym.Typ, Params[x]));
      end;
      funcExpr.Initialize(FExec.CompilerContext);
      if Assigned(funcExpr.Typ) then begin
         if funcExpr.Typ.Size > 1 then begin
            // Allocate space on the stack to store the Result value
            FExec.Stack.Push(funcExpr.Typ.Size);
            try
               // Result-space is just behind the temporary-params
               funcExpr.SetResultAddr(FExec.Stack.StackPointer-FParamSize);

               // Execute function.
               // Result is stored on the stack
               funcExpr.GetDataPtr(FExec, resultDataPtr);

               // Copy Result
               resultDataPtr.CopyData(FResult, 0, funcExpr.Typ.Size);
            finally
               FExec.Stack.Pop(funcExpr.Typ.Size);
            end;
         end else begin
            funcExpr.EvalAsVariant(FExec, FResult[0]);
         end;
         FProgramInfo.Execution.DataContext_Create(FResult, 0, resultDataPtr);
         SetChild(Result, FProgramInfo, funcExpr.Typ, resultDataPtr);
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
   locData : IDataContext;
begin
   if not FUsesTempParams then
      InitTempParams;

   tp := TTempParam(FTempParams.FindSymbol(s, cvMagic));

   if Assigned(tp) then begin
      FProgramInfo.Execution.DataContext_Create(tp.FData, 0, locData);
      SetChild(Result, FProgramInfo, tp.Typ, locData);
   end else raise Exception.CreateFmt(RTE_NoParameterFound, [s, FTypeSym.Caption]);
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

// GetInherited
//
function TInfoFunc.GetInherited : IInfo;
begin
   if FTypeSym is TMethodSymbol then
      Result:=TInfoFunc.Create(FProgramInfo, TMethodSymbol(FTypeSym).ParentMeth,
                               FDataPtr, FDataMaster, FScriptObj, FClassSym.Parent, True)
   else Result:=inherited GetInherited;
end;

// CreateTempFuncExpr
//
function TInfoFunc.CreateTempFuncExpr(context : TdwsCompilerContext) : TFuncExprBase;
var
   caller : TExprBase;
   cf : TCreateFunctionOptions;
begin
   if FDataPtr.DataLength>0 then begin
      Result:=TFuncPtrExpr.Create(context, cNullPos,
                                  TConstExpr.Create(cNullPos, FTypeSym.AsFuncSymbol, FDataPtr[0]));
   end else begin
      if FForceStatic then
         cf := [cfoForceStatic]
      else cf := [];
      Result:=CreateFuncExpr(ProgramInfo.CompilerContext, cNullPos,
                             FTypeSym.AsFuncSymbol, FScriptObj,
                             FClassSym, cf);
   end;
   if Result is TFuncExpr then begin
      caller:=FExec.CallStackLastExpr;
      if caller<>nil then begin
         // called from script
         TFuncExpr(Result).Level:=(caller as TFuncExpr).Level;
         if TFuncExpr(Result).Level=0 then
            TFuncExpr(Result).Level:=1;
      end else begin
         // called from Delphi-side outside of script
         TFuncExpr(Result).Level:=0;
      end;
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
   locData : IDataContext;
begin
   sym := TRecordSymbol(FTypeSym).Members.FindLocal(s);

   if not (sym is TFieldSymbol) then
      raise Exception.CreateFmt(RTE_NoRecordMemberFound, [s, FTypeSym.Caption]);

   DataPtr.CreateOffset(TFieldSymbol(sym).Offset, locData);
   SetChild(Result, FProgramInfo, sym.Typ, locData, FDataMaster);
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
  locData : IDataContext;
begin
  elemTyp := FTypeSym;
  elemOff := 0;
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

   FDataPtr.CreateOffset(elemOff, locData);
   SetChild(Result, FProgramInfo, elemTyp, locData, FDataMaster);
end;

function TInfoStaticArray.GetMember(const s: String): IInfo;
var
  h, l: Integer;
begin
  h := TStaticArraySymbol(FTypeSym).HighBound;
  l := TStaticArraySymbol(FTypeSym).LowBound;
  if UnicodeSameText('length', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.CompilerContext.TypInteger, h - l + 1)
  else if UnicodeSameText('low', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.CompilerContext.TypInteger, l)
  else if UnicodeSameText('high', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.CompilerContext.TypInteger, h)
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
function TInfoDynamicArrayBase.SelfDynArray : IScriptDynArray;
begin
   Result:=IScriptDynArray(FDataPtr.AsInterface[0]);
end;

// ------------------
// ------------------ TInfoDynamicArrayLength ------------------
// ------------------

// Create
//
constructor TInfoDynamicArrayLength.Create(ProgramInfo: TProgramInfo; const DataPtr: IDataContext; Delta: Integer);
begin
   inherited Create(ProgramInfo, ProgramInfo.CompilerContext.TypInteger, DataPtr);
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
   Result:=SelfDynArray.ArrayLength+FDelta;
end;

// SetValue
//
procedure TInfoDynamicArrayLength.SetValue(const Value: Variant);
begin
   SetValueAsInteger(Value);
end;

// SetValueAsInteger
//
procedure TInfoDynamicArrayLength.SetValueAsInteger(const Value: Int64);
begin
   SelfDynArray.ArrayLength:=Value-FDelta;
end;

// ------------------
// ------------------ TInfoDynamicArray ------------------
// ------------------

// Element
//
function TInfoDynamicArray.Element(const indices : array of Integer): IInfo;
var
   x : Integer;
   elemTyp : TSymbol;
   elemOff, elemIndex : Integer;
   dynArray : IScriptDynArray;
   locData : IDataContext;
   intf : IUnknown;
begin
   dynArray:=SelfDynArray;

   elemTyp:=FTypeSym;
   elemIndex:=0;
   if Length(indices)=0 then
      raise Exception.Create(RTE_TooFewIndices);

   for x:=0 to High(indices) do begin
      if Assigned(elemTyp) and (elemTyp.BaseType.ClassType=TDynamicArraySymbol) then
         elemTyp:=elemTyp.BaseType.Typ
      else raise Exception.Create(RTE_TooManyIndices);

      if Cardinal(indices[x])>=Cardinal(dynArray.ArrayLength) then begin
         if indices[x]<0 then
            raise Exception.CreateFmt(RTE_ArrayLowerBoundExceeded, [x])
         else raise Exception.CreateFmt(RTE_ArrayUpperBoundExceeded, [x]);
      end;

      elemIndex := indices[x];
      elemOff := elemIndex*dynArray.ElementSize;

      if x<High(indices) then begin
         if dynArray.VarType(elemOff) <> varUnknown then
            raise Exception.Create(RTE_TooManyIndices);
         dynArray.EvalAsInterface(elemOff, intf);
         dynArray := intf as IScriptDynArray;
      end;
   end;

   locData :=  TArrayElementDataContext.Create(dynArray, elemIndex);

   SetChild(Result, FProgramInfo, elemTyp, locData, FDataMaster);
end;

// GetMember
//
function TInfoDynamicArray.GetMember(const s: String): IInfo;
begin
   if UnicodeSameText('length', s) or UnicodeSameText('count', s) then
      Result:=TInfoDynamicArrayLength.Create(FProgramInfo, DataPtr, 0)
   else if UnicodeSameText('low', s) then
      Result:=TInfoConst.Create(FProgramInfo, FProgramInfo.CompilerContext.TypInteger, 0)
   else if UnicodeSameText('high', s) then
      Result:=TInfoDynamicArrayLength.Create(FProgramInfo, DataPtr, -1)
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
begin
   Result := SelfDynArray.ToData;
end;

// SetData
//
procedure TInfoDynamicArray.SetData(const Value: TData);
begin
   SelfDynArray.ReplaceData(value);
end;

// GetScriptObj
//
function TInfoDynamicArray.GetScriptObj : IScriptObj;
begin
   Result:=(FDataPtr.AsInterface[0] as IScriptObj);
end;

// GetScriptDynArray
//
function TInfoDynamicArray.GetScriptDynArray: IScriptDynArray;
begin
   Result:=IScriptDynArray(FDataPtr.AsInterface[0]);
end;

// ------------------
// ------------------ TInfoOpenArray ------------------
// ------------------

// GetMember
//
function TInfoOpenArray.GetMember(const s: String): IInfo;
begin
   if UnicodeSameText('length', s) then
      Result := TInfoConst.Create(FProgramInfo, FProgramInfo.CompilerContext.TypInteger, FDataPtr.DataLength)
   else if UnicodeSameText('low', s) then
      Result := TInfoConst.Create(FProgramInfo, FProgramInfo.CompilerContext.TypInteger, 0)
   else if UnicodeSameText('high', s) then
      Result := TInfoConst.Create(FProgramInfo, FProgramInfo.CompilerContext.TypInteger, FDataPtr.DataLength-1)
   else
      raise Exception.CreateFmt(RTE_NoMemberOfClass, [s, FTypeSym.Caption]);
end;

// Element
//
function TInfoOpenArray.Element(const Indices: array of Integer): IInfo;
var
   elemTyp : TSymbol;
   elemOff, elemIdx : Integer;
   locData : IDataContext;
begin
   if Length(Indices)>1 then
      raise Exception.Create(RTE_TooManyIndices);

   elemIdx := Indices[0];

   if elemIdx >= FDataPtr.DataLength then
      raise Exception.CreateFmt(RTE_ArrayUpperBoundExceeded, [elemIdx]);

   if elemIdx < 0 then
      raise Exception.CreateFmt(RTE_ArrayLowerBoundExceeded, [elemIdx]);

   elemTyp := FProgramInfo.CompilerContext.TypVariant;
   elemOff := elemIdx * elemTyp.Size;

   FDataPtr.CreateOffset(elemOff, locData);
   SetChild(Result, FProgramInfo, elemTyp, locData, FDataMaster);
end;

// ------------------
// ------------------ TInfoAssociativeArrayBase ------------------
// ------------------

// SelfAssocArray
//
function TInfoAssociativeArrayBase.SelfAssocArray : IScriptAssociativeArray;
begin
   Result:=IScriptAssociativeArray(FDataPtr.AsInterface[0]);
end;

// ------------------
// ------------------ TInfoAssociativeArrayLength ------------------
// ------------------

// GetValue
//
function TInfoAssociativeArrayLength.GetValue : Variant;
begin
   Result := GetValueAsInteger;
end;

// GetValueAsInteger
//
function TInfoAssociativeArrayLength.GetValueAsInteger : Int64;
begin
   Result := SelfAssocArray.Count;
end;

// SetValue
//
procedure TInfoAssociativeArrayLength.SetValue(const Value: Variant);
begin
   SetValueAsInteger(Value);
end;

// SetValueAsInteger
//
procedure TInfoAssociativeArrayLength.SetValueAsInteger(const Value: Int64);
begin
   if Value <> 0 then
      raise Exception.Create(RTE_IncorrectParameterValue);
   SelfAssocArray.Clear;
end;

// ------------------
// ------------------ TInfoAssociativeArray ------------------
// ------------------

// GetMember
//
function TInfoAssociativeArray.GetMember(const s: String): IInfo;
begin
   if UnicodeSameText('length', s) or UnicodeSameText('count', s) then
      Result := TInfoAssociativeArrayLength.Create(FProgramInfo, FTypeSym, DataPtr)
   else raise Exception.CreateFmt(RTE_NoMemberOfArray, [s, FTypeSym.Caption]);
end;

// ------------------
// ------------------ TInfoConst ------------------
// ------------------

constructor TInfoConst.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Value: Variant);
var
   locData : IDataContext;
begin
  SetLength(FData, TypeSym.Size);
  VarCopySafe(FData[0], Value);
  ProgramInfo.Execution.DataContext_Create(FData, 0, locData);
  inherited Create(ProgramInfo, TypeSym, locData);
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
  TypeSym: TSymbol; const DataPtr: IDataContext;
  PropSym: TPropertySymbol; const ScriptObj: IScriptObj);
begin
  inherited Create(ProgramInfo,TypeSym, DataPtr);
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
  locData : IDataContext;
begin
  if not Assigned(FTempParams) then
    InitTempParams;

  tp := TTempParam(FTempParams.FindSymbol(s, cvMagic));

  if Assigned(tp) then begin
    FProgramInfo.Execution.DataContext_Create(tp.FData, 0, locData);
    SetChild(Result, FProgramInfo, tp.Typ, locData);
  end else
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
  result := GetData[0]; //IInfo(Self).Data[0];
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
  locData : IDataContext;
begin
  if FPropSym.ReadSym.AsFuncSymbol<>nil then begin
    func := TInfoFunc.Create(FProgramInfo,FPropSym.ReadSym, FDataPtr,
      FDataMaster,FScriptObj,FScriptObj.ClassSym);
    AssignIndices(func, FPropSym.ReadSym.AsFuncSymbol.Params);
    result := func.Call.Data;
  end
  else if FPropSym.ReadSym is TFieldSymbol then begin
    FProgramInfo.Execution.DataContext_CreateOffset(FScriptObj, TFieldSymbol(FPropSym.ReadSym).Offset, locData);
    SetChild(func, FProgramInfo,FPropSym.ReadSym.Typ, locData);
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
  locData : IDataContext;
begin
  if FPropSym.WriteSym.AsFuncSymbol<>nil then
  begin
    func := TInfoFunc.Create(FProgramInfo,FPropSym.WriteSym,FDataPtr,
                             FDataMaster,FScriptObj,FScriptObj.ClassSym);

    params := FPropSym.WriteSym.AsFuncSymbol.Params;
    AssignIndices(func,params);

    paramName := params[params.Count - 1].Name;
    func.Parameter[paramName].Data := Value;

    func.Call;
  end
  else if FPropSym.WriteSym is TFieldSymbol then
  begin
    FProgramInfo.Execution.DataContext_CreateOffset(FScriptObj, TFieldSymbol(FPropSym.WriteSym).Offset, locData);
    SetChild(func, FProgramInfo,FPropSym.WriteSym.Typ, locData);
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
  SetData(dat);
end;

{ TInfoConnector }

function TInfoConnector.GetMember(const s: String): IInfo;
begin
  TInfo.SetChild(Result, FProgramInfo, FTypeSym, FDataPtr,
                 TConnectorMemberDataMaster.Create(FExec, FTypeSym, s, FDataPtr[0]));
end;

function TInfoConnector.GetMethod(const s: String): IInfo;
begin
  Result := TInfoConnectorCall.Create(FProgramInfo, FTypeSym,
                                      FDataPtr, TConnectorSymbol(FTypeSym).ConnectorType, s);
end;

{ TInfoConnectorCall }

constructor TInfoConnectorCall.Create(ProgramInfo: TProgramInfo;
  TypeSym: TSymbol; const DataPtr: IDataContext;
  const ConnectorType: IConnectorType; const Name: String);
begin
  inherited Create(ProgramInfo, TypeSym, DataPtr);
  FConnectorType := ConnectorType;
  FName := Name;
end;

function TInfoConnectorCall.Call(const Params: array of Variant): IInfo;
var
  x: Integer;
  expr: TConnectorCallExpr;
  resultData: TData;
  locData : IDataContext;
begin
  expr := TConnectorCallExpr.Create(cNullPos, FName,
    TConstExpr.Create(cNullPos, FProgramInfo.CompilerContext.TypVariant, FDataPtr[0]));

  try
    for x := 0 to Length(Params) - 1 do
      expr.AddArg(TConstExpr.Create(cNullPos, FProgramInfo.CompilerContext.TypVariant, Params[x]));

    if expr.AssignConnectorSym(FExec.Prog, FConnectorType) then
    begin
      if Assigned(expr.Typ) then
      begin
        SetLength(resultData, 1);
        expr.EvalAsVariant(FExec, resultData[0]);
        FProgramInfo.Execution.DataContext_Create(resultData, 0, locData);
        TInfo.SetChild(Result, FProgramInfo, expr.Typ, locData);
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

{ TExternalVarDataMaster }

// Read
//
procedure TExternalVarDataMaster.Read(exec : TdwsExecution; const Data: TData);
var
   resultDataPtr : IDataContext;
   funcExpr : TFuncExprBase;
begin
   // Read an external var
   if TExternalVarSymbol(FSym).ReadFunc<>nil then begin
      funcExpr := TFuncSimpleExpr.Create((exec as TdwsProgramExecution).CompilerContext,
                                         cNullPos, TExternalVarSymbol(FSym).ReadFunc);
      try
         funcExpr.Initialize((exec as TdwsProgramExecution).CompilerContext);
         if funcExpr.Typ.Size > 1 then begin // !! > 1 untested !!
            funcExpr.SetResultAddr(FCaller.Stack.FrameSize);
            // Allocate space on the stack to store the Result value
            FCaller.Stack.Push(funcExpr.Typ.Size);
            try
               // Execute function.
               funcExpr.GetDataPtr(exec, resultDataPtr);
               // Copy Result
               resultDataPtr.CopyData(Data, 0, funcExpr.Typ.Size);
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
   funcExpr : TFuncExprBase;
   context : TdwsCompilerContext;
begin
   if TExternalVarSymbol(FSym).WriteFunc<>nil then begin
      context := (exec as TdwsProgramExecution).CompilerContext;
      funcExpr := TFuncSimpleExpr.Create(context, cNullPos, TExternalVarSymbol(FSym).WriteFunc);
      try
         funcExpr.AddArg(TConstExpr.Create(cNullPos, FSym.Typ, Data, 0));
         if (funcExpr is TFuncExpr) then
            TFuncExpr(funcExpr).AddPushExprs(context);
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
   readExpr: TConnectorReadMemberExpr;
   dataSource: TData;
   baseExpr : TConstExpr;
begin
   dataSource := nil;
   baseExpr := TConstExpr.Create(cNullPos, FCaller.Prog.Root.SystemTable.SymbolTable.TypVariant, FBaseValue);
   readExpr := TConnectorReadMemberExpr.CreateNew(
      cNullPos, FName, baseExpr, TConnectorSymbol(FSym).ConnectorType
   );
   try
      if readExpr<>nil then begin
         dataSource := readExpr.DataPtr[exec].AsPData^;
         DWSCopyData(dataSource, 0, Data, 0, readExpr.Typ.Size);
      end else begin
         baseExpr.Free;
         raise Exception.Create(RTE_ConnectorReadError);
      end;
   finally
      readExpr.Free;
   end;
end;

procedure TConnectorMemberDataMaster.Write(exec : TdwsExecution; const Data: TData);
var
   baseExpr, valueExpr : TConstExpr;
   writeExpr: TConnectorWriteMemberExpr;
   typVariant : TTypeSymbol;
begin
   typVariant := FCaller.Prog.Root.SystemTable.SymbolTable.TypVariant;
   baseExpr := TConstExpr.Create(cNullPos, typVariant, FBaseValue);
   valueExpr := TConstExpr.Create(cNullPos, typVariant, Data, 0);
   writeExpr := TConnectorWriteExpr.CreateNew(
      FCaller.CompilerContext, cNullPos, FName,
      baseExpr, valueExpr, TConnectorSymbol(FSym).ConnectorType
   );
   try
      if writeExpr<>nil then
          writeExpr.EvalNoResult(exec);
   finally
      writeExpr.Free;
   end;
end;

end.
