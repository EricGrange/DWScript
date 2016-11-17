unit UTestDispatcher;

interface

Uses
 Windows, SysUtils, ActiveX, Variants, ComObj, dwsExprs, dwsFunctions, dwsLegacy;

Type

 //
 TDispItem = Class(TInterfacedObject, IDispatch)

  Function GetTypeInfoCount(Out Count : Integer) : HResult; StdCall;
  Function GetTypeInfo(Index, LocaleId : Integer; Out TypeInfo) : HResult; StdCall;
  Function GetIDsOfNames(Const IID : TGUID; Names : Pointer; NameCount, LocaleId : Integer; DispIds : Pointer) : HResult; StdCall;
  Function Invoke(DispId : Integer; Const IID : TGUID; LocaleID : Integer; Flags : Word; Var AParams; VarResult, ExcepInfo, ArgErr : Pointer) : HResult; StdCall;

  // Custom method
  Function Call(Const AParams : Array Of Variant) : Variant;

  destructor Destroy; override;

 End;
 //

  TDispCallProxyFunc = Class(TInternalFunctionWithExecute)

   Procedure Execute(info : TProgramInfo); Override;

  End;


implementation

{ TDispItem }

//
Function TDispItem.GetTypeInfoCount(Out Count : Integer) : HResult;
Begin

 Result := E_NOTIMPL;

End;
//

//
Function TDispItem.GetTypeInfo(Index, LocaleId : Integer; Out TypeInfo) : HResult;
Begin

 Result := E_NOTIMPL;

End;
//

//
Function TDispItem.GetIDsOfNames(Const IID : TGUID; Names : Pointer; NameCount, LocaleId : Integer; DispIds : Pointer) : HResult;
Type

 TDispIDs = Array [0 .. High(Integer) Div SizeOf(Integer) - 1] Of Integer;
 PDispIDs = ^TDispIDs;

Var

 NIndex : Integer;

Begin

 For NIndex := 0 To NameCount - 1 Do
  PDispIDs(DispIds)[NIndex] := NIndex;

 Result := S_OK;

End;
//

//
Function TDispItem.Invoke(DispId : Integer; const IID : TGUID; LocaleID: Integer; Flags : Word; Var AParams; VarResult, ExcepInfo, ArgErr : Pointer) : HResult;
Const

 MaxDispArgs = 64;

Var

 PIndex     : Integer;
 Params     : Array [0 .. MaxDispArgs - 1] Of Variant;
 ParamCount : Integer;

 //
 Procedure _AddParam(Const AValue : OleVariant);
 Begin

 Params[ParamCount] := AValue;
 Inc(ParamCount);

 End;
 //

Begin

 If (Flags and DISPATCH_METHOD) = DISPATCH_METHOD Then
  Begin

   If TDispParams(AParams).cNamedArgs > 0 Then
    Result := DISP_E_NONAMEDARGS
   Else
    Begin

     If TDispParams(AParams).cArgs > MaxDispArgs Then
      Raise Exception.Create('To many params ' + IntToStr(TDispParams(AParams).cArgs));

     ParamCount := 0;

     With TDispParams(AParams) Do
      For PIndex := cArgs - 1 DownTo 0 Do
       _AddParam(OleVariant(TVariantArgList(rgvarg^)[PIndex]));

     If Assigned(VarResult) Then
      OleVariant(VarResult^) := Call(Slice(Params, ParamCount))
     Else
      Call(Slice(Params, ParamCount));

     Result := S_OK;

    End;

  End
 Else
  If Flags And DISPATCH_PROPERTYGET <> 0 Then
   Begin

    If TDispParams(AParams).cArgs > 0 Then
     Result := DISP_E_BADPARAMCOUNT
    Else
     Begin

      OleVariant(VarResult^) := TDispItem.Create As IDispatch;
      Result := S_OK;

     End;

   End
  Else
   Result := DISP_E_MEMBERNOTFOUND;

End;
//

//
Function TDispItem.Call(Const AParams : Array Of Variant) : Variant;
Const

 LineBreak : Array [Boolean] Of String = ('', sLineBreak);

 //
 Function _GetArrayInfo(Const AArray : Variant) : String;
 Var

 Index : Integer;

 Begin

 Result := '';

 For Index := VarArrayLowBound(AArray, 1) To VarArrayHighBound(AArray, 1) Do
  Result := Result + Format('%s    [%d] %s', [sLineBreak, Index, VarTypeAsText(TVarData(VarArrayGet(AArray, [Index])).VType)]);

 End;
 //

Var

 Index : Integer;
 S     : String;

Begin

 For Index := Low(AParams) To High(AParams) Do
 Begin

  S := S + LineBreak[S <> ''] + Format('param index: %d, type: %s (%d)', [Index, VarTypeAsText(TVarData(FindVarData(AParams[Index])^).VType), TVarData(FindVarData(AParams[Index])^).VType]);
  If TVarData(FindVarData(AParams[Index])^).VType And varArray <> 0 Then
   S := S + _GetArrayInfo(AParams[Index]);

 End;

 Result := S;

End;
//

{ TDispCallProxyFunc }

//
Procedure TDispCallProxyFunc.Execute(info : TProgramInfo);
Begin
 Info.ResultAsVariant := TDispItem.Create As IDispatch;
End;
//

// ------------------
// ------------------ TDispItem ------------------
// ------------------

// Destroy
//
destructor TDispItem.Destroy;
begin
   inherited;

end;

initialization

   RegisterInternalFunction(TDispCallProxyFunc, 'DispCallProxy', [], 'Variant');

end.
