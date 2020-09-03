unit dwsClassesLibModule;

interface

uses
  Classes, SysUtils, dwsComp, dwsExprs, dwsHashtables, dwsSymbols,
  dwsClasses, dwsFileSystem, dwsUtils;

type
  TdwsClassesLib = class(TDataModule)
    dwsUnit: TdwsUnit;
    procedure dwsUnitClassesTListMethodsAddEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTListMethodsCountEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTListMethodsDeleteEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTListMethodsIndexOfEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTListMethodsRemoveEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTListMethodsDestroyEval(Info: TProgramInfo;
      ExtObject: TObject);                             
    procedure dwsUnitClassesTHashtableMethodsSizeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTHashtableMethodsCapacityEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTIntegerHashtableMethodsPutEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTIntegerHashtableMethodsGetEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTIntegerHashtableMethodsHasKeyEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTIntegerHashtableMethodsRemoveKeyEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringHashtableMethodsPutEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringHashtableMethodsGetEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringHashtableMethodsHasKeyEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringHashtableMethodsRemoveKeyEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStackMethodsPushEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStackMethodsPopEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStackMethodsPeekEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStackMethodsCountEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTQueueMethodsPushEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTQueueMethodsPopEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTQueueMethodsPeekEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTQueueMethodsCountEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTQueueMethodsDestroyEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStackMethodsDestroyEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringHashtableMethodsDestroyEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTIntegerHashtableMethodsDestroyEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTListMethodsClearEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTListMethodsInsertEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsDestroyEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsAddEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsGetEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsIndexOfEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsIndexOfNameEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsInsertEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsGetValuesEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsGetValueFromIndexEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsGetNamesEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsClearEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsGetTextEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsSetTextEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsGetCountEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsGetCommaTextEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsSetCommaTextEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsLoadFromFileEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsSaveToFileEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsDeleteEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsExchangeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsMoveEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsAddStringsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsGetObjectsEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsAddObjectEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsInsertObjectEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsIndexOfObjectEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringListMethodsSortEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringListMethodsFindEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringListMethodsGetDuplicatesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringListMethodsSetDuplicatesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringListMethodsGetSortedEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringListMethodsSetSortedEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsGetStringsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsSetStringsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTListMethodsGetItemsEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTListMethodsSetItemsEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsSetObjectsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsSetValuesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsSetValueFromIndexEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTHashtableMethodsClearEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTListConstructorsCreateEval(Info: TProgramInfo;
      var ExtObject: TObject);
    procedure dwsUnitClassesTStringsConstructorsCreateEval(Info: TProgramInfo;
      var ExtObject: TObject);
    procedure dwsUnitClassesTIntegerHashtableConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsUnitClassesTStringHashtableConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsUnitClassesTStackConstructorsCreateEval(Info: TProgramInfo;
      var ExtObject: TObject);
    procedure dwsUnitClassesTQueueConstructorsCreateEval(Info: TProgramInfo;
      var ExtObject: TObject);
    procedure dwsUnitClassesTStringBuilderMethodsAppendEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringBuilderMethodsLengthEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringBuilderMethodsClearEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringBuilderMethodsToStringEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringBuilderConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsUnitClassesTStringBuilderCleanUp(ExternalObject: TObject);
    procedure dwsUnitClassesTStringsMethodsRemoveEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTStringListMethodsGetCaseSensitiveEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringListMethodsSetCaseSensitiveEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitClassesTStringsMethodsContainsEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsUnitClassesTListMethodsContainsEval(Info: TProgramInfo;
      ExtObject: TObject);
  private
    FScript: TDelphiWebScript;
    procedure SetScript(const Value: TDelphiWebScript);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Script: TDelphiWebScript read FScript write SetScript;
  end;

implementation

{$R *.DFM}

{ TdwsLib }

procedure TdwsClassesLib.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Script) then
    SetScript(nil)
end;

procedure TdwsClassesLib.SetScript(const Value: TDelphiWebScript);
var
  x: Integer;
begin
  if Assigned(FScript) then
    FScript.RemoveFreeNotification(Self);
  if Assigned(Value) then
    Value.FreeNotification(Self);

  FScript := Value;
  for x := 0 to ComponentCount - 1 do
    if Components[x] is TdwsUnit then
      TdwsUnit(Components[x]).Script := Value;
end;

procedure TdwsClassesLib.dwsUnitClassesTListConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := TInterfaceList.Create;
end;

procedure TdwsClassesLib.dwsUnitClassesTListMethodsDestroyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  ExtObject.Free;
end;

procedure TdwsClassesLib.dwsUnitClassesTListMethodsAddEval(Info: TProgramInfo;
  ExtObject: TObject);
begin
  Info.ResultAsVariant := TInterfaceList(ExtObject).Add(Info.ValueAsVariant['Obj']);
end;

procedure TdwsClassesLib.dwsUnitClassesTListMethodsClearEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TInterfaceList(ExtObject).Clear;
end;

procedure TdwsClassesLib.dwsUnitClassesTListMethodsContainsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean:=(TInterfaceList(ExtObject).IndexOf(Info.ValueAsVariant['Obj'])>=0);
end;

procedure TdwsClassesLib.dwsUnitClassesTListMethodsCountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TInterfaceList(ExtObject).Count;
end;

procedure TdwsClassesLib.dwsUnitClassesTListMethodsDeleteEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TInterfaceList(ExtObject).Delete(Info.ValueAsInteger['Index']);
end;

procedure TdwsClassesLib.dwsUnitClassesTListMethodsGetItemsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsVariant := TInterfaceList(ExtObject)[Info.ValueAsInteger['Index']];
end;

procedure TdwsClassesLib.dwsUnitClassesTListMethodsIndexOfEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TInterfaceList(ExtObject).IndexOf(Info.ValueAsVariant['Obj']);
end;

procedure TdwsClassesLib.dwsUnitClassesTListMethodsInsertEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TInterfaceList(ExtObject).Insert(Info.ValueAsInteger['Index'], Info.ValueAsVariant['Obj']);
end;

procedure TdwsClassesLib.dwsUnitClassesTListMethodsRemoveEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TInterfaceList(ExtObject).Remove(Info.ValueAsVariant['Obj']);
end;

procedure TdwsClassesLib.dwsUnitClassesTListMethodsSetItemsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TInterfaceList(ExtObject)[Info.ValueAsInteger['Index']] := Info.ValueAsVariant['Value'];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
  if not Assigned(ExtObject) then
    ExtObject := TdwsStringList.Create;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsDestroyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  ExtObject.Free;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsAddEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TdwsStrings(ExtObject).Add(Info.ValueAsString['Str']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsAddObjectEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TdwsStrings(ExtObject).AddObject(Info.ValueAsString['S'], Info.ValueAsVariant['AObject']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsAddStringsEval(
  Info: TProgramInfo; ExtObject: TObject);
var
  IObj: IScriptObj;
  StringsObj: TdwsStringList;
begin
  IObj := IScriptObj(IUnknown(Info.ValueAsVariant['Strings']));
  if IObj = nil then
    StringsObj := nil
  else
    StringsObj := TdwsStringList(IObj.ExternalObject);
  TdwsStrings(ExtObject).AddStrings(StringsObj);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsClearEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject).Clear;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsContainsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean:=(TdwsStrings(ExtObject).IndexOf(Info.ValueAsString['Str'])>=0);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsDeleteEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject).Delete(Info.ParamAsInteger[0]);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsRemoveEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject).Remove(Info.ParamAsString[0]);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsExchangeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject).Exchange(Info.ValueAsInteger['Index1'], Info.ValueAsInteger['Index2']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsGetEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TdwsStrings(ExtObject)[Info.ParamAsInteger[0]];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsGetCommaTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TdwsStrings(ExtObject).CommaText;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsGetCountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TdwsStrings(ExtObject).Count;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsGetNamesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TdwsStrings(ExtObject).Names[Info.ParamAsInteger[0]];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsGetObjectsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsVariant := IUnknown(Pointer(TdwsStrings(ExtObject).Objects[Info.ParamAsInteger[0]]));
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsGetStringsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TdwsStrings(ExtObject)[Info.ParamAsInteger[0]];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsGetTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TdwsStrings(ExtObject).Text;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsGetValuesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TdwsStrings(ExtObject).Values[Info.ParamAsString[0]];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsGetValueFromIndexEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsString := TdwsStrings(ExtObject).ValueFromIndex[Info.ParamAsInteger[0]];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsInsertObjectEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject).InsertObject(Info.ValueAsInteger['Index'], Info.ValueAsString['S'], Info.ValueAsVariant['AObject']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsIndexOfEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TdwsStrings(ExtObject).IndexOf(Info.ValueAsString['Str']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsIndexOfNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TdwsStrings(ExtObject).IndexOfName(Info.ValueAsString['Str']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsIndexOfObjectEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TdwsStrings(ExtObject).IndexOfObject(Info.ValueAsVariant['AObject']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsInsertEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject).Insert(Info.ValueAsInteger['Index'], Info.ValueAsString['Str']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsLoadFromFileEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   stream : TStream;
   fileSystem : IdwsFileSystem;
begin
   fileSystem:=Info.Execution.FileSystem;
   stream:=fileSystem.OpenFileStream(Info.ValueAsString['FileName'], fomReadOnly);
   try
      TdwsStrings(ExtObject).LoadFromStream(stream);
   finally
      stream.Free;
   end;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsMoveEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject).Move(Info.ValueAsInteger['CurIndex'], Info.ValueAsInteger['NewIndex']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsSaveToFileEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   stream : TStream;
   fileSystem : IdwsFileSystem;
begin
   fileSystem:=Info.Execution.FileSystem;
   stream:=fileSystem.OpenFileStream(Info.ValueAsString['FileName'], fomCreate);
   try
      TdwsStrings(ExtObject).SaveToStream(stream);
   finally
      stream.Free;
   end;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsSetCommaTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject).CommaText := Info.ValueAsString['Value'];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsSetObjectsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject).Objects[Info.ValueAsInteger['Index']] := Info.ValueAsVariant['Value'];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsSetStringsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject)[Info.ValueAsInteger['Index']] := Info.ValueAsString['Value'];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsSetTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject).Text := Info.ValueAsString['Value'];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsSetValuesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject).Values[Info.ParamAsString[0]] := Info.ParamAsString[1];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringsMethodsSetValueFromIndexEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStrings(ExtObject).ValueFromIndex[Info.ParamAsInteger[0]] := Info.ParamAsString[1];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringListMethodsSortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStringList(ExtObject).Sort;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringListMethodsFindEval(
  Info: TProgramInfo; ExtObject: TObject);
var
  Index: Integer;
begin
  Info.ResultAsBoolean := TdwsStringList(ExtObject).Find(Info.ValueAsString['S'], Index);
  Info.ValueAsInteger['Index'] := Index;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringListMethodsGetCaseSensitiveEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TdwsStringList(ExtObject).CaseSensitive;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringListMethodsGetDuplicatesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := Integer(TdwsStringList(ExtObject).Duplicates);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringListMethodsSetCaseSensitiveEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStringList(ExtObject).CaseSensitive := Info.ValueAsBoolean['value'];
end;

procedure TdwsClassesLib.dwsUnitClassesTStringListMethodsSetDuplicatesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStringList(ExtObject).Duplicates := TDuplicates(Info.ValueAsInteger['Value']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringListMethodsGetSortedEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TdwsStringList(ExtObject).Sorted;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringListMethodsSetSortedEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TdwsStringList(ExtObject).Sorted := Info.ValueAsBoolean['Value'];
end;

procedure TdwsClassesLib.dwsUnitClassesTHashtableMethodsSizeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := THashTable(ExtObject).Size;
end;

procedure TdwsClassesLib.dwsUnitClassesTHashtableMethodsCapacityEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := THashTable(ExtObject).Capacity;
end;

procedure TdwsClassesLib.dwsUnitClassesTHashtableMethodsClearEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  THashTable(ExtObject).Clear;
end;

procedure TdwsClassesLib.dwsUnitClassesTIntegerHashtableConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := TIntegerHashtable.Create;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringHashtableMethodsDestroyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  ExtObject.Free;
end;

procedure TdwsClassesLib.dwsUnitClassesTIntegerHashtableMethodsPutEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TIntegerHashTable(ExtObject).Put(Info.ValueAsInteger['Key'], Info.ValueAsVariant['Value']);
end;

procedure TdwsClassesLib.dwsUnitClassesTIntegerHashtableMethodsGetEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsVariant := TIntegerHashTable(ExtObject).Get(Info.ValueAsInteger['Key']);
end;

procedure TdwsClassesLib.dwsUnitClassesTIntegerHashtableMethodsHasKeyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TIntegerHashTable(ExtObject).HasKey(Info.ValueAsInteger['Key']);
end;

procedure TdwsClassesLib.dwsUnitClassesTIntegerHashtableMethodsRemoveKeyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsVariant := TIntegerHashTable(ExtObject).RemoveKey(Info.ValueAsInteger['Key']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringHashtableConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := TStringHashtable.Create;
end;

procedure TdwsClassesLib.dwsUnitClassesTIntegerHashtableMethodsDestroyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  ExtObject.Free;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringHashtableMethodsPutEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TStringHashtable(ExtObject).Put(Info.ValueAsString['Key'], Info.ValueAsVariant['Value']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringHashtableMethodsGetEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsVariant := TStringHashtable(ExtObject).Get(Info.ValueAsString['Key']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringHashtableMethodsHasKeyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsBoolean := TStringHashtable(ExtObject).HasKey(Info.ValueAsString['Key']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStringHashtableMethodsRemoveKeyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsVariant := TStringHashtable(ExtObject).RemoveKey(Info.ValueAsString['Key']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStackConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := TInterfaceList.Create;
end;

procedure TdwsClassesLib.dwsUnitClassesTStackMethodsDestroyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  ExtObject.Free;
end;

procedure TdwsClassesLib.dwsUnitClassesTStackMethodsPushEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TInterfaceList(ExtObject).Add(Info.ValueAsVariant['Obj']);
end;

procedure TdwsClassesLib.dwsUnitClassesTStackMethodsPopEval(Info: TProgramInfo;
  ExtObject: TObject);
var
  intfList: TInterfaceList;
begin
  intfList := TInterfaceList(ExtObject);
  Info.ResultAsVariant := intfList[intfList.Count - 1];
  intfList.Delete(intfList.Count - 1);
end;

procedure TdwsClassesLib.dwsUnitClassesTStackMethodsPeekEval(
  Info: TProgramInfo; ExtObject: TObject);
var
  intfList: TInterfaceList;
begin
  intfList := TInterfaceList(ExtObject);
  Info.ResultAsVariant := intfList[intfList.Count - 1];
end;

procedure TdwsClassesLib.dwsUnitClassesTStackMethodsCountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TInterfaceList(ExtObject).Count;
end;

procedure TdwsClassesLib.dwsUnitClassesTQueueConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := TInterfaceList.Create;
end;

procedure TdwsClassesLib.dwsUnitClassesTQueueMethodsDestroyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  ExtObject.Free;
end;

procedure TdwsClassesLib.dwsUnitClassesTQueueMethodsPushEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  TInterfaceList(ExtObject).Add(Info.ValueAsVariant['Obj']);
end;

procedure TdwsClassesLib.dwsUnitClassesTQueueMethodsPopEval(Info: TProgramInfo;
  ExtObject: TObject);
var
  intfList: TInterfaceList;
begin
  intfList := TInterfaceList(ExtObject);
  Info.ResultAsVariant := intfList[0];
  intfList.Delete(0);
end;

procedure TdwsClassesLib.dwsUnitClassesTQueueMethodsPeekEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsVariant := TInterfaceList(ExtObject)[0];
end;

procedure TdwsClassesLib.dwsUnitClassesTQueueMethodsCountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := TInterfaceList(ExtObject).Count;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringBuilderConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject:=TWriteOnlyBlockStream.Create;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringBuilderCleanUp(
  ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringBuilderMethodsAppendEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   TWriteOnlyBlockStream(ExtObject).WriteString(Info.ParamAsString[0]);
   Info.ResultAsVariant := Info.ScriptObj;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringBuilderMethodsClearEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   TWriteOnlyBlockStream(ExtObject).Clear;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringBuilderMethodsLengthEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=TWriteOnlyBlockStream(ExtObject).Size;
end;

procedure TdwsClassesLib.dwsUnitClassesTStringBuilderMethodsToStringEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=TWriteOnlyBlockStream(ExtObject).ToString;
end;

end.
