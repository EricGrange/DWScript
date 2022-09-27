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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsTabularLibModule;

interface

uses
  System.SysUtils, System.Classes, dwsComp, dwsExprs, dwsSymbols;

type
  TdwsTabularLib = class(TDataModule)
    dwsTabular: TdwsUnit;
    procedure dwsTabularClassesTabularDataConstructorsCreateFromDataSetEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsTabularClassesTabularDataCleanUp(ExternalObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsEvaluateAggregateEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsEvaluateNewColumnEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsExportToSeparatedEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsDropColumnEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsAddColumnEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTabularClassesTabularDataConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsTabularClassesTabularDataConstructorsConnectToSharedEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsLockAndShareEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsColumnNamesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsEvaluateEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTabularClassesTabularDataMethodsColumnStringsEval(
      Info: TProgramInfo; ExtObject: TObject);
  private
    FScript : TDelphiWebScript;
  protected
    procedure SetScript(const val : TDelphiWebScript);
  public
    property Script : TDelphiWebScript read FScript write SetScript;
  end;

implementation

uses
   dwsUtils, dwsTabular, dwsDatabaseLibModule, dwsDatabase, dwsJIT, dwsXPlatform,
   dwsDynamicArrays;

{$R *.dfm}

var
   vSharedTabulars : TSimpleNameObjectHash<TdwsTabular>;
   vSharedTabularsLock : TdwsCriticalSection;

procedure PurgeSharedTabulars;
begin
   vSharedTabularsLock.Enter;
   try
      for var i := 0 to vSharedTabulars.HighIndex do begin
         var tab := vSharedTabulars.BucketObject[i];
         if (tab <> nil) and (tab.SharedCount = 0) then begin
            vSharedTabulars.BucketObject[i] := nil;
            tab.Free;
         end;
      end;
   finally
      vSharedTabularsLock.Leave;
   end;
end;

type
   TScriptTabular = class
      FTabular : TdwsTabular;
      FJIT : TdwsTabularJIT;
      FShareName : String;
      constructor CreateNew;
      constructor CreateFromShared(const name : String);
      destructor Destroy; override;
      procedure LockAndShare(const name : String);
      procedure ParseOptions(const options : IScriptDynArray);
      function PrepareExprFromOpcode(const opCodes : IScriptDynArray) : TdwsTabularExpression;
   end;

constructor TScriptTabular.CreateNew;
begin
   inherited Create;
   FTabular := TdwsTabular.Create;
end;

constructor TScriptTabular.CreateFromShared(const name : String);
begin
   inherited Create;
   if name = '' then
      raise EdwsTabular.Create('Share name cannot be empty');
   vSharedTabularsLock.Enter;
   try
      FTabular := vSharedTabulars.Objects[name];
      if FTabular = nil then begin
         FTabular := TdwsTabular.Create;
         vSharedTabulars.Objects[name] := FTabular
      end;
      FTabular.IncSharedCount
   finally
      vSharedTabularsLock.Leave;
   end;
   FShareName := name;
end;

destructor TScriptTabular.Destroy;
begin
   inherited;
   FreeAndNil(FJIT);
   if FTabular <> nil then begin
      if FTabular.SharedCount > 0 then
         FTabular.DecSharedCount
      else FreeAndNil(FTabular);
   end;
end;

// LockAndShare
//
procedure TScriptTabular.LockAndShare(const name : String);
var
   existing : TdwsTabular;
begin
   if name = '' then
      raise EdwsTabular.Create('Share name cannot be empty');
   if FShareName <> '' then
      raise EdwsTabular.CreateFmt('Already shared as "%s"', [ name ]);
   vSharedTabularsLock.Enter;
   try
      existing := vSharedTabulars.Objects[name];
      if existing <> nil then begin
         if existing.SharedCount = 0 then
            existing.Free
         else raise EdwsTabular.CreateFmt('Shared tabular "%s" already in use', [ name ]);
      end;
      FTabular.IncSharedCount;
      vSharedTabulars.Objects[name] := FTabular;
   finally
      vSharedTabularsLock.Leave;
   end;
   FShareName := name;
end;

function TScriptTabular.PrepareExprFromOpcode(const opCodes : IScriptDynArray) : TdwsTabularExpression;
var
   bufStr : String;
   i : Integer;
begin
   Result := TdwsTabularExpression.Create(FTabular, FJIT);
   try
      for i := 0 to opCodes.ArrayLength-1 do begin
         case opCodes.VarType(i) of
            varDouble, varInt64 :
               Result.PushConst(opCodes.AsFloat[i]);
         else
            opCodes.EvalAsString(i, bufStr);
            Result.Opcode(bufStr);
         end;
      end;
   except
      Result.Free;
      raise;
   end;
end;

procedure TScriptTabular.ParseOptions(const options : IScriptDynArray);
var
   buf : String;
   autoJIT, initJIT : Boolean;
begin
   autoJIT := True;
   initJIT := False;

   for var i := 0 to options.ArrayLength-1 do begin
      options.EvalAsString(i, buf);
      if buf = 'jit' then
         initJIT := True
      else if buf = 'nojit' then
         autoJIT := False;
   end;
   if autoJIT and (FTabular <> nil) then begin
      // JIT overhead is not worth it for smaller datasets
      if FTabular.RowCount >= 256 then
         initJIT := True;
   end;
   if initJIT then
      FJIT := TdwsTabularJIT.Create;
end;

procedure TdwsTabularLib.SetScript(const val : TDelphiWebScript);
begin
   dwsTabular.Script := val;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataCleanUp(
  ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   var tabular := TScriptTabular.CreateNew;
   tabular.ParseOptions(Info.ParamAsScriptDynArray[0]);
   ExtObject := tabular;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataConstructorsConnectToSharedEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   var tabular := TScriptTabular.CreateFromShared(Info.ParamAsString[0]);
   tabular.ParseOptions(Info.ParamAsScriptDynArray[1]);
   ExtObject := tabular;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataConstructorsCreateFromDataSetEval(
  Info: TProgramInfo; var ExtObject: TObject);
var
   dsFields : array of IdwsDataField;
   dsNumeric : array of Boolean;
   buf : String;
begin
   var ds := Info.ParamAsScriptObj[0];
   var scriptDS := (ds.ExternalObject as TScriptDataSet).Intf;

   var scriptTabular := TScriptTabular.CreateNew;
   ExtObject := scriptTabular;

   var tabular := scriptTabular.FTabular;

   var nbFields := scriptDS.FieldCount;
   SetLength(dsFields, nbFields);
   SetLength(dsNumeric, nbFields);
   for var i := 0 to nbFields-1 do begin
      dsFields[i] := scriptDS.Fields[i];
      var declaredType := LowerCase(dsFields[i].DeclaredType);
      dsNumeric[i] := (declaredType = 'double') or (declaredType = 'float') or (declaredType = 'numeric');
      if dsNumeric[i] then
         tabular.AddColumn(dsFields[i].Name, [ tcvNumeric ])
      else tabular.AddColumn(dsFields[i].Name)
   end;

   while not scriptDS.Eof do begin
      for var i := 0 to nbFields-1 do begin
         var column := tabular.ColumnData[i];
         if dsNumeric[i] then begin
            if dsFields[i].IsNull then
               column.AddNull
            else column.Add(dsFields[i].AsFloat);
         end else begin
            dsFields[i].GetAsString(buf);
            column.Add(buf, dsFields[i].AsFloat);
         end;
      end;
      scriptDS.Next;
   end;

   scriptTabular.ParseOptions(Info.ParamAsScriptDynArray[1]);
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsAddColumnEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var scriptTabular := Info.ScriptObj.ExternalObject as TScriptTabular;
   var tabular := scriptTabular.FTabular;
   var name := Info.ParamAsString[0];
   if name = '' then
      raise Exception.Create('Name should not be empty');
   if tabular.IndexOfColumn(name) >= 0 then
      raise Exception.CreateFmt('Column with name "%s" already exists', [ name ]);
   var values := Info.ParamAsScriptDynArray[1];
   if (tabular.ColumnCount > 0) and (values.ArrayLength <> tabular.RowCount) then
      raise Exception.CreateFmt('Mismatching number of elements (got %d, expected %d)',
                                [ values.ArrayLength, tabular.RowCount ]);
   var column := tabular.AddColumn(name);
   for var i := 0 to values.ArrayLength-1 do
      column.Add(values.AsFloat[i]);
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsColumnNamesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var tabular := Info.ScriptObj.ExternalObject as TScriptTabular;
   Info.ResultAsStringArray := tabular.FTabular.ColumnNames;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsColumnStringsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var tabular := Info.ScriptObj.ExternalObject as TScriptTabular;
   var columnName := Info.ParamAsString[0];
   var column := tabular.FTabular.ColumnByName(columnName);
   if column = nil then
      raise EdwsTabular.CreateFmt('Unknown column "%s"', [ columnName ]);
   if tcvString in column.Values then
      Info.ResultAsStringArray := column.Strings
   else begin
      var buf : TStringDynArray;
      SetLength(buf, Length(column.Numbers));
      for var i := 0 to High(buf) do
         FastFloatToStr(column.Numbers[i], buf[i], FormatSettings);
      Info.ResultAsStringArray := buf;
   end;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsDropColumnEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var tabular := Info.ScriptObj.ExternalObject as TScriptTabular;
   info.ResultAsBoolean := tabular.FTabular.DropColumn(Info.ParamAsString[0]);
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsEvaluateAggregateEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var tabular := Info.ScriptObj.ExternalObject as TScriptTabular;
   var aggregateFunc := Info.ParamAsString[0];
   var opCodes := Info.ParamAsScriptDynArray[1];
   var fromIndex := Info.ParamAsInteger[2];
   var toIndex := Info.ParamAsInteger[3];

   var expr := tabular.PrepareExprFromOpcode(opCodes);
   try
      expr.JITCompile([ tegSum ]);
      if aggregateFunc = 'sum' then begin
         Info.ResultAsFloat := expr.EvaluateAggregate(tegSum, fromIndex, toIndex);
      end else raise EdwsTabular.CreateFmt('Unsupported aggregate function "%s"', [ aggregateFunc ]);
   finally
      expr.Free;
   end;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsEvaluateNewColumnEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var tabular := Info.ScriptObj.ExternalObject as TScriptTabular;
   var opCodes := Info.ParamAsScriptDynArray[1];

   var expr := tabular.PrepareExprFromOpcode(opCodes);
   try
      expr.JITCompile([]);
      var column := tabular.FTabular.AddColumn(Info.ParamAsString[0]);
      column.Add(expr.EvaluateAll);
   finally
      expr.Free;
   end;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsEvaluateEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var tabular := Info.ScriptObj.ExternalObject as TScriptTabular;
   var opCodes := Info.ParamAsScriptDynArray[0];
   var data : TdwsTabularNumberArray;

   var expr := tabular.PrepareExprFromOpcode(opCodes);
   try
      expr.JITCompile([]);
      data := expr.EvaluateAll;
   finally
      expr.Free;
   end;

   var dyn := TScriptDynamicNativeFloatArray.Create(Info.FuncSym.Result.Typ);
   dyn.SetArrayLength(Length(data));
   for var i := 0 to dyn.ArrayLength-1 do
      dyn.SetAsFloat(i, data[i]);
   Info.ResultAsVariant := dyn as IScriptDynArray;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsExportToSeparatedEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   wobs : TWriteOnlyBlockStream;
   columns : TdwsTabularColumns;
   separator, quote : String;
   separatorFirst, quoteFirst : Char;

   procedure WriteQuoted(const s : String);
   begin
      var buf := s;
      FastStringReplace(buf, quote, quote+quote);
      wobs.WriteString(quote + buf + quote);
   end;

   procedure Write(const s : String);
   var
      p : PChar;
   begin
      if s = '' then Exit;
      p := PChar(s);
      repeat
         if (p^ = separatorFirst) or (p^ = quoteFirst) then begin
            WriteQuoted(s);
            Exit;
         end;
         Inc(p);
      until p^ = #0;
      wobs.WriteString(s);
   end;

var
   buf : String;
begin
   var scriptTabular := Info.ScriptObj.ExternalObject as TScriptTabular;
   var tabular := scriptTabular.FTabular;
   var columnNames := Info.ParamAsScriptDynArray[0];

   quote := Info.ParamAsString[2];
   if separator <> '' then
      separatorFirst := separator[1]
   else separatorFirst := #0;

   separator := Info.ParamAsString[1];
   if quote <> '' then
      quoteFirst := quote[1]
   else quoteFirst := #0;

   wobs := TWriteOnlyBlockStream.AllocFromPool;
   try
      var nbCols := columnNames.ArrayLength;
      SetLength(columns, nbCols);
      for var i := 0 to nbCols-1 do begin
         columnNames.EvalAsString(i, buf);
         var col := tabular.ColumnByName(buf);
         if col = nil then
            raise EdwsTabular.CreateFmt('Unknown column "%s"', [ buf ]);
         columns[i] := col;
         if i > 0 then
            wobs.WriteString(separator);
         Write(buf);
      end;
      wobs.WriteCRLF;
      for var k := 0 to tabular.RowCount-1 do begin
         for var i := 0 to nbCols-1 do begin
            if i > 0 then
               wobs.WriteString(separator);
            Write(columns[i].Strings[k]);
         end;
         wobs.WriteCRLF;
      end;
      Info.ResultAsString := wobs.ToString;
   finally
      wobs.Free;
   end;
end;

procedure TdwsTabularLib.dwsTabularClassesTabularDataMethodsLockAndShareEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var scriptTabular := Info.ScriptObj.ExternalObject as TScriptTabular;
   scriptTabular.LockAndShare(Info.ParamAsString[0]);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vSharedTabulars := TSimpleNameObjectHash<TdwsTabular>.Create;
   vSharedTabularsLock := TdwsCriticalSection.Create;

finalization

   PurgeSharedTabulars;
   FreeAndNil(vSharedTabularsLock);
   FreeAndNil(vSharedTabulars);

end.
