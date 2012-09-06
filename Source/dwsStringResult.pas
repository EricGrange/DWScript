{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at                                          }
{                                                                      }
{    http://www.mozilla.org/MPL/                                       }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Original Code is DelphiWebScriptII source code, released      }
{    January 1, 2001                                                   }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. Portions created by Matthias Ackermann are             }
{    Copyright (C) 2000 Matthias Ackermann, Switzerland. All           }
{    Rights Reserved.                                                  }
{                                                                      }
{    Contributor(s): Daniele Teti <d.teti@bittime.it>                  }
{                                                                      }
{**********************************************************************}
unit dwsStringResult;

{$I dws.inc}

interface

uses
  Variants, Classes, SysUtils, dwsExprs, dwsSymbols, dwsComp, dwsUtils;

type
  TdwsStringResult = class(TdwsResult)
  private
    FStrBuilder: TWriteOnlyBlockStream;
    function GetStr : String;
  public
    constructor Create(resultType : TdwsResultType); override;
    destructor Destroy; override;
    procedure AddString(const Str: String); override;
    procedure Clear; override;
    procedure SetStr(const Str: String);
    function ReadLn: String;
    function ReadChar: String;
    function ToString : String; override;
    property Str: String read GetStr;
  end;

  TChangeStringEvent = procedure (Result: TdwsStringResult; const Str: String) of object;
  TReadStringEvent = procedure (Result: TdwsStringResult; var Str: String) of object;

  TdwsStringResultType = class(TdwsResultType)
  private
    FOnAddString: TChangeStringEvent;
    FOnSetString: TChangeStringEvent;
    FOnReadLn: TReadStringEvent;
    FOnReadChar: TReadStringEvent;
  public
    function CreateProgResult: TdwsResult; override;
  published
    property OnAddString: TChangeStringEvent read FOnAddString write FOnAddString;
    property OnSetString: TChangeStringEvent read FOnSetString write FOnSetString;
    property OnReadLn: TReadStringEvent read FOnReadLn write FOnReadLn;
    property OnReadChar: TReadStringEvent read FOnReadChar write FOnReadChar;
  end;

  Tdws2StringsUnit = class(TdwsUnitComponent)
  protected
    procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  dwsFunctions, dwsStrings;

type
  TWriteFunction = class(TInternalFunction)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

  TWriteLnFunction = class(TInternalFunction)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

  TWriteAllFunction = class(TInternalFunction)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

  TReadCharFunction = class(TInternalFunction)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

  TReadLnFunction = class(TInternalFunction)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

  TReadAllFunction = class(TInternalFunction)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

{ TdwsStringResult }

// Create
//
constructor TdwsStringResult.Create(resultType : TdwsResultType);
begin
   inherited;
   FStrBuilder:=TWriteOnlyBlockStream.Create;
end;

// Destroy
//
destructor TdwsStringResult.Destroy;
begin
   inherited;
   FStrBuilder.Free;
end;

procedure TdwsStringResult.AddString(const Str: String);
begin
  FStrBuilder.WriteString(Str);
  if Assigned(TdwsStringResultType(ResultType).OnAddString) then
    TdwsStringResultType(ResultType).OnAddString(Self, Str)
end;

// Clear
//
procedure TdwsStringResult.Clear;
begin
   FStrBuilder.Clear;
end;

procedure TdwsStringResult.SetStr(const Str: String);
begin
  FStrBuilder.Clear;
  FStrBuilder.WriteString(Str);
  if Assigned(TdwsStringResultType(ResultType).OnSetString) then
    TdwsStringResultType(ResultType).OnSetString(Self, Str)
end;

function TdwsStringResult.ReadLn: String;
begin
  if Assigned(TdwsStringResultType(ResultType).OnReadLn) then
    TdwsStringResultType(ResultType).OnReadLn(Self, Result)
  else
    Result := '';
end;

function TdwsStringResult.ReadChar: String;
begin
  if Assigned(TdwsStringResultType(ResultType).OnReadLn) then
    TdwsStringResultType(ResultType).OnReadLn(Self, Result)
  else
    Result := '';
end;

// ToString
//
function TdwsStringResult.ToString : String;
begin
   Result:=GetStr;
end;

// GetStr
//
function TdwsStringResult.GetStr : String;
begin
   Result:=FStrBuilder.ToString;
end;

{ TdwsStringResultType }

function TdwsStringResultType.CreateProgResult: TdwsResult;
begin
  Result := TdwsStringResult.Create(Self);
end;

{ Tdws2StringUnit }

procedure Tdws2StringsUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
var
  emptyArg: array of String;
begin
  TWriteFunction.Create(SymbolTable, 'WriteStr', ['Str', SYS_VARIANT], '', []);
  TWriteFunction.Create(SymbolTable, 'Print', ['Str', SYS_VARIANT], '', []);
  TWriteLnFunction.Create(SymbolTable, 'WriteLn', ['Str', SYS_VARIANT], '', []);
  TWriteLnFunction.Create(SymbolTable, 'PrintLn', ['Str', SYS_VARIANT], '', []);
  TWriteAllFunction.Create(SymbolTable, 'WriteAll', ['Str', SYS_VARIANT], '', []);

  SetLength(emptyArg, 0);
  TReadCharFunction.Create(SymbolTable, 'ReadChar', emptyArg, SYS_STRING, []);
  TReadLnFunction.Create(SymbolTable, 'ReadLn', emptyArg, SYS_STRING, []);
  TReadAllFunction.Create(SymbolTable, 'ReadAll', emptyArg, SYS_STRING, []);
end;

constructor Tdws2StringsUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName := 'Strings';
end;

{ TWriteFunction }

procedure TWriteFunction.Execute(info : TProgramInfo);
begin
  Info.Execution.Result.AddString(Info.ValueAsString['Str']);
end;

{ TWriteLnFunction }

procedure TWriteLnFunction.Execute(info : TProgramInfo);
begin
  Info.Execution.Result.AddString(Info.ValueAsString['Str'] + #13#10);
end;

{ TWriteAllFunction }

procedure TWriteAllFunction.Execute(info : TProgramInfo);
begin
  (Info.Execution.Result as TdwsStringResult).SetStr(VarToStr(Info.ValueAsVariant['Str']));
end;

{ TReadCharFunction }

procedure TReadCharFunction.Execute(info : TProgramInfo);
begin
   Info.ResultAsString := TdwsStringResult(Info.Execution.Result).ReadChar;
end;

{ TReadLnFunction }

procedure TReadLnFunction.Execute(info : TProgramInfo);
begin
   Info.ResultAsString := TdwsStringResult(Info.Execution.Result).ReadLn;
end;

{ TReadAllFunction }

procedure TReadAllFunction.Execute(info : TProgramInfo);
begin
   Info.ResultAsString := TdwsStringResult(Info.Execution.Result).Str;
end;

end.
 