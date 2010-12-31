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

{$I dws.inc}

unit dwsStringResult;

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
    procedure AddString(const Str: string); override;
    procedure SetStr(const Str: string);
    function ReadLn: string;
    function ReadChar: string;
    property Str: string read GetStr;
  end;

  TChangeStringEvent = procedure (Result: TdwsStringResult; const Str: string) of object;
  TReadStringEvent = procedure (Result: TdwsStringResult; var Str: string) of object;

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
    procedure Execute; override;
  end;

  TWriteLnFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TWriteAllFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TReadCharFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TReadLnFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TReadAllFunction = class(TInternalFunction)
  public
    procedure Execute; override;
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

procedure TdwsStringResult.AddString(const Str: string);
begin
  FStrBuilder.WriteString(Str);
  if Assigned(TdwsStringResultType(ResultType).OnAddString) then
    TdwsStringResultType(ResultType).OnAddString(Self, Str)
end;

procedure TdwsStringResult.SetStr(const Str: string);
begin
  FStrBuilder.Clear;
  FStrBuilder.WriteString(Str);
  if Assigned(TdwsStringResultType(ResultType).OnSetString) then
    TdwsStringResultType(ResultType).OnSetString(Self, Str)
end;

function TdwsStringResult.ReadLn: string;
begin
  if Assigned(TdwsStringResultType(ResultType).OnReadLn) then
    TdwsStringResultType(ResultType).OnReadLn(Self, Result)
  else
    Result := '';
end;

function TdwsStringResult.ReadChar: string;
begin
  if Assigned(TdwsStringResultType(ResultType).OnReadLn) then
    TdwsStringResultType(ResultType).OnReadLn(Self, Result)
  else
    Result := '';
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
  emptyArg: array of string;
begin
  TWriteFunction.Create(SymbolTable, 'WriteStr', ['Str', SYS_VARIANT], '', False);
  TWriteLnFunction.Create(SymbolTable, 'WriteLn', ['Str', SYS_VARIANT], '', False);
  TWriteAllFunction.Create(SymbolTable, 'WriteAll', ['Str', SYS_VARIANT], '', False);

  SetLength(emptyArg, 0);
  TReadCharFunction.Create(SymbolTable, 'ReadChar', emptyArg, SYS_STRING, False);
  TReadLnFunction.Create(SymbolTable, 'ReadLn', emptyArg, SYS_STRING, False);
  TReadAllFunction.Create(SymbolTable, 'ReadAll', emptyArg, SYS_STRING, False);
end;

constructor Tdws2StringsUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName := 'Strings';
end;

{ TWriteFunction }

procedure TWriteFunction.Execute;
begin
  Info.Caller.Result.AddString(Info.ValueAsString['Str']);
end;

{ TWriteLnFunction }

procedure TWriteLnFunction.Execute;
begin
  Info.Caller.Result.AddString(Info.ValueAsString['Str'] + #13#10);
end;

{ TWriteAllFunction }

procedure TWriteAllFunction.Execute;
begin
  (Info.Caller.Result as TdwsStringResult).SetStr(VarToStr(Info.ValueAsVariant['Str']));
end;

{ TReadCharFunction }

procedure TReadCharFunction.Execute;
begin
  Info.ResultAsString := TdwsStringResult(Info.Caller.Result).ReadChar;
end;

{ TReadLnFunction }

procedure TReadLnFunction.Execute;
begin
  Info.ResultAsString := TdwsStringResult(Info.Caller.Result).ReadLn;
end;

{ TReadAllFunction }

procedure TReadAllFunction.Execute;
begin
  Info.ResultAsString := TdwsStringResult(Info.Caller.Result).Str;
end;

end.
 