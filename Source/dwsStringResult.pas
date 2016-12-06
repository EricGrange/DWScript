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
  Variants, Classes, SysUtils, dwsExprs, dwsSymbols, dwsComp, dwsUtils,
  dwsLegacy;

type
   TdwsStringResult = class(TdwsResult)
      private
         FStrBuilder : TWriteOnlyBlockStream;

         function GetStr : String;

      public
         constructor Create(resultType : TdwsResultType); override;
         destructor Destroy; override;

         procedure AddString(const Str: String); override;
         procedure Clear; override;
         procedure SetStr(const Str: String);

         function ReadLn : String;
         function ReadChar : String;
         function ToString : String; override;

         property Str : String read GetStr;
   end;

   TChangeStringEvent = procedure (result : TdwsStringResult; var str : String) of object;
   TReadStringEvent = procedure (result : TdwsStringResult; var str : String) of object;

   TdwsStringResultType = class(TdwsResultType)
      private
         FOnAddString : TChangeStringEvent;
         FOnSetString : TChangeStringEvent;
         FOnReadLn : TReadStringEvent;
         FOnReadChar : TReadStringEvent;

      protected
         procedure DoAddString(result : TdwsStringResult; var str : String); virtual;
         procedure DoSetString(result : TdwsStringResult; var str : String); virtual;
         procedure DoReadLn(result : TdwsStringResult; var str : String); virtual;
         procedure DoReadChar(result : TdwsStringResult; var str : String); virtual;

      public
         procedure AddResultSymbols(SymbolTable: TSymbolTable); override;
         function CreateProgResult: TdwsResult; override;

      published
         property OnAddString: TChangeStringEvent read FOnAddString write FOnAddString;
         property OnSetString: TChangeStringEvent read FOnSetString write FOnSetString;
         property OnReadLn: TReadStringEvent read FOnReadLn write FOnReadLn;
         property OnReadChar: TReadStringEvent read FOnReadChar write FOnReadChar;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   dwsFunctions, dwsStrings;

type
  TWriteFunction = class(TInternalFunctionWithExecute)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

  TWriteLnFunction = class(TInternalFunctionWithExecute)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

  TWriteAllFunction = class(TInternalFunctionWithExecute)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

  TReadCharFunction = class(TInternalFunctionWithExecute)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

  TReadLnFunction = class(TInternalFunctionWithExecute)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

  TReadAllFunction = class(TInternalFunctionWithExecute)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

{ TdwsStringResult }

// Create
//
constructor TdwsStringResult.Create(resultType : TdwsResultType);
begin
   inherited;
   FStrBuilder:=TWriteOnlyBlockStream.AllocFromPool;
end;

// Destroy
//
destructor TdwsStringResult.Destroy;
begin
   inherited;
   FStrBuilder.ReturnToPool;
end;

// AddString
//
procedure TdwsStringResult.AddString(const Str: String);
var
   buf : String;
begin
   buf:=str;
   TdwsStringResultType(ResultType).DoAddString(Self, buf);
   FStrBuilder.WriteString(buf);
end;

// Clear
//
procedure TdwsStringResult.Clear;
begin
   FStrBuilder.Clear;
end;

// SetStr
//
procedure TdwsStringResult.SetStr(const Str: String);
var
   buf : String;
begin
   buf:=str;
   FStrBuilder.Clear;
   TdwsStringResultType(ResultType).DoSetString(Self, buf);
   FStrBuilder.WriteString(buf);
end;

// ReadLn
//
function TdwsStringResult.ReadLn : String;
begin
   TdwsStringResultType(ResultType).DoReadLn(Self, Result);
end;

// ReadChar
//
function TdwsStringResult.ReadChar : String;
begin
   TdwsStringResultType(ResultType).DoReadChar(Self, Result);
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

// AddResultSymbols
//
procedure TdwsStringResultType.AddResultSymbols(SymbolTable: TSymbolTable);
begin
   TWriteFunction.Create(SymbolTable, 'WriteStr', ['Str', SYS_VARIANT], '', []);
   TWriteLnFunction.Create(SymbolTable, 'WriteLn', ['Str', SYS_VARIANT], '', []);
   TWriteAllFunction.Create(SymbolTable, 'WriteAll', ['Str', SYS_VARIANT], '', []);

   TWriteFunction.Create(SymbolTable, 'Print', ['Str', SYS_VARIANT], '', []);
   TWriteLnFunction.Create(SymbolTable, 'PrintLn', ['Str', SYS_VARIANT], '', []);

   TReadCharFunction.Create(SymbolTable, 'ReadChar', [], SYS_STRING, []);
   TReadLnFunction.Create(SymbolTable, 'ReadLn', [], SYS_STRING, []);
   TReadAllFunction.Create(SymbolTable, 'ReadAll', [], SYS_STRING, []);
end;

// DoAddString
//
procedure TdwsStringResultType.DoAddString(result : TdwsStringResult; var str : String);
begin
   if Assigned(FOnAddString) then
      FOnAddString(result, str);
end;

// DoSetString
//
procedure TdwsStringResultType.DoSetString(result : TdwsStringResult; var str : String);
begin
   if Assigned(FOnAddString) then
      FOnSetString(result, str);
end;

// DoReadLn
//
procedure TdwsStringResultType.DoReadLn(result : TdwsStringResult; var str : String);
begin
   if Assigned(FOnReadLn) then
      FOnReadLn(result, str)
   else str:='';
end;

// DoReadChar
//
procedure TdwsStringResultType.DoReadChar(result : TdwsStringResult; var str : String);
begin
   if Assigned(FOnReadChar) then
      FOnReadChar(result, str)
   else str:='';
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
