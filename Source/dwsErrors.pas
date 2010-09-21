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
unit dwsErrors;

interface

uses
  Classes, SysUtils, dwsStrings, dwsSymbols;

type
  TMsgs = class;

   TSourceFile = class
      public
         SourceFile: string;
         SourceCode: string;
   end;

   PScriptPos = ^TScriptPos;
   TScriptPos = packed record
      private
         // 12bits for the column (4096)
         // 20bits for the line (1048576)
         FLineCol : Cardinal;

         function GetLine : Integer; inline;
         procedure SetLine(const aLine : Integer); inline;
         function GetCol : Integer; inline;
         procedure SetCol(const aCol : Integer); inline;

      public
         SourceFile: TSourceFile;
         property Line : Integer read GetLine write SetLine;
         property Col : Integer read GetCol write SetCol;

         function SamePosAs(const aPos : TScriptPos) : Boolean;

         procedure IncCol; inline;
         procedure IncLine; inline;
   end;
   TScriptPosArray = array of TScriptPos; // dynamic array that can hold ScriptPos settings (needed for ReadNameList)

  TdwsMsg = class
  private
    FMsgs: TMsgs;
    FText: string;
  public
    constructor Create(Msgs: TMsgs; Text: string);
    function AsInfo: string; virtual;
    function AsString: string; virtual;
  end;

  // Messages without position
  TInfoMsg = class(TdwsMsg)
    function AsInfo: string; override;
  end;

  TErrorMsg = class(TdwsMsg)
    function AsInfo: string; override;
  end;

  // Messages with position
  TScriptMsg = class(TdwsMsg)
    Pos: TScriptPos;
    constructor Create(Msgs: TMsgs; const Text: string; const P: TScriptPos); overload;
    function AsInfo: string; override;
  end;

  THintMsg = class(TScriptMsg)
    function AsInfo: string; override;
  end;

  TWarningMsg = class(TScriptMsg)
    function AsInfo: string; override;
  end;

  TCompilerErrorMsg = class(TScriptMsg)
    function AsInfo: string; override;
  end;

  TExecutionErrorMsg = class(TScriptMsg)
    function AsInfo: string; override;
  end;

  TLastScriptError = record
    Pos : TScriptPos;
    ExceptObj : TObject;
  end;

  TMsgs = class
  private
    FSourceFiles: TList;
    FMessages: TList;
    FHasErrors: Boolean;
    FHasCompilerErrors: Boolean;
    FHasExecutionErrors: Boolean;
    FLastScriptError: TLastScriptError;

    function GetMsg(Index: Integer): TdwsMsg;
    function GetMsgCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterSourceFile(const SourceFile: string; const SourceCode: string): TSourceFile;
    function GetSourceFile(SourceFile: string): TSourceFile;

    procedure AddInfo(const Text: string);
    procedure AddError(const Text: string);
    procedure AddErrorStop(const Text: string);

    // Called in compiler
    procedure AddCompilerInfo(const Text: string);
    procedure AddCompilerHint(const Pos: TScriptPos; const Text: string);
    procedure AddCompilerWarning(const Pos: TScriptPos; const Text: string);
    procedure AddCompilerError(const Text: string); overload;
    procedure AddCompilerError(const Pos: TScriptPos; const Text: string); overload;
    procedure AddCompilerStop(const Pos: TScriptPos; const Text: string);
    procedure AddCompilerStopFmt(const Pos: TScriptPos; const textFormat : String; const args: array of const);

    // Called during execution
    procedure AddExecutionError(const Text: string); overload;
    procedure AddExecutionError(const Pos: TScriptPos; const Text: string); overload;
    procedure AddExecutionStop(const Pos: TScriptPos; const Text: string);

    procedure SetLastScriptError(const Pos: TScriptPos; ExceptObj : TObject = nil);

    procedure Clear;

    function AsInfo: string;
    function AsString: string;

    property Msgs[Index: Integer]: TdwsMsg read GetMsg; default;
    property Count: Integer read GetMsgCount;
    property HasErrors: Boolean read FHasErrors;
    property HasCompilerErrors: Boolean read FHasCompilerErrors;
    property HasExecutionErrors: Boolean read FHasExecutionErrors;
  end;

  // The script initialization failed because a class needs one or more methods
  // to be implemented.
  EClassIncompleteError = class(Exception)
  private
    FClassSymObj: TObject;   // object that refers to the TClassSymbol
  public
    property ClassSymObj: TObject read FClassSymObj write FClassSymObj;
  end;

  EClassMethodImplIncompleteError = class(EClassIncompleteError);
  EClassPropertyIncompleteError = class(EClassIncompleteError);

  // The script has to be stopped because of an error
  EScriptError = class(Exception);

  EReraise = class(Exception);

  // Is thrown by "raise" statements in script code
  EScriptException = class(Exception)
  private
    FTyp: TSymbol;
    FValue: Variant;
    FPos: TScriptPos;
  public
    constructor Create(const Message: string; const ExceptionObj: IScriptObj; const Pos: TScriptPos); overload;
    constructor Create(const Message: string; const Value: Variant; Typ: TSymbol; const Pos: TScriptPos); overload;
    property ExceptionObj: Variant read FValue;
    property Value: Variant read FValue;
    property Typ: TSymbol read FTyp;
    property Pos: TScriptPos read FPos;
  end;

const
   cNullPos: TScriptPos = (FLineCol: 0; SourceFile: nil);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
  cNoScriptError: TLastScriptError = (Pos: (FLineCol: 0; SourceFile: nil); ExceptObj: nil);

// ------------------
// ------------------ TScriptPos ------------------
// ------------------

// GetLine
//
function TScriptPos.GetLine : Integer;
begin
   Result:=FLineCol and $FFFFF;
end;

// SetLine
//
procedure TScriptPos.SetLine(const aLine : Integer);
begin
   FLineCol:=(FLineCol and $FFF00000) or Cardinal(aLine);
end;

// GetCol
//
function TScriptPos.GetCol : Integer;
begin
   Result:=(FLineCol shr 20) and $FFF;
end;

// SetCol
//
procedure TScriptPos.SetCol(const aCol : Integer);
begin
   FLineCol:=(FLineCol and $FFFFF) or (Cardinal(aCol) shl 20);
end;

// SamePosAs
//
function TScriptPos.SamePosAs(const aPos : TScriptPos) : Boolean;
begin
   Result:=    (FLineCol=aPos.FLineCol)
           and (SourceFile=aPos.SourceFile);
end;

// IncCol
//
procedure TScriptPos.IncCol;
begin
   Inc(FLineCol, $100000);
end;

// IncLine
//
procedure TScriptPos.IncLine;
begin
   Inc(FLineCol);
end;

// ------------------
// ------------------ TMsgs ------------------
// ------------------

constructor TMsgs.Create;
begin
  FSourceFiles := TList.Create;
  FMessages := TList.Create;
  FLastScriptError := cNoScriptError;
end;

destructor TMsgs.Destroy;
var
  x: Integer;
begin
  Clear;
  for x := 0 to FSourceFiles.Count - 1 do
    TSourceFile(FSourceFiles[x]).Free;
  FSourceFiles.Free;
  FMessages.Free;
  inherited;
end;

function TMsgs.RegisterSourceFile(const SourceFile: string; const SourceCode:
  string): TSourceFile;
var
  sf: TSourceFile;
begin
  sf := GetSourceFile(SourceFile);
  if not Assigned(sf) or (sf.SourceCode <> SourceCode) then
  begin
    Result := TSourceFile.Create;
    if SourceFile = MSG_MainModule then
      Result.SourceFile := MSG_MainModule
    else
      Result.SourceFile := AnsiLowerCase(ExpandFileName(SourceFile));
    Result.SourceCode := SourceCode;
    FSourceFiles.Add(Result);
  end
  else
    Result := sf;
end;

function TMsgs.GetSourceFile(SourceFile: string): TSourceFile;
var
  x: Integer;
begin
  Result := nil;
  if SourceFile <> MSG_MainModule then
    SourceFile := AnsiLowerCase(ExpandFileName(SourceFile));
  for x := 0 to FSourceFiles.Count - 1 do
    if TSourceFile(FSourceFiles[x]).SourceFile = SourceFile then
    begin
      Result := TSourceFile(FSourceFiles[x]);
      exit;
    end;
end;

procedure TMsgs.AddInfo;
begin
  FMessages.Add(TInfoMsg.Create(Self, Text));
end;

procedure TMsgs.AddError(const Text: string);
begin
  FMessages.Add(TErrorMsg.Create(Self, Text));
  FHasErrors := True;
end;

procedure TMsgs.AddErrorStop(const Text: string);
begin
  AddError(Text);
  raise EScriptError.Create('')
end;

procedure TMsgs.AddCompilerInfo;
begin
  FMessages.Add(TInfoMsg.Create(Self, Text));
end;

procedure TMsgs.AddCompilerHint(const Pos: TScriptPos; const Text: string);
begin
  FMessages.Add(THintMsg.Create(Self, Text, Pos));
end;

procedure TMsgs.AddCompilerWarning(const Pos: TScriptPos; const Text: string);
begin
  FMessages.Add(TWarningMsg.Create(Self, Text, Pos));
end;

procedure TMsgs.AddCompilerError(const Pos: TScriptPos; const Text: string);
var
   lastMsg : TdwsMsg;
   lastCompilerErrMsg : TCompilerErrorMsg;
begin
   if FMessages.Count>0 then begin
      lastMsg:=TdwsMsg(FMessages[FMessages.Count-1]);
      if lastMsg is TCompilerErrorMsg then begin
         lastCompilerErrMsg:=TCompilerErrorMsg(lastMsg);
         if (lastCompilerErrMsg.FText=Text) and lastCompilerErrMsg.Pos.SamePosAs(Pos) then Exit;
      end;
   end;
   FMessages.Add(TCompilerErrorMsg.Create(Self, Text, Pos));
   FHasCompilerErrors := True;
end;

procedure TMsgs.AddCompilerError(const Text: string);
begin
  AddCompilerError(cNullPos, Text);
end;

procedure TMsgs.AddCompilerStop(const Pos: TScriptPos; const Text: string);
begin
  AddCompilerError(Pos, Text);
  raise EScriptError.Create('')
end;

// AddCompilerStopFmt
//
procedure TMsgs.AddCompilerStopFmt(const Pos: TScriptPos; const textFormat : String; const args: array of const);
begin
   AddCompilerStop(Pos, Format(textFormat, args));
end;

procedure TMsgs.AddExecutionError(const Pos: TScriptPos; const Text: string);
begin
  FMessages.Add(TExecutionErrorMsg.Create(Self, Text, Pos));
  FHasExecutionErrors := True;
end;

procedure TMsgs.AddExecutionError(const Text: string);
begin
//   if Assigned(FLastScriptError.ExceptObj) and (FLastScriptError.ExceptObj = ExceptObject) then
      AddExecutionError(FLastScriptError.Pos, Text)
//   else AddExecutionError(Text);
end;

procedure TMsgs.AddExecutionStop(const Pos: TScriptPos; const Text: string);
begin
   if (Count=0) or (Msgs[Count-1].FText<>Text) then
     AddExecutionError(Pos, Text);
  raise EScriptError.Create(Text);
end;

procedure TMsgs.Clear;
var
  x: Integer;
begin
  for x := 0 to FMessages.Count - 1 do
    TdwsMsg(FMessages[x]).Free;
  FMessages.Clear;
  FHasErrors := False;
  FHasCompilerErrors := False;
  FHasExecutionErrors := False;
  FLastScriptError := cNoScriptError;
end;

function TMsgs.GetMsg(Index: Integer): TdwsMsg;
begin
  Result := FMessages[Index];
end;

function TMsgs.GetMsgCount: Integer;
begin
  Result := FMessages.Count;
end;

procedure TMsgs.SetLastScriptError(const Pos: TScriptPos; ExceptObj : TObject);
begin
  // new exception or non-NullPos if same Exception
  if (FLastScriptError.ExceptObj <> ExceptObj)
     or (FLastScriptError.Pos.Line <= 0) and Assigned(FLastScriptError.ExceptObj) then
  begin
    FLastScriptError.Pos := Pos;
    FLastScriptError.ExceptObj := ExceptObj;
  end;
end;

function TMsgs.AsInfo: string;
var
  x: Integer;
begin
  Result := '';
  for x := 0 to Count - 1 do
    Result := Result + Msgs[x].AsInfo + #13#10
end;

function TMsgs.AsString: string;
var
  x: Integer;
begin
  Result := '';
  for x := 0 to Count - 1 do
    Result := Result + Msgs[x].AsString + #13#10
end;

{ TdwsMsg }

function TdwsMsg.AsInfo: string;
begin
  Result := FText;
end;

function TdwsMsg.AsString: string;
begin
  Result := FText;
end;

constructor TdwsMsg.Create;
begin
  FMsgs := Msgs;
  FText := Text;
end;

{ TInfoMsg }

function TInfoMsg.AsInfo: string;
begin
  Result := Format(MSG_Info, [inherited AsInfo]);
end;

{ TErrorMsg }

function TErrorMsg.AsInfo: string;
begin
  Result := Format(MSG_Error, [inherited AsInfo]);
end;

{ TScriptMsg }

constructor TScriptMsg.Create(Msgs: TMsgs; const Text: string; const P: TScriptPos);
begin
  inherited Create(Msgs, Text);
  Pos := P;
end;

function TScriptMsg.AsInfo: string;
begin
  if (Pos.Line = cNullPos.Line) and (Pos.Col = cNullPos.Col) then
    Result := FText
  else if not Assigned(Pos.SourceFile) or (Pos.SourceFile.SourceFile = MSG_MainModule) then
    Result := Format(MSG_ScriptMsg, [FText, Pos.Line, Pos.Col])
  else
    Result := Format(MSG_ScriptMsgLong, [FText, Pos.Line, Pos.Col,
                                         ExtractFileName(Pos.SourceFile.SourceFile)])
end;

{ THintMsg }

function THintMsg.AsInfo: string;
begin
  Result := Format(MSG_Hint, [inherited AsInfo]);
end;

{ TWarningMsg }

function TWarningMsg.AsInfo: string;
begin
  Result := Format(MSG_Warning, [inherited AsInfo]);
end;

{ TErrorMsg }

function TCompilerErrorMsg.AsInfo: string;
begin
  Result := Format(MSG_SyntaxError, [inherited AsInfo]);
end;

{ TRuntimeErrorMsg }

function TExecutionErrorMsg.AsInfo: string;
begin
  Result := Format(MSG_RuntimeError, [inherited AsInfo]);
end;

{ EScriptException }

constructor EScriptException.Create(const Message: string; const Value: Variant;
  Typ: TSymbol; const Pos: TScriptPos);
begin
  inherited Create(Message);
  FValue := Value;
  FTyp := Typ;
  FPos := Pos;
end;

constructor EScriptException.Create(const Message: string;
  const ExceptionObj: IScriptObj; const Pos: TScriptPos);
begin
  Create(Message,ExceptionObj,ExceptionObj.ClassSym,Pos);
end;

end.
