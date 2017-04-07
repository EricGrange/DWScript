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
unit dwsErrors;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsStrings, dwsUtils, dwsScriptSource, dwsXPlatform, dwsJSON;

type

   TdwsMessage = class;
   TScriptMessage = class;
   TdwsMessageList = class;

   // TdwsMessage
   //
   TdwsMessage = class abstract (TRefCountedObject)
      private
         FMsgs : TdwsMessageList;
         FText : String;

      protected
         property MessageList : TdwsMessageList read FMsgs;

      public
         constructor Create(aMessageList : TdwsMessageList; const Text: String);

         procedure WriteToJSON(writer : TdwsJSONWriter); virtual;

         function AsInfo : String; virtual; abstract;
         function IsError : Boolean; virtual;
         function IsValid : Boolean; virtual;

         property Text : String read FText;
   end;

   TInfoMessage = class(TdwsMessage)
      function AsInfo: String; override;
   end;

   IAutoFixSourceBuffer = interface

      function GetLines(idx : Integer) : String;
      procedure SetLines(idx : Integer; const newLine : String);
      function GetCount : Integer;

      property Lines[idx : Integer] : String read GetLines write SetLines;
      property Count : Integer read GetCount;

      function CodeAt(col, line, nb : Integer) : String;
      procedure ReplaceAt(col, line, nb : Integer; const newText : String);
      procedure InsertLine(line : Integer; const lineText : String);
      procedure DeleteLine(line : Integer);

      procedure MoveCursorTo(col, row : Integer);
      procedure MoveCursorToMark;

   end;

   // TdwsAutoFixAction
   //
   TdwsAutoFixAction = class abstract
      private
         FCaption : String;
         FMsg : TScriptMessage;
         FNextAction : TdwsAutoFixAction;

      public
         constructor Create(msg : TScriptMessage; const caption : String);
         destructor Destroy; override;

         function Apply(const buffer : IAutoFixSourceBuffer) : String; virtual; abstract;
         function Detach : TdwsAutoFixAction; virtual;

         property Msg : TScriptMessage read FMsg write FMsg;
         property Caption : String read FCaption;
         property NextAction : TdwsAutoFixAction read FNextAction;
   end;

   // TScriptMessage
   //
   TScriptMessage = class(TdwsMessage)
      private
         FScriptPos : TScriptPos;
         FAutoFix : TdwsAutoFixAction;

      public
         constructor Create(msgs: TdwsMessageList; const text : String; const p : TScriptPos); overload;
         destructor Destroy; override;

         procedure WriteToJSON(writer : TdwsJSONWriter); override;
         function AsInfo : String; override;

         function Pos : TScriptPos; deprecated 'Renamed as ScriptPos';
         function Line : Integer; inline;
         function Col : Integer; inline;
         function SourceName : String; inline;

         property ScriptPos : TScriptPos read FScriptPos write FScriptPos;
         property AutoFix : TdwsAutoFixAction read FAutoFix write FAutoFix;
   end;

   TScriptMessageClass = class of TScriptMessage;

   TdwsHintsLevel = (hlDisabled, hlNormal, hlStrict, hlPedantic);

   // THintMessage
   //
   THintMessage = class(TScriptMessage)
      private
         FLevel : TdwsHintsLevel;

      public
         constructor Create(msgs: TdwsMessageList; const text : String; const p : TScriptPos;
                            aLevel : TdwsHintsLevel);

         procedure WriteToJSON(writer : TdwsJSONWriter); override;
         function AsInfo: String; override;

         property Level : TdwsHintsLevel read FLevel;
   end;

   TWarningMessage = class(TScriptMessage)
      function AsInfo: String; override;
   end;

   TErrorMessage = class(TScriptMessage)
      function IsError : Boolean; override;
   end;

   TCompilerErrorMessage = class(TErrorMessage)
      function AsInfo: String; override;
   end;

   TSyntaxErrorMessage = class(TErrorMessage)
      function AsInfo: String; override;
   end;

   TdwsMessageListState = (mlsInProgress, mlsStopped, mlsCompleted);

   // TdwsMessageList
   //
   TdwsMessageList = class
      private
         FMessageList : TTightList;
         FSourceFiles : TTightList;
         FErrorsCount : Integer;
         FState : TdwsMessageListState;

      protected
         function GetMsg(Index: Integer): TdwsMessage;
         function GetMsgCount : Integer; inline;
         function GetHasErrors : Boolean; inline;

         function GetSourceFile(const scriptPos : TScriptPos) : TSourceFile;

      public
         destructor Destroy; override;

         procedure AddInfo(const Text: String);
         function LastMessagePos : TScriptPos;

         procedure AddMessage(aMessage : TdwsMessage); virtual;
         procedure AddMsgs(src : TdwsMessageList; lineOffset, colOffset : Integer);
         procedure Clear;
         procedure Delete(i : Integer);
         procedure RemoveInvalidDeferred;

         function AsInfo : String;
         function AsJSON : String;

         procedure WriteJSONValue(writer : TdwsJSONWriter);

         property Msgs[index : Integer] : TdwsMessage read GetMsg; default;
         property Count : Integer read GetMsgCount;
         property HasErrors : Boolean read GetHasErrors;
         property State : TdwsMessageListState read FState write FState;
   end;

   // TdwsCompileMessageList
   //
   TdwsCompileMessageList = class (TdwsMessageList)
      private
         FHintsLevel : TdwsHintsLevel;
         FWarningsDisabled : Boolean;

      public
         function AddCompilerInfo(const Text: String) : TInfoMessage;

         function AddCompilerHint(const aScriptPos: TScriptPos; const Text : String;
                                  const aLevel : TdwsHintsLevel = hlNormal) : TScriptMessage; overload;
         function AddCompilerHintFmt(const aScriptPos: TScriptPos; const textFormat :String;
                                     const args : array of const;
                                     const aLevel : TdwsHintsLevel = hlNormal) : TScriptMessage; overload;

         function AddCompilerWarning(const aScriptPos: TScriptPos; const Text: String) : TScriptMessage;
         function AddCompilerWarningFmt(const aScriptPos: TScriptPos; const textFormat : String;
                                        const args: array of const) : TScriptMessage;

         function AddCompilerError(const aScriptPos: TScriptPos; const Text: String;
                                   messageClass : TScriptMessageClass) : TScriptMessage; overload;
         function AddCompilerError(const aScriptPos: TScriptPos; const Text: String) : TScriptMessage; overload;
         function AddCompilerErrorFmt(const aScriptPos: TScriptPos; const textFormat : String;
                                      const args: array of const; messageClass : TScriptMessageClass) : TScriptMessage; overload;
         function AddCompilerErrorFmt(const aScriptPos: TScriptPos; const textFormat : String;
                                      const args: array of const) : TScriptMessage; overload;

         function AddCompilerException(const aScriptPos: TScriptPos; e : Exception) : TScriptMessage;

         procedure AddCompilerStop(const aScriptPos: TScriptPos; const Text: String;
                                   messageClass : TScriptMessageClass); overload;
         procedure AddCompilerStop(const aScriptPos: TScriptPos; const Text: String); overload;
         procedure AddCompilerStopFmt(const aScriptPos: TScriptPos; const textFormat : String;
                                      const args: array of const; messageClass : TScriptMessageClass); overload;
         procedure AddCompilerStopFmt(const aScriptPos: TScriptPos; const textFormat : String;
                                      const args: array of const); overload;

         property HintsLevel : TdwsHintsLevel read FHintsLevel write FHintsLevel;
         property WarningsDisabled : Boolean read FWarningsDisabled write FWarningsDisabled;
   end;

   // The script initialization failed because a class needs one or more methods
   // to be implemented.
   EClassIncompleteError = class(Exception)
      private
         FClassSymObj : TRefCountedObject;   // object that refers to the TClassSymbol

      public
         property ClassSymObj : TRefCountedObject read FClassSymObj write FClassSymObj;
   end;

   EClassPropertyIncompleteError = class(EClassIncompleteError);

   // Compilation Exception
   ECompileException = class(Exception)
      private
         FScriptPos : TScriptPos;

      public
         constructor CreatePosFmt(const aScriptPos: TScriptPos; const Msg: String; const Args: array of const);
         constructor CreateFromException(const aScriptPos: TScriptPos; e : Exception);

         property ScriptPos : TScriptPos read FScriptPos write FScriptPos;
   end;

   // An optimization failed with an exception
   EOptimizationException = class(ECompileException)
   end;

   // The compilation has to be stopped because of an error
   ECompileError = class(ECompileException)
   end;

   EReraise = class(Exception);

   // TdwsAFAReplace
   //
   TdwsAFAReplace = class (TdwsAutoFixAction)
      private
         FOldText  : String;
         FNewText : String;

      public
         function Apply(const buffer : IAutoFixSourceBuffer) : String; override;

         property OldText : String read FOldText write FOldText;
         property NewText : String read FNewText write FNewText;
   end;

   // TdwsAFAAddImplementation
   //
   TdwsAFAAddImplementation = class (TdwsAutoFixAction)
      private
         FText : String;

      public
         function Apply(const buffer : IAutoFixSourceBuffer) : String; override;

         property Text : String read FText write FText;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ ECompileException ------------------
// ------------------

// CreatePosFmt
//
constructor ECompileException.CreatePosFmt(const aScriptPos: TScriptPos; const Msg: String; const Args: array of const);
begin
   inherited CreateFmt(msg, args);
   FScriptPos:=aScriptPos;
end;

// CreateFromException
//
constructor ECompileException.CreateFromException(const aScriptPos: TScriptPos; e : Exception);
begin
   inherited Create(e.Message);
   FScriptPos:=aScriptPos;
end;

// ------------------
// ------------------ TdwsMessageList ------------------
// ------------------

// Destroy
//
destructor TdwsMessageList.Destroy;
begin
   FMessageList.Clean;
   FSourceFiles.Clean;
   inherited;
end;

// Clear
//
procedure TdwsMessageList.Clear;
begin
   FMessageList.Clean;
   FSourceFiles.Clean;
   FErrorsCount:=0;
end;

// Delete
//
procedure TdwsMessageList.Delete(i : Integer);
var
   msg : TdwsMessage;
begin
   Assert(Cardinal(i) < Cardinal(FMessageList.Count));
   msg := GetMsg(i);
   FMessageList.Delete(i);
   if msg.IsError then
      Dec(FErrorsCount);
   msg.Free;
end;

// RemoveInvalidDeferred
//
procedure TdwsMessageList.RemoveInvalidDeferred;
var
   i : Integer;
   msg : TdwsMessage;
begin
   FErrorsCount:=0;
   for i:=FMessageList.Count-1 downto 0 do begin
      msg:=GetMsg(i);
      if not msg.IsValid then begin
         msg.Free;
         FMessageList.Delete(i);
      end else if msg.IsError then
         Inc(FErrorsCount);
   end;
end;

// GetMsg
//
function TdwsMessageList.GetMsg(Index: Integer): TdwsMessage;
begin
   Result:=TdwsMessage(FMessageList.List[Index]);
end;

// GetMsgCount
//
function TdwsMessageList.GetMsgCount: Integer;
begin
   Result:=FMessageList.Count;
end;

// GetHasErrors
//
function TdwsMessageList.GetHasErrors : Boolean;
begin
   Result:=(FErrorsCount>0);
end;

// GetSourceFile
//
function TdwsMessageList.GetSourceFile(const scriptPos : TScriptPos) : TSourceFile;
var
   i : Integer;
begin
   for i:=0 to FSourceFiles.Count-1 do begin
      Result:=TSourceFile(FSourceFiles.List[i]);
      if Result.Name=scriptPos.SourceName then Exit;
   end;
   Result:=nil;
end;

// AddMessage
//
procedure TdwsMessageList.AddMessage(aMessage: TdwsMessage);
begin
   FMessageList.Add(aMessage);
   if aMessage.IsError then
      if aMessage.IsValid then
         Inc(FErrorsCount);
end;

// AddMsgs
//
procedure TdwsMessageList.AddMsgs(src : TdwsMessageList; lineOffset, colOffset : Integer);
var
   i, col : Integer;
   msg : TdwsMessage;
   scriptMsg : TScriptMessage;
   sf : TSourceFile;
begin
   for i:=0 to src.Count-1 do begin
      msg:=src.Msgs[i];
      if msg is TScriptMessage then begin
         scriptMsg:=TScriptMessage(msg);
         sf:=GetSourceFile(scriptMsg.ScriptPos);
         if sf=nil then begin
            sf:=TSourceFile.Create;
            sf.Name:=scriptMsg.ScriptPos.SourceName;
            sf.Code:=scriptMsg.ScriptPos.SourceCode;
            FSourceFiles.Add(sf);
         end;
         scriptMsg.FScriptPos.SourceFile:=sf;
         if scriptMsg.SourceName=MSG_MainModule then begin
            if scriptMsg.ScriptPos.Line=1 then
               col:=scriptMsg.ScriptPos.Col+colOffset
            else col:=scriptMsg.ScriptPos.Col;
            scriptMsg.ScriptPos.SetColLine(col, scriptMsg.ScriptPos.Line+lineOffset);
         end else begin
            scriptMsg.ScriptPos.SetColLine(scriptMsg.ScriptPos.Col, scriptMsg.ScriptPos.Line);
         end;
      end;
      AddMessage(msg);
   end;
   src.FMessageList.Clear;
   src.FSourceFiles.Clear;
end;

// AddInfo
//
procedure TdwsMessageList.AddInfo(const Text: String);
begin
   AddMessage(TInfoMessage.Create(Self, Text));
end;

// LastMessagePos
//
function TdwsMessageList.LastMessagePos : TScriptPos;
var
   lastMsg : TdwsMessage;
begin
   if Count=0 then
      lastMsg:=nil
   else lastMsg:=Msgs[Count-1];
   if lastMsg is TScriptMessage then
      Result:=TScriptMessage(lastMsg).ScriptPos
   else Result:=cNullPos
end;

// AsInfo
//
function TdwsMessageList.AsInfo: String;
var
   i: Integer;
begin
   Result:='';
   for i:=0 to Count-1 do
      Result:=Result+Msgs[i].AsInfo+#13#10
end;

// AsJSON
//
function TdwsMessageList.AsJSON : String;
var
   wr : TdwsJSONWriter;
begin
   wr := TdwsJSONWriter.Create(nil);
   try
      WriteJSONValue(wr);
      Result := wr.ToString;
   finally
      wr.Free;
   end;
end;

// WriteJSONValue
//
procedure TdwsMessageList.WriteJSONValue(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginArray;
   for i := 0 to Count-1 do begin
      writer.BeginObject;
      Msgs[i].WriteToJSON(writer);
      writer.EndObject;
   end;
   writer.EndArray;
end;

// ------------------
// ------------------ TdwsMessage ------------------
// ------------------

// Create
//
constructor TdwsMessage.Create(aMessageList : TdwsMessageList; const Text: String);
begin
   FMsgs:=aMessageList;
   FText:=Text;
   aMessageList.AddMessage(Self);
end;

// WriteToJSON
//
procedure TdwsMessage.WriteToJSON(writer : TdwsJSONWriter);
begin
   writer.WriteString('text', Text);
end;

// IsError
//
function TdwsMessage.IsError : Boolean;
begin
   Result:=False;
end;

// IsValid
//
function TdwsMessage.IsValid : Boolean;
begin
   Result:=True;
end;

// ------------------
// ------------------ TInfoMessage ------------------
// ------------------

// AsInfo
//
function TInfoMessage.AsInfo: String;
begin
   Result:=Format(MSG_Info, [Text]);
end;

// ------------------
// ------------------ TScriptMessage ------------------
// ------------------

// Create
//
constructor TScriptMessage.Create(Msgs: TdwsMessageList; const Text: String; const P: TScriptPos);
begin
   inherited Create(Msgs, Text);
   ScriptPos:=P;
end;

// Destroy
//
destructor TScriptMessage.Destroy;
begin
   inherited;
   FAutoFix.Free;
end;

// WriteToJSON
//
procedure TScriptMessage.WriteToJSON(writer : TdwsJSONWriter);
begin
   inherited WriteToJSON(writer);
   if StrEndsWith(ClassName, 'Message') then
      writer.WriteString('type', Copy(ClassName, 2, Length(ClassName)-1-Length('Message')));
   writer.WriteName('pos').BeginObject;
      writer.WriteString('file', ScriptPos.SourceName);
      writer.WriteInteger('line', ScriptPos.Line);
      writer.WriteInteger('col', ScriptPos.Col);
   writer.EndObject;
end;

// AsInfo
//
function TScriptMessage.AsInfo: String;
begin
   Result:=FText+ScriptPos.AsInfo
end;

// Pos
//
function TScriptMessage.Pos : TScriptPos;
begin
   Result:=ScriptPos;
end;

// Line
//
function TScriptMessage.Line : Integer;
begin
   Result:=ScriptPos.Line;
end;

// Col
//
function TScriptMessage.Col : Integer;
begin
   Result:=ScriptPos.Col;
end;

// SourceName
//
function TScriptMessage.SourceName : String;
begin
   Result:=ScriptPos.SourceName;
end;

// ------------------
// ------------------ THintMessage ------------------
// ------------------

// Create
//
constructor THintMessage.Create(msgs: TdwsMessageList; const text : String;
                                const p : TScriptPos; aLevel : TdwsHintsLevel);
begin
   inherited Create(msgs, text, p);
   FLevel:=aLevel;
end;

// WriteToJSON
//
procedure THintMessage.WriteToJSON(writer : TdwsJSONWriter);
const
   cHintsLevels : array [TdwsHintsLevel] of String = (
      'Disabled', 'Normal', 'Strict', 'Pedantic'
   );
begin
   inherited WriteToJSON(writer);
   writer.WriteString('hintLevel', cHintsLevels[Level]);
end;

// AsInfo
//
function THintMessage.AsInfo: String;
begin
   Result:=Format(MSG_Hint, [inherited AsInfo]);
end;

// ------------------
// ------------------ TWarningMessage ------------------
// ------------------

// AsInfo
//
function TWarningMessage.AsInfo: String;
begin
   Result:=Format(MSG_Warning, [inherited AsInfo]);
end;

// ------------------
// ------------------ TErrorMessage ------------------
// ------------------

// IsError
//
function TErrorMessage.IsError : Boolean;
begin
   Result:=True;
end;

// ------------------
// ------------------ TCompilerErrorMessage ------------------
// ------------------

// AsInfo
//
function TCompilerErrorMessage.AsInfo: String;
begin
   Result:=Format(MSG_CompileError, [inherited AsInfo]);
end;

// ------------------
// ------------------ TSyntaxErrorMessage ------------------
// ------------------

// AsInfo
//
function TSyntaxErrorMessage.AsInfo: String;
begin
   Result:=Format(MSG_SyntaxError, [inherited AsInfo]);
end;

// ------------------
// ------------------ TdwsCompileMessageList ------------------
// ------------------

// AddCompilerInfo
//
function TdwsCompileMessageList.AddCompilerInfo(const Text: String) : TInfoMessage;
begin
   Result:=TInfoMessage.Create(Self, Text);
end;

// AddCompilerHint
//
function TdwsCompileMessageList.AddCompilerHint(const aScriptPos: TScriptPos;
      const Text: String; const aLevel : TdwsHintsLevel = hlNormal) : TScriptMessage;
begin
   if aLevel<=HintsLevel then
      Result:=THintMessage.Create(Self, Text, aScriptPos, aLevel)
   else Result:=nil;
end;

// AddCompilerHintFmt
//
function TdwsCompileMessageList.AddCompilerHintFmt(const aScriptPos: TScriptPos;
               const textFormat : String; const args: array of const;
               const aLevel : TdwsHintsLevel = hlNormal) : TScriptMessage;
begin
   Result:=AddCompilerHint(aScriptPos, Format(textFormat, args), aLevel);
end;

// AddCompilerWarning
//
function TdwsCompileMessageList.AddCompilerWarning(const aScriptPos: TScriptPos;
      const Text: String) : TScriptMessage;
begin
   if not WarningsDisabled then
      Result:=TWarningMessage.Create(Self, Text, aScriptPos)
   else Result:=nil;
end;

// AddCompilerWarningFmt
//
function TdwsCompileMessageList.AddCompilerWarningFmt(const aScriptPos: TScriptPos;
      const textFormat : String; const args: array of const) : TScriptMessage;
begin
   Result:=AddCompilerWarning(aScriptPos, Format(textFormat, args));
end;

// AddCompilerError
//
function TdwsCompileMessageList.AddCompilerError(const aScriptPos: TScriptPos;
      const Text: String; messageClass : TScriptMessageClass) : TScriptMessage;
begin
   Result:=messageClass.Create(Self, Text, aScriptPos);
end;

// AddCompilerError
//
function TdwsCompileMessageList.AddCompilerError(const aScriptPos: TScriptPos;
      const Text: String) : TScriptMessage;
begin
   Result:=AddCompilerError(aScriptPos, Text, TSyntaxErrorMessage);
end;

// AddCompilerErrorFmt
//
function TdwsCompileMessageList.AddCompilerErrorFmt(const aScriptPos: TScriptPos;
      const textFormat: String; const args: array of const;
      messageClass : TScriptMessageClass) : TScriptMessage;
begin
   Result:=AddCompilerError(aScriptPos, Format(textFormat, args), messageClass);
end;

// AddCompilerException
//
function TdwsCompileMessageList.AddCompilerException(const aScriptPos: TScriptPos; e : Exception) : TScriptMessage;
begin
   Result:=AddCompilerError(aScriptPos, E.Message);
end;

// AddCompilerErrorFmt
//
function TdwsCompileMessageList.AddCompilerErrorFmt(const aScriptPos: TScriptPos;
      const textFormat: String; const args: array of const) : TScriptMessage;
begin
   Result:=AddCompilerErrorFmt(aScriptPos, textFormat, args, TSyntaxErrorMessage);
end;

// AddCompilerStop
//
procedure TdwsCompileMessageList.AddCompilerStop(const aScriptPos: TScriptPos;
      const Text: String; messageClass : TScriptMessageClass);
begin
   AddCompilerError(aScriptPos, Text, messageClass);
   State:=mlsStopped;
   raise ECompileError.Create(Text);
end;

// AddCompilerStop
//
procedure TdwsCompileMessageList.AddCompilerStop(const aScriptPos: TScriptPos;
                                                 const Text: String);
begin
   AddCompilerStop(aScriptPos, Text, TSyntaxErrorMessage);
end;

// AddCompilerStopFmt
//
procedure TdwsCompileMessageList.AddCompilerStopFmt(const aScriptPos: TScriptPos;
      const textFormat : String; const args: array of const;
      messageClass : TScriptMessageClass);
begin
   AddCompilerStop(aScriptPos, Format(textFormat, args), messageClass);
end;

// AddCompilerStopFmt
//
procedure TdwsCompileMessageList.AddCompilerStopFmt(const aScriptPos: TScriptPos;
      const textFormat : String; const args: array of const);
begin
   AddCompilerStop(aScriptPos, Format(textFormat, args), TSyntaxErrorMessage);
end;

// ------------------
// ------------------ TdwsAutoFixAction ------------------
// ------------------

// Create
//
constructor TdwsAutoFixAction.Create(msg : TScriptMessage; const caption : String);
begin
   inherited Create;
   FCaption:=caption;
   FMsg:=msg;
   FNextAction:=msg.AutoFix;
   msg.AutoFix:=Self;
end;

// Destroy
//
destructor TdwsAutoFixAction.Destroy;
begin
   FNextAction.Free;
   inherited;
end;

// Detach
//
function TdwsAutoFixAction.Detach : TdwsAutoFixAction;
begin
   FMsg.AutoFix:=nil;
   FMsg:=nil;
   Result:=Self;
end;

// ------------------
// ------------------ TdwsAFAReplace ------------------
// ------------------

// Apply
//
function TdwsAFAReplace.Apply(const buffer : IAutoFixSourceBuffer) : String;
var
   check : String;
begin
   check:=buffer.CodeAt(Msg.ScriptPos.Col, Msg.ScriptPos.Line, Length(OldText));
   if check=OldText then begin
      buffer.ReplaceAt(Msg.ScriptPos.Col, Msg.ScriptPos.Line, Length(OldText), NewText);
      Result:='';
   end else Result:=AFA_NoLongerApplicable;
end;

// ------------------
// ------------------ TdwsAFAAddImplementation ------------------
// ------------------

// Apply
//
function TdwsAFAAddImplementation.Apply(const buffer : IAutoFixSourceBuffer) : String;
var
   i : Integer;
   sl : TStringList;
begin
   sl:=TStringList.Create;
   try
      sl.Text:=Text;
      for i:=0 to sl.Count-1 do
         buffer.InsertLine(buffer.Count, sl[i]);
   finally
      sl.Free;
   end;
   buffer.MoveCursorToMark;
end;

end.
