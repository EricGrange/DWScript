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
   Classes, SysUtils, dwsStrings, dwsUtils, dwsXPlatform, dwsJSON;

type

   TdwsMessage = class;
   TScriptMessage = class;
   TdwsMessageList = class;

   // TSourceFile
   //
   TSourceFile = class (TRefCountedObject)
      private
         FLineCount : Integer;
         FCode : UnicodeString;

      {$ifdef FPC}
      protected
         procedure SetCode(const sourceCode : UnicodeString);
      {$endif}

      public
         Name : UnicodeString;
         property Code : UnicodeString read FCode write {$ifdef FPC}SetCode{$else}FCode{$endif};
         function LineCount : Integer;
   end;

   // TScriptPos
   //
   PScriptPos = ^TScriptPos;
   TScriptPos = packed record
      public
         Line : Integer;
         Col : Integer;
         SourceFile : TSourceFile;

         const cLineMask = $FFFFF;

         class function Create(aSourceFile : TSourceFile; aLine, aCol : Integer) : TScriptPos; static;

         procedure Clear; inline;

         function SamePosAs(const aPos : TScriptPos) : Boolean;
         function IsMainModule : Boolean;
         function IsSourceFile(const name : UnicodeString) : Boolean;
         function SourceName : UnicodeString; inline;
         function SourceCode : UnicodeString; inline;
         function Defined : Boolean;

         procedure IncCol; inline;
         procedure NewLine; inline;
         procedure SetColLine(aCol, aLine : Integer); inline;
         procedure SetLineCol(const aPos : TScriptPos); inline;

         function IsBeforeOrEqual(const aPos : TScriptPos) : Boolean;

         function AsInfo : UnicodeString;
   end;
   TScriptPosArray = array of TScriptPos; // dynamic array that can hold ScriptPos settings (needed for ReadNameList)

   // TdwsMessage
   //
   TdwsMessage = class abstract (TRefCountedObject)
      private
         FMsgs : TdwsMessageList;
         FText : UnicodeString;

      protected
         property MessageList : TdwsMessageList read FMsgs;

      public
         constructor Create(aMessageList : TdwsMessageList; const Text: UnicodeString);

         procedure WriteToJSON(writer : TdwsJSONWriter); virtual;

         function AsInfo : UnicodeString; virtual; abstract;
         function IsError : Boolean; virtual;
         function IsValid : Boolean; virtual;

         property Text : UnicodeString read FText;
   end;

   TInfoMessage = class(TdwsMessage)
      function AsInfo: UnicodeString; override;
   end;

   IAutoFixSourceBuffer = interface

      function GetLines(idx : Integer) : UnicodeString;
      procedure SetLines(idx : Integer; const newLine : UnicodeString);
      function GetCount : Integer;

      property Lines[idx : Integer] : UnicodeString read GetLines write SetLines;
      property Count : Integer read GetCount;

      function CodeAt(col, line, nb : Integer) : UnicodeString;
      procedure ReplaceAt(col, line, nb : Integer; const newText : UnicodeString);
      procedure InsertLine(line : Integer; const lineText : UnicodeString);
      procedure DeleteLine(line : Integer);

      procedure MoveCursorTo(col, row : Integer);
      procedure MoveCursorToMark;

   end;

   // TdwsAutoFixAction
   //
   TdwsAutoFixAction = class abstract
      private
         FCaption : UnicodeString;
         FMsg : TScriptMessage;
         FNextAction : TdwsAutoFixAction;

      public
         constructor Create(msg : TScriptMessage; const caption : UnicodeString);
         destructor Destroy; override;

         function Apply(const buffer : IAutoFixSourceBuffer) : UnicodeString; virtual; abstract;
         function Detach : TdwsAutoFixAction; virtual;

         property Msg : TScriptMessage read FMsg write FMsg;
         property Caption : UnicodeString read FCaption;
         property NextAction : TdwsAutoFixAction read FNextAction;
   end;

   // TScriptMessage
   //
   TScriptMessage = class(TdwsMessage)
      private
         FScriptPos : TScriptPos;
         FAutoFix : TdwsAutoFixAction;

      public
         constructor Create(msgs: TdwsMessageList; const text : UnicodeString; const p : TScriptPos); overload;
         destructor Destroy; override;

         procedure WriteToJSON(writer : TdwsJSONWriter); override;
         function AsInfo : UnicodeString; override;

         function Pos : TScriptPos; deprecated 'Renamed as ScriptPos';
         function Line : Integer; inline;
         function Col : Integer; inline;
         function SourceName : UnicodeString; inline;

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
         constructor Create(msgs: TdwsMessageList; const text : UnicodeString; const p : TScriptPos;
                            aLevel : TdwsHintsLevel);

         procedure WriteToJSON(writer : TdwsJSONWriter); override;
         function AsInfo: UnicodeString; override;

         property Level : TdwsHintsLevel read FLevel;
   end;

   TWarningMessage = class(TScriptMessage)
      function AsInfo: UnicodeString; override;
   end;

   TErrorMessage = class(TScriptMessage)
      function IsError : Boolean; override;
   end;

   TCompilerErrorMessage = class(TErrorMessage)
      function AsInfo: UnicodeString; override;
   end;

   TSyntaxErrorMessage = class(TErrorMessage)
      function AsInfo: UnicodeString; override;
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

         procedure AddInfo(const Text: UnicodeString);
         function LastMessagePos : TScriptPos;

         procedure AddMessage(aMessage : TdwsMessage); virtual;
         procedure AddMsgs(src : TdwsMessageList; lineOffset, colOffset : Integer);
         procedure Clear;
         procedure RemoveInvalidDeferred;

         function AsInfo : UnicodeString;
         function AsJSON : UnicodeString;

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
         function AddCompilerInfo(const Text: UnicodeString) : TInfoMessage;

         function AddCompilerHint(const aScriptPos: TScriptPos; const Text : UnicodeString;
                                  const aLevel : TdwsHintsLevel = hlNormal) : TScriptMessage; overload;
         function AddCompilerHintFmt(const aScriptPos: TScriptPos; const textFormat : UnicodeString;
                                     const args : array of const;
                                     const aLevel : TdwsHintsLevel = hlNormal) : TScriptMessage; overload;

         function AddCompilerWarning(const aScriptPos: TScriptPos; const Text: UnicodeString) : TScriptMessage;
         function AddCompilerWarningFmt(const aScriptPos: TScriptPos; const textFormat : UnicodeString;
                                        const args: array of const) : TScriptMessage;

         function AddCompilerError(const aScriptPos: TScriptPos; const Text: UnicodeString;
                                   messageClass : TScriptMessageClass) : TScriptMessage; overload;
         function AddCompilerError(const aScriptPos: TScriptPos; const Text: UnicodeString) : TScriptMessage; overload;
         function AddCompilerErrorFmt(const aScriptPos: TScriptPos; const textFormat : UnicodeString;
                                      const args: array of const; messageClass : TScriptMessageClass) : TScriptMessage; overload;
         function AddCompilerErrorFmt(const aScriptPos: TScriptPos; const textFormat : UnicodeString;
                                      const args: array of const) : TScriptMessage; overload;

         function AddCompilerException(const aScriptPos: TScriptPos; e : Exception) : TScriptMessage;

         procedure AddCompilerStop(const aScriptPos: TScriptPos; const Text: UnicodeString;
                                   messageClass : TScriptMessageClass); overload;
         procedure AddCompilerStop(const aScriptPos: TScriptPos; const Text: UnicodeString); overload;
         procedure AddCompilerStopFmt(const aScriptPos: TScriptPos; const textFormat : UnicodeString;
                                      const args: array of const; messageClass : TScriptMessageClass); overload;
         procedure AddCompilerStopFmt(const aScriptPos: TScriptPos; const textFormat : UnicodeString;
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
         constructor CreatePosFmt(const aScriptPos: TScriptPos; const Msg: UnicodeString; const Args: array of const);
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
         FOldText  : UnicodeString;
         FNewText : UnicodeString;

      public
         function Apply(const buffer : IAutoFixSourceBuffer) : UnicodeString; override;

         property OldText : UnicodeString read FOldText write FOldText;
         property NewText : UnicodeString read FNewText write FNewText;
   end;

   // TdwsAFAAddImplementation
   //
   TdwsAFAAddImplementation = class (TdwsAutoFixAction)
      private
         FText : UnicodeString;

      public
         function Apply(const buffer : IAutoFixSourceBuffer) : UnicodeString; override;

         property Text : UnicodeString read FText write FText;
   end;

procedure ConcatScriptPosArray(var dest : TScriptPosarray; const src : TScriptPosarray; nb : Integer);

const
   cNullPos: TScriptPos = (Line: 0; Col: 0; SourceFile: nil);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ConcatScriptPosArray
//
procedure ConcatScriptPosArray(var dest : TScriptPosarray; const src : TScriptPosarray; nb : Integer);
var
   nd : Integer;
begin
   if nb=0 then Exit;
   nd:=Length(dest);
   SetLength(dest, nd+nb);
   System.Move(src[0], dest[nd], SizeOf(TScriptPos)*nb);
end;

// ------------------
// ------------------ TScriptPos ------------------
// ------------------

// Create
//
class function TScriptPos.Create(aSourceFile : TSourceFile; aLine, aCol : Integer) : TScriptPos;
begin
   Result.SourceFile:=aSourceFile;
   Result.Line:=aLine;
   Result.Col:=aCol;
end;

// Clear
//
procedure TScriptPos.Clear;
begin
   SourceFile:=nil;
   Line:=0;
   Col:=Line;
end;

// SamePosAs
//
function TScriptPos.SamePosAs(const aPos : TScriptPos) : Boolean;
begin
   Result:=    (Line=aPos.Line) and (Col=aPos.Col)
           and (SourceFile=aPos.SourceFile);
end;

// IsMainModule
//
function TScriptPos.IsMainModule : Boolean;
begin
   Result:=(SourceFile=nil) or (SourceFile.Name=MSG_MainModule);
end;

// IsSourceFile
//
function TScriptPos.IsSourceFile(const name : UnicodeString) : Boolean;
begin
   Result:=(SourceFile<>nil) and UnicodeSameText(SourceFile.Name, name);
end;

// SourceName
//
function TScriptPos.SourceName : UnicodeString;
begin
   if SourceFile<>nil then
      Result:=SourceFile.Name
   else Result:='';
end;

// SourceCode
//
function TScriptPos.SourceCode : UnicodeString;
begin
   if SourceFile<>nil then
      Result:=SourceFile.Code
   else Result:='';
end;

// Defined
//
function TScriptPos.Defined : Boolean;
begin
   Result:=(SourceFile<>nil) and ((Line or Col)<>0);
end;

// IncCol
//
procedure TScriptPos.IncCol;
begin
   Inc(Col);
end;

// NewLine
//
procedure TScriptPos.NewLine;
begin
   Inc(Line);
   Col:=1;
end;

// SetColLine
//
procedure TScriptPos.SetColLine(aCol, aLine : Integer);
begin
   Col:=aCol;
   Line:=aLine;
end;

// SetLineCol
//
procedure TScriptPos.SetLineCol(const aPos : TScriptPos);
begin
   PUInt64(@Self)^:=PUInt64(@aPos)^;
end;

// IsBeforeOrEqual
//
function TScriptPos.IsBeforeOrEqual(const aPos : TScriptPos) : Boolean;
begin
   Result:=    (SourceFile=aPos.SourceFile)
           and (   (Line<aPos.Line)
                or ((Line=aPos.Line) and (Col<=aPos.Col)));
end;

// AsInfo
//
function TScriptPos.AsInfo : UnicodeString;
begin
   if SourceFile=nil then
      Result:=''
   else begin
      if not IsMainModule then
         Result:=UnicodeFormat(MSG_ScriptPosFile, [SourceFile.Name])
      else Result:='';
      if Col<>cNullPos.Col then begin
         if Result<>'' then
            Result:=', '+Result;
         Result:=UnicodeFormat(MSG_ScriptPosColumn, [Col])+Result;
      end;
      if Line<>cNullPos.Line then begin
         if Result<>'' then
            Result:=', '+Result;
         Result:=UnicodeFormat(MSG_ScriptPosLine, [Line])+Result;
      end;
      if Result<>'' then
         Result:=' ['+Result+']';
   end;
end;

// ------------------
// ------------------ ECompileException ------------------
// ------------------

// CreatePosFmt
//
constructor ECompileException.CreatePosFmt(const aScriptPos: TScriptPos; const Msg: UnicodeString; const Args: array of const);
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
procedure TdwsMessageList.AddInfo(const Text: UnicodeString);
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
function TdwsMessageList.AsInfo: UnicodeString;
var
   i: Integer;
begin
   Result:='';
   for i:=0 to Count-1 do
      Result:=Result+Msgs[i].AsInfo+#13#10
end;

// AsJSON
//
function TdwsMessageList.AsJSON : UnicodeString;
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
constructor TdwsMessage.Create(aMessageList : TdwsMessageList; const Text: UnicodeString);
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
function TInfoMessage.AsInfo: UnicodeString;
begin
   Result:=Format(MSG_Info, [Text]);
end;

// ------------------
// ------------------ TScriptMessage ------------------
// ------------------

// Create
//
constructor TScriptMessage.Create(Msgs: TdwsMessageList; const Text: UnicodeString; const P: TScriptPos);
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
function TScriptMessage.AsInfo: UnicodeString;
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
function TScriptMessage.SourceName : UnicodeString;
begin
   Result:=ScriptPos.SourceName;
end;

// ------------------
// ------------------ THintMessage ------------------
// ------------------

// Create
//
constructor THintMessage.Create(msgs: TdwsMessageList; const text : UnicodeString;
                                const p : TScriptPos; aLevel : TdwsHintsLevel);
begin
   inherited Create(msgs, text, p);
   FLevel:=aLevel;
end;

// WriteToJSON
//
procedure THintMessage.WriteToJSON(writer : TdwsJSONWriter);
const
   cHintsLevels : array [TdwsHintsLevel] of UnicodeString = (
      'Disabled', 'Normal', 'Strict', 'Pedantic'
   );
begin
   inherited WriteToJSON(writer);
   writer.WriteString('hintLevel', cHintsLevels[Level]);
end;

// AsInfo
//
function THintMessage.AsInfo: UnicodeString;
begin
   Result:=UnicodeFormat(MSG_Hint, [inherited AsInfo]);
end;

// ------------------
// ------------------ TWarningMessage ------------------
// ------------------

// AsInfo
//
function TWarningMessage.AsInfo: UnicodeString;
begin
   Result:=UnicodeFormat(MSG_Warning, [inherited AsInfo]);
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
function TCompilerErrorMessage.AsInfo: UnicodeString;
begin
   Result:=UnicodeFormat(MSG_CompileError, [inherited AsInfo]);
end;

// ------------------
// ------------------ TSyntaxErrorMessage ------------------
// ------------------

// AsInfo
//
function TSyntaxErrorMessage.AsInfo: UnicodeString;
begin
   Result:=UnicodeFormat(MSG_SyntaxError, [inherited AsInfo]);
end;

// ------------------
// ------------------ TdwsCompileMessageList ------------------
// ------------------

// AddCompilerInfo
//
function TdwsCompileMessageList.AddCompilerInfo(const Text: UnicodeString) : TInfoMessage;
begin
   Result:=TInfoMessage.Create(Self, Text);
end;

// AddCompilerHint
//
function TdwsCompileMessageList.AddCompilerHint(const aScriptPos: TScriptPos;
      const Text: UnicodeString; const aLevel : TdwsHintsLevel = hlNormal) : TScriptMessage;
begin
   if aLevel<=HintsLevel then
      Result:=THintMessage.Create(Self, Text, aScriptPos, aLevel)
   else Result:=nil;
end;

// AddCompilerHintFmt
//
function TdwsCompileMessageList.AddCompilerHintFmt(const aScriptPos: TScriptPos;
               const textFormat : UnicodeString; const args: array of const;
               const aLevel : TdwsHintsLevel = hlNormal) : TScriptMessage;
begin
   Result:=AddCompilerHint(aScriptPos, UnicodeFormat(textFormat, args), aLevel);
end;

// AddCompilerWarning
//
function TdwsCompileMessageList.AddCompilerWarning(const aScriptPos: TScriptPos;
      const Text: UnicodeString) : TScriptMessage;
begin
   if not WarningsDisabled then
      Result:=TWarningMessage.Create(Self, Text, aScriptPos)
   else Result:=nil;
end;

// AddCompilerWarningFmt
//
function TdwsCompileMessageList.AddCompilerWarningFmt(const aScriptPos: TScriptPos;
      const textFormat : UnicodeString; const args: array of const) : TScriptMessage;
begin
   Result:=AddCompilerWarning(aScriptPos, UnicodeFormat(textFormat, args));
end;

// AddCompilerError
//
function TdwsCompileMessageList.AddCompilerError(const aScriptPos: TScriptPos;
      const Text: UnicodeString; messageClass : TScriptMessageClass) : TScriptMessage;
begin
   Result:=messageClass.Create(Self, Text, aScriptPos);
end;

// AddCompilerError
//
function TdwsCompileMessageList.AddCompilerError(const aScriptPos: TScriptPos;
      const Text: UnicodeString) : TScriptMessage;
begin
   Result:=AddCompilerError(aScriptPos, Text, TSyntaxErrorMessage);
end;

// AddCompilerErrorFmt
//
function TdwsCompileMessageList.AddCompilerErrorFmt(const aScriptPos: TScriptPos;
      const textFormat: UnicodeString; const args: array of const;
      messageClass : TScriptMessageClass) : TScriptMessage;
begin
   Result:=AddCompilerError(aScriptPos, UnicodeFormat(textFormat, args), messageClass);
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
      const textFormat: UnicodeString; const args: array of const) : TScriptMessage;
begin
   Result:=AddCompilerErrorFmt(aScriptPos, textFormat, args, TSyntaxErrorMessage);
end;

// AddCompilerStop
//
procedure TdwsCompileMessageList.AddCompilerStop(const aScriptPos: TScriptPos;
      const Text: UnicodeString; messageClass : TScriptMessageClass);
begin
   AddCompilerError(aScriptPos, Text, messageClass);
   State:=mlsStopped;
   raise ECompileError.Create(Text);
end;

// AddCompilerStop
//
procedure TdwsCompileMessageList.AddCompilerStop(const aScriptPos: TScriptPos;
                                                 const Text: UnicodeString);
begin
   AddCompilerStop(aScriptPos, Text, TSyntaxErrorMessage);
end;

// AddCompilerStopFmt
//
procedure TdwsCompileMessageList.AddCompilerStopFmt(const aScriptPos: TScriptPos;
      const textFormat : UnicodeString; const args: array of const;
      messageClass : TScriptMessageClass);
begin
   AddCompilerStop(aScriptPos, UnicodeFormat(textFormat, args), messageClass);
end;

// AddCompilerStopFmt
//
procedure TdwsCompileMessageList.AddCompilerStopFmt(const aScriptPos: TScriptPos;
      const textFormat : UnicodeString; const args: array of const);
begin
   AddCompilerStop(aScriptPos, UnicodeFormat(textFormat, args), TSyntaxErrorMessage);
end;

// ------------------
// ------------------ TSourceFile ------------------
// ------------------

{$ifdef FPC}
// SetCode
//
procedure TSourceFile.SetCode(const sourceCode: UnicodeString);
begin
   if Length(sourceCode)>3 then begin
      if (Ord(sourceCode[1])=$EF) and (Ord(sourceCode[2])=$BB) and (Ord(sourceCode[3])=$BF) then begin
         // UTF-8
         FCode:=StrDeleteLeft(sourceCode, 3);
      end else if (Ord(sourceCode[1])=$FE) and (Ord(sourceCode[2])=$FF) then begin
         // UTF-16 BE
         FCode:=UTF8Encode(StrDeleteLeft(sourceCode, 2));
      end else if (Ord(sourceCode[1])=$FF) and (Ord(sourceCode[2])=$FE) then begin
         // UTF-16 LE
         // TODO: revert bytes...
         FCode:=UTF8Encode(StrDeleteLeft(sourceCode, 2));
      end else FCode:=sourceCode;
   end else FCode:=sourceCode;
end;
{$endif}

// LineCount
//
function TSourceFile.LineCount : Integer;
var
   i : Integer;
begin
   if FLineCount=0 then begin
      FLineCount:=1;
      for i:=1 to Length(Code) do
         if Code[i]=#10 then
            Inc(FLineCount);
   end;
   Result:=FLineCount;
end;

// ------------------
// ------------------ TdwsAutoFixAction ------------------
// ------------------

// Create
//
constructor TdwsAutoFixAction.Create(msg : TScriptMessage; const caption : UnicodeString);
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
function TdwsAFAReplace.Apply(const buffer : IAutoFixSourceBuffer) : UnicodeString;
var
   check : UnicodeString;
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
function TdwsAFAAddImplementation.Apply(const buffer : IAutoFixSourceBuffer) : UnicodeString;
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
