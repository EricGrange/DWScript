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
   Classes, SysUtils, dwsStrings, dwsUtils, dwsXPlatform;

type

   TdwsMessage = class;
   TScriptMessage = class;
   TdwsMessageList = class;

   // TSourceFile
   //
   TSourceFile = class (TRefCountedObject)
      private
         FLineCount : Integer;
         FCode : String;

      {$ifdef FPC}
      protected
         procedure SetCode(const sourceCode : String);
      {$endif}

      public
         Name : String;
         property Code : String read FCode write {$ifdef FPC}SetCode{$else}FCode{$endif};
         function LineCount : Integer;
   end;

   // TScriptPos
   //
   PScriptPos = ^TScriptPos;
   TScriptPos = packed record
      private
         FLine, FCol : Integer;

      public
         SourceFile : TSourceFile;

         const cLineMask = $FFFFF;

         class function Create(aSourceFile : TSourceFile; aLine, aCol : Integer) : TScriptPos; static;

         property Line : Integer read FLine write FLine;
         property Col : Integer read FCol write FCol;

         function SamePosAs(const aPos : TScriptPos) : Boolean;
         function IsMainModule : Boolean;
         function IsSourceFile(const name : String) : Boolean;
         function Defined : Boolean;

         procedure IncCol; inline;
         procedure NewLine; inline;
         procedure SetColLine(aCol, aLine : Integer); inline;
         procedure SetLineCol(const aPos : TScriptPos); inline;

         function IsBeforeOrEqual(const aPos : TScriptPos) : Boolean;

         function AsInfo : String;
   end;
   TScriptPosArray = array of TScriptPos; // dynamic array that can hold ScriptPos settings (needed for ReadNameList)

   // TdwsMessage
   //
   TdwsMessage = class abstract (TRefCountedObject)
      private
         FMsgs : TdwsMessageList;
         FText : String;

      public
         constructor Create(Msgs: TdwsMessageList; const Text: String);

         function AsInfo : String; virtual; abstract;
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
         FPos : TScriptPos;
         FAutoFix : TdwsAutoFixAction;

      public
         constructor Create(msgs: TdwsMessageList; const text : String; const p : TScriptPos); overload;
         destructor Destroy; override;

         function AsInfo : String; override;

         property Pos : TScriptPos read FPos write FPos;
         property AutoFix : TdwsAutoFixAction read FAutoFix write FAutoFix;
   end;

   TScriptMessageClass = class of TScriptMessage;

   THintMessage = class(TScriptMessage)
      function AsInfo: String; override;
   end;

   TWarningMessage = class(TScriptMessage)
      function AsInfo: String; override;
   end;

   TErrorMessage = class(TScriptMessage)
   end;

   TCompilerErrorMessage = class(TErrorMessage)
      function AsInfo: String; override;
   end;

   TSyntaxErrorMessage = class(TErrorMessage)
      function AsInfo: String; override;
   end;

   // TdwsMessageList
   //
   TdwsMessageList = class
      private
         FMessageList : TTightList;
         FSourceFiles : TTightList;
         FHasErrors : Boolean;

      protected
         function GetMsg(Index: Integer): TdwsMessage;
         function GetMsgCount: Integer;

      public
         destructor Destroy; override;

         procedure AddInfo(const Text: String);
         function LastMessagePos : TScriptPos;

         procedure AddMsg(aMessage : TdwsMessage); virtual;
         procedure AddMsgs(src : TdwsMessageList; lineOffset, colOffset : Integer);
         procedure Clear;

         function AsInfo: String;

         property Msgs[index : Integer] : TdwsMessage read GetMsg; default;
         property Count : Integer read GetMsgCount;
         property HasErrors : Boolean read FHasErrors write FHasErrors;
   end;

   TdwsHintsLevel = (hlDisabled, hlNormal, hlStrict, hlPedantic);

   // TdwsCompileMessageList
   //
   TdwsCompileMessageList = class (TdwsMessageList)
      private
         FHintsLevel : TdwsHintsLevel;
         FWarningsDisabled : Boolean;

      public
         function AddCompilerInfo(const Text: String) : TInfoMessage;

         function AddCompilerHint(const Pos: TScriptPos; const Text : String;
                                   const aLevel : TdwsHintsLevel = hlNormal) : TScriptMessage; overload;
         function AddCompilerHintFmt(const Pos: TScriptPos; const textFormat : String;
                                      const args : array of const;
                                      const aLevel : TdwsHintsLevel = hlNormal) : TScriptMessage; overload;

         function AddCompilerWarning(const Pos: TScriptPos; const Text: String) : TScriptMessage;
         function AddCompilerWarningFmt(const Pos: TScriptPos; const textFormat : String;
                                        const args: array of const) : TScriptMessage;

         function AddCompilerError(const Pos: TScriptPos; const Text: String;
                                   messageClass : TScriptMessageClass) : TScriptMessage; overload;
         function AddCompilerError(const Pos: TScriptPos; const Text: String) : TScriptMessage; overload;
         function AddCompilerErrorFmt(const Pos: TScriptPos; const textFormat : String;
                                      const args: array of const; messageClass : TScriptMessageClass) : TScriptMessage; overload;
         function AddCompilerErrorFmt(const Pos: TScriptPos; const textFormat : String;
                                      const args: array of const) : TScriptMessage; overload;

         procedure AddCompilerStop(const Pos: TScriptPos; const Text: String;
                                   messageClass : TScriptMessageClass); overload;
         procedure AddCompilerStop(const Pos: TScriptPos; const Text: String); overload;
         procedure AddCompilerStopFmt(const Pos: TScriptPos; const textFormat : String;
                                      const args: array of const; messageClass : TScriptMessageClass); overload;
         procedure AddCompilerStopFmt(const Pos: TScriptPos; const textFormat : String;
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
         constructor CreatePosFmt(const pos : TScriptPos; const Msg: String; const Args: array of const);
         constructor CreateFromException(const pos : TScriptPos; e : Exception);

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

const
   cNullPos: TScriptPos = (FLine: 0; FCol: 0; SourceFile: nil);
   cFakePos: TScriptPos = (FLine: 0; FCol: 0; SourceFile: nil);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TScriptPos ------------------
// ------------------

// Create
//
class function TScriptPos.Create(aSourceFile : TSourceFile; aLine, aCol : Integer) : TScriptPos;
begin
   Result.SourceFile:=aSourceFile;
   Result.FLine:=aLine;
   Result.FCol:=aCol;
end;

// SamePosAs
//
function TScriptPos.SamePosAs(const aPos : TScriptPos) : Boolean;
begin
   Result:=    (FLine=aPos.Line) and (Col=aPos.Col)
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
function TScriptPos.IsSourceFile(const name : String) : Boolean;
begin
   Result:=(SourceFile<>nil) and (SourceFile.Name=name);
end;

// Defined
//
function TScriptPos.Defined : Boolean;
begin
   Result:=(SourceFile<>nil) and ((FLine or FCol)<>0);
end;

// IncCol
//
procedure TScriptPos.IncCol;
begin
   Inc(FCol);
end;

// NewLine
//
procedure TScriptPos.NewLine;
begin
   Inc(FLine);
   FCol:=1;
end;

// SetColLine
//
procedure TScriptPos.SetColLine(aCol, aLine : Integer);
begin
   FCol:=aCol;
   FLine:=aLine;
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
function TScriptPos.AsInfo : String;
begin
   if SourceFile=nil then
      Result:=''
   else begin
      if not IsMainModule then
         Result:=Format(MSG_ScriptPosFile, [SourceFile.Name])
      else Result:='';
      if Col<>cNullPos.Col then begin
         if Result<>'' then
            Result:=', '+Result;
         Result:=Format(MSG_ScriptPosColumn, [Col])+Result;
      end;
      if Line<>cNullPos.Line then begin
         if Result<>'' then
            Result:=', '+Result;
         Result:=Format(MSG_ScriptPosLine, [Line])+Result;
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
constructor ECompileException.CreatePosFmt(const pos : TScriptPos; const Msg: String; const Args: array of const);
begin
   inherited CreateFmt(msg, args);
   FScriptPos:=pos;
end;

// CreateFromException
//
constructor ECompileException.CreateFromException(const pos : TScriptPos; e : Exception);
begin
   inherited Create(e.Message);
   FScriptPos:=pos;
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
   FHasErrors:=False;
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

// AddMsg
//
procedure TdwsMessageList.AddMsg(aMessage: TdwsMessage);
begin
   FMessageList.Add(aMessage);
end;

// AddMsgs
//
procedure TdwsMessageList.AddMsgs(src : TdwsMessageList; lineOffset, colOffset : Integer);
var
   i, col : Integer;
   msg : TdwsMessage;
   srcMsg : TScriptMessage;
   sf : TSourceFile;
begin
   for i:=0 to src.Count-1 do begin
      msg:=src.Msgs[i];
      if msg is TScriptMessage then begin
         srcMsg:=TScriptMessage(msg);
         sf:=TSourceFile.Create;
         sf.Name:=srcMsg.Pos.SourceFile.Name;
         sf.Code:=srcMsg.Pos.SourceFile.Code;
         FSourceFiles.Add(sf);
         srcMsg.FPos.SourceFile:=sf;
         if srcMsg.Pos.Line=1 then
            col:=srcMsg.Pos.Col+colOffset
         else col:=srcMsg.Pos.Col;
         srcMsg.Pos.SetColLine(col, srcMsg.Pos.Line+lineOffset);
      end;
      AddMsg(msg);
   end;
   src.FMessageList.Clear;
   src.FSourceFiles.Clear;
end;

// AddInfo
//
procedure TdwsMessageList.AddInfo(const Text: String);
begin
   AddMsg(TInfoMessage.Create(Self, Text));
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
      Result:=TScriptMessage(lastMsg).Pos
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

// ------------------
// ------------------ TdwsMessage ------------------
// ------------------

// Create
//
constructor TdwsMessage.Create(Msgs: TdwsMessageList; const Text: String);
begin
   FMsgs:=Msgs;
   FText:=Text;
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
   Pos:=P;
end;

// Destroy
//
destructor TScriptMessage.Destroy;
begin
   inherited;
   FAutoFix.Free;
end;

// AsInfo
//
function TScriptMessage.AsInfo: String;
begin
   Result:=FText+Pos.AsInfo
end;

// ------------------
// ------------------ THintMessage ------------------
// ------------------

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
   AddMsg(Result);
end;

// AddCompilerHint
//
function TdwsCompileMessageList.AddCompilerHint(const Pos: TScriptPos;
      const Text: String; const aLevel : TdwsHintsLevel = hlNormal) : TScriptMessage;
begin
   if aLevel<=HintsLevel then begin
      Result:=THintMessage.Create(Self, Text, Pos);
      AddMsg(Result);
   end else Result:=nil;
end;

// AddCompilerHintFmt
//
function TdwsCompileMessageList.AddCompilerHintFmt(const Pos: TScriptPos;
               const textFormat : String; const args: array of const;
               const aLevel : TdwsHintsLevel = hlNormal) : TScriptMessage;
begin
   Result:=AddCompilerHint(Pos, Format(textFormat, args), aLevel);
end;

// AddCompilerWarning
//
function TdwsCompileMessageList.AddCompilerWarning(const Pos: TScriptPos;
      const Text: String) : TScriptMessage;
begin
   if not WarningsDisabled then begin
      Result:=TWarningMessage.Create(Self, Text, Pos);
      AddMsg(Result);
   end else Result:=nil;
end;

// AddCompilerWarningFmt
//
function TdwsCompileMessageList.AddCompilerWarningFmt(const Pos: TScriptPos;
      const textFormat : String; const args: array of const) : TScriptMessage;
begin
   Result:=AddCompilerWarning(Pos, Format(textFormat, args));
end;

// AddCompilerError
//
function TdwsCompileMessageList.AddCompilerError(const Pos: TScriptPos;
      const Text: String; messageClass : TScriptMessageClass) : TScriptMessage;
begin
   Result:=messageClass.Create(Self, Text, Pos);
   AddMsg(Result);
   FHasErrors:=True;
end;

// AddCompilerError
//
function TdwsCompileMessageList.AddCompilerError(const Pos: TScriptPos;
      const Text: String) : TScriptMessage;
begin
   Result:=AddCompilerError(Pos, Text, TSyntaxErrorMessage);
end;

// AddCompilerErrorFmt
//
function TdwsCompileMessageList.AddCompilerErrorFmt(const Pos: TScriptPos;
      const textFormat: String; const args: array of const;
      messageClass : TScriptMessageClass) : TScriptMessage;
begin
   Result:=AddCompilerError(Pos, Format(textFormat, args), messageClass);
end;

// AddCompilerErrorFmt
//
function TdwsCompileMessageList.AddCompilerErrorFmt(const Pos: TScriptPos;
      const textFormat: String; const args: array of const) : TScriptMessage;
begin
   Result:=AddCompilerErrorFmt(Pos, textFormat, args, TSyntaxErrorMessage);
end;

// AddCompilerStop
//
procedure TdwsCompileMessageList.AddCompilerStop(const Pos: TScriptPos;
      const Text: String; messageClass : TScriptMessageClass);
begin
   AddCompilerError(Pos, Text, messageClass);
   raise ECompileError.Create(Text);
end;

// AddCompilerStop
//
procedure TdwsCompileMessageList.AddCompilerStop(const Pos: TScriptPos;
                                                 const Text: String);
begin
   AddCompilerStop(Pos, Text, TSyntaxErrorMessage);
end;

// AddCompilerStopFmt
//
procedure TdwsCompileMessageList.AddCompilerStopFmt(const Pos: TScriptPos;
      const textFormat : String; const args: array of const;
      messageClass : TScriptMessageClass);
begin
   AddCompilerStop(Pos, Format(textFormat, args), messageClass);
end;

// AddCompilerStopFmt
//
procedure TdwsCompileMessageList.AddCompilerStopFmt(const Pos: TScriptPos;
      const textFormat : String; const args: array of const);
begin
   AddCompilerStop(Pos, Format(textFormat, args), TSyntaxErrorMessage);
end;

// ------------------
// ------------------ TSourceFile ------------------
// ------------------

{$ifdef FPC}
// SetCode
//
procedure TSourceFile.SetCode(const sourceCode: String);
begin
   if Length(sourceCode)>3 then begin
      if (Ord(sourceCode[1])=$EF) and (Ord(sourceCode[2])=$BB) and (Ord(sourceCode[3])=$BF) then begin
         // UTF-8
         FCode:=Copy(sourceCode, 4, MaxInt);
      end else if (Ord(sourceCode[1])=$FE) and (Ord(sourceCode[2])=$FF) then begin
         // UTF-16 BE
         FCode:=UTF8Encode(Copy(sourceCode, 3, MaxInt));
      end else if (Ord(sourceCode[1])=$FF) and (Ord(sourceCode[2])=$FE) then begin
         // UTF-16 LE
         // TODO: revert bytes...
         FCode:=UTF8Encode(Copy(sourceCode, 3, MaxInt));
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
   check:=buffer.CodeAt(Msg.Pos.Col, Msg.Pos.Line, Length(OldText));
   if check=OldText then begin
      buffer.ReplaceAt(Msg.Pos.Col, Msg.Pos.Line, Length(OldText), NewText);
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
