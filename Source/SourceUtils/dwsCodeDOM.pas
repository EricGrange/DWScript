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
unit dwsCodeDOM;

{$I ../dws.inc}

interface

uses
   Classes, SysUtils, Windows,
   dwsUtils, dwsTokenizer, dwsTokenTypes, dwsScriptSource, dwsErrors;

type

   TdwsCodeDOMToken = class
      private
         FBeginPos : TScriptPos;
         FEndPos : TScriptPos;
         FTokenType : TTokenType;
         FRawString : String;
         FPrev : TdwsCodeDOMToken;
         FNext : TdwsCodeDOMToken;

      public
         property BeginPos : TScriptPos read FBeginPos write FBeginPos;
         property EndPos : TScriptPos read FEndPos write FEndPos;
         property TokenType : TTokenType read FTokenType write FTokenType;
         property RawString : String read FRawString write FRawString;

         property Prev : TdwsCodeDOMToken read FPrev;
         property Next : TdwsCodeDOMToken read FNext;
   end;

   TdwsCodeDOMNode = class;
   TdwsCodeDOMSnippet = class;

   TdwsCodeDOMContext = class
      private
         FToken : TdwsCodeDOMToken;
         FHeadToken : TdwsCodeDOMToken;
         FTailToken : TdwsCodeDOMToken;
         FMessages : TdwsCompileMessageList;
         FSnippetPool : TdwsCodeDOMNode;

         procedure AppendToken(newToken : TdwsCodeDOMToken);

         procedure ClearSnippetPool;
         function AllocateSnippet : TdwsCodeDOMSnippet;
         procedure ReleaseSnippet(aSnippet : TdwsCodeDOMSnippet);

      public
         constructor Create(aMessages : TdwsCompileMessageList);
         destructor Destroy; override;

         procedure DoBeforeTokenizerAction(Sender : TTokenizer; action : TConvertAction);

         procedure Release(var token : TdwsCodeDOMNode);

         procedure Clear;

         property Messages : TdwsCompileMessageList read FMessages;

         property Token : TdwsCodeDOMToken read FToken write FToken;
         function TokenType : TTokenType; inline;

         function TokenBeginPos : TScriptPos; inline;
         function TokenBeginPosLine : Integer;
         function TokenEndPosLine : Integer;
         function PreviousTokenEndPosLine : Integer;

         function Next : Boolean; inline;
         function Prev : Boolean; inline;

         property HeadToken : TdwsCodeDOMToken read FHeadToken;
   end;

   TdwsCodeDOMNodeClass = class of TdwsCodeDOMNode;
   TdwsCodeDOMOutput = class;

   TdwsCodeDOMNode = class (TRefCountedObject)
      private
         FParent : TdwsCodeDOMNode;
         FChildren : TTightList;

      protected
         function GetChild(idx : Integer) : TdwsCodeDOMNode; inline;

         procedure WritePropertiesToOutline(wobs : TWriteOnlyBlockStream; indent : Integer); virtual;
         procedure Prepare; virtual;

         procedure ReleaseSelf(context : TdwsCodeDOMContext); virtual;
         procedure ReleaseChildren(context : TdwsCodeDOMContext);

      public
         constructor Create;
         destructor Destroy; override;

         procedure WriteToOutput(output : TdwsCodeDOMOutput); virtual;
         procedure WriteToOutline(wobs : TWriteOnlyBlockStream; indent : Integer);

         property Parent : TdwsCodeDOMNode read FParent;
         function ParentOfClass(nodeClass : TdwsCodeDOMNodeClass) : TdwsCodeDOMNode;
         function HasParentOfClass(nodeClass : TdwsCodeDOMNodeClass) : Boolean;
         function StatementsSection : Boolean; virtual;

         property Child[idx : Integer] : TdwsCodeDOMNode read GetChild;
         function ChildCount : Integer; inline;
         function IndexOfChild(node : TdwsCodeDOMNode) : Integer;
         function Extract(node : TdwsCodeDOMNode) : Integer; overload;
         function Extract(index : Integer) : TdwsCodeDOMNode; overload;

         function ChildIsOfClass(index : Integer; nodeClass : TdwsCodeDOMNodeClass) : Boolean;
         function ChildIsTokenType(index : Integer; tokenType : TTokenType) : Boolean;

         procedure AddChildrenFrom(node : TdwsCodeDOMNode);

         function AddChild(node : TdwsCodeDOMNode) : Integer;
         function AddChildSnippet(context : TdwsCodeDOMContext) : TdwsCodeDOMSnippet;
         procedure AddChildSnippetsUntil(context : TdwsCodeDOMContext; tokens : TTokenTypes);
   end;

   TdwsCodeDOMOutput = class
      private
         FStream : TWriteOnlyBlockStream;
         FIndent : String;
         FIndentBuf : String;
         FIndentDepth : Integer;
         FTailChar : Char;

      protected

      public
         constructor Create;
         destructor Destroy; override;

         procedure WriteString(const s : String);
         procedure WriteTokenString(tt : TTokenType);
         function WriteNewLine : TdwsCodeDOMOutput;
         procedure WriteSemi;
         procedure SkipSpace;

         function WriteChild(node : TdwsCodeDOMNode; var i : Integer) : TdwsCodeDOMOutput;
         function WriteChildren(node : TdwsCodeDOMNode; var i : Integer) : TdwsCodeDOMOutput;
         function WriteChildrenBeforeToken(node : TdwsCodeDOMNode; var i : Integer; token : TTokenType) : TdwsCodeDOMOutput;
         function WriteChildrenBeforeTokens(node : TdwsCodeDOMNode; var i : Integer; const tokens : TTokenTypes) : TdwsCodeDOMOutput;
         function WriteChildrenUntilToken(node : TdwsCodeDOMNode; var i : Integer; token : TTokenType) : TdwsCodeDOMOutput;

         property Indent : String read FIndent write FIndent;
         property IndentDepth : Integer read FIndentDepth;

         function IncIndent : TdwsCodeDOMOutput;
         function DecIndent : TdwsCodeDOMOutput;

         function IncIndentNewLine : TdwsCodeDOMOutput;
         function DecIndentNewLine : TdwsCodeDOMOutput;

         function ToString : String; override;
   end;

   TdwsCodeDOMSnippet = class (TdwsCodeDOMNode)
      private
         FSnippet : String;
         FTokenType : TTokenType;
         FNewLine : Boolean;

      protected
         procedure WritePropertiesToOutline(wobs : TWriteOnlyBlockStream; indent : Integer); override;

         procedure ReleaseSelf(context : TdwsCodeDOMContext); override;

      public
         class function ParseFromContext(context : TdwsCodeDOMContext) : TdwsCodeDOMSnippet;

         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;

         property Snippet : String read FSnippet write FSnippet;
         property TokenType : TTokenType read FTokenType write FTokenType;
         property NewLine : Boolean read FNewLine write FNewLine;
   end;

   TdwsCodeDOM = class (TRefCountedObject)
      private
         FSource : TSourceFile;
         FRoot : TdwsCodeDOMNode;
         FPrepared : Boolean;

      protected

      public
         destructor Destroy; override;

         procedure Parse(const source : TSourceFile);
         procedure Prepare;

         property Source : TSourceFile read FSource write FSource;
         property Root : TdwsCodeDOMNode read FRoot write FRoot;
         property Prepared : Boolean read FPrepared;

   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsCodeDOMNodes;

// ------------------
// ------------------ TdwsCodeDOMContext ------------------
// ------------------

// Create
//
constructor TdwsCodeDOMContext.Create(aMessages : TdwsCompileMessageList);
begin
   inherited Create;
   Assert(aMessages <> nil);
   FMessages := aMessages;
end;

// Destroy
//
destructor TdwsCodeDOMContext.Destroy;
begin
   inherited;
   Clear;
   ClearSnippetPool;
end;

// ClearSnippetPool
//
procedure TdwsCodeDOMContext.ClearSnippetPool;
begin
   while FSnippetPool <> nil do begin
      var snip := FSnippetPool;
      FSnippetPool := snip.FParent;
      snip.FParent := nil;
      snip.Free;
   end;
end;

// AllocateSnippet
//
function TdwsCodeDOMContext.AllocateSnippet : TdwsCodeDOMSnippet;
begin
   if FSnippetPool = nil then
      Result := TdwsCodeDOMSnippet.Create
   else begin
      Result := TdwsCodeDOMSnippet(FSnippetPool);
      FSnippetPool := Result.FParent;
      Result.FParent := nil;
   end;
end;

// ReleaseSnippet
//
procedure TdwsCodeDOMContext.ReleaseSnippet(aSnippet : TdwsCodeDOMSnippet);
begin
   Assert(aSnippet.FChildren.Count = 0);
   aSnippet.FParent := FSnippetPool;
   aSnippet.FTokenType := ttNone;
   aSnippet.FNewLine := False;
   aSnippet.FSnippet := '';
   FSnippetPool := aSnippet;
end;

// Clear
//
procedure TdwsCodeDOMContext.Clear;
begin
   var p := FHeadToken;
   while p <> nil do begin
      var pNext := p.Next;
      p.Free;
      p := pNext;
   end;
   FHeadToken := nil;
   FTailToken := nil;
   FToken := nil;
end;

// DoBeforeTokenizerAction
//
procedure TdwsCodeDOMContext.DoBeforeTokenizerAction(Sender : TTokenizer; action : TConvertAction);
begin
   var token := TdwsCodeDOMToken.Create;
   var tok := Sender.GetToken;

//   if (FTailToken <> nil) and (FTailToken.EndPos.Line < tok.FScriptPos.Line) then begin
//      token.FRawString := #10;
//      AppendToken(token);
//      token := TdwsCodeDOMToken.Create;
//   end;

   token.FBeginPos := tok.FScriptPos;
   token.FEndPos := Sender.CurrentPos;
   token.FRawString := tok.AsString;

   var tokenEndPosPtr := Sender.PosPtr;
   var tokPosPtr := tok.FPosPtr;

   case action of
      caNone : token.FTokenType := ttNone;
      caClear : begin
         token.FTokenType := ttCOMMENT;
         case tokenEndPosPtr^ of
            #13, #10 : Inc(tokenEndPosPtr);
         end;
      end;
      caSwitch : begin
         token.FTokenType := ttSWITCH;
         Dec(tokPosPtr, 2);
         case tokenEndPosPtr^ of
            #13, #10 : Inc(tokenEndPosPtr);
         end;
      end;
      caString, caChar, caCharHex, caMultiLineString : token.FTokenType := ttStrVal;
      caBin, caHex, caInteger : token.FTokenType := ttIntVal;
      caFloat : token.FTokenType := ttFloatVal;
      caDotDot : begin
         token.FTokenType := ttDOTDOT;
         if     (FTailToken <> nil) and (FTailToken.TokenType = ttIntVal)
            and StrEndsWith(FTailToken.FRawString, '.') then begin
            SetLength(FTailToken.FRawString, Length(FTailToken.FRawString)-1);
            FTailToken.FEndPos.DecCol;
            tokPosPtr := tokenEndPosPtr;
            token.FBeginPos.IncCol;
            Dec(tokPosPtr, 2);
         end;
      end;
      caNameEscaped : begin
         token.FTokenType := ttNAME;
         token.FBeginPos.DecCol;
         Dec(tokPosPtr);
      end;
   else
      token.FTokenType := Sender.RawTokenBufferNameToType;
   end;

   SetString(
      token.FRawString, tokPosPtr,
      (IntPtr(tokenEndPosPtr) - IntPtr(tokPosPtr)) div SizeOf(Char)
   );

   AppendToken(token);
end;

// Release
//
procedure TdwsCodeDOMContext.Release(var token : TdwsCodeDOMNode);
begin
   if token = nil then Exit;
   token.ReleaseChildren(Self);
   token.ReleaseSelf(Self);
   token := nil;
end;

// AppendToken
//
procedure TdwsCodeDOMContext.AppendToken(newToken : TdwsCodeDOMToken);
begin
   newToken.FPrev := FTailToken;
   if FTailToken <> nil then
      FTailToken.FNext := newToken
   else begin
      FHeadToken := newToken;
      FTailToken := newToken;
   end;
   FTailToken := newToken;
end;

// TokenType
//
function TdwsCodeDOMContext.TokenType : TTokenType;
begin
   if FToken <> nil then
      Result := FToken.TokenType
   else Result := ttNone;
end;

// TokenBeginPos
//
function TdwsCodeDOMContext.TokenBeginPos : TScriptPos;
begin
   if FToken <> nil then
      Result := FToken.BeginPos
   else Result := cNullPos;
end;

// TokenBeginPosLine
//
function TdwsCodeDOMContext.TokenBeginPosLine : Integer;
begin
   if FToken <> nil then
      Result := FToken.BeginPos.Line
   else Result := 0;
end;

// TokenEndPosLine
//
function TdwsCodeDOMContext.TokenEndPosLine : Integer;
begin
   if FToken <> nil then
      Result := FToken.EndPos.Line
   else Result := 0;
end;

// PreviousTokenEndPosLine
//
function TdwsCodeDOMContext.PreviousTokenEndPosLine : Integer;
begin
   if (FToken <> nil) and (FToken.FPrev <> nil) then
      Result := FToken.FPrev.EndPos.Line
   else Result := 0;
end;

// Next
//
function TdwsCodeDOMContext.Next : Boolean;
begin
   if FToken <> nil then
      FToken := FToken.Next;
   Result := FToken <> nil;
end;

// Prev
//
function TdwsCodeDOMContext.Prev : Boolean;
begin
   if FToken <> nil then
      FToken := FToken.Prev;
   Result := FToken <> nil;
end;

// ------------------
// ------------------ TdwsCodeDOMNode ------------------
// ------------------

// Create
//
constructor TdwsCodeDOMNode.Create;
begin
   inherited;
end;

// Destroy
//
destructor TdwsCodeDOMNode.Destroy;
begin
   inherited;
   FChildren.Clean;
end;

// GetChild
//
function TdwsCodeDOMNode.GetChild(idx : Integer) : TdwsCodeDOMNode;
begin
   Result := TdwsCodeDOMNode(FChildren.List[idx]);
end;

// ChildCount
//
function TdwsCodeDOMNode.ChildCount : Integer;
begin
   Result := FChildren.Count;
end;

// WriteToOutput
//
procedure TdwsCodeDOMNode.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   for var i := 0 to ChildCount-1 do
      Child[i].WriteToOutput(output);
end;

// WriteToOutline
//
procedure TdwsCodeDOMNode.WriteToOutline(wobs : TWriteOnlyBlockStream; indent : Integer);
begin
   WritePropertiesToOutline(wobs, indent);
   for var i := 0 to ChildCount-1 do
      Child[i].WriteToOutline(wobs, indent + 3);
end;

// WritePropertiesToOutline
//
procedure TdwsCodeDOMNode.WritePropertiesToOutline(wobs : TWriteOnlyBlockStream; indent : Integer);
begin
   wobs.WriteIndent(indent);
   if StrBeginsWith(ClassName, 'TdwsCodeDOM') then
      wobs.WriteSubString(ClassName, 12)
   else wobs.WriteString( ClassName);
   wobs.WriteCRLF;
end;

// Prepare
//
procedure TdwsCodeDOMNode.Prepare;
begin
   for var i := 0 to ChildCount-1 do
      Child[i].Prepare;
end;

// ReleaseSelf
//
procedure TdwsCodeDOMNode.ReleaseSelf(context : TdwsCodeDOMContext);
begin
   Destroy;
end;

// ParentOfClass
//
function TdwsCodeDOMNode.ParentOfClass(nodeClass : TdwsCodeDOMNodeClass) : TdwsCodeDOMNode;
begin
   Result := Self.Parent;
   while (Result <> nil) and not Result.InheritsFrom(nodeClass) do
      Result := Result.Parent;
end;

// HasParentOfClass
//
function TdwsCodeDOMNode.HasParentOfClass(nodeClass : TdwsCodeDOMNodeClass) : Boolean;
begin
   Result := (ParentOfClass(nodeClass) <> nil);
end;

// StatementsSection
//
function TdwsCodeDOMNode.StatementsSection : Boolean;
begin
   Result := True;
end;

// ReleaseChildren
//
procedure TdwsCodeDOMNode.ReleaseChildren(context : TdwsCodeDOMContext);
begin
   for var i := 0 to ChildCount-1 do begin
      var c := Child[i];
      context.Release(c);
   end;
   FChildren.Clear;
end;

// IndexOfChild
//
function TdwsCodeDOMNode.IndexOfChild(node : TdwsCodeDOMNode) : Integer;
begin
   Result := FChildren.IndexOf(node);
end;

// Extract
//
function TdwsCodeDOMNode.Extract(node : TdwsCodeDOMNode) : Integer;
begin
   Result := FChildren.IndexOf(node);
   Assert(Result >= 0);
   Extract(Result);
end;

// Extract
//
function TdwsCodeDOMNode.Extract(index : Integer) : TdwsCodeDOMNode;
begin
   Result := Child[index];
   FChildren.Delete(index);
   Result.FParent := nil;
end;

// ChildIsOfClass
//
function TdwsCodeDOMNode.ChildIsOfClass(index : Integer; nodeClass : TdwsCodeDOMNodeClass) : Boolean;
begin
   Result := (index < ChildCount) and Child[index].InheritsFrom(nodeClass);
end;

// ChildIsTokenType
//
function TdwsCodeDOMNode.ChildIsTokenType(index : Integer; tokenType : TTokenType) : Boolean;
begin
   if index < ChildCount then begin
      var c := Child[index];
      Result :=     c.InheritsFrom(TdwsCodeDOMSnippet)
                and (TdwsCodeDOMSnippet(c).TokenType = tokenType);
   end else Result := False;
end;

// AddChild
//
function TdwsCodeDOMNode.AddChild(node : TdwsCodeDOMNode) : Integer;
begin
   Assert(node.Parent = nil);
   node.FParent := Self;
   Result := FChildren.Add(node);
end;

// AddChildrenFrom
//
procedure TdwsCodeDOMNode.AddChildrenFrom(node : TdwsCodeDOMNode);
begin
   for var i := 0 to node.ChildCount-1 do begin
      var child := node.Child[i];
      child.FParent := nil;
      AddChild(child);
   end;
   node.FChildren.Clear;
end;

// AddChildSnippet
//
function TdwsCodeDOMNode.AddChildSnippet(context : TdwsCodeDOMContext) : TdwsCodeDOMSnippet;
begin
   Result := TdwsCodeDOMSnippet.ParseFromContext(context);
   AddChild(Result);
end;

// AddChildSnippetsUntil
//
procedure TdwsCodeDOMNode.AddChildSnippetsUntil(context : TdwsCodeDOMContext; tokens : TTokenTypes);
begin
   Include(tokens, ttNone);
   while not (context.TokenType in tokens) do
      AddChildSnippet(context);
end;

// ------------------
// ------------------ TdwsCodeDOMOutput ------------------
// ------------------

// Create
//
constructor TdwsCodeDOMOutput.Create;
begin
   inherited;
   FStream := TWriteOnlyBlockStream.AllocFromPool;
   FTailChar := #0;
end;

// Destroy
//
destructor TdwsCodeDOMOutput.Destroy;
begin
   inherited;
   FStream.ReturnToPool;
end;

// WriteString
//
procedure TdwsCodeDOMOutput.WriteString(const s : String);
begin
   if s = '' then Exit;
   case FTailChar of
      #10 : FStream.WriteString(FIndentBuf);
      #0..#9, #11..' ', '(', '[', '$', '{', '}', '#', '.' : ;
   else
      case s[1] of
         ',', ';', '.', '(', ')', '[', ']', '{', '}' : ;
      else
         FStream.WriteString(' ');
      end;
   end;
   FStream.WriteString(s);
   FTailChar := s[Length(s)];
end;

// WriteTokenString
//
procedure TdwsCodeDOMOutput.WriteTokenString(tt : TTokenType);
begin
   WriteString(cTokenStrings[tt]);
end;

// WriteNewLine
//
function TdwsCodeDOMOutput.WriteNewLine : TdwsCodeDOMOutput;
begin
   if FTailChar <> #10 then begin
      FStream.WriteChar(#10);
      FTailChar := #10;
   end;
   Result := Self;
end;

// WriteSemi
//
procedure TdwsCodeDOMOutput.WriteSemi;
begin
   WriteString(';');
end;

// SkipSpace
//
procedure TdwsCodeDOMOutput.SkipSpace;
begin
   FTailChar := #0;
end;

// WriteChild
//
function TdwsCodeDOMOutput.WriteChild(node : TdwsCodeDOMNode; var i : Integer) : TdwsCodeDOMOutput;
begin
   if i < node.ChildCount then begin
      node.Child[i].WriteToOutput(Self);
      Inc(i);
   end;
   Result := Self;
end;

// WriteChildren
//
function TdwsCodeDOMOutput.WriteChildren(node : TdwsCodeDOMNode; var i : Integer) : TdwsCodeDOMOutput;
begin
   while i < node.ChildCount do begin
      node.Child[i].WriteToOutput(Self);
      Inc(i);
   end;
   Result := Self;
end;

// WriteChildrenBeforeToken
//
function TdwsCodeDOMOutput.WriteChildrenBeforeToken(node : TdwsCodeDOMNode; var i : Integer; token : TTokenType) : TdwsCodeDOMOutput;
begin
   while i < node.ChildCount do begin
      var c := node.Child[i];
      if c.InheritsFrom(TdwsCodeDOMSnippet) and (TdwsCodeDOMSnippet(c).TokenType = token) then Break;
      c.WriteToOutput(Self);
      Inc(i);
   end;
   Result := Self;
end;

// WriteChildrenBeforeTokens
//
function TdwsCodeDOMOutput.WriteChildrenBeforeTokens(node : TdwsCodeDOMNode; var i : Integer; const tokens : TTokenTypes) : TdwsCodeDOMOutput;
begin
   while i < node.ChildCount do begin
      var c := node.Child[i];
      if c.InheritsFrom(TdwsCodeDOMSnippet) and (TdwsCodeDOMSnippet(c).TokenType in tokens) then Break;
      c.WriteToOutput(Self);
      Inc(i);
   end;
   Result := Self;
end;

// WriteChildrenUntilToken
//
function TdwsCodeDOMOutput.WriteChildrenUntilToken(node : TdwsCodeDOMNode; var i : Integer; token : TTokenType) : TdwsCodeDOMOutput;
begin
   WriteChildrenBeforeToken(node, i, token);
   Result := WriteChild(node, i);
end;

// IncIndent
//
function TdwsCodeDOMOutput.IncIndent : TdwsCodeDOMOutput;
begin
   Inc(FIndentDepth);
   FIndentBuf := FIndentBuf + FIndent;
   Result := Self;
end;

// DecIndent
//
function TdwsCodeDOMOutput.DecIndent : TdwsCodeDOMOutput;
begin
   Assert(FIndentDepth > 0);
   Dec(FIndentDepth);
   SetLength(FIndentBuf, Length(FIndentBuf) - Length(FIndent));
   Result := Self;
end;

// IncIndentNewLine
//
function TdwsCodeDOMOutput.IncIndentNewLine : TdwsCodeDOMOutput;
begin
   IncIndent;
   Result := WriteNewLine;
end;

// DecIndentNewLine
//
function TdwsCodeDOMOutput.DecIndentNewLine : TdwsCodeDOMOutput;
begin
   DecIndent;
   Result := WriteNewLine;
end;

// ToString
//
function TdwsCodeDOMOutput.ToString : String;
begin
   WriteNewLine;
   Result := FStream.ToString;
end;

// ------------------
// ------------------ TdwsCodeDOM ------------------
// ------------------

// Destroy
//
destructor TdwsCodeDOM.Destroy;
begin
   inherited;
   FRoot.Free;
end;

// Parse
//
procedure TdwsCodeDOM.Parse(const source : TSourceFile);
begin
   FreeAndNil(FRoot);

//   var context := TdwsCodeDOMContext.Create;
//   try
//      FTokenizer.BeginSourceFile(source);
//      FTokenizer.OnBeforeAction := context.DoBeforeTokenizerAction;
//      try
//         while FTokenizer.HasTokens do
//            FTokenizer.KillToken;
//      finally
//         FTokenizer.OnBeforeAction := nil;
//         FTokenizer.EndSourceFile;
//      end;
//
//      context.FToken := context.FHeadToken;
//
//      FRoot := TdwsCodeDOMMain.Create;
//      FRoot.DoParse(context);
//   finally
//      context.Free;
//   end;
end;

// Prepare
//
procedure TdwsCodeDOM.Prepare;
begin
   Assert(not Prepared);
   FPrepared := True;
   Root.Prepare;
end;

// ------------------
// ------------------ TdwsCodeDOMSnippet ------------------
// ------------------

// ParseFromContext
//
class function TdwsCodeDOMSnippet.ParseFromContext(context : TdwsCodeDOMContext) : TdwsCodeDOMSnippet;
begin
   Result := context.AllocateSnippet;
   Result.FSnippet := context.Token.RawString;
   var line := context.Token.EndPos.Line;
   if StrEndsWith(Result.FSnippet, #$0D) or StrEndsWith(Result.FSnippet, #$0A) then begin
      Result.FSnippet := TrimRight(Result.FSnippet);
      Result.FNewLine := True;
   end;
   Result.FTokenType := context.TokenType;
   if context.Next and (not Result.NewLine) then begin
      // special cases of newline preservation (or not)
      if (context.Token.BeginPos.Line > line) then
         Result.FNewLine := True;
   end;
end;

// ReleaseSelf
//
procedure TdwsCodeDOMSnippet.ReleaseSelf(context : TdwsCodeDOMContext);
begin
   context.ReleaseSnippet(Self);
end;

// WriteToOutput
//
procedure TdwsCodeDOMSnippet.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   case TokenType of
      ttIntVal, ttFloatVal, ttNAME, ttSWITCH, ttCOMMENT :
         output.WriteString(Snippet);
      ttStrVal :
         output.WriteString(Snippet);
      ttSEMI : begin
         output.WriteTokenString(TokenType);
         inherited;
         output.WriteNewLine;
         Exit;
      end;
   else
      output.WriteTokenString(TokenType);
   end;
   inherited;
   if NewLine then
      output.WriteNewLine;
end;

// WritePropertiesToOutline
//
procedure TdwsCodeDOMSnippet.WritePropertiesToOutline(wobs : TWriteOnlyBlockStream; indent : Integer);
begin
   wobs.WriteIndent(indent);
   wobs.WriteString('Token ');
   wobs.WriteString(cTokenStrings[TokenType]);
   if Snippet <> cTokenStrings[TokenType] then begin
      wobs.WriteString(' <<');
      wobs.WriteString(Snippet);
      wobs.WriteString('>>');
   end;
   if NewLine then
      wobs.WriteString(' [LF]');
   wobs.WriteCRLF;
end;

end.
