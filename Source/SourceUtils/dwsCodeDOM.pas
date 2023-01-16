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
         procedure Clear;

         property BeginPos : TScriptPos read FBeginPos write FBeginPos;
         property EndPos : TScriptPos read FEndPos write FEndPos;
         property TokenType : TTokenType read FTokenType write FTokenType;
         property RawString : String read FRawString write FRawString;

         property Prev : TdwsCodeDOMToken read FPrev;
         property Next : TdwsCodeDOMToken read FNext;
   end;

   TdwsCodeDOMTokenPool = class
      private
         FPool : TdwsCodeDOMToken;

      public
         destructor Destroy; override;

         procedure Clear;
         function Allocate : TdwsCodeDOMToken;
         procedure Release(aSnippet : TdwsCodeDOMToken);
   end;


   TdwsCodeDOMNode = class;
   TdwsCodeDOMSnippet = class;

   TdwsCodeDOMSnippetPool = class
      private
         FPool : TdwsCodeDOMNode;

      public
         destructor Destroy; override;

         procedure Clear;
         function Allocate : TdwsCodeDOMSnippet;
         procedure Release(aSnippet : TdwsCodeDOMSnippet);
   end;

   TdwsCodeDOMContext = class
      private
         FToken : TdwsCodeDOMToken;
         FHeadToken : TdwsCodeDOMToken;
         FTailToken : TdwsCodeDOMToken;
         FMessages : TdwsCompileMessageList;
         FSnippetPool : TdwsCodeDOMSnippetPool;
         FTokenPool : TdwsCodeDOMTokenPool;

         procedure AppendToken(newToken : TdwsCodeDOMToken);

      public
         constructor Create(
            aMessages : TdwsCompileMessageList;
            snippetPool : TdwsCodeDOMSnippetPool;
            tokenPool : TdwsCodeDOMTokenPool
         );
         destructor Destroy; override;

         procedure DoBeforeTokenizerAction(Sender : TTokenizer; action : TConvertAction);

         procedure Clear;

         property Messages : TdwsCompileMessageList read FMessages;

         property SnippetPool : TdwsCodeDOMSnippetPool read FSnippetPool;
         function AllocateSnippet : TdwsCodeDOMSnippet; inline;
         procedure ReleaseSnippet(var aSnippet : TdwsCodeDOMSnippet); inline;

         property TokenPool : TdwsCodeDOMTokenPool read FTokenPool;
         function AllocateToken : TdwsCodeDOMToken; inline;
         procedure ReleaseToken(var token : TdwsCodeDOMToken); inline;

         procedure ReleaseNode(var node : TdwsCodeDOMNode); inline;

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
         procedure InsertChild(index : Integer; node : TdwsCodeDOMNode);
   end;

   TdwsCodeDOMOutputFlag = (
      ofSkipNewLine,
      ofSkipExtraLineAfterNextNewLine,
      ofLastWasExtraLine
   );
   TdwsCodeDOMOutputFlags = set of TdwsCodeDOMOutputFlag;

   TdwsCodeDOMOutputState = record
      Position : Integer;
      Line, Col : Integer;
      ColMax : Integer;
      IndentDepth : Integer;
      TailChar : Char;
      Flags : TdwsCodeDOMOutputFlags;
   end;

   TdwsCodeDOMOutput = class
      private
         FOutputBuffer : array of Char;
         FState : TdwsCodeDOMOutputState;
         FMaxDesiredColumn : Integer;
         FMaxToleranceColumn : Integer;
         FIndentBuf : String;
         FIndentChar : Char;
         FTabSize : Integer;
         FIndentSize : Integer;

      protected
         procedure Write(const buf : String);

      public
         constructor Create;

         procedure WriteString(const s : String);
         function WritePreLine : TdwsCodeDOMOutput;
         function WriteNewLine : TdwsCodeDOMOutput;

         function SkipSpace : TdwsCodeDOMOutput;
         function SkipNewLine : TdwsCodeDOMOutput;
         function DiscardSkipNewLine : TdwsCodeDOMOutput;

         procedure SkipExtraLineAfterNextNewLine;

         function WriteChild(node : TdwsCodeDOMNode; var i : Integer) : TdwsCodeDOMOutput;
         function WriteChildren(node : TdwsCodeDOMNode; var i : Integer) : TdwsCodeDOMOutput;
         function WriteChildrenBeforeToken(node : TdwsCodeDOMNode; var i : Integer; token : TTokenType) : TdwsCodeDOMOutput;
         function WriteChildrenBeforeTokens(node : TdwsCodeDOMNode; var i : Integer; const tokens : TTokenTypes) : TdwsCodeDOMOutput;
         function WriteChildrenUntilToken(node : TdwsCodeDOMNode; var i : Integer; token : TTokenType) : TdwsCodeDOMOutput;

         property Position : Integer read FState.Position;
         property Line : Integer read FState.Line;
         property Col : Integer read FState.Col;
         property ColMax : Integer read FState.ColMax;
         property IndentDepth : Integer read FState.IndentDepth;

         function SaveState : TdwsCodeDOMOutputState;
         procedure RestoreState(const aState : TdwsCodeDOMOutputState);

         property IndentChar : Char read FIndentChar write FIndentChar;
         property IndentSize : Integer read FIndentSize write FIndentSize;
         property TabSize : Integer read FTabSize write FTabSize;

         property MaxDesiredColumn : Integer read FMaxDesiredColumn write FMaxDesiredColumn;
         property MaxToleranceColumn : Integer read FMaxToleranceColumn write FMaxToleranceColumn;

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
         FPreLine : Boolean;
         FNewLine : Boolean;

      protected
         procedure WritePropertiesToOutline(wobs : TWriteOnlyBlockStream; indent : Integer); override;

         procedure ReleaseSelf(context : TdwsCodeDOMContext); override;

      public
         class function ParseFromContext(context : TdwsCodeDOMContext) : TdwsCodeDOMSnippet;

         procedure WriteToOutput(output : TdwsCodeDOMOutput); override;

         property Snippet : String read FSnippet write FSnippet;
         property TokenType : TTokenType read FTokenType write FTokenType;
         property PreLine : Boolean read FPreLine write FPreLine;
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
// ------------------ TdwsCodeDOMToken ------------------
// ------------------

// Clear
//
procedure TdwsCodeDOMToken.Clear;
begin
   FBeginPos.Clear;
   FEndPos.Clear;
   FTokenType := ttNone;
   FRawString := '';
   FPrev := nil;
   FNext := nil;
end;

// ------------------
// ------------------ TdwsCodeDOMTokenPool ------------------
// ------------------

// Destroy
//
destructor TdwsCodeDOMTokenPool.Destroy;
begin
   Clear;
   inherited;
end;

// Clear
//
procedure TdwsCodeDOMTokenPool.Clear;
begin
   while FPool <> nil do
      Allocate.Free;
end;

// Allocate
//
function TdwsCodeDOMTokenPool.Allocate : TdwsCodeDOMToken;
begin
   if FPool <> nil then begin
      Result := FPool;
      FPool := Result.Next;
      Result.FNext := nil;
   end else Result := TdwsCodeDOMToken.Create;
end;

// Release
//
procedure TdwsCodeDOMTokenPool.Release(aSnippet : TdwsCodeDOMToken);
begin
   aSnippet.Clear;
   aSnippet.FNext := FPool;
   FPool := aSnippet;
end;

// ------------------
// ------------------ TdwsCodeDOMSnippetPool ------------------
// ------------------

// Destroy
//
destructor TdwsCodeDOMSnippetPool.Destroy;
begin
   inherited;
   Clear;
end;

// Clear
//
procedure TdwsCodeDOMSnippetPool.Clear;
begin
   while FPool <> nil do begin
      var snip := FPool;
      FPool := snip.FParent;
      snip.FParent := nil;
      snip.Free;
   end;
end;

// Allocate
//
function TdwsCodeDOMSnippetPool.Allocate : TdwsCodeDOMSnippet;
begin
   if FPool = nil then
      Result := TdwsCodeDOMSnippet.Create
   else begin
      Result := TdwsCodeDOMSnippet(FPool);
      FPool := Result.FParent;
      Result.FParent := nil;
   end;
end;

// Release
//
procedure TdwsCodeDOMSnippetPool.Release(aSnippet : TdwsCodeDOMSnippet);
begin
   Assert(aSnippet.FChildren.Count = 0);
   aSnippet.FParent := FPool;
   aSnippet.FTokenType := ttNone;
   aSnippet.FNewLine := False;
   aSnippet.FPreLine := False;
   aSnippet.FSnippet := '';
   FPool := aSnippet;
end;

// ------------------
// ------------------ TdwsCodeDOMContext ------------------
// ------------------

// Create
//
constructor TdwsCodeDOMContext.Create(
   aMessages : TdwsCompileMessageList;
   snippetPool : TdwsCodeDOMSnippetPool;
   tokenPool : TdwsCodeDOMTokenPool
);
begin
   inherited Create;
   Assert(aMessages <> nil);
   FMessages := aMessages;
   FSnippetPool := snippetPool;
   FTokenPool := tokenPool;
end;

// Destroy
//
destructor TdwsCodeDOMContext.Destroy;
begin
   inherited;
   Clear;
end;

// Clear
//
procedure TdwsCodeDOMContext.Clear;
begin
   var p := FHeadToken;
   while p <> nil do begin
      var pNext := p.Next;
      TokenPool.Release(p);
      p := pNext;
   end;
   FHeadToken := nil;
   FTailToken := nil;
   FToken := nil;
end;

// AllocateSnippet
//
function TdwsCodeDOMContext.AllocateSnippet : TdwsCodeDOMSnippet;
begin
   Result := FSnippetPool.Allocate;
end;

// ReleaseSnippet
//
procedure TdwsCodeDOMContext.ReleaseSnippet(var aSnippet : TdwsCodeDOMSnippet);
begin
   FSnippetPool.Release(aSnippet);
   aSnippet := nil;
end;

// AllocateToken
//
function TdwsCodeDOMContext.AllocateToken : TdwsCodeDOMToken;
begin
   Result := TokenPool.Allocate;
end;

// ReleaseToken
//
procedure TdwsCodeDOMContext.ReleaseToken(var token : TdwsCodeDOMToken);
begin
   TokenPool.Release(token);
   token := nil;
end;

// ReleaseNode
//
procedure TdwsCodeDOMContext.ReleaseNode(var node : TdwsCodeDOMNode);
begin
   if node = nil then Exit;
   node.ReleaseChildren(Self);
   node.ReleaseSelf(Self);
   node := nil;
end;

// DoBeforeTokenizerAction
//
procedure TdwsCodeDOMContext.DoBeforeTokenizerAction(Sender : TTokenizer; action : TConvertAction);
begin
   var token := AllocateToken;
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
      caEndOfText : begin
         if tokPosPtr <> nil then begin
            // treat unfinished tokens stuff as a comment
            token.FTokenType := ttCOMMENT;
         end else begin
            token.Free;
            Exit;
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
      context.ReleaseNode(c);
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

// InsertChild
//
procedure TdwsCodeDOMNode.InsertChild(index : Integer; node : TdwsCodeDOMNode);
begin
   Assert(node.Parent = nil);
   node.FParent := Self;
   FChildren.Insert(index, node);
end;

// ------------------
// ------------------ TdwsCodeDOMOutput ------------------
// ------------------

// Create
//
constructor TdwsCodeDOMOutput.Create;
begin
   inherited;
   SetLength(FOutputBuffer, 256);
   FState.TailChar := #0;
   FState.Line := 1;
   FState.Col := 1;
   FState.ColMax := 1;
   FMaxDesiredColumn := 80;
   FMaxToleranceColumn := 90;
   FTabSize := 3;
end;

// Write
//
procedure TdwsCodeDOMOutput.Write(const buf : String);
begin
   if buf = '' then Exit;
   var n := Length(buf);
   var capacity := Length(FOutputBuffer);
   if FState.Position + n > capacity then
      SetLength(FOutputBuffer, capacity + (capacity shr 2) + n);

   var pBuf := PChar(buf);
   System.Move(pBuf^, FOutputBuffer[FState.Position], n*SizeOf(Char));
   Inc(FState.Position, n);

   while pBuf^ <> #0 do begin
      case pBuf^ of
         #10 : begin
            Inc(FState.Line);
            if FState.Col > FState.ColMax then
               FState.ColMax := FState.Col;
            FState.Col := 1;
         end;
         #9 : Inc(FState.Col, TabSize);
      else
         Inc(FState.Col);
      end;
      Inc(pBuf);
   end;
   if FState.Col > FState.ColMax then
      FState.ColMax := FState.Col;
end;

// WriteString
//
procedure TdwsCodeDOMOutput.WriteString(const s : String);
begin
   if s = '' then Exit;
   case FState.TailChar of
      #10 : Write(FIndentBuf);
      #0..#9, #11..' ', '(', '[', '$', '{', '#', '.' : ;
   else
      case s[1] of
         ',', ';', '.', ')', ']', '}' : //, '(', ')', '[', ']', '{', '}' : ;
      else
         Write(' ');
      end;
   end;

   Write(s);
   FState.TailChar := s[Length(s)];
   Exclude(FState.Flags, ofLastWasExtraLine);
end;

// WritePreLine
//
function TdwsCodeDOMOutput.WritePreLine : TdwsCodeDOMOutput;
begin
   if not (ofLastWasExtraLine in FState.Flags) then begin
      Write(#10);
      FState.TailChar := #10;
      Include(FState.Flags, ofLastWasExtraLine);
   end;
   Result := Self;
end;

// WriteNewLine
//
function TdwsCodeDOMOutput.WriteNewLine : TdwsCodeDOMOutput;
begin
   if ofSkipNewLine in FState.Flags then
      Exclude(FState.Flags, ofSkipNewLine)
   else if FState.TailChar <> #10 then begin
      if ofSkipExtraLineAfterNextNewLine in FState.Flags then begin
         Exclude(FState.Flags, ofSkipExtraLineAfterNextNewLine);
         Write(#10#10);
         Include(FState.Flags, ofLastWasExtraLine);
      end else begin
         Write(#10);
         Exclude(FState.Flags, ofLastWasExtraLine);
      end;
      FState.TailChar := #10;
   end;
   Result := Self;
end;

// SkipSpace
//
function TdwsCodeDOMOutput.SkipSpace : TdwsCodeDOMOutput;
begin
   FState.TailChar := #0;
   Result := Self;
end;

// SkipNewLine
//
function TdwsCodeDOMOutput.SkipNewLine : TdwsCodeDOMOutput;
begin
   Include(FState.Flags, ofSkipNewLine);
   Result := Self;
end;

// DiscardSkipNewLine
//
function TdwsCodeDOMOutput.DiscardSkipNewLine : TdwsCodeDOMOutput;
begin
   Exclude(FState.Flags, ofSkipNewLine);
   Result := Self;
end;

// SkipExtraLineAfterNextNewLine
//
procedure TdwsCodeDOMOutput.SkipExtraLineAfterNextNewLine;
begin
   Include(FState.Flags, ofSkipExtraLineAfterNextNewLine);
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

// SaveState
//
function TdwsCodeDOMOutput.SaveState : TdwsCodeDOMOutputState;
begin
   Result := FState;
end;

// RestoreState
//
procedure TdwsCodeDOMOutput.RestoreState(const aState : TdwsCodeDOMOutputState);
begin
   // can only restore backwards
   Assert(Cardinal(aState.Position) <= Cardinal(FState.Position));
   FState := aState;
   FIndentBuf := StringOfChar(IndentChar, IndentDepth*IndentSize);
end;

// IncIndent
//
function TdwsCodeDOMOutput.IncIndent : TdwsCodeDOMOutput;
begin
   Inc(FState.IndentDepth);
   FIndentBuf := StringOfChar(IndentChar, IndentDepth*IndentSize);
   Result := Self;
end;

// DecIndent
//
function TdwsCodeDOMOutput.DecIndent : TdwsCodeDOMOutput;
begin
   Assert(FState.IndentDepth > 0);
   Dec(FState.IndentDepth);
   SetLength(FIndentBuf, Length(FIndentBuf) - FIndentSize);
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
   SetString(Result, PChar(FOutputBuffer), FState.Position);
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
   if (context.Token.Prev <> nil) and (context.Token.Prev.EndPos.Line < context.Token.BeginPos.Line-1) then
      Result.FPreLine := True;
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
   if PreLine then
      output.WritePreLine;
   case TokenType of
      ttIntVal, ttFloatVal, ttNAME, ttSWITCH, ttCOMMENT :
         output.WriteString(Snippet);
      ttStrVal :
         output.WriteString(Snippet);
      ttSEMI : begin
         output.WriteString(';');
         inherited;
         output.WriteNewLine;
         Exit;
      end;
   else
      output.WriteString(cTokenStrings[TokenType]);
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
   if PreLine then
      wobs.WriteString(' [PL]');
   if NewLine then
      wobs.WriteString(' [LF]');
   wobs.WriteCRLF;
end;

end.
