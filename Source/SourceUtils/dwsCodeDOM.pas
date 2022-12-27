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
   dwsUtils, dwsTokenizer, dwsTokenTypes, dwsScriptSource;

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

   TdwsCodeDOMContext = class
      private
         FToken : TdwsCodeDOMToken;
         FHeadToken : TdwsCodeDOMToken;
         FTailToken : TdwsCodeDOMToken;
         FDebug : TStrings;

         procedure AppendToken(newToken : TdwsCodeDOMToken);

      public
         destructor Destroy; override;

         procedure DoBeforeTokenizerAction(Sender : TTokenizer; action : TConvertAction);

         procedure Clear;

         procedure Debug(const msgFmt : String; const args : array of const);

         property Token : TdwsCodeDOMToken read FToken write FToken;
         function TokenType : TTokenType; inline;

         function TokenBeginPos : TScriptPos; inline;

         function Next : Boolean; inline;
         function Prev : Boolean; inline;

         property HeadToken : TdwsCodeDOMToken read FHeadToken;
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
         procedure WriteNewLine;
         procedure WriteSemi;
         procedure SkipSpace;

         property Indent : String read FIndent write FIndent;
         property IndentDepth : Integer read FIndentDepth;

         procedure IncIndent;
         procedure DecIndent;

         procedure IncIndentNewLine;
         procedure DecIndentNewLine;

         function ToString : String; override;
   end;

   TdwsCodeDOMNodeClass = class of TdwsCodeDOMNode;
   TdwsCodeDOMSnippet = class;

   TdwsCodeDOMNode = class (TRefCountedObject)
      private
         FParent : TdwsCodeDOMNode;
         FChildren : TObjectList<TdwsCodeDOMNode>;

      protected
         function GetChild(idx : Integer) : TdwsCodeDOMNode;

         procedure WriteChildren(output : TdwsCodeDOMOutput; var i : Integer);
         procedure WriteChildrenUntilToken(output : TdwsCodeDOMOutput; var i : Integer; token : TTokenType);
         procedure WritePropertiesToOutline(wobs : TWriteOnlyBlockStream; indent : Integer); virtual;
         procedure Prepare; virtual;

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
         function ChildCount : Integer;
         function IndexOfChild(node : TdwsCodeDOMNode) : Integer;
         function Extract(node : TdwsCodeDOMNode) : Integer; overload;
         function Extract(index : Integer) : TdwsCodeDOMNode; overload;
         function ExtractTokenType(index : Integer) : TTokenType;
         procedure ExtractAndEnsureTokenType(index : Integer; const allowedTypes : TTokenTypes);

         procedure AddChildrenFrom(node : TdwsCodeDOMNode);

         function AddChild(node : TdwsCodeDOMNode) : Integer;
         function AddChildSnippet(context : TdwsCodeDOMContext) : TdwsCodeDOMSnippet;
         procedure AddChildSnippetsUntil(context : TdwsCodeDOMContext; tokens : TTokenTypes);
   end;

   TdwsCodeDOMSnippet = class (TdwsCodeDOMNode)
      private
         FSnippet : String;
         FTokenType : TTokenType;
         FNewLine : Boolean;

      protected
         procedure WritePropertiesToOutline(wobs : TWriteOnlyBlockStream; indent : Integer); override;

      public
         constructor ParseFromContext(context : TdwsCodeDOMContext);

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

// Destroy
//
destructor TdwsCodeDOMContext.Destroy;
begin
   inherited;
   Clear;
   FDebug.Free;
end;

// Clear
//
procedure TdwsCodeDOMContext.Clear;
begin
   if FDebug <> nil then
      FDebug.Clear;
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

// Debug
//
procedure TdwsCodeDOMContext.Debug(const msgFmt : String; const args: array of const);
begin
   if FDebug = nil then
      FDebug := TStringList.Create;
   if Token <> nil then begin
      FDebug.Add(
           Format(msgFmt, args) + ' '
         + IntToStr(Token.BeginPos.Line) + ',' + IntToStr(Token.BeginPos.Col)
         + ' (' + Token.RawString + ')'
      );
   end else begin
      FDebug.Add(
           Format(msgFmt, args) + ' EOS'
      );
   end;
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

   case action of
      caNone : token.FTokenType := ttNone;
      caClear : begin
         token.FTokenType := ttCOMMENT;
         case tokenEndPosPtr^ of
            #13, #10 : Inc(tokenEndPosPtr);
         end;
      end;
      caString, caChar, caCharHex, caMultiLineString : token.FTokenType := ttStrVal;
      caBin, caHex, caInteger : token.FTokenType := ttIntVal;
      caFloat : token.FTokenType := ttFloatVal;
   else
      token.FTokenType := Sender.RawTokenBufferNameToType;
   end;

   SetString(
      token.FRawString, tok.FPosPtr,
      (IntPtr(tokenEndPosPtr) - IntPtr(tok.FPosPtr)) div SizeOf(Char)
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
      #0..#9, #11..' ', '(', ')', '[', ']' : ;
   else
      case s[1] of
         ',', ';', '.', '(', ')', '[', ']' : ;
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
procedure TdwsCodeDOMOutput.WriteNewLine;
begin
   if FTailChar <> #10 then begin
      FStream.WriteChar(#10);
      FTailChar := #10;
   end;
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

// IncIndent
//
procedure TdwsCodeDOMOutput.IncIndent;
begin
   Inc(FIndentDepth);
   FIndentBuf := FIndentBuf + FIndent;
end;

// DecIndent
//
procedure TdwsCodeDOMOutput.DecIndent;
begin
   Assert(FIndentDepth > 0);
   Dec(FIndentDepth);
   SetLength(FIndentBuf, Length(FIndentBuf) - Length(FIndent));
end;

// IncIndentNewLine
//
procedure TdwsCodeDOMOutput.IncIndentNewLine;
begin
   IncIndent;
   WriteNewLine;
end;

// DecIndentNewLine
//
procedure TdwsCodeDOMOutput.DecIndentNewLine;
begin
   DecIndent;
   WriteNewLine;
end;

// ToString
//
function TdwsCodeDOMOutput.ToString : String;
begin
   WriteNewLine;
   Result := FStream.ToString;
end;

// ------------------
// ------------------ TdwsCodeDOMNode ------------------
// ------------------

// Create
//
constructor TdwsCodeDOMNode.Create;
begin
   inherited;
   FChildren := TObjectList<TdwsCodeDOMNode>.Create;
end;

// Destroy
//
destructor TdwsCodeDOMNode.Destroy;
begin
   inherited;
   FChildren.Free;
end;

// WriteToOutput
//
procedure TdwsCodeDOMNode.WriteToOutput(output : TdwsCodeDOMOutput);
begin
   for var i := 0 to FChildren.Count-1 do
      FChildren[i].WriteToOutput(output);
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

// GetChild
//
function TdwsCodeDOMNode.GetChild(idx : Integer) : TdwsCodeDOMNode;
begin
   Result := FChildren[idx];
end;

// WriteChildren
//
procedure TdwsCodeDOMNode.WriteChildren(output : TdwsCodeDOMOutput; var i : Integer);
begin
   while i < ChildCount do begin
      var c := Child[i];
      c.WriteToOutput(output);
      Inc(i);
   end;
end;

// WriteChildrenUntilToken
//
procedure TdwsCodeDOMNode.WriteChildrenUntilToken(output : TdwsCodeDOMOutput; var i : Integer; token : TTokenType);
begin
   while i < ChildCount do begin
      var c := Child[i];
      if c.InheritsFrom(TdwsCodeDOMSnippet) and (TdwsCodeDOMSnippet(c).TokenType = token) then Break;
      c.WriteToOutput(output);
      Inc(i);
   end;
end;

// ChildCount
//
function TdwsCodeDOMNode.ChildCount : Integer;
begin
   Result := FChildren.Count;
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
   Result := FChildren.Extract(index);
   Result.FParent := nil;
end;

// ExtractTokenType
//
function TdwsCodeDOMNode.ExtractTokenType(index : Integer) : TTokenType;
begin
   var node := Extract(index);
   if node is TdwsCodeDOMSnippet then begin
      Result := TdwsCodeDOMSnippet(node).TokenType;
      node.Free;
   end else begin
      node.Free;
      Result := ttNone;
      Assert(False);
   end;
end;

// ExtractAndEnsureTokenType
//
procedure TdwsCodeDOMNode.ExtractAndEnsureTokenType(index : Integer; const allowedTypes : TTokenTypes);
begin
   var tt := ExtractTokenType(index);
   Assert(tt in allowedTypes);
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
   node.FChildren.ExtractAll;
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
constructor TdwsCodeDOMSnippet.ParseFromContext(context : TdwsCodeDOMContext);
begin
   Create;
   FSnippet := context.Token.RawString;
   var line := context.Token.EndPos.Line;
   if StrEndsWith(FSnippet, #$0D) or StrEndsWith(FSnippet, #$0A) then begin
      FSnippet := TrimRight(FSnippet);
      FNewLine := True;
   end;
   FTokenType := context.TokenType;
   if context.Next then
      if (not NewLine) and (context.TokenType = ttCOMMENT) and (context.Token.BeginPos.Line = line) then
         AddChild(TdwsCodeDOMSnippet.ParseFromContext(context))
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
