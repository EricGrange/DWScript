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
unit dwsURLRewriter;

interface

uses SysUtils, StrUtils, dwsUtils, dwsXPlatform, dwsJSON;

const
   cMAX_REWRITTEN_URL_SIZE = 2048;

type

   TdwsURLRewriteRule = class
      private
         FPattern : String;
         FRewrite : String;
         FHead : Integer;
         FHitCount : Integer;

      protected

      public
         constructor Create(const aPattern, aRewrite : String); virtual;

         function Apply(const originURL : String; var rewrittenURL : String) : Boolean; virtual; abstract;

         property Pattern : String read FPattern;
         property Rewrite : String read FRewrite;
         property Head : Integer read FHead;
         property HitCount : Integer read FHitCount;

         procedure WriteToJSON(wr : TdwsJSONWriter; withHitCount : Boolean);
   end;

   TdwsURLRewriteRuleGeneric = class (TdwsURLRewriteRule)
      private
         FPatternChunks : array of String;
         FRewriteChunks : array of String;

      protected

      public
         constructor Create(const aPattern, aRewrite : String); override;

         function Apply(const originURL : String; var rewrittenURL : String) : Boolean; override;
   end;

   TdwsURLRewriteRuleStartMatch = class (TdwsURLRewriteRule)
      private
         FStart : String;
         FRewrite : String;
         FStartLength, FRewriteLength : Integer;
         FPassThrough : Boolean;

      protected

      public
         constructor Create(const aPattern, aRewrite : String); override;

         function Apply(const originURL : String; var rewrittenURL : String) : Boolean; override;
   end;

   TdwsURLRewriteRules = array of TdwsURLRewriteRule;

   TdwsURLRewriter = class
      private
         FRules : TdwsURLRewriteRules;
         FCount : Integer;
         FLock : TMultiReadSingleWrite;

      protected
         function GetAsJSON : String;
         procedure SetAsJSON(const js : String);

         function CreateRule(const aPattern, aRewrite : String) : TdwsURLRewriteRule;

      public
         constructor Create;
         destructor Destroy; override;

         procedure AddRule(const aPattern, aRewrite : String);
         property  Count : Integer read FCount;
         procedure Clear;

         function Apply(const originURL : String; var rewrittenURL : String) : Boolean;

         procedure WriteToJSON(wr : TdwsJSONWriter; withHitCount : Boolean);
         procedure ReadFromJSON(jv : TdwsJSONValue);
         property AsJSON : String read GetAsJSON write SetAsJSON;
   end;

   EdwsURLRewriterException = class(Exception);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// StringHead2Chars
//
function StringHead2Chars(const s : String) : Integer; inline;
begin
   if Length(s) >= 2 then
      if SizeOf(Char) = 1 then
         Result := PWord(s)^
      else Result := PInteger(s)^
   else Result := 0;
end;

// ------------------
// ------------------ TdwsURLRewriter ------------------
// ------------------

// Create
//
constructor TdwsURLRewriter.Create;
begin
   inherited;
   FLock := TMultiReadSingleWrite.Create;
end;

// Destroy
//
destructor TdwsURLRewriter.Destroy;
begin
   Clear;
   FLock.Free;
   inherited;
end;

// CreateRule
//
function TdwsURLRewriter.CreateRule(const aPattern, aRewrite : String) : TdwsURLRewriteRule;
begin
   if     (Pos('*', aPattern) = Length(aPattern))
      and (Pos('$1', aRewrite) = Length(aRewrite)-1) then begin

      Result := TdwsURLRewriteRuleStartMatch.Create(aPattern, aRewrite)

   end else begin
      Result := TdwsURLRewriteRuleGeneric.Create(aPattern, aRewrite);
   end;
end;

// AddRule
//
procedure TdwsURLRewriter.AddRule(const aPattern, aRewrite : String);
var
   rule : TdwsURLRewriteRule;
begin
   rule := CreateRule(aPattern, aRewrite);
   FLock.BeginWrite;
   try
      SetLength(FRules, FCount+1);
      FRules[FCount] := rule;
      Inc(FCount);
   finally
      FLock.EndWrite;
   end;
end;

// Clear
//
procedure TdwsURLRewriter.Clear;
var
   i : Integer;
   oldRules : TdwsURLRewriteRules;
begin
   FLock.BeginWrite;
   try
      oldRules := FRules;
      FRules := nil;
      FCount := 0;
   finally
      FLock.EndWrite;
   end;
   for i := 0 to High(oldRules) do
      oldRules[i].Free;
end;

// Apply
//
function TdwsURLRewriter.Apply(const originURL : String; var rewrittenURL : String) : Boolean;
var
   i : Integer;
   strHead : Integer;
   rule : TdwsURLRewriteRule;
begin
   if originURL = '' then Exit(False);

   strHead := StringHead2Chars(originURL);

   FLock.BeginRead;
   try
      for i := 0 to High(FRules) do begin
         rule := FRules[i];
         if (rule.Head = 0) or (rule.Head = strHead) then
            if rule.Apply(originURL, rewrittenURL) then Exit(True);
      end;
   finally
      FLock.EndRead;
   end;
   Result := False;
end;

// WriteToJSON
//
procedure TdwsURLRewriter.WriteToJSON(wr : TdwsJSONWriter; withHitCount : Boolean);
var
   i : Integer;
begin
   wr.BeginArray;
   FLock.BeginRead;
   try
      for i := 0 to High(FRules) do
         FRules[i].WriteToJSON(wr, withHitCount)
   finally
      FLock.EndRead;
   end;
   wr.EndArray;
end;

// ReadFromJSON
//
procedure TdwsURLRewriter.ReadFromJSON(jv : TdwsJSONValue);
var
   i : Integer;
   ruleJV : TdwsJSONValue;
begin
   try
      FLock.BeginWrite;
      try
         for i := 0 to High(FRules) do
            FRules[i].Free;
         FRules := nil;
         FCount := jv.ElementCount;
         SetLength(FRules, FCount);
         for i := 0 to jv.ElementCount-1 do begin
            ruleJV := jv.Elements[i];
            FRules[i] := CreateRule(ruleJV.Items['pattern'].AsString, ruleJV.Items['rewrite'].AsString);
         end;
      finally
         FLock.EndWrite;
      end;
   except
      Clear;
      raise;
   end;
end;

// GetAsJSON
//
function TdwsURLRewriter.GetAsJSON : String;
var
   wr : TdwsJSONWriter;
begin
   wr := TdwsJSONWriter.Create;
   try
      WriteToJSON(wr, True);
      Result := wr.ToString;
   finally
      wr.Free;
   end;
end;

// SetAsJSON
//
procedure TdwsURLRewriter.SetAsJSON(const js : String);
var
   jv : TdwsJSONValue;
begin
   if js = '' then
      Clear
   else begin
      jv := TdwsJSONValue.ParseString(js);
      try
         ReadFromJSON(jv);
      finally
         jv.Free;
      end;
   end;
end;

// ------------------
// ------------------ TdwsURLRewriteRule ------------------
// ------------------

// Create
//
constructor TdwsURLRewriteRule.Create(const aPattern, aRewrite : String);
begin
   inherited Create;

   if aPattern = '' then
      raise EdwsURLRewriterException.Create('Pattern cannot be empty');

   FPattern := aPattern;
   FRewrite := aRewrite;
end;

// WriteToJSON
//
procedure TdwsURLRewriteRule.WriteToJSON(wr : TdwsJSONWriter; withHitCount : Boolean);
begin
   wr.BeginObject;
      wr.WriteString('pattern', Pattern);
      wr.WriteString('rewrite', Rewrite);
      if withHitCount then
         wr.WriteInteger('hits', HitCount);
   wr.EndObject;
end;

// ------------------
// ------------------ TdwsURLRewriteRuleGeneric ------------------
// ------------------

// Create
//
constructor TdwsURLRewriteRuleGeneric.Create(const aPattern, aRewrite : String);

   procedure AddPatternChunk(startIncluded, stopExcluded : Integer);
   var
      n : Integer;
   begin
      n := Length(FPatternChunks);
      if n = 10 then
         raise EdwsURLRewriterException.Create('Too many wildcards (max 9)');
      SetLength(FPatternChunks, n+1);
      FPatternChunks[n] := Copy(aPattern, startIncluded, stopExcluded-startIncluded);
   end;

   procedure AddRewriteChunk(startIncluded, stopExcluded : Integer);
   var
      n : Integer;
   begin
      if startIncluded < stopExcluded then begin
         n := Length(FRewriteChunks);
         SetLength(FRewriteChunks, n+1);
         FRewriteChunks[n] := Copy(aRewrite, startIncluded, stopExcluded-startIncluded);
      end;
   end;

var
   i, start, k : Integer;
begin
   inherited Create(aPattern, aRewrite);

   // parse pattern for *
   i := 1;
   start := i;
   while i <= Length(aPattern) do begin
      if aPattern[i] = '*' then begin
         AddPatternChunk(start, i);
         if (i = start) and (i > 1) then
            raise EdwsURLRewriterException.Create('Pattern is ambiguous');
         start := i + 1;
      end;
      Inc(i);
   end;
   if (start <= Length(aPattern)) or (aPattern[Length(aPattern)] = '*') then
      AddPatternChunk(start, Length(aPattern)+1);

   FHead := StringHead2Chars(FPatternChunks[0]);

   // parse rewrite for $
   i := 1;
   start := i;
   while i <= Length(aRewrite) do begin
      if aRewrite[i] = '$' then begin
         if i = Length(aRewrite) then
            raise EdwsURLRewriterException.Create('Rewrite error: $ missing argument');
         k := StrToIntDef(Copy(aRewrite, i+1, 1), 10);
         if (k < 1) or (k > Length(FPatternChunks)-1)  then
            raise EdwsURLRewriterException.Create('Rewrite error: $ invalid argument');
         AddRewriteChunk(start, i);
         start := i + 2;
         AddRewriteChunk(i, start);
         i := start;
      end else Inc(i);
   end;
   if start <= Length(aRewrite) then
      AddRewriteChunk(start, Length(aRewrite)+1);
end;

// Apply
//
function TdwsURLRewriteRuleGeneric.Apply(const originURL : String; var rewrittenURL : String) : Boolean;
var
   buffer : array [0..cMAX_REWRITTEN_URL_SIZE-1] of Char;
   bufferIndex : Integer;

   procedure Append(p : PChar; sizeInChars : Integer);
   begin
      if bufferIndex + sizeInChars >= cMAX_REWRITTEN_URL_SIZE then
         raise EdwsURLRewriterException.Create('Rewritten URL too long');
      System.Move(p^, buffer[bufferIndex], sizeInChars*SizeOf(Char));
      Inc(bufferIndex, sizeInChars);
   end;

   procedure AppendString(const s : String); inline;
   begin
      if s <> '' then
         Append(Pointer(s), Length(s));
   end;

var
   i, n, k, prev : Integer;
   lenOrigin : Integer;
   snippetStart, snippetLength : array [1..9] of Integer;
   pOrigin : PChar;
begin
   n := 0;
   prev := 1;
   if FPatternChunks[0] <> '' then begin
      if not StrBeginsWith(originURL, FPatternChunks[0]) then Exit(False);
      prev := Length(FPatternChunks[0]) + 1;
   end;

   lenOrigin := Length(originURL);
   for i := 1 to High(FPatternChunks) do begin
      if FPatternChunks[i] = '' then begin
         Assert(n < 9);
         Inc(n);
         snippetStart[n] := prev;
         snippetLength[n] := lenOrigin-prev+1;
         prev := lenOrigin+1;
         Break;
      end else begin
         k := PosEx(FPatternChunks[i], originURL, prev);
         if k <= 0 then Exit(False);
         Assert(n < 9);
         Inc(n);
         snippetStart[n] := prev;
         snippetLength[n] := k-prev;
         prev := k + Length(FPatternChunks[i]);
      end;
   end;
   if prev <= lenOrigin then Exit(False);

   bufferIndex := 0;
   pOrigin := Pointer(originURL);
   for i := 0 to High(FRewriteChunks) do begin
      if FRewriteChunks[i][1] = '$' then begin
         k := Ord(FRewriteChunks[i][2]) - Ord('0');
         Assert(k in [1..9]);
         if snippetLength[k] > 0 then begin
            Append(@pOrigin[snippetStart[k]-1], snippetLength[k]);
         end;
      end else begin
         AppendString(FRewriteChunks[i]);
      end;
   end;
   SetString(rewrittenURL, PChar(@buffer[0]), bufferIndex);
   Inc(FHitCount);
   Result := True;
end;

// ------------------
// ------------------ TdwsURLRewriteRuleStartMatch ------------------
// ------------------

// Create
//
constructor TdwsURLRewriteRuleStartMatch.Create(const aPattern, aRewrite : String);
begin
   inherited Create(aPattern, aRewrite);

   Assert(Pos('*', aPattern) = Length(aPattern));

   FStartLength := Length(aPattern)-1;
   FStart := Copy(aPattern, 1, FStartLength);
   FHead := StringHead2Chars(FStart);

   Assert(Pos('$1', aRewrite) = Length(aRewrite)-1);

   FRewriteLength := Length(aRewrite)-1;
   FRewrite := Copy(aRewrite, 1, FRewriteLength);

   FPassThrough := (FStart = FRewrite);
end;

// Apply
//
function TdwsURLRewriteRuleStartMatch.Apply(const originURL : String; var rewrittenURL : String) : Boolean;
var
   n : Integer;
   p : PChar;
begin
   n := Length(originURL);
   if n < FStartLength then Exit(False);
   if not CompareMem(Pointer(FStart), Pointer(originURL), FStartLength*SizeOf(Char)) then Exit(False);

   if FPassThrough then begin
      rewrittenURL := originURL;
   end else begin
      SetLength(rewrittenURL, FRewriteLength + n - FStartLength);
      p := PChar(Pointer(rewrittenURL));
      System.Move(Pointer(FRewrite)^, p^, FRewriteLength*SizeOf(Char));
      Inc(p, FRewriteLength);
      System.Move(originURL[FStartLength], p^, n - FStartLength);
   end;
   Result := True;
end;

end.
