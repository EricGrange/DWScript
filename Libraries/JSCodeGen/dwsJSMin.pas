{**********************************************************************}
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Eric Grange                                                       }
{    based on JSMin (http://javascript.crockford.com/jsmin.html)       }
{                                                                      }
{**********************************************************************}
{ jsmin.c
   2011-01-22

Copyright (c) 2002 Douglas Crockford  (www.crockford.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

The Software shall be used for Good, not Evil.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
}
unit dwsJSMin;

interface

uses Classes, dwsUtils, SysUtils;

function JavaScriptMinify(const src : String) : String; overload;
procedure JavaScriptMinify(const src : String; output : TWriteOnlyBlockStream); overload;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// JavaScriptMinify
//
function JavaScriptMinify(const src : String) : String;
var
   buf : TWriteOnlyBlockStream;
begin
   buf:=TWriteOnlyBlockStream.Create;
   try
      JavaScriptMinify(src, buf);
      Result:=buf.ToUnicodeString;
   finally
      buf.Free;
   end;
end;

// JavaScriptMinify
//
procedure JavaScriptMinify(const src : String; output : TWriteOnlyBlockStream);
const
   EOF = #0;
var
   iter : PChar;
   theA, theB : Char;
   outputPos : Integer;
   outputBuf : String;

   function IsAlphaNum(c : Char) : Boolean;
   var
      oc : Integer;
   begin
      oc:=Ord(c);
      Result:=(oc>=126) or (AnsiChar(oc) in ['0'..'9', 'A'..'Z', 'a'..'z', '_', '$', '\']);
   end;

   function Get : Char;
   begin
      Result:=iter^;
      Inc(iter);
      case Result of
         EOF, #10, ' '..#255 : Exit;
         #13  : Result:=#10
      else
         Result:=' ';
      end;
   end;

   procedure Put(c : Char);
   begin
      outputBuf[outputPos]:=c;
      Inc(outputPos);
   end;

   function Next : Char;
   begin
      Result:=get();
      if Result<>'/' then Exit;
      case iter^ of
         '/' : repeat
                  Result:=get();
               until Result<=#10;
         '*' :  begin
            get();
            while True do begin
               case get() of
                  '*' : if iter^='/' then begin
                     get();
                     Break;
                  end;
                  EOF : raise Exception.Create('Error: JSMIN Unterminated comment.');
               end;
            end;
            Result:=' ';
         end;
      end;
   end;

   (* action -- do something! What you do is determined by the argument:
           1   Output A. Copy B to A. Get the next B.
           2   Copy B to A. Get the next B. (Delete A).
           3   Get the next B. (Delete B).
      action treats a AnsiString as a single character. Wow!
      action recognizes a regular expression if it is preceded by ( or , or =.
   *)
   procedure Action(d: Integer);
   begin
      if d <= 1 then
         put( theA );
      if d <= 2 then begin
         theA := theB;
         if (theA='''') or (theA='"') then begin
            while True do begin
               put( theA );
               theA := get();
               if theA = theB then Break;
               if theA <= #10 then
                  raise Exception.CreateFmt('Error: JSMIN unterminated AnsiString literal: #$%x', [Integer(theA)]);
               if theA='\' then begin
                  put( theA );
                  theA := get();
               end;
            end;
         end;
      end;
      if d <= 3 then begin
          theB := next();
          if (theB='/') and (Ord(theA)<127) and (AnsiChar(theA) in ['(', ',', '=', '[', '!', ':', '&', '|', '?', '{', '}', ';', #10]) then begin
             put( theA );
             put( theB );
             while True do begin
                theA := get();
                if theA = '/' then
                   Break
                else if theA = '\' then begin
                   put( theA );
                   theA := get();
                end else if theA <= #10 then
                   raise Exception.CreateFmt('Error: JSMIN unterminated Regular Expression literal : #$%x', [Integer(theA)]);
                put( theA );
             end;
             theB := next();
          end;
      end;
   end;


begin
   if src='' then Exit;
   iter:=PChar(src);
   SetLength(outputBuf, Length(src)+1);
   outputPos:=1;

   theA:=#10;
   action( 3 );
   while (theA<>EOF) do begin
      case theA of
         ' ' : begin
            if IsAlphaNum(theB) then
               action(1)
            else action(2);
         end;
         #10 : begin
            case theB of
               '{', '[', '(', '+', '-' : action(1);
               ' ' : action(3);
            else
               if IsAlphaNum(theB) then
                  action(1)
               else action(2);
            end;
         end;
      else
         if (theA=';') and (theB='}') then
            action(2)
         else case theB of
            ' ' : begin
               if IsAlphaNum(theA) then
                  action(1)
               else action(3);
            end;
            #10 : begin
               if CharInSet(theA, ['}', ']', ')', '+', '-', '"', '''']) then
                  action(1)
               else if IsAlphaNum(theA) then
                  action(1)
               else action(3);
            end;
         else
            action(1);
         end;
      end;
   end;

   SetLength(outputBuf, outputPos-1);
   output.WriteString(outputBuf);
end;

end.

