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
unit dwsCSSPack;

interface

uses dwsXPlatform;

type

   TCSSPackContext = record
      InPtr : PWideChar;
      OutBuffer : String;
      OutPtr : PWideChar;
      LastOut : Char;
      Line, Col : Integer;
      InParams : Integer;

      procedure Write(c : WideChar); inline;
      procedure Skip; inline;
      procedure Copy; inline;
      procedure CopyIdent;
   end;

   TCSSPack = class
      private

      protected
         class procedure ProcessWhiteSpace(var context : TCSSPackContext);
         class procedure ProcessComment(var context : TCSSPackContext);
         class procedure ProcessSString(var context : TCSSPackContext);
         class procedure ProcessDString(var context : TCSSPackContext);
         class procedure ProcessHexColor(var context : TCSSPackContext);
         class procedure ProcessZero(var context : TCSSPackContext);

      public
         class function Compress(const src : UnicodeString) : String;

   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TCSSPackContext ------------------
// ------------------

// Write
//
procedure TCSSPackContext.Write(c : WideChar);
begin
   LastOut:=c;
   OutPtr^:=c;
   Inc(OutPtr);
end;

// Skip
//
procedure TCSSPackContext.Skip;
begin
   if InPtr^=#10 then begin
      Inc(Line);
      Col:=1;
   end else Inc(Col);
   Inc(InPtr);
end;

// Copy
//
procedure TCSSPackContext.Copy;
begin
   Write(InPtr^);
   Skip;
end;

// CopyIdent
//
procedure TCSSPackContext.CopyIdent;
var
   input : PWideChar;
   output : PWideChar;
   c : WideChar;
begin
   input:=InPtr;
   output:=OutPtr;

   repeat
      c:=input^;
      if (c='-') or ((c>='a') and (c<='z')) then begin
         output^:=c;
         Inc(output);
      end else break;
      Inc(input);
      Inc(Col);
   until False;

   InPtr:=input;
   OutPtr:=output;
   Dec(output);
   LastOut:=output^;
end;

// ------------------
// ------------------ TCSSPack ------------------
// ------------------

// Compress
//
class function TCSSPack.Compress(const src : UnicodeString) : String;
var
   context : TCSSPackContext;
begin
   context.InPtr:=PWideChar(src);
   SetLength(context.OutBuffer, Length(src));
   context.OutPtr:=PWideChar(context.OutBuffer);
   context.Line:=1;
   context.Col:=1;
   context.LastOut:='}';
   context.InParams:=0;

   repeat
      case context.InPtr^ of
         #0 :
            break;
         #1..' ' :
            ProcessWhiteSpace(context);
         '"' :
            ProcessDString(context);
         '''' :
            ProcessSString(context);
         '#' :
            case context.LastOut of
               ':', ' ', '(', ',' :
                  ProcessHexColor(context);
            else
               context.Copy;
            end;
         '(' : begin
            Inc(context.InParams);
            context.Copy;
         end;
         ')' : begin
            Dec(context.InParams);
            context.Copy;
         end;
         '/' :
            case context.InPtr[1] of
               '*' : ProcessComment(context)
            else
               context.Copy
            end;
         '0' :
            case context.LastOut of
               ':', ' ', '(', ',' :
                  ProcessZero(context)
            else
               context.Copy
            end;
         '-', 'a'..'z' :
            context.CopyIdent;
         '{', '>' : begin
            if context.LastOut=' ' then
               Dec(context.OutPtr);
            context.Copy;
         end;
         '}', ';' : begin
            if context.LastOut=';' then
               Dec(context.OutPtr);
            context.Copy;
         end;
      else
         context.Copy;
      end;
   until False;

   Result:=context.OutBuffer;
   SetLength(Result, (NativeUInt(context.OutPtr)-NativeUInt(Pointer(Result))) div SizeOf(WideChar));
end;

// ProcessWhiteSpace
//
class procedure TCSSPack.ProcessWhiteSpace(var context : TCSSPackContext);
begin
   case context.LastOut of
      ' ', ':', '}', ',', '{', ';', '>' : ;
   else
      context.Write(' ');
   end;
   repeat
      case context.InPtr^ of
         #1..' ' : context.Skip;
      else
         Break;
      end;
   until False;
end;

// ProcessComment
//
class procedure TCSSPack.ProcessComment(var context : TCSSPackContext);
begin
   Assert(context.InPtr[0]='/');
   Assert(context.InPtr[1]='*');

   if context.InPtr[2]='!' then begin

      // important comment, don't strip
      repeat
         case context.InPtr^ of
            #0 :
               break;
            '/' :
               if context.LastOut='*' then begin
                  context.Copy;
                  Break;
               end;
         end;
         context.Copy;
      until False;

   end else begin

      context.Skip;
      context.Skip;

      repeat
         case context.InPtr^ of
            '*' : begin
               context.Skip;
               if context.InPtr^='/' then begin
                  context.Skip;
                  Break;
               end;
            end;
            #0 :
               break;
         else
            context.Skip;
         end;
      until False;

   end;
end;

// ProcessSString
//
class procedure TCSSPack.ProcessSString(var context : TCSSPackContext);
begin
   Assert(context.InPtr^='''');

   repeat
      context.Copy;
      case context.InPtr[0] of
         #0 :
            break;
         '''' : begin
            context.Copy;
            break;
         end;
         '\' : if context.InPtr[1]='''' then
            context.Copy;
      end;
   until False;
end;

// ProcessDString
//
class procedure TCSSPack.ProcessDString(var context : TCSSPackContext);
begin
   Assert(context.InPtr^='"');

   repeat
      context.Copy;
      case context.InPtr[0] of
         #0 :
            break;
         '"' : begin
            context.Copy;
            break;
         end;
         '\' : if context.InPtr[1]='"' then
            context.Copy;
      end;
   until False;
end;

// ProcessHexColor
//
class procedure TCSSPack.ProcessHexColor(var context : TCSSPackContext);
var
   colorStart : PWideChar;
begin
   Assert(context.InPtr[0]='#');
   context.Copy;
   colorStart:=context.OutPtr;

   repeat
      case context.InPtr^ of
         '0'..'9', 'a'..'f', 'A'..'F' :
            context.Copy;
      else
         break;
      end;
   until False;

   case context.InPtr^ of
      #1..' ', ';', '}' : begin
         if     (context.OutPtr=@colorStart[6])
            and (colorStart[0]=colorStart[1])
            and (colorStart[2]=colorStart[3])
            and (colorStart[4]=colorStart[5]) then begin

            colorStart[1]:=colorStart[2];
            colorStart[2]:=colorStart[4];
            Dec(context.OutPtr, 3);
         end;
      end;
   end;
end;

// ProcessZero
//
class procedure TCSSPack.ProcessZero(var context : TCSSPackContext);
var
   zeroStart : PWideChar;
begin
   Assert(context.InPtr[0]='0');
   context.Copy;
   zeroStart:=context.OutPtr;

   repeat
      case context.InPtr^ of
         'a'..'z' :
            context.Copy;
         '.' :
            if context.OutPtr=zeroStart then begin
               Dec(context.OutPtr, 1);
               exit;
            end else break;
         '%' : begin
            context.Copy;
            if context.InParams=0 then
               break
            else exit;
         end;
      else
         break;
      end;
   until False;

   case context.InPtr^ of
      #1..' ', ';', '}' : begin
         if (context.OutPtr=@zeroStart[1]) and (zeroStart[0]='%') then begin

            Dec(context.OutPtr, 1);

         end else if context.OutPtr=@zeroStart[2] then begin
            if    ((zeroStart[0]='p') and (zeroStart[1]='x'))
               or ((zeroStart[0]='e') and (zeroStart[1]='m'))
               or ((zeroStart[0]='p') and (zeroStart[1]='t')) then begin

               Dec(context.OutPtr, 2);

            end;
         end;
      end;
   end;
end;

end.
