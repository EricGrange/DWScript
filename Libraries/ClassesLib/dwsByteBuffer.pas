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
unit dwsByteBuffer;

{$I dws.inc}

interface

uses
   Types, SysUtils, dwsXPlatform, dwsUtils, dwsStrings, dwsJSON;

type

   IdwsByteBuffer = interface
      ['{6AC89DF7-11B9-4136-B9B9-1F2EE0727158}']

      function GetLength : NativeInt;
      procedure SetLength(n : NativeInt);

      function GetPosition : NativeInt;
      procedure SetPosition(n : NativeInt);

         function GetByteP : Byte;
         function GetByteA(index : NativeInt) : Byte;
         function GetWordP : Word;
         function GetWordA(index : NativeInt) : Word;
         function GetInt16P : Int16;
         function GetInt16A(index : NativeInt) : Int16;
         function GetDWordP : DWord;
         function GetDWordA(index : NativeInt) : DWord;
         function GetInt32P : Int32;
         function GetInt32A(index : NativeInt) : Int32;
         function GetInt64P : Int64;
         function GetInt64A(index : NativeInt) : Int64;
         function GetSingleP : Double;
         function GetSingleA(index : NativeInt) : Double;
         function GetDoubleP : Double;
         function GetDoubleA(index : NativeInt) : Double;

         procedure SetByteP(v : Byte);
         procedure SetByteA(index : NativeInt; v : Byte);
         procedure SetWordP(v : Word);
         procedure SetWordA(index : NativeInt; v : Word);
         procedure SetInt16P(v : Word);
         procedure SetInt16A(index : NativeInt; v : Int16);
         procedure SetDWordP(v : DWord);
         procedure SetDWordA(index : NativeInt; v : DWORD);
         procedure SetInt32P(v : Int32);
         procedure SetInt32A(index : NativeInt; v : Int32);
         procedure SetInt64P(v : Int64);
         procedure SetInt64A(index : NativeInt; v : Int64);
         procedure SetSingleP(v : Single);
         procedure SetSingleA(index : NativeInt; v : Single);
         procedure SetDoubleP(v : Double);
         procedure SetDoubleA(index : NativeInt; v : Double);
   end;

   TdwsByteBuffer = class (TInterfacedObject, IdwsByteBuffer, IGetSelf, IJSONWriteAble)
      private
         FData : array of Byte;
         FCount : NativeInt;
         FPosition : NativeInt;
         FReadOnly : Boolean;

      protected
         procedure RangeCheck(index, size : Integer); inline;

         function GetSelf : TObject;

         function GetLength : NativeInt;
         procedure SetLength(n : NativeInt);

         function GetPosition : NativeInt;
         procedure SetPosition(n : NativeInt);

      public
         function ToString : String; override;
         procedure WriteToJSON(writer : TdwsJSONWriter);

         procedure AssignString(const s : UnicodeString); inline;
         procedure EvalAsString(var s : UnicodeString); inline;

         function GetByteP : Byte;
         function GetWordP : Word;
         function GetInt16P : Int16;
         function GetDWordP : DWord;
         function GetInt32P : Int32;
         function GetInt64P : Int64;
         function GetSingleP : Double;
         function GetDoubleP : Double;

         function GetByteA(index : NativeInt) : Byte;
         function GetWordA(index : NativeInt) : Word;
         function GetInt16A(index : NativeInt) : Int16;
         function GetDWordA(index : NativeInt) : DWord;
         function GetInt32A(index : NativeInt) : Int32;
         function GetInt64A(index : NativeInt) : Int64;
         function GetSingleA(index : NativeInt) : Double;
         function GetDoubleA(index : NativeInt) : Double;

         procedure SetByteP(v : Byte);
         procedure SetWordP(v : Word);
         procedure SetInt16P(v : Word);
         procedure SetDWordP(v : DWord);
         procedure SetInt32P(v : Int32);
         procedure SetInt64P(v : Int64);
         procedure SetSingleP(v : Single);
         procedure SetDoubleP(v : Double);

         procedure SetByteA(index : NativeInt; v : Byte);
         procedure SetWordA(index : NativeInt; v : Word);
         procedure SetInt16A(index : NativeInt; v : Int16);
         procedure SetDWordA(index : NativeInt; v : DWORD);
         procedure SetInt32A(index : NativeInt; v : Int32);
         procedure SetInt64A(index : NativeInt; v : Int64);
         procedure SetSingleA(index : NativeInt; v : Single);
         procedure SetDoubleA(index : NativeInt; v : Double);

         property Count : NativeInt read FCount write SetLength;
         property Position : NativeInt read FPosition write SetPosition;
         property ReadOnly : Boolean read FReadOnly write FReadOnly;
   end;

   EdwsByteBuffer = class (Exception);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   PInt16 = ^Int16;
   PDWord = ^DWord;
   PInt32 = ^Int32;

// ------------------
// ------------------ TdwsByteBuffer ------------------
// ------------------

// GetSelf
//
function TdwsByteBuffer.GetSelf : TObject;
begin
   Result := Self;
end;

// GetLength
//
function TdwsByteBuffer.GetLength : NativeInt;
begin
   Result := FCount;
end;

// SetLength
//
procedure TdwsByteBuffer.SetLength(n : NativeInt);
begin
   if n < 0 then n := 0;
   if n = FCount then Exit;
   if n = 0 then begin
      FData := nil;
      FCount := 0;
      FPosition := 0;
   end else begin
      System.SetLength(FData, n);
      FCount := n;
      if FPosition >= n then
         FPosition := n-1;
   end;
end;

// GetPosition
//
function TdwsByteBuffer.GetPosition : NativeInt;
begin
   Result := FPosition;
end;

// SetPosition
//
procedure TdwsByteBuffer.SetPosition(n : NativeInt);
begin
   if Cardinal(n) >= Cardinal(FCount) then
      raise EdwsByteBuffer.CreateFmt('Position %d out of range (%d)', [n, FCount]);
   FPosition := n;
end;

// ToString
//
function TdwsByteBuffer.ToString : String;
var
   wr : TdwsJSONWriter;
begin
   wr := TdwsJSONWriter.Create;
   try
      WriteToJSON(wr);
      Result := wr.ToString;
   finally
      wr.Free;
   end;
end;

// WriteToJSON
//
procedure TdwsByteBuffer.WriteToJSON(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginArray;
   for i := 0 to Count-1 do
      writer.WriteInteger(FData[i]);
   writer.EndArray;
end;

// RangeCheck
//
procedure TdwsByteBuffer.RangeCheck(index, size : Integer);
begin
   if (index < 0) or (index + size - 1 >= FCount) then
      raise EdwsByteBuffer.CreateFmt('Out of range (index %d, size %d for length %d)',
                                     [index, size, FCount]);
end;

// AssignString
//
procedure TdwsByteBuffer.AssignString(const s : UnicodeString);
var
   i : Integer;
begin
   Count := Length(s);
   for i := 1 to Count do
      FData[i-1] := Byte(Ord(s[i]));
   Position := 0;
end;

// EvalAsString
//
procedure TdwsByteBuffer.EvalAsString(var s : UnicodeString);
begin
   BytesToScriptString(PByte(FData), Count, s);
end;

// GetByteP
//
function TdwsByteBuffer.GetByteP : Byte;
begin
   Result := GetByteA(FPosition);
   Inc(FPosition);
end;

// GetWordP
//
function TdwsByteBuffer.GetWordP : Word;
begin
   Result := GetWordA(FPosition);
   Inc(FPosition, 2);
end;

// GetInt16P
//
function TdwsByteBuffer.GetInt16P : Int16;
begin
   Result := GetInt16A(FPosition);
   Inc(FPosition, 2);
end;

// GetDWordP
//
function TdwsByteBuffer.GetDWordP : DWord;
begin
   Result := GetDWordA(FPosition);
   Inc(FPosition, 4);
end;

// GetInt32P
//
function TdwsByteBuffer.GetInt32P : Int32;
begin
   Result := GetInt32A(FPosition);
   Inc(FPosition, 4);
end;

// GetInt64P
//
function TdwsByteBuffer.GetInt64P : Int64;
begin
   Result := GetInt64A(FPosition);
   Inc(FPosition, 8);
end;

// GetSingleP
//
function TdwsByteBuffer.GetSingleP : Double;
begin
   Result := GetSingleA(FPosition);
   Inc(FPosition, 4);
end;

// GetDoubleP
//
function TdwsByteBuffer.GetDoubleP : Double;
begin
   Result := GetDoubleA(FPosition);
   Inc(FPosition, 8);
end;

// GetByteA
//
function TdwsByteBuffer.GetByteA(index : NativeInt) : Byte;
begin
   RangeCheck(index, 1);
   Result := FData[index];
end;

// GetWordA
//
function TdwsByteBuffer.GetWordA(index : NativeInt) : Word;
begin
   RangeCheck(index, 2);
   Result := PWord(@FData[index])^;
end;

// GetInt16A
//
function TdwsByteBuffer.GetInt16A(index : NativeInt) : Int16;
begin
   RangeCheck(index, 2);
   Result := PInt16(@FData[index])^;
end;

// GetDWordA
//
function TdwsByteBuffer.GetDWordA(index : NativeInt) : DWord;
begin
   RangeCheck(index, 4);
   Result := PDWord(@FData[index])^;
end;

// GetInt32A
//
function TdwsByteBuffer.GetInt32A(index : NativeInt) : Int32;
begin
   RangeCheck(index, 4);
   Result := PInt32(@FData[index])^;
end;

// GetInt64A
//
function TdwsByteBuffer.GetInt64A(index : NativeInt) : Int64;
begin
   RangeCheck(index, 8);
   Result := PInt64(@FData[index])^;
end;

// GetSingleA
//
function TdwsByteBuffer.GetSingleA(index : NativeInt) : Double;
begin
   RangeCheck(index, 4);
   Result := PSingle(@FData[index])^;
end;

// GetDoubleA
//
function TdwsByteBuffer.GetDoubleA(index : NativeInt) : Double;
begin
   RangeCheck(index, 8);
   Result := PDouble(@FData[index])^;
end;

// SetByteP
//
procedure TdwsByteBuffer.SetByteP(v : Byte);
begin
   SetByteA(FPosition, v);
   Inc(FPosition);
end;

// SetWordP
//
procedure TdwsByteBuffer.SetWordP(v : Word);
begin
   SetWordA(FPosition, v);
   Inc(FPosition, 2);
end;

// SetInt16P
//
procedure TdwsByteBuffer.SetInt16P(v : Word);
begin
   SetInt16A(FPosition, v);
   Inc(FPosition, 2);
end;

// SetDWordP
//
procedure TdwsByteBuffer.SetDWordP(v : DWord);
begin
   SetDWordA(FPosition, v);
   Inc(FPosition, 4);
end;

// SetInt32P
//
procedure TdwsByteBuffer.SetInt32P(v : Int32);
begin
   SetInt32A(FPosition, v);
   Inc(FPosition, 4);
end;

// SetInt64P
//
procedure TdwsByteBuffer.SetInt64P(v : Int64);
begin
   SetInt64A(FPosition, v);
   Inc(FPosition, 8);
end;

// SetSingleP
//
procedure TdwsByteBuffer.SetSingleP(v : Single);
begin
   SetSingleA(FPosition, v);
   Inc(FPosition, 4);
end;

// SetDoubleP
//
procedure TdwsByteBuffer.SetDoubleP(v : Double);
begin
   SetDoubleA(FPosition, v);
   Inc(FPosition, 8);
end;

// SetByteA
//
procedure TdwsByteBuffer.SetByteA(index : NativeInt; v : Byte);
begin
   RangeCheck(index, 1);
   FData[index] := v;
end;

// SetWordA
//
procedure TdwsByteBuffer.SetWordA(index : NativeInt; v : Word);
begin
   RangeCheck(index, 2);
   PWord(@FData[index])^ := v;
end;

// SetInt16A
//
procedure TdwsByteBuffer.SetInt16A(index : NativeInt; v : Int16);
begin
   RangeCheck(index, 2);
   PInt16(@FData[index])^ := v;
end;

// SetDWordA
//
procedure TdwsByteBuffer.SetDWordA(index : NativeInt; v : DWORD);
begin
   RangeCheck(index, 4);
   PWord(@FData[index])^ := v;
end;

// SetInt32A
//
procedure TdwsByteBuffer.SetInt32A(index : NativeInt; v : Int32);
begin
   RangeCheck(index, 4);
   PInt32(@FData[index])^ := v;
end;

// SetInt64A
//
procedure TdwsByteBuffer.SetInt64A(index : NativeInt; v : Int64);
begin
   RangeCheck(index, 8);
   PInt64(@FData[index])^ := v;
end;

// SetSingleA
//
procedure TdwsByteBuffer.SetSingleA(index : NativeInt; v : Single);
begin
   RangeCheck(index, 4);
   PSingle(@FData[index])^ := v;
end;

// SetDoubleA
//
procedure TdwsByteBuffer.SetDoubleA(index : NativeInt; v : Double);
begin
   RangeCheck(index, 8);
   PDouble(@FData[index])^ := v;
end;

end.
