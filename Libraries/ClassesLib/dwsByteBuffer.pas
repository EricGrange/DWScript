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

   IdwsByteBuffer = interface(IGetSelf)
      ['{6AC89DF7-11B9-4136-B9B9-1F2EE0727158}']

      function GetCount : NativeInt;
      procedure SetCount(n : NativeInt);

      function GetPosition : NativeInt;
      procedure SetPosition(n : NativeInt);

      procedure Assign(const buffer : IdwsByteBuffer);
      procedure AssignDataString(const s : String);
      procedure AssignJSON(const s : String);
      procedure AssignBase64(const s : String);
      procedure AssignHexString(const s : String);

      procedure ToDataString(var s : String);
      procedure ToJSON(var s : String);
      procedure ToBase64(var s : String);
      procedure ToHexString(var s : String);

      function Copy(offset, size : NativeInt) : IdwsByteBuffer;

      function GetByteP : Byte;
      function GetWordP : Word;
      function GetInt16P : Int16;
      function GetDWordP : DWord;
      function GetInt32P : Int32;
      function GetInt64P : Int64;
      function GetSingleP : Double;
      function GetDoubleP : Double;
      procedure GetDataStringP(size : NativeInt; var result : String);

      function GetByteA(index : NativeInt) : Byte;
      function GetWordA(index : NativeInt) : Word;
      function GetInt16A(index : NativeInt) : Int16;
      function GetDWordA(index : NativeInt) : DWord;
      function GetInt32A(index : NativeInt) : Int32;
      function GetInt64A(index : NativeInt) : Int64;
      function GetSingleA(index : NativeInt) : Double;
      function GetDoubleA(index : NativeInt) : Double;
      procedure GetDataStringA(index : NativeInt; size : NativeInt; var result : String);

      procedure SetByteP(v : Byte);
      procedure SetWordP(v : Word);
      procedure SetInt16P(v : Word);
      procedure SetDWordP(v : DWord);
      procedure SetInt32P(v : Int32);
      procedure SetInt64P(v : Int64);
      procedure SetSingleP(v : Single);
      procedure SetDoubleP(v : Double);
      procedure SetDataStringP(const v : String);

      procedure SetByteA(index : NativeInt; v : Byte);
      procedure SetWordA(index : NativeInt; v : Word);
      procedure SetInt16A(index : NativeInt; v : Int16);
      procedure SetDWordA(index : NativeInt; v : DWORD);
      procedure SetInt32A(index : NativeInt; v : Int32);
      procedure SetInt64A(index : NativeInt; v : Int64);
      procedure SetSingleA(index : NativeInt; v : Single);
      procedure SetDoubleA(index : NativeInt; v : Double);
      procedure SetDataStringA(index : NativeInt; const v : String);
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

         function GetCount : NativeInt;
         procedure SetCount(n : NativeInt);

         function GetPosition : NativeInt;
         procedure SetPosition(n : NativeInt);

      public
         function ToString : String; override;
         procedure WriteToJSON(writer : TdwsJSONWriter);

         procedure Assign(const buffer : IdwsByteBuffer);
         procedure AssignDataString(const s : String);
         procedure AssignJSON(const s : String);
         procedure AssignBase64(const s : String);
         procedure AssignHexString(const s : String);

         procedure ToDataString(var s : String);
         procedure ToJSON(var s : String);
         procedure ToBase64(var s : String);
         procedure ToHexString(var s : String);

         function Copy(offset, size : NativeInt) : IdwsByteBuffer;

         function GetByteP : Byte;
         function GetWordP : Word;
         function GetInt16P : Int16;
         function GetDWordP : DWord;
         function GetInt32P : Int32;
         function GetInt64P : Int64;
         function GetSingleP : Double;
         function GetDoubleP : Double;
         procedure GetDataStringP(size : NativeInt; var result : String);

         function GetByteA(index : NativeInt) : Byte;
         function GetWordA(index : NativeInt) : Word;
         function GetInt16A(index : NativeInt) : Int16;
         function GetDWordA(index : NativeInt) : DWord;
         function GetInt32A(index : NativeInt) : Int32;
         function GetInt64A(index : NativeInt) : Int64;
         function GetSingleA(index : NativeInt) : Double;
         function GetDoubleA(index : NativeInt) : Double;
         procedure GetDataStringA(index : NativeInt; size : NativeInt; var result : String);

         procedure SetByteP(v : Byte);
         procedure SetWordP(v : Word);
         procedure SetInt16P(v : Word);
         procedure SetDWordP(v : DWord);
         procedure SetInt32P(v : Int32);
         procedure SetInt64P(v : Int64);
         procedure SetSingleP(v : Single);
         procedure SetDoubleP(v : Double);
         procedure SetDataStringP(const v : String);

         procedure SetByteA(index : NativeInt; v : Byte);
         procedure SetWordA(index : NativeInt; v : Word);
         procedure SetInt16A(index : NativeInt; v : Int16);
         procedure SetDWordA(index : NativeInt; v : DWORD);
         procedure SetInt32A(index : NativeInt; v : Int32);
         procedure SetInt64A(index : NativeInt; v : Int64);
         procedure SetSingleA(index : NativeInt; v : Single);
         procedure SetDoubleA(index : NativeInt; v : Double);
         procedure SetDataStringA(index : NativeInt; const v : String);

         property Count : NativeInt read FCount write SetCount;
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

uses dwsEncoding;

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

// GetCount
//
function TdwsByteBuffer.GetCount : NativeInt;
begin
   Result := FCount;
end;

// SetCount
//
procedure TdwsByteBuffer.SetCount(n : NativeInt);
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
   if NativeUInt(n) >= NativeUInt(Count) then
      raise EdwsByteBuffer.CreateFmt('Position %d out of range (length %d)', [n, Count]);
   FPosition := n;
end;

// ToString
//
function TdwsByteBuffer.ToString : String;
begin
   ToDataString(Result);
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

// Assign
//
procedure TdwsByteBuffer.Assign(const buffer : IdwsByteBuffer);
var
   bb : TdwsByteBuffer;
begin
   bb := buffer.GetSelf as TdwsByteBuffer;
   Count := bb.Count;
   if Count > 0 then
      System.Move(bb.FData[0], FData[0], Count);
end;

// AssignDataString
//
procedure TdwsByteBuffer.AssignDataString(const s : UnicodeString);
begin
   Count := Length(s);
   if Count > 0 then
      WordsToBytes(Pointer(s), @FData[0], Count);
   Position := 0;
end;

// AssignJSON
//
procedure TdwsByteBuffer.AssignJSON(const s : String);
var
   i : Integer;
   v : TdwsJSONValue;
begin
   v := TdwsJSONValue.ParseString(s);
   try
      if v.ValueType <> jvtArray then
         raise EdwsByteBuffer.Create(CPE_ArrayExpected);
      Count := v.ElementCount;
      for i := 0 to Count-1 do
         FData[i] := Byte(v.Elements[i].AsInteger);
   finally
      v.Free;
   end;
end;

// AssignBase64
//
procedure TdwsByteBuffer.AssignBase64(const s : String);
var
   buf : RawByteString;
begin
   buf := Base64Decode(s);
   Count := Length(buf);
   if Count > 0 then
      System.Move(Pointer(buf), FData[0], Count);
end;

// AssignHexString
//
procedure TdwsByteBuffer.AssignHexString(const s : String);
begin
   Count := Length(s) div 2;
   if Count > 0 then
      HexToBin(PChar(Pointer(s)), @FData[0], Count);
end;

// ToDataString
//
procedure TdwsByteBuffer.ToDataString(var s : UnicodeString);
begin
   BytesToScriptString(PByte(FData), Count, s);
end;

// ToJSON
//
procedure TdwsByteBuffer.ToJSON(var s : String);
var
   wr : TdwsJSONWriter;
begin
   wr := TdwsJSONWriter.Create;
   try
      WriteToJSON(wr);
      s := wr.ToString;
   finally
      wr.Free;
   end;
end;

// ToBase64
//
procedure TdwsByteBuffer.ToBase64(var s : String);
begin
   s := Base64Encode(Pointer(FData), Count, cBase64);
end;

// ToHexString
//
procedure TdwsByteBuffer.ToHexString(var s : String);
begin
   s := BinToHex(FData, Count);
end;

// Copy
//
function TdwsByteBuffer.Copy(offset, size : NativeInt) : IdwsByteBuffer;
var
   newBuffer : TdwsByteBuffer;
begin
   if (offset < 0) or (offset >= Count) then
      raise EdwsByteBuffer.CreateFmt('Offset out of range (index %d for length %d)',
                                     [offset, Count]);

   if offset + size > Count then
      size := Count-offset;

   newBuffer := TdwsByteBuffer.Create;
   if size > 0 then begin
      newBuffer.SetCount(size);
      System.Move(FData[offset], newBuffer.FData[0], size);
   end;
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

// GetDataStringP
//
procedure TdwsByteBuffer.GetDataStringP(size : NativeInt; var result : String);
begin
   GetDataStringA(FPosition, size, result);
   Inc(FPosition, size);
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

// GetDataStringA
//
procedure TdwsByteBuffer.GetDataStringA(index : NativeInt; size : NativeInt; var result : String);
begin
   if size < 0 then
      raise EdwsByteBuffer.CreateFmt('Invalid size %d', [size]);
   if size = 0 then
      result := ''
   else begin
      RangeCheck(index, size);
      BytesToScriptString(@FData[index], size, Result);
   end;
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

// SetDataStringP
//
procedure TdwsByteBuffer.SetDataStringP(const v : String);
begin
   SetDataStringA(FPosition, v);
   Inc(FPosition, Length(v));
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
   PDWord(@FData[index])^ := v;
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

// SetDataStringA
//
procedure TdwsByteBuffer.SetDataStringA(index : NativeInt; const v : String);
var
   n : Integer;
begin
   if v = '' then Exit;
   n := Length(v);
   RangeCheck(index, n);
   WordsToBytes(Pointer(v), @FData[index], n);
end;

end.
