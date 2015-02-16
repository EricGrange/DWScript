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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit UdwsUtilsTests;

interface

uses Classes, SysUtils, Math, dwsXPlatformTests, dwsUtils, dwsXPlatform;

type

   TdwsUtilsTests = class (TTestCase)
      private
         FTightList : TTightList;

      protected
         procedure TightListOutOfBoundsDelete;
         procedure TightListOutOfBoundsInsert;
         procedure TightListOutOfBoundsMove;

      published

         procedure StackIntegerGenericTest;
         procedure StackLotsOfIntegerGenericTest;
         procedure StackIntegerTest;
         procedure StackLotsOfIntegerTest;
         procedure WriteOnlyBlockStreamTest;
         procedure WOBSBigFirstTest;
         procedure WOBSBigSecondTest;
         procedure TightListTest;
         procedure LookupTest;
         procedure SortedListExtract;
         procedure SimpleListOfInterfaces;
         procedure UnifierTest;

         procedure UnicodeCompareTextTest;
         procedure FastCompareTextSortedValues;

         procedure FastIntToStrTest;

         procedure VarRecArrayTest;

         procedure StrContainsTest;

         procedure SortTest;
         procedure SortReverseTest;

         procedure IntToHexTest;

         procedure QueueTest;

         procedure StringHash;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsUtilsTests ------------------
// ------------------

// StackIntegerGenericTest
//
procedure TdwsUtilsTests.StackIntegerGenericTest;
var
   stack : TSimpleStack<Integer>;
begin
   stack:=TSimpleStack<Integer>.Create;

   CheckEquals(0, stack.Count);

   stack.Push(123);

   CheckEquals(1, stack.Count);
   CheckEquals(123, stack.Peek);

   stack.Push(456);

   CheckEquals(2, stack.Count);
   CheckEquals(456, stack.Peek);

   CheckEquals(456, stack.Peek);
   stack.Pop;

   CheckEquals(1, stack.Count);

   CheckEquals(123, stack.Peek);
   stack.Pop;

   CheckEquals(0, stack.Count);

   stack.Free;
end;

// StackLotsOfIntegerGenericTest
//
procedure TdwsUtilsTests.StackLotsOfIntegerGenericTest;
var
   i, j : Integer;
   stack : TSimpleStack<Integer>;
begin
   stack:=TSimpleStack<Integer>.Create;
   try
      for i:=1 to 1000 do
         stack.Push(i);

      CheckEquals(1000, stack.Count, 'nb');

      for i:=9 downto 0 do begin
         for j:=100 downto 1 do begin
            CheckEquals(i*100+j, stack.Peek, 'pop');
            stack.Pop;
         end;
         for j:=1 to 100 do
            stack.Push(j);
         for j:=100 downto 1 do begin
            CheckEquals(j, stack.Peek, 'pop bis');
            stack.Pop;
         end;
      end;

      CheckEquals(0, stack.Count, 'final nb');
   finally
      stack.Free;
   end;
end;

// StackIntegerTest
//
procedure TdwsUtilsTests.StackIntegerTest;
var
   stack : TSimpleIntegerStack;
begin
   stack:=TSimpleIntegerStack.Allocate;

   CheckEquals(0, stack.Count);

   stack.Push(123);

   CheckEquals(1, stack.Count);
   CheckEquals(123, stack.Peek);

   stack.Push(456);

   CheckEquals(2, stack.Count);
   CheckEquals(456, stack.Peek);

   CheckEquals(456, stack.Peek);
   stack.Pop;

   CheckEquals(1, stack.Count);

   CheckEquals(123, stack.Peek);
   stack.Pop;

   CheckEquals(0, stack.Count);

   stack.Free;
end;

// StackLotsOfIntegerTest
//
procedure TdwsUtilsTests.StackLotsOfIntegerTest;
var
   i, j : Integer;
   stack : TSimpleIntegerStack;
begin
   stack:=TSimpleIntegerStack.Allocate;
   try
      for i:=1 to 1000 do
         stack.Push(i);

      CheckEquals(1000, stack.Count, 'nb');

      for i:=9 downto 0 do begin
         for j:=100 downto 1 do begin
            CheckEquals(i*100+j, stack.Peek, 'pop');
            stack.Pop;
         end;
         for j:=1 to 100 do
            stack.Push(j);
         for j:=100 downto 1 do begin
            CheckEquals(j, stack.Peek, 'pop bis');
            stack.Pop;
         end;
      end;

      CheckEquals(0, stack.Count, 'final nb');
   finally
      stack.Free;
   end;
end;

// WriteOnlyBufferBlockTest
//
procedure TdwsUtilsTests.WriteOnlyBlockStreamTest;
var
   buffer : TWriteOnlyBlockStream;
   b : Integer;
   bs : AnsiString;
begin
   buffer:=TWriteOnlyBlockStream.Create;

   CheckEquals(0, buffer.Position);

   b:=Ord('1');
   buffer.Write(b, 1);
   CheckEquals(1, buffer.Position);

   bs:='23';
   buffer.Write(bs[1], 2);
   CheckEquals(3, buffer.Position);

   for b:=Ord('4') to Ord('9') do
      buffer.Write(b, 1);
   CheckEquals(9, buffer.Position);

   SetLength(bs, buffer.Size);
   buffer.StoreData(bs[1]);

   buffer.Free;

   CheckEquals(AnsiString('123456789'), bs);
end;

// WOBSBigFirstTest
//
procedure TdwsUtilsTests.WOBSBigFirstTest;
var
   buffer : TWriteOnlyBlockStream;
   i : Integer;
   bw, br : TBytes;
begin
   buffer:=TWriteOnlyBlockStream.Create;

   SetLength(bw, cWriteOnlyBlockStreamBlockSize*2);
   for i:=0 to High(bw) do
      bw[i]:=Byte(i and 255);

   buffer.Write(bw[0], Length(bw));

   CheckEquals(Length(bw), buffer.Size, 'size');

   SetLength(br, buffer.Size);
   buffer.StoreData(br[0]);

   for i:=0 to High(br) do
      if br[i]<>bw[i] then
         CheckEquals(bw[i], br[i], IntToStr(i));

   buffer.Free;
end;

// WOBSBigSecondTest
//
procedure TdwsUtilsTests.WOBSBigSecondTest;
var
   buffer : TWriteOnlyBlockStream;
   i : Integer;
   bw, br : TBytes;
begin
   buffer:=TWriteOnlyBlockStream.Create;

   SetLength(bw, cWriteOnlyBlockStreamBlockSize*2);
   for i:=0 to High(bw) do
      bw[i]:=Byte(i and 255);

   buffer.WriteByte(123);

   buffer.Write(bw[0], Length(bw));

   CheckEquals(Length(bw)+1, buffer.Size, 'size');

   SetLength(br, buffer.Size);
   buffer.StoreData(br[0]);

   CheckEquals(123, br[0], '0');
   for i:=1 to High(br) do
      if br[i]<>bw[i-1] then
         CheckEquals(bw[i], br[i], IntToStr(i));

   buffer.Free;
end;

// TightListOutOfBoundsDelete
//
procedure TdwsUtilsTests.TightListOutOfBoundsDelete;
begin
   FTightList.Delete(-1);
end;

// TightListOutOfBoundsInsert
//
procedure TdwsUtilsTests.TightListOutOfBoundsInsert;
begin
   FTightList.Insert(999, nil);
end;

// TightListOutOfBoundsMove
//
procedure TdwsUtilsTests.TightListOutOfBoundsMove;
begin
   FTightList.Insert(1, nil);
end;

// TightListTest
//
procedure TdwsUtilsTests.TightListTest;
var
   s : TRefCountedObject;
begin
   s:=TRefCountedObject.Create;

   CheckEquals(-1, FTightList.IndexOf(nil), 'empty search');

   CheckException(TightListOutOfBoundsDelete, ETightListOutOfBound, 'OutOfBounds Delete');
   CheckException(TightListOutOfBoundsInsert, ETightListOutOfBound, 'OutOfBounds Insert');
   CheckException(TightListOutOfBoundsMove, ETightListOutOfBound, 'OutOfBounds Move');

   FTightList.Add(s);
   CheckEquals(-1, FTightList.IndexOf(nil), 'single search nil');
   CheckEquals(0, FTightList.IndexOf(s), 'single search Self');

   FTightList.MoveItem(0, 0);

   CheckEquals(0, FTightList.IndexOf(s), 'single search Self 2');

   FTightList.Add(nil);
   CheckEquals(1, FTightList.IndexOf(nil), 'two search nil');
   CheckEquals(0, FTightList.IndexOf(s), 'two search Self');
   CheckEquals(-1, FTightList.IndexOf(Pointer(-1)), 'two search -1');

   FTightList.MoveItem(0, 1);

   CheckEquals(0, FTightList.IndexOf(nil), 'two search nil 2');
   CheckEquals(1, FTightList.IndexOf(s), 'two search Self 2');

   FTightList.MoveItem(1, 0);

   CheckEquals(1, FTightList.IndexOf(nil), 'two search nil 3');
   CheckEquals(0, FTightList.IndexOf(s), 'two search Self 3');

   FTightList.Add(nil);
   FTightList.MoveItem(2, 0);

   CheckEquals(0, FTightList.IndexOf(nil), 'three search nil');
   CheckEquals(1, FTightList.IndexOf(s), 'three search Self');

   FTightList.Clear;

   s.Free;
end;

// LookupTest
//
procedure TdwsUtilsTests.LookupTest;
var
   lookup : TObjectsLookup;
   obj : TRefCountedObject;
begin
   lookup:=TObjectsLookup.Create;
   try
      CheckFalse(lookup.IndexOf(nil)>=0, 'empty');
      lookup.Add(nil);
      CheckTrue(lookup.IndexOf(nil)>=0, 'nil');
      obj:=TRefCountedObject.Create;
      CheckFalse(lookup.IndexOf(obj)>=0, 'obj');
      lookup.Add(obj);
      CheckTrue(lookup.IndexOf(nil)>=0, 'nil bis');
      CheckTrue(lookup.IndexOf(obj)>=0, 'obj bis');
      lookup.Clean;
   finally
      lookup.Free;
   end;
end;

// SortedListExtract
//
type
   TTestSortedList = class (TSortedList<TRefCountedObject>)
      function Compare(const item1, item2 : TRefCountedObject) : Integer; override;
   end;
function TTestSortedList.Compare(const item1, item2 : TRefCountedObject) : Integer;
begin
   Result:=NativeInt(item1)-NativeInt(item2);
end;
procedure TdwsUtilsTests.SortedListExtract;
var
   list : TSortedList<TRefCountedObject>;
begin
   list:=TTestSortedList.Create;
   list.Add(nil);
   list.Add(nil);
   CheckEquals(2, list.Count);
   list.ExtractAt(0);
   CheckEquals(1, list.Count);
   list.ExtractAt(0);
   CheckEquals(0, list.Count);
   list.Free;
end;

// SimpleListOfInterfaces
//
procedure TdwsUtilsTests.SimpleListOfInterfaces;
var
   list : TSimpleList<IGetSelf>;
   obj1 : IGetSelf;
begin
   obj1:=TInterfacedSelfObject.Create;

   list:=TSimpleList<IGetSelf>.Create;
   try
      list.Add(nil);
      list.Add(obj1);

      CheckEquals(2, list.Count, 'count');
      CheckEquals(2, (obj1.GetSelf as TRefCountedObject).RefCount, 'ref 1');

      list.Extract(0);
      list.Add(obj1);

      CheckEquals(3, (obj1.GetSelf as TRefCountedObject).RefCount, 'ref 2');

      list.Extract(1);

      CheckEquals(2, (obj1.GetSelf as TRefCountedObject).RefCount, 'ref 3');

      list.Extract(0);

      CheckEquals(1, (obj1.GetSelf as TRefCountedObject).RefCount, 'ref 4');
   finally
      list.Free;
   end;
end;

// UnifierTest
//
procedure TdwsUtilsTests.UnifierTest;
var
   s1, s2 : String;
begin
   s1:=IntToStr(123);
   s2:=IntToStr(123);

   Check(Pointer(s1)<>Pointer(s2), 'initial');

   s1:=UnifiedString(s1);
   s2:=UnifiedString(s2);

   Check(Pointer(s1)=Pointer(s2), 'unified');
end;

// UnicodeCompareTextTest
//
procedure TdwsUtilsTests.UnicodeCompareTextTest;
begin
   CheckTrue(UnicodeCompareText('', '')=0, 'both empty');

   CheckTrue(UnicodeCompareText('a', '')>0, 'a, empty');
   CheckTrue(UnicodeCompareText('', 'a')<0, 'empty, a');
   CheckTrue(UnicodeCompareText('é', '')>0, 'é, empty');
   CheckTrue(UnicodeCompareText('', 'é')<0, 'empty, é');

   CheckTrue(UnicodeCompareText('abc', 'abc')=0, 'abc, abc');
   CheckTrue(UnicodeCompareText('abcd', 'abc')>0, 'abcd, abc');
   CheckTrue(UnicodeCompareText('abc', 'abcd')<0, 'abc, abcd');
   CheckTrue(UnicodeCompareText('abc', 'abd')<0, 'abc, abd');
   CheckTrue(UnicodeCompareText('abd', 'abc')>0, 'abc, abd');

   CheckTrue(UnicodeCompareText('abe', 'abé')<0, 'abe, abé');
   CheckTrue(UnicodeCompareText('abé', 'abe')>0, 'abé, abe');
   CheckTrue(UnicodeCompareText('abéa', 'abéz')<0, 'abéa, abéz');
   CheckTrue(UnicodeCompareText('abéz', 'abéa')>0, 'abéz, abéa');

   CheckTrue(UnicodeCompareText('abé', 'abÉ')=0, 'abé, abÉ');
   CheckTrue(UnicodeCompareText('abéaa', 'abÉz')<0, 'abéaa, abÉz');
   CheckTrue(UnicodeCompareText('abéz', 'abÉaa')>0, 'abéz, abÉaa');

   CheckTrue(UnicodeCompareText('se', 'sé')<0, 'se, sé');
   CheckTrue(UnicodeCompareText('sé', 'sé')=0, 'sé, sé');
   CheckTrue(UnicodeCompareText('se', 'se')=0, 'se, se');
   CheckTrue(UnicodeCompareText('sé', 'se')>0, 'sé, se');

   CheckTrue(UnicodeCompareText('su', 'sé')>0, 'su, sé');
   CheckTrue(UnicodeCompareText('su', 'se')>0, 'su, se');
   CheckTrue(UnicodeCompareText('sé', 'su')<0, 'sé, su');
   CheckTrue(UnicodeCompareText('se', 'su')<0, 'se, su');
   CheckTrue(UnicodeCompareText('su', 'se')>0, 'su, se');
   CheckTrue(UnicodeCompareText('se', 'su')<0, 'se, su');

   CheckTrue(UnicodeCompareText('sup', 'sé')>0, 'su, sé');
   CheckTrue(UnicodeCompareText('sup', 'se')>0, 'su, se');
   CheckTrue(UnicodeCompareText('sé', 'sup')<0, 'sé, su');
   CheckTrue(UnicodeCompareText('se', 'sup')<0, 'se, su');
   CheckTrue(UnicodeCompareText('sup', 'se')>0, 'su, se');
   CheckTrue(UnicodeCompareText('se', 'sup')<0, 'se, su');
end;

// FastCompareTextSortedValues
//
procedure TdwsUtilsTests.FastCompareTextSortedValues;
var
   i, k : Integer;
   sl : TStringList;
   fsl : TFastCompareTextList;
begin
   RandSeed:=0;
   sl:=TStringList.Create;
   fsl:=TFastCompareTextList.Create;
   try
      for i:=1 to 50 do begin
         k:=Round(IntPower(10, 2+Random(8)));
         sl.Values[IntToStr(Random(k))]:=IntToStr(Random(10000));
      end;
      fsl.Assign(sl);

      // check unsorted
      for i:=0 to sl.Count-1 do
         CheckEquals(sl.ValueFromIndex[i], fsl.Values[sl.Names[i]], IntToStr(i));
      CheckEquals('', fsl.Values['none'], 'none');

      // check sorted
      fsl.Sorted:=True;
      for i:=0 to sl.Count-1 do
         CheckEquals(sl.ValueFromIndex[i], fsl.Values[sl.Names[i]], IntToStr(i));
      CheckEquals('', fsl.Values['none'], 'none');
   finally
      sl.Free;
      fsl.Free;
   end;
end;

// FastIntToStrTest
//
procedure TdwsUtilsTests.FastIntToStrTest;
var
   i : Integer;
   n : Int64;
   s : UnicodeString;
begin
   FastInt64ToStr(0, s);
   CheckEquals('0', s);
   FastInt64ToStr(123, s);
   CheckEquals('123', s);
   FastInt64ToStr(123456, s);
   CheckEquals('123456', s);
   FastInt64ToStr(1234567, s);
   CheckEquals('1234567', s);
   FastInt64ToStr(12345678, s);
   CheckEquals('12345678', s);
   FastInt64ToStr(123456789, s);
   CheckEquals('123456789', s);
   FastInt64ToStr(123456789123456789, s);
   CheckEquals('123456789123456789', s);
   FastInt64ToStr(-8301034833169298228, s);
   CheckEquals('-8301034833169298228', s);

   n:=1;
   for i:=1 to 20 do begin
      FastInt64ToStr(n, s);
      CheckEquals(IntToStr(n), s);
      n:=n*10;
   end;
   n:=-1;
   for i:=1 to 20 do begin
      FastInt64ToStr(n, s);
      CheckEquals(IntToStr(n), s);
      n:=n*10;
   end;

   FastInt64ToStr(High(Int64), s);
   CheckEquals(IntToStr(High(Int64)), s);
   FastInt64ToStr(Low(Int64), s);
   CheckEquals(IntToStr(Low(Int64)), s);
end;

// VarRecArrayTest
//
procedure TdwsUtilsTests.VarRecArrayTest;
var
   v : TVarRecArrayContainer;
begin
   v:=TVarRecArrayContainer.Create;
   try
      v.Add(True);
      v.Add(False);

      CheckEquals(2, Length(v.VarRecArray));

      CheckEquals(vtBoolean, v.VarRecArray[0].VType, 'type 0');
      CheckEquals(True, v.VarRecArray[0].VBoolean, 'value 0');

      CheckEquals(vtBoolean, v.VarRecArray[1].VType, 'type 1');
      CheckEquals(False, v.VarRecArray[1].VBoolean, 'value 1');
   finally
      v.Free;
   end;
end;

// StrContainsTest
//
procedure TdwsUtilsTests.StrContainsTest;
begin
   CheckTrue(StrContains('banana', 'a'));
   CheckFalse(StrContains('banana', 'z'));
   CheckTrue(StrContains('banana', 'na'));
   CheckTrue(StrContains('banana', 'b'));
   CheckTrue(StrContains('banana', 'ba'));
   CheckTrue(StrContains('bananas', 'as'));
   CheckTrue(StrContains('bananas', 's'));
end;

// SortTest
//
type
   TSortable = class
      Items : array of Integer;
      function Compare(i1, i2 : Integer) : Integer;
      procedure Swap(i1, i2 : Integer);
   end;
function TSortable.Compare(i1, i2 : Integer) : Integer;
begin
   Result:=Items[i1]-Items[i2];
end;
procedure TSortable.Swap(i1, i2 : Integer);
var
   t : Integer;
begin
   t:=Items[i1];
   Items[i1]:=Items[i2];
   Items[i2]:=t;
end;

procedure TdwsUtilsTests.SortTest;
var
   i : Integer;
   s : TSortable;
   qs : TQuickSort;
begin
   s:=TSortable.Create;
   try
      qs.CompareMethod:=s.Compare;
      qs.SwapMethod:=s.Swap;
      qs.Sort(0, High(s.Items));

      SetLength(s.Items, 1);
      s.Items[0]:=-1;
      qs.Sort(0, High(s.Items));
      CheckEquals(-1, s.Items[0]);

      SetLength(s.Items, 2);
      s.Items[0]:=-2;
      s.Items[1]:=-3;
      qs.Sort(0, High(s.Items));
      CheckEquals(-3, s.Items[0]);
      CheckEquals(-2, s.Items[1]);

      SetLength(s.Items, 3);
      s.Items[0]:=-4;
      s.Items[1]:=-5;
      s.Items[2]:=-6;
      qs.Sort(0, High(s.Items));
      CheckEquals(-6, s.Items[0]);
      CheckEquals(-5, s.Items[1]);
      CheckEquals(-4, s.Items[2]);

      SetLength(s.Items, 6);
      for i:=0 to High(s.Items) do
         s.Items[i]:=(i+1) and 3;
      qs.Sort(0, High(s.Items));
      CheckEquals(0, s.Items[0]);
      CheckEquals(1, s.Items[1]);
      CheckEquals(1, s.Items[2]);
      CheckEquals(2, s.Items[3]);
      CheckEquals(2, s.Items[4]);
      CheckEquals(3, s.Items[5]);
   finally
      s.Free;
   end;
end;

// SortReverseTest
//
procedure TdwsUtilsTests.SortReverseTest;
var
   k, n : Integer;
   s : TSortable;
   qs : TQuickSort;
begin
   s:=TSortable.Create;
   try
      qs.CompareMethod:=s.Compare;
      qs.SwapMethod:=s.Swap;

      for n:=2 to 65 do begin
         SetLength(s.Items, n);
         for k:=0 to n-1 do
            s.Items[k]:=n-1-k;
         qs.Sort(0, High(s.Items));
         for k:=1 to n-1 do
            CheckEquals(k, s.Items[k]);
      end;
   finally
      s.Free;
   end;
end;

// IntToHexTest
//
procedure TdwsUtilsTests.IntToHexTest;
begin
   CheckEquals(SysUtils.IntToHex(Int64(-1), 1), Int64ToHex(-1, 1));
   CheckEquals(SysUtils.IntToHex(Int64(0), 3), Int64ToHex(0, 3));
   CheckEquals(SysUtils.IntToHex(Int64(12345), 3), Int64ToHex(12345, 3));
   CheckEquals(SysUtils.IntToHex(Int64(12345), 6), Int64ToHex(12345, 6));
   CheckEquals(SysUtils.IntToHex($123456789, 6), Int64ToHex($123456789, 6));
end;

// QueueTest
//
procedure TdwsUtilsTests.QueueTest;
var
   q : TSimpleQueue<Variant>;
   v : Variant;
begin
   q:=TSimpleQueue<Variant>.Create;
   try
      CheckEquals(0, q.Count);

      CheckFalse(q.Pop(v), 'pop');
      CheckFalse(q.Pull(v), 'pull');

      q.Push(1);
      q.Push('two');
      CheckEquals(2, q.Count);
      CheckEquals('two', q.Pop);
      CheckEquals(1, q.Pop);

      q.Insert(2);
      q.Insert('three');
      CheckEquals('three', q.Pull);
      CheckEquals(2, q.Pull);

      q.Push(3);
      q.Push('four');
      CheckEquals(2, q.Count);
      CheckEquals(3, q.Pull);
      CheckEquals('four', q.Pull);

      CheckEquals(0, q.Count);
   finally
      q.Free;
   end;
end;

// StringHash
//
procedure TdwsUtilsTests.StringHash;
begin
   CheckEquals(SimpleLowerCaseStringHash(''), SimpleStringHash(''), 'empty');
   CheckEquals(SimpleLowerCaseStringHash('abc'), SimpleStringHash('abc'), 'abc');
   CheckEquals(SimpleLowerCaseStringHash('ABC'), SimpleStringHash(LowerCase('ABC')), 'ABC');
   CheckEquals(SimpleLowerCaseStringHash('éRic'), SimpleStringHash(UnicodeLowerCase('éRic')), 'éRic');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('UtilsTests', TdwsUtilsTests);

end.
