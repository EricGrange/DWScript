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

uses Classes, SysUtils, Math, dwsXPlatformTests, dwsUtils,
   dwsXPlatform, dwsWebUtils, dwsTokenStore, dwsCryptoXPlatform,
   dwsEncodingLibModule, dwsGlobalVars, dwsEncoding;

type

   TdwsUtilsTests = class (TTestCase)
      private
         FTightList : TTightList;
         FDummy : TObject;

      protected
         procedure SetUp; override;
         procedure TearDown; override;

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

         procedure LoadTextFromBufferTest;

         procedure URLEncodedEncoder;

         procedure VariantClearAssignString;

         procedure MultiThreadedTokenStore;
         procedure TokenStoreData;

         procedure Base32EncoderTest;

         procedure NameObjectHashTest;

         procedure MultiThreadedGlobalVars;
         procedure Eratosthenes;
         procedure GlobalVarsCollect;

         procedure BytesWords;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vGlobals : TGlobalVars;

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

// SetUp
//
procedure TdwsUtilsTests.SetUp;
begin
   FDummy:=TObject.Create;
end;

// TearDown
//
procedure TdwsUtilsTests.TearDown;
begin
   FDummy.Free;
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

// LoadTextFromBufferTest
//
procedure TdwsUtilsTests.LoadTextFromBufferTest;

   function Buffer(const a : array of const) : TBytes;
   var
      i : Integer;
   begin
      SetLength(Result, Length(a));
      for i:=0 to High(a) do begin
         case a[i].VType of
            vtInteger : Result[i]:=a[i].VInteger;
            vtChar : Result[i]:=Ord(a[i].VChar);
            vtWideChar : Result[i]:=Ord(a[i].VWideChar);
         else
            Assert(False);
         end;
      end;
   end;

begin
   CheckEquals('hello', LoadTextFromBuffer(Buffer(['h', 'e', 'l', 'l', 'o'])), 'hello');
   CheckEquals('hél', LoadTextFromBuffer(Buffer(['h', $C3, $A9, 'l'])), 'hél');
   CheckEquals('utf8é', LoadTextFromBuffer(Buffer([$EF, $BB, $BF, 'u', 't', 'f', '8', $C3, $A9])), 'utf8é');
   CheckEquals('Bé', LoadTextFromBuffer(Buffer([$FE, $FF, 0, 'B', 0, $E9])), 'Bé');
   CheckEquals('Lé', LoadTextFromBuffer(Buffer([$FF, $FE, 'L', 0, $E9, 0])), 'Lé');
end;

// URLEncodedEncoder
//
procedure TdwsUtilsTests.URLEncodedEncoder;
begin
   CheckEquals('', WebUtils.EncodeURLEncoded(''), 'empty');
   CheckEquals('a', WebUtils.EncodeURLEncoded('a'), 'a');
   CheckEquals('a%3D', WebUtils.EncodeURLEncoded('a='), 'a=');
   CheckEquals('%3D%3D%3D%3D%3D%3D', WebUtils.EncodeURLEncoded('======'), '======');
end;

// VariantClearAssignString
//
procedure TdwsUtilsTests.VariantClearAssignString;
var
   v : Variant;
begin
   VarCopySafe(v, 'a');
   CheckEquals('a', v, 'a');
   VarClearSafe(v);
   CheckEquals('', v, 'a clear');

   VarCopySafe(v, 'b');
   CheckEquals('b', v, 'b');
   VarCopySafe(v, 'c');
   CheckEquals('c', v, 'c');

   VarCopySafe(v, 1);
   CheckEquals(1, v, '1');

   VarCopySafe(v, 'd');
   CheckEquals('d', v, 'd');

   v:=123;
   CheckEquals(123, v, '123');
   VarCopySafe(v, 'e');
   CheckEquals('e', v, 'e');
end;

// MultiThreadedTokenStore
//
type
   TTokenLoadThread = class(TThread)
      FStore : TdwsTokenStore;
      procedure Execute; override;
   end;
procedure TTokenLoadThread.Execute;
var
   i : Integer;
begin
   for i:=1 to 20000 do
      FStore.Register(CryptographicToken(120), Random(100), '');
end;
procedure TdwsUtilsTests.MultiThreadedTokenStore;
var
   store : TdwsTokenStore;
   threads : array [0..3] of TTokenLoadThread;
   i : Integer;
begin
   store:=TdwsTokenStore.Create;
   try
      store.CollectionIntervalMilliseconds:=50;
      for i:=0 to High(threads) do begin
         threads[i]:=TTokenLoadThread.Create(True);
         threads[i].FStore:=store;
         threads[i].Start;
      end;
      for i:=0 to High(threads) do begin
         threads[i].WaitFor;
         threads[i].Free;
      end;
   finally
      store.Free;
   end;
end;

// TokenStoreData
//
procedure TdwsUtilsTests.TokenStoreData;
var
   store : TdwsTokenStore;
begin
   store:=TdwsTokenStore.Create;
   try
      store.Register('a', 1000, 'aa');
      store.Register('b', 1000, 'bb');
      store.Register('c', 1000, 'aa');
      CheckEquals('aa', store.TokenData['a']);
      CheckEquals('bb', store.TokenData['b']);
      CheckEquals('aa', store.TokenData['c']);
      store.RemoveByData('aa');
      CheckEquals('', store.TokenData['a']);
      CheckEquals('bb', store.TokenData['b']);
      CheckEquals('', store.TokenData['c']);
   finally
      store.Free;
   end;
end;

// Base32EncoderTest
//
procedure TdwsUtilsTests.Base32EncoderTest;
begin
   CheckEquals('', Base32Encode(''));
   CheckEquals('GE', Base32Encode('1'));
   CheckEquals('GEZA', Base32Encode('12'));
   CheckEquals('GEZDG', Base32Encode('123'));
   CheckEquals('GEZDGNA', Base32Encode('1234'));
   CheckEquals('GEZDGNBV', Base32Encode('12345'));
   CheckEquals('GEZDGNBVGY', Base32Encode('123456'));
   CheckEquals('GEZDGNBVGY3Q', Base32Encode('1234567'));
   CheckEquals('GEZDGNBVGY3TQ', Base32Encode('12345678'));
   CheckEquals('GEZDGNBVGY3TQOI', Base32Encode('123456789'));
   CheckEquals('GEZDGNBVGY3TQOJQ', Base32Encode('1234567890'));
   CheckEquals('GEZDGNBVGY3TQOJQME', Base32Encode('1234567890a'));

   CheckEquals('', Base32Decode(''));
   CheckEquals('1', Base32Decode('GE'));
   CheckEquals('12', Base32Decode('GEZA'));
   CheckEquals('12', Base32Decode('GEZA===='));
   CheckEquals('123', Base32Decode('GEZDG'));
   CheckEquals('1234', Base32Decode('GEZDGNA'));
   CheckEquals('12345', Base32Decode('GEZDGNBV'));
   CheckEquals('123456', Base32Decode('GEZDGNBVGY'));
   CheckEquals('1234567', Base32Decode('GEZDGNBVGY3Q'));
   CheckEquals('12345678', Base32Decode('GEZDGNBVGY3TQ'));
   CheckEquals('123456789', Base32Decode('GEZDGNBVGY3TQOI'));
   CheckEquals('1234567890', Base32Decode('GEZDGNBVGY3TQOJQ'));
   CheckEquals('1234567890a', Base32Decode('GEZDGNBVGY3TQOJQME'));
end;

// NameObjectHashTest
//
procedure TdwsUtilsTests.NameObjectHashTest;
var
   i : Integer;
   h : Cardinal;
   name1, name2, name1c : String;
   noh : TNameObjectHash;
begin
   // generate names for test, name1 & name2 should not collide
   // while name1c should clash with name1
   name1:='0';
   h:=SimpleStringHash(name1) and (cNameObjectHashMinSize-1);
   for i:=1 to cNameObjectHashMinSize*4 do begin
      if (SimpleStringHash(IntToStr(i)) and (cNameObjectHashMinSize-1))=h then begin
         if name1c='' then
            name1c:=IntToStr(i);
      end else begin
         if name2='' then
            name2:=IntToStr(i);
      end;
   end;
   Assert((name2<>'') and (name1c<>''), 'either you got darn unlucky or the hash function is bugged...');

   noh:=TNameObjectHash.Create;
   try
      noh.AddObject(name1, Self);
      CheckEquals(noh.Count, 1);
      CheckTrue(Self=noh.Objects[name1], '1 a');
      CheckTrue(nil=noh.Objects[name1c], '1 b');
      CheckTrue(nil=noh.Objects[name2], '1 c');

      noh.AddObject(name2, noh);
      CheckEquals(noh.Count, 2);
      CheckTrue(Self=noh.Objects[name1], '2 a');
      CheckTrue(nil=noh.Objects[name1c], '2 b');
      CheckTrue(noh=noh.Objects[name2], '2 c');

      noh.AddObject(name1c, FDummy);
      CheckEquals(noh.Count, 3);
      CheckTrue(Self=noh.Objects[name1], '3 a');
      CheckTrue(FDummy=noh.Objects[name1c], '3 b');
      CheckTrue(noh=noh.Objects[name2], '3 c');

      noh.Bucket[noh.BucketIndex[name1]].HashCode:=0;
      noh.Pack;
      CheckEquals(noh.Count, 2);
      CheckTrue(nil=noh.Objects[name1], '4 a');
      CheckTrue(FDummy=noh.Objects[name1c], '4 b');
      CheckTrue(noh=noh.Objects[name2], '4 c');

      noh.Bucket[noh.BucketIndex[name2]].HashCode:=0;
      noh.Pack;
      CheckEquals(noh.Count, 1);
      CheckTrue(nil=noh.Objects[name1], '5 a');
      CheckTrue(FDummy=noh.Objects[name1c], '5 b');
      CheckTrue(nil=noh.Objects[name2], '5 c');

      noh.Bucket[noh.BucketIndex[name1c]].HashCode:=0;
      noh.Pack;
      CheckEquals(noh.Count, 0);
      CheckTrue(nil=noh.Objects[name1], '6 a');
      CheckTrue(nil=noh.Objects[name1c], '6 b');
      CheckTrue(nil=noh.Objects[name2], '6 c');
   finally
      noh.Free;
   end;
end;

// MultiThreadedGlobalVars
//
type
   TGlobalVarStress = class (TThread)
      procedure Execute; override;
   end;
procedure TGlobalVarStress.Execute;
var
   i : Integer;
   v : Variant;
   names : array [0..15] of String;
begin
   FreeOnTerminate:=False;
   for i:=0 to High(names) do
      names[i]:=IntToHex(i, 4);
   for i:=1 to 50000 do begin
      vGlobals.TryRead(names[(i + 60) and High(names)], v);
      vGlobals.Write(names[i and High(names)], i, 0);
   end;
end;
procedure TdwsUtilsTests.MultiThreadedGlobalVars;
var
   i : Integer;
   threads : array [0..3] of TGlobalVarStress;
begin
   vGlobals.Initialize;
   try
      for i:=0 to High(threads) do
         threads[i]:=TGlobalVarStress.Create;
      for i:=0 to High(threads) do begin
         threads[i].WaitFor;
         threads[i].Free;
      end;
      CheckNotEquals('', vGlobals.NamesCommaText);
      vGlobals.Cleanup;
      CheckEquals('', vGlobals.NamesCommaText);
   finally
      vGlobals.Finalize;
   end;
end;

// Eratosthenes
//
type
   TSieveResult = class(TSimpleInt64List)
      procedure AddFind(const s : String);
   end;
procedure TSieveResult.AddFind(const s : String);
begin
   Add(StrToInt64(s));
end;
procedure TdwsUtilsTests.Eratosthenes;
const
   cMAX = 10000;
var
   i, j : Integer;
   v : Variant;
   si : String;
   primes : TSieveResult;
begin
   vGlobals.Initialize;
   try
      for i:=2 to cMAX do begin
         FastInt64ToStr(i, si);
         if not vGlobals.TryRead(si, v) then begin
            vGlobals.Write(si, 1, 0);
            j:=i+i;
            while j<=cMAX do begin
               vGlobals.Write(IntToStr(j), 0, 0);
               j:=j+i;
            end;
         end;
      end;
      for i:=2 to cMAX do begin
         FastInt64ToStr(i, si);
         if vGlobals.TryRead(si, v) and (v=0) then
            vGlobals.Delete(si);
      end;
      primes:=TSieveResult.Create;
      try
         vGlobals.EnumerateNames('*', primes.AddFind);
         primes.Sort;
         CheckEquals(1229, primes.Count);
         CheckEquals(2, primes[0]);
         CheckEquals(3, primes[1]);
         CheckEquals(5, primes[2]);
         CheckEquals(7, primes[3]);
         CheckEquals(9967, primes[1227]);
         CheckEquals(9949, primes[1226]);
         CheckEquals(9973, primes[1228]);
      finally
         primes.Free;
      end;
   finally
      vGlobals.Finalize;
   end;
end;

// GlobalVarsCollect
//
procedure TdwsUtilsTests.GlobalVarsCollect;
var
   i, k : Integer;
   v : Variant;
   gv : TGlobalVars;
   t : Int64;
begin
   gv.Initialize;
   try
      for i:=1 to 10 do
         gv.Write('survivor'+IntToStr(i), i, 0);
      for i:=1 to 10000 do begin
         gv.Write(IntToStr(i), i, 1e-10);
         if (i and 1023)=0 then begin
            t:=GetSystemMilliseconds;
            while t=GetSystemMilliseconds do Sleep(10);
            for k:=1 to 30 do
               gv.IncrementalCollect;
            CheckEquals(10, gv.Count, IntToStr(i));
         end;
      end;
      for i:=1 to 10 do begin
         CheckTrue(gv.TryRead('survivor'+IntToStr(i), v));
         CheckEquals(i, v);
      end;
   finally
      gv.Finalize;
   end;
end;

// BytesWords
//
procedure TdwsUtilsTests.BytesWords;
var
   buf : String;
begin
   buf := 'Example';
   StringBytesToWords(buf, False);
   CheckEquals('4500780061006d0070006c006500', dwsUtils.BinToHex(ScriptStringToRawByteString(buf)), 'bytes to words no swap');

   StringWordsToBytes(buf, False);
   CheckEquals('Example', buf, 'words to bytes no swap');

   StringBytesToWords(buf, True);
   CheckEquals('004500780061006d0070006c0065', dwsUtils.BinToHex(ScriptStringToRawByteString(buf)), 'bytes to words with swap');

   StringWordsToBytes(buf, True);
   CheckEquals('Example', buf, 'words to bytes with swap');
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
