unit UdwsUtilsTests;

interface

uses Windows, Classes, SysUtils, TestFrameWork, dwsUtils;

type

   TdwsUtilsTests = class (TTestCase)
      private

      public

      published

         procedure StackIntegerTest;
         procedure WriteOnlyBlockStreamTest;
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

// StackIntegerTest
//
procedure TdwsUtilsTests.StackIntegerTest;
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

   CheckEquals(456, stack.Pop);

   CheckEquals(1, stack.Count);

   CheckEquals(123, stack.Pop);

   CheckEquals(0, stack.Count);

   stack.Free;
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

   CheckEquals('123456789', bs);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('dwsUtilsTests', TdwsUtilsTests.Suite);

end.
