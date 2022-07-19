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
unit dwsTabular;

{$i dws.inc}

{.$define TABULAR_SINGLES}

{$ifdef WIN64}
   {$define ENABLE_JIT64}
{$endif}


interface

uses SysUtils, dwsUtils
   {$ifdef ENABLE_JIT64}
   , dwsJIT, dwsJITFixups, dwsJITx86_64, dwsJITx86Intrinsics
   {$endif};

type

   EdwsTabular = class (Exception);

   PdwsTabularNumberStats = ^TdwsTabularNumberStats;
   TdwsTabularNumberStats = record
      public
         Mini, Maxi : Double;
         Average, Sum, SumOfSquares : Double;
         Count : Integer;
   end;

   TdwsTabularNumber = {$ifdef TABULAR_SINGLES} Single {$else} Double {$endif};
   PdwsTabularNumber = ^TdwsTabularNumber;
   TdwsTabularNumberArray = array of TdwsTabularNumber;
   TdwsTabularNumberStaticArray = array [0..MaxInt div SizeOf(TdwsTabularNumber)-1] of TdwsTabularNumber;
   PdwsTabularNumberArray = ^TdwsTabularNumberStaticArray;

   TdwsTabularColumnValue = ( tcvNumeric, tcvString );
   TdwsTabularColumnValues = set of TdwsTabularColumnValue;

   TdwsTabularColumn = class
      private
         FNumbers : TdwsTabularNumberArray;
         FStrings : TStringDynArray;
         FUnifier : TStringUnifier;

         FCount : Integer;
         FCapacity : Integer;
         FCountEmpty : Integer;
         FCountNonNumeric : Integer;

         FValues : TdwsTabularColumnValues;
         FNumberStats : PdwsTabularNumberStats;

      protected
         procedure ClearStats; inline;
         procedure SetCount(n : Integer);
         procedure Grow(minCount : Integer);

      public
         constructor Create(aValues : TdwsTabularColumnValues = [ tcvNumeric, tcvString ]);
         destructor Destroy; override;

         property Numbers : TdwsTabularNumberArray read FNumbers;
         function NumbersDataPtr : PdwsTabularNumberArray;
         property Strings : TStringDynArray read FStrings;
         function StringsDataPtr : PStringArray;

         procedure Add(const v : String); overload;
         procedure Add(const v : TdwsTabularNumber); overload;
         procedure Add(const s : String; d : TdwsTabularNumber); overload;
         procedure Add(const values : TdwsTabularNumberArray); overload;
         procedure AddNull;

         property Values : TdwsTabularColumnValues read FValues;

         property Count : Integer read FCount write SetCount;
         property CountEmpty : Integer read FCountEmpty;
         property CountNonNumeric : Integer read FCountNonNumeric;

         function NumberStats : TdwsTabularNumberStats;

         function CountDistinctStrings : Integer;
         function DistinctStrings : TStringDynArray;
   end;

   TdwsTabularColumns = array of TdwsTabularColumn;

   {$ifdef ENABLE_JIT64}
   TdwsTabularJIT = class;
   {$else}
   TdwsTabularJIT = class end;
   {$endif}

   // 32 is the number of bytes in an YMM register
   TdwsTabularBatchResult = array [0..32 div SizeOf(TdwsTabularNumber) - 1] of TdwsTabularNumber;
   PdwsTabularBatchResult = ^TdwsTabularBatchResult;

   TdwsTabular = class
      private
         FColumnNames : TStringDynArray;
         FColumnData : TdwsTabularColumns;
         FSharedCount : Integer;

      protected
         procedure EnsureNotShared(const attemptedAction : String); inline;

      public
         destructor Destroy; override;

         function AddColumn(const name : String; aValues : TdwsTabularColumnValues = [ tcvNumeric, tcvString ]) : TdwsTabularColumn;
         function DropColumn(const name : String) : Boolean;
         function IndexOfColumn(const name : String) : Integer;
         function ColumnByName(const name : String) : TdwsTabularColumn;

         function NumbersPtrOfColumn(const name : String) : Pointer;
         function StringsPtrOfColumn(const name : String) : Pointer;

         property ColumnNames : TStringDynArray read FColumnNames;
         property ColumnData : TdwsTabularColumns read FColumnData;

         function RowCount : Integer;
         function ColumnCount : Integer;

         property SharedCount : Integer read FSharedCount;
         procedure IncSharedCount;
         procedure DecSharedCount;
   end;

   TdwsTabularStack = class
      private
         FRowIndex : NativeInt;
         FData : TdwsTabularNumberArray;
         FStackPtr : PdwsTabularNumber;
         FStackSize : Integer;

      protected
         function GetPeek : TdwsTabularNumber; inline;
         procedure SetPeek(const val : TdwsTabularNumber); inline;

      public
         constructor Create(stackSize : Integer = 256);

         property RowIndex : NativeInt read FRowIndex write FRowIndex;
         property StackSize : Integer read FStackSize;
         property Peek : TdwsTabularNumber read GetPeek write SetPeek;
         property PeekPtr : PdwsTabularNumber read FStackPtr;

         procedure Clear;
         procedure Push(const v : TdwsTabularNumber); inline;
         function Pop : TdwsTabularNumber; inline;
         function DecStackPtr : PdwsTabularNumber; inline;
   end;

   PdwsTabularOpcode = ^TdwsTabularOpcode;
   TdwsTabularOpcode = record
      Method : procedure (stack : TdwsTabularStack; op : PdwsTabularOpcode);
      Operand1, Operand2 : TdwsTabularNumber;
      NumberPtr : PdwsTabularNumberArray;
      StringPtr : PStringArray;
      Lookup : TObject;
      StackDepth : Integer;
   end;
   TdwsTabularOpcodes = array of TdwsTabularOpcode;

   TdwsNameValueBucket = record
      Name : String;
      Value : TdwsTabularNumber;
   end;
   TdwsNameValues = class (TSimpleHash<TdwsNameValueBucket>)
      protected
         function SameItem(const item1, item2 : TdwsNameValueBucket) : Boolean; override;
         function GetItemHashCode(const item1 : TdwsNameValueBucket) : Cardinal; override;

         function GetValues(const name : String) : TdwsTabularNumber;
         procedure SetValues(const name : String; const v : TdwsTabularNumber);

      public
         class procedure DoLookup(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;

         procedure FromJSON(const j : String);
         function ToJSON : String;

         property Values[const name : String] : TdwsTabularNumber read GetValues write SetValues;
   end;

   TdwsLinearNameValues = class
      private
         FNames : TStringDynArray;
         FValues : TdwsTabularNumberArray;
         FCount : Integer;
         FUnified : Boolean;

      protected
         procedure Add(const name : String; const value : TdwsTabularNumber);
         procedure Sort;
         function Compare(a, b : NativeInt) : Integer;
         procedure Swap(a, b : NativeInt);

      public
         constructor Create(nv : TdwsNameValues; unifier : TStringUnifier);

         property Count : Integer read FCount;

         class procedure DoLookup(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoLookupUnified(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
   end;

   TdwsTabularExpressionAggreggate = (
      tegSum, tegMax, tegMin
   );
   TdwsTabularExpressionAggreggates = array of TdwsTabularExpressionAggreggate;

   TdwsTabularExpression = class
      private
         FTabular : TdwsTabular;
         FOpcodes : TdwsTabularOpcodes;
         FOpcodeCount : Integer;
         FObjectBag : array of TObject;
         FStackDepth : Integer;
         FMaxStackDepth : Integer;

         {$ifdef ENABLE_JIT64}
         FCodeBlock : TdwsJITCodeBlock;
         FCodePtr : Pointer;
         FJITAggregates : TdwsTabularExpressionAggreggates;
         {$endif}
         FJIT : TdwsTabularJIT;
         FJITHasCalls : Boolean;

      protected
         function AddOpCode : PdwsTabularOpcode;
         procedure DeleteLastOpCode;

         procedure StackDelta(delta : Integer);

         function LastOpCode : PdwsTabularOpcode;
         function LastOpCodeIsConst : Boolean;
         function LastOpCodeIsMultConst : Boolean;
         function PrevOpCode : PdwsTabularOpcode;
         function PrevOpCodeIsMultConst : Boolean;

         class procedure DoPushConst(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoPushNumField(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoPushNumFieldDef(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoDup(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoMultAddConst(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoMultConstAdd(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoAdd(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoAddConst(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoSub(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoMult(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoMultConst(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoDiv(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoMin(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoMax(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoReLu(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoSMAPE(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoLn(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoExp(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoDouble(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoSqr(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoSqrt(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoAbs(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoGTRE(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoGTREConst(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoEqual(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoNotEqual(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoRound(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoFloor(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;
         class procedure DoCeil(stack : TdwsTabularStack; op : PdwsTabularOpcode); static;

         {$ifdef ENABLE_JIT64}
         procedure JITCall(stack : TdwsTabularStack; var result : TdwsTabularBatchResult; ops : PdwsTabularOpcode);
         {$endif}

      public
         constructor Create(aTabular : TdwsTabular; aJIT : TdwsTabularJIT);
         destructor Destroy; override;

         property Tabular : TdwsTabular read FTabular;
         property JIT : TdwsTabularJIT read FJIT;

         procedure Opcode(const code : String);
         procedure Clear;

         procedure PushConst(const c : TdwsTabularNumber);
         procedure PushNumField(const name : String);
         procedure PushNumFieldDef(const name : String; const default : TdwsTabularNumber);
         procedure PushLookupField(const name : String; values : TdwsNameValues; const default : TdwsTabularNumber);
         procedure Dup(depth : Integer);

         procedure MultAddConst(const m, a : TdwsTabularNumber);
         procedure Add;
         procedure Sub;
         procedure Mult;
         procedure Divide;
         procedure Min;
         procedure Max;
         procedure ReLu;
         procedure SMAPE;
         procedure Ln;
         procedure Exp;
         procedure Sqr;
         procedure Sqrt;
         procedure Abs;
         procedure GreaterOrEqual;
         procedure Equal;
         procedure NotEqual;
         procedure Round;
         procedure Floor;
         procedure Ceil;

         property MaxStackDepth : Integer read FMaxStackDepth;
         property StackDepth : Integer read FStackDepth;

         function JITCompile(const batchAggregates : TdwsTabularExpressionAggreggates) : Boolean;

         function Evaluate(stack : TdwsTabularStack) : TdwsTabularNumber;

         function EvaluateAggregate(aggregate : TdwsTabularExpressionAggreggate;
                                    fromIndex : NativeInt = 0; toIndex : NativeInt = -1) : Double;
         function EvaluateAll : TdwsTabularNumberArray;
   end;

   {$ifdef ENABLE_JIT64}
   TdwsTabularJIT = class (TdwsJITx86_64)
      private

      protected

      public
         constructor Create; override;
         function JITExpression(expr : TdwsTabularExpression; const batchAggregates : TdwsTabularExpressionAggreggates) : Boolean;
   end;
   {$endif}

var
   vTabularDisableJIT : Boolean = False;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses System.Math, dwsJSON, dwsXXHash, dwsXPlatform;

{$R-}
{$Q-}

var
   vDotFormatSettings : TFormatSettings;

const
   cEmptyFloatAsInt : {$ifdef TABULAR_SINGLES}
                      Cardinal = $80000000;
                      {$else}
                      UInt64 = $8000000000000000;
                      {$endif}

function EmptyFloat : TdwsTabularNumber; inline;
begin
   Result := PdwsTabularNumber(@cEmptyFloatAsInt)^;
end;

function IsEmptyFloat(var p : TdwsTabularNumber) : Boolean; inline;
begin
   {$ifdef TABULAR_SINGLES}
   Result := PCardinal(@p)^ = cEmptyFloatAsInt;
   {$else}
   Result := PUInt64(@p)^ = cEmptyFloatAsInt;
   {$endif}
end;

// ------------------
// ------------------ TdwsTabularColumn ------------------
// ------------------

// Create
//
constructor TdwsTabularColumn.Create(aValues : TdwsTabularColumnValues = [ tcvNumeric, tcvString ]);
begin
   inherited Create;
   FValues := aValues;
   if tcvString in aValues then
      FUnifier := TStringUnifier.Create;
end;

// Destroy
//
destructor TdwsTabularColumn.Destroy;
begin
   ClearStats;
   FUnifier.Free;
   inherited;
end;

// ClearStats
//
procedure TdwsTabularColumn.ClearStats;
begin
   if FNumberStats <> nil then begin
      FreeMem(FNumberStats);
      FNumberStats := nil;
   end;
end;

// SetCount
//
procedure TdwsTabularColumn.SetCount(n : Integer);
var
   buf : Double;
begin
   Assert((n >= 0) and (n <= Count), 'only removing rows is supported');
   if n < Count then begin
      if tcvString in Values then begin
         for var k := n-1 to Count-1 do begin
            if FStrings[k] = '' then
               Dec(FCountEmpty)
            else if not TryStrToDouble(FStrings[k], buf) then
               Dec(FCountNonNumeric);
         end;
         SetLength(FStrings, n);
         if tcvNumeric in Values then
            SetLength(FNumbers, n);
      end else if tcvNumeric in Values then begin
         for var k := n-1 to Count-1 do begin
            if IsEmptyFloat(FNumbers[k]) then
               Dec(FCountEmpty);
         end;
         SetLength(FNumbers, n);
      end;
      FCount := n;
      FCapacity := n;
   end;
end;

// Grow
//
procedure TdwsTabularColumn.Grow(minCount : Integer);
begin
   FCapacity := FCapacity + FCapacity div 4 + 8;
   if FCapacity < minCount then
      FCapacity := minCount + 8;
   if tcvNumeric in Values then
      SetLength(FNumbers, FCapacity);
   if tcvString in Values then
      SetLength(FStrings, FCapacity);
end;

// DataPtr
//
function TdwsTabularColumn.NumbersDataPtr : PdwsTabularNumberArray;
begin
   Result := PdwsTabularNumberArray(FNumbers);
end;

// StringsDataPtr
//
function TdwsTabularColumn.StringsDataPtr : PStringArray;
begin
   Result := PStringArray(FStrings);
end;

// Add
//
procedure TdwsTabularColumn.Add(const v : String);
var
   d : Double;
begin
   if tcvNumeric in Values then begin
      if v = '' then
         Add('', EmptyFloat)
      else if TryStrToDouble(v, d) then
         Add(v, d)
      else Add(v, 0);
   end else if tcvString in Values then
      Add(v, 0);
end;

// AddNull
//
procedure TdwsTabularColumn.AddNull;
begin
   Add('', EmptyFloat);
end;

// Add
//
procedure TdwsTabularColumn.Add(const v : TdwsTabularNumber);

   procedure AddWithString;
   begin
      var buf : String;
      FastFloatToStr(v, buf, vDotFormatSettings);
      Add(buf, v);
   end;

begin
   if tcvString in Values then
      AddWithString
   else Add('', v);
end;

// Add
//
procedure TdwsTabularColumn.Add(const s : String; d : TdwsTabularNumber);
var
   n : Integer;
   isEmpty : Boolean;
begin
   ClearStats;
   n := FCount;
   FCount := n+1;
   if FCount >= FCapacity then
      Grow(FCount);

   if tcvString in Values then begin
      FUnifier.UnifyAssign(s, FStrings[n]);
      isEmpty := (s = '');
   end else isEmpty := False;
   if tcvNumeric in Values then begin
      if isEmpty then
         FNumbers[n] := EmptyFloat
      else begin
         FNumbers[n] := d;
         isEmpty := IsEmptyFloat(FNumbers[n]);
      end;
   end;
   if isEmpty then
      Inc(FCountEmpty);
end;

// Add
//
procedure TdwsTabularColumn.Add(const values : TdwsTabularNumberArray);
begin
   for var v in values do
      Add(v);
end;

// NumberStats
//
function TdwsTabularColumn.NumberStats : TdwsTabularNumberStats;
begin
   if not (tcvNumeric in Values) then
      raise EdwsTabular.Create('Column is not numeric');
   if FNumberStats = nil then begin
      FNumberStats := AllocMem(SizeOf(TdwsTabularNumberStats));
      FNumberStats.Count := FCount;
      if FCount > 0 then begin
         var v := FNumbers[0];
         FNumberStats.Mini := v;
         FNumberStats.Maxi := v;
         FNumberStats.Sum := v;
         FNumberStats.SumOfSquares := v*v;
         for var i := 1 to FCount-1 do begin
            v := FNumbers[i];
            if v < FNumberStats.Mini then FNumberStats.Mini := v;
            if v > FNumberStats.Maxi then FNumberStats.Maxi := v;
            FNumberStats.Sum := FNumberStats.Sum + v;
            FNumberStats.SumOfSquares := FNumberStats.SumOfSquares + v*v;
         end;
         FNumberStats.Average := FNumberStats.Sum / FCount;
      end;
   end;
   Result := FNumberStats^;
end;

// CountDistinctStrings
//
function TdwsTabularColumn.CountDistinctStrings : Integer;
begin
   if tcvString in Values then
      Result := FUnifier.Count
   else Result := 0;
end;

// DistinctStrings
//
function TdwsTabularColumn.DistinctStrings : TStringDynArray;
begin
   if tcvString in Values then
      Result := FUnifier.DistinctStrings;
end;

// ------------------
// ------------------ TdwsTabular ------------------
// ------------------

// EnsureNotShared
//
procedure TdwsTabular.EnsureNotShared(const attemptedAction : String);
begin
   if SharedCount > 0 then
      raise EdwsTabular.CreateFmt('Cannot %s on shared TabularData', [ attemptedAction ]);
end;

// Destroy
//
destructor TdwsTabular.Destroy;
begin
   EnsureNotShared('Destroy');
   for var i := 0 to High(FColumnData) do
      FColumnData[i].Free;
   inherited;
end;

// AddColumn
//
function TdwsTabular.AddColumn(const name : String; aValues : TdwsTabularColumnValues = [ tcvNumeric, tcvString ]) : TdwsTabularColumn;
var
   n : Integer;
begin
   EnsureNotShared('AddColumn');
   Result := TdwsTabularColumn.Create(aValues);
   n := Length(FColumnNames);
   SetLength(FColumnNames, n+1);
   SetLength(FColumnData, n+1);
   FColumnNames[n] := name;
   FColumnData[n] := Result;
end;

// DropColumn
//
function TdwsTabular.DropColumn(const name : String) : Boolean;
begin
   EnsureNotShared('DropColumn');
   var idx := IndexOfColumn(name);
   if idx >= 0 then begin
      var col := FColumnData[idx];
      Delete(FColumnData, idx, 1);
      Delete(FColumnNames, idx, 1);
      col.Free;
      Result := True;
   end else Result := False;
end;

// IndexOfColumn
//
function TdwsTabular.IndexOfColumn(const name : String) : Integer;
begin
   for var i := 0 to High(FColumnNames) do
      if FColumnNames[i] = name then Exit(i);
   Result := -1;
end;

// ColumnByName
//
function TdwsTabular.ColumnByName(const name : String) : TdwsTabularColumn;
begin
   var i := IndexOfColumn(name);
   if i >= 0 then
      Result := FColumnData[i]
   else Result := nil;
end;

// NumbersPtrOfColumn
//
function TdwsTabular.NumbersPtrOfColumn(const name : String) : Pointer;
begin
   var i := IndexOfColumn(name);
   if i < 0 then
      raise EdwsTabular.CreateFmt('Unknown column "%s"', [ name ]);
   Result := FColumnData[i].NumbersDataPtr;
end;

// StringsPtrOfColumn
//
function TdwsTabular.StringsPtrOfColumn(const name : String) : Pointer;
begin
   var i := IndexOfColumn(name);
   if i < 0 then
      raise EdwsTabular.CreateFmt('Unknown column "%s"', [ name ]);
   Result := FColumnData[i].StringsDataPtr;
end;

// RowCount
//
function TdwsTabular.RowCount : Integer;
begin
   if Length(FColumnData) > 0 then
      Result := FColumnData[0].Count
   else Result := 0;
end;

// ColumnCount
//
function TdwsTabular.ColumnCount : Integer;
begin
   Result := Length(FColumnData);
end;

// IncSharedCount
//
procedure TdwsTabular.IncSharedCount;
begin
   AtomicIncrement(FSharedCount);
end;

// DecSharedCount
//
procedure TdwsTabular.DecSharedCount;
begin
   AtomicDecrement(FSharedCount);
end;

// ------------------
// ------------------ TdwsTabularStack ------------------
// ------------------

// Create
//
constructor TdwsTabularStack.Create(stackSize : Integer = 256);
begin
   inherited Create;
   SetLength(FData, stackSize);
   FStackSize := stackSize;
   Clear;
end;

// Clear
//
procedure TdwsTabularStack.Clear;
begin
   FStackPtr := @FData[0];
   Dec(FStackPtr);
end;

// Push
//
procedure TdwsTabularStack.Push(const v : TdwsTabularNumber);
begin
   PdwsTabularNumberArray(FStackPtr)[1] := v;
   Inc(FStackPtr);
end;

// Pop
//
function TdwsTabularStack.Pop : TdwsTabularNumber;
begin
   Result := FStackPtr^;
   Dec(FStackPtr);
end;

// DecStackPtr
//
function TdwsTabularStack.DecStackPtr : PdwsTabularNumber;
begin
   Result := FStackPtr;
   Dec(Result);
   FStackPtr := Result;
end;

// GetPeek
//
function TdwsTabularStack.GetPeek : TdwsTabularNumber;
begin
   Result := FStackPtr^;
end;

// SetPeek
//
procedure TdwsTabularStack.SetPeek(const val : TdwsTabularNumber);
begin
   FStackPtr^ := val;
end;

// ------------------
// ------------------ TdwsNameValues ------------------
// ------------------

// SameItem
//
function TdwsNameValues.SameItem(const item1, item2 : TdwsNameValueBucket) : Boolean;
begin
   Result := item1.Name = item2.Name;
end;

// GetItemHashCode
//
function TdwsNameValues.GetItemHashCode(const item1 : TdwsNameValueBucket) : Cardinal;
begin
   Result := SimpleStringHash(item1.Name);
end;

// GetValues
//
function TdwsNameValues.GetValues(const name : String) : TdwsTabularNumber;
var
   bucket : TdwsNameValueBucket;
begin
   bucket.Name := name;
   if Match(bucket) then
      Result := bucket.Value
   else Result := 0;
end;

// SetValues
//
procedure TdwsNameValues.SetValues(const name : String; const v : TdwsTabularNumber);
var
   bucket : TdwsNameValueBucket;
begin
   bucket.Name := name;
   bucket.Value := v;
   Replace(bucket);
end;

// DoLookup
//
class procedure TdwsNameValues.DoLookup(stack : TdwsTabularStack; op : PdwsTabularOpcode);
var
   bucket : TdwsNameValueBucket;
begin
   bucket.Name := op.StringPtr[stack.RowIndex];
   if TdwsNameValues(op.Lookup).Match(bucket) then
      stack.Push(bucket.Value)
   else stack.Push(op.Operand1);
end;

// FromJSON
//
procedure TdwsNameValues.FromJSON(const j : String);
begin
   var jv := TdwsJSONValue.ParseString(j);
   try
      if jv.ValueType <> jvtObject then
         raise EdwsTabular.CreateFmt('JSON object expected but got %s', [ j ]);
      for var i := 0 to jv.ElementCount-1 do begin
         Values[jv.Names[i]] := jv.Elements[i].AsNumber;
      end;
   finally
      jv.Free;
   end;
end;

// ToJSON
//
function TdwsNameValues.ToJSON : String;
begin
   var wr := TdwsJSONWriter.Create;
   try
      wr.BeginObject;
         var bucket : TdwsNameValueBucket;
         for var i := 0 to Capacity-1 do
            if HashBucketValue(i, bucket) then
               wr.WriteName(bucket.Name).WriteNumber(bucket.Value);
      wr.EndObject;
      Result := wr.ToString;
   finally
      wr.Free;
   end;
end;

// ------------------
// ------------------ TdwsLinearNameValues ------------------
// ------------------

// Create
//
constructor TdwsLinearNameValues.Create(nv : TdwsNameValues; unifier : TStringUnifier);
var
   bucket : TdwsNameValueBucket;
   i : Integer;
   buf : String;
begin
   inherited Create;
   FUnified := unifier <> nil;
   for i := 0 to nv.Capacity-1 do
      if nv.HashBucketValue(i, bucket) then
         if FUnified then begin
            unifier.UnifyAssign(bucket.Name, buf);
            Add(buf, bucket.Value)
         end else Add(bucket.Name, bucket.Value);
   if not FUnified then
      Sort;
end;

// DoLookup
//
class procedure TdwsLinearNameValues.DoLookup(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var ps : PString := @(op.StringPtr[stack.RowIndex]);
   if ps^ <> '' then begin
      var top := 0;
      var lnv := TdwsLinearNameValues(op.Lookup);
      var bottom := lnv.FCount - 1;
      while top <= bottom do begin
         var mid := (top + bottom) shr 1;
         var c := CompareStr(lnv.FNames[mid], ps^);
         if c < 0 then
            top := mid + 1
         else begin
            bottom := mid - 1;
            if c = 0 then begin
               stack.Push(lnv.FValues[mid]);
               Exit;
            end;
         end;
      end;
   end;
   stack.Push(op.Operand1);
end;

// DoLookupUnified
//
class procedure TdwsLinearNameValues.DoLookupUnified(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := Pointer(op.StringPtr[stack.RowIndex]);
   if p <> nil then begin
      var lnv := TdwsLinearNameValues(op.Lookup);
      for var i := lnv.FCount-1 downto 0 do begin
         if Pointer(lnv.FNames[i]) = p then begin
            stack.Push(lnv.FValues[i]);
            Exit;
         end;
      end;
   end;
   stack.Push(op.Operand1);
end;

// Add
//
procedure TdwsLinearNameValues.Add(const name : String; const value : TdwsTabularNumber);
begin
   var n := FCount;
   FCount := n+1;
   SetLength(FNames, FCount);
   SetLength(FValues, FCount);
   FNames[n] := name;
   FValues[n] := value;
end;

// Sort
//
procedure TdwsLinearNameValues.Sort;
begin
   var qs : TQuickSort;
   qs.CompareMethod := Compare;
   qs.SwapMethod := Swap;
end;

// Compare
//
function TdwsLinearNameValues.Compare(a, b : NativeInt) : Integer;
begin
   Result := CompareStr(FNames[a], FNames[b]);
end;

// Swap
//
procedure TdwsLinearNameValues.Swap(a, b : NativeInt);
var
   bufP : Pointer;
   bufV : TdwsTabularNumber;
begin
   bufP := PPointer(@FNames[a])^;
   PPointer(@FNames[a])^ := PPointer(@FNames[b])^;
   PPointer(@FNames[b])^ := bufP;

   bufV := FValues[a];
   FValues[a] := FValues[b];
   FValues[b] := bufV;
end;

// ------------------
// ------------------ TdwsTabularExpression ------------------
// ------------------

// Create
//
constructor TdwsTabularExpression.Create(aTabular : TdwsTabular; aJIT : TdwsTabularJIT);
begin
   inherited Create;
   FTabular := aTabular;
   FJIT := aJIT;
end;

// Destroy
//
destructor TdwsTabularExpression.Destroy;
begin
   Clear;
   inherited;
end;

// Opcode
//
procedure TdwsTabularExpression.Opcode(const code : String);

   procedure RaiseSyntaxError;
   begin
      raise EdwsTabular.CreateFmt('Syntax error for "%s"', [ code ]);
   end;

   procedure ParseNum;
   var
      v : Double;
   begin
      if TryStrToDouble(code, v) then
         PushConst(v)
      else RaiseSyntaxError;
   end;

   procedure ParseField;
   var
      v : Double;
      name, tail : String;
   begin
      var p := Pos('"', code, 2);
      if p <= 0 then RaiseSyntaxError;
      name := Copy(code, 2, p-2);
      tail := Copy(code, p+1);
      if tail = '' then begin
         PushNumField(name);
         Exit;
      end;
      if tail[1] <> ' ' then RaiseSyntaxError;
      p := Pos(' ', tail, 2);
      if p <= 0 then begin
         if not TryStrToDouble(@tail[2], v) then
            RaiseSyntaxError;
         PushNumFieldDef(name, v);
      end else begin
         if not TryStrToDouble(@tail[2], v) then
            RaiseSyntaxError;
         var values := TdwsNameValues.Create;
         try
            values.FromJSON(Copy(tail, p+1));
         except
            values.Free;
            raise;
         end;
         PushLookupField(name, values, v);
      end;
   end;

begin
   var n := Length(code);
   case n of
      0 : Exit;
      1 : case code[1] of
            '+' : Add;
            '-' : Sub;
            '*' : Mult;
            '/' : Divide;
            '=' : Equal;
            '0'..'9' : PushConst(Ord(Code[1])-Ord('0'));
         else
            RaiseSyntaxError;
         end;
      2 : case code[1] of
            'l' :
               if code = 'ln' then Ln
               else RaiseSyntaxError;
            '>' :
               if code = '>=' then GreaterOrEqual
               else RaiseSyntaxError;
            '<' :
               if code = '<>' then NotEqual
               else RaiseSyntaxError;
            '0'..'9', '-', '.' : ParseNum;
         else
            RaiseSyntaxError;
         end;
      3 : case code[1] of
            'a' :
               if code = 'abs' then Abs
               else RaiseSyntaxError;
            'd' :
               if code = 'dup' then Dup(0)
               else RaiseSyntaxError;
            'e' :
               if code = 'exp' then Exp
               else RaiseSyntaxError;
            'm' :
               if code = 'min' then Min
               else if code = 'max' then Max
               else RaiseSyntaxError;
            's' :
               if code = 'sqr' then Sqr
               else RaiseSyntaxError;
            '"' : ParseField;
            '0'..'9', '-', '.' : ParseNum;
         else
            RaiseSyntaxError
         end;
      4 : case code[1] of
            'c' :
               if code = 'ceil' then Ceil
               else RaiseSyntaxError;
            'd' :
               if StrBeginsWith(code, 'dup') then begin
                  var offset := Ord(code[4])-Ord('0');
                  if offset in [0..9] then
                     Dup(offset)
                  else RaiseSyntaxError;
               end else RaiseSyntaxError;
            'r' :
               if code = 'relu' then ReLu
               else RaiseSyntaxError;
            's' :
               if code = 'sqrt' then Sqrt
               else RaiseSyntaxError;
            '"' : ParseField;
            '0'..'9', '-', '.' : ParseNum;
         else
            RaiseSyntaxError
         end;
      5 : case code[1] of
            'f' :
               if code = 'floor' then Floor
               else RaiseSyntaxError;
            'r' :
               if code = 'round' then Round
               else RaiseSyntaxError;
            's' :
               if code = 'smape' then SMAPE
               else RaiseSyntaxError;
            '"' : ParseField;
            '0'..'9', '-', '.' : ParseNum;
         else
            RaiseSyntaxError
         end;
   else
      case code[1] of
         '"' : ParseField;
         '0'..'9', '-', '.' : ParseNum;
      else
         RaiseSyntaxError
      end;
   end;
end;

// Clear
//
procedure TdwsTabularExpression.Clear;
begin
   for var i := 0 to High(FObjectBag) do
      FObjectBag[i].Free;
   SetLength(FObjectBag, 0);
   SetLength(FOpcodes, 0);

   {$ifdef ENABLE_JIT64}
   if FCodeBlock <> nil then begin
      FJITHasCalls := False;
      FCodePtr := nil;
      var sub := FCodeBlock.SubAllocator;
      FreeAndNil(FCodeBlock);
      JIT.Allocator.ReturnSpare(sub);
   end;
   {$endif}
end;

// AddOpCode
//
function TdwsTabularExpression.AddOpCode : PdwsTabularOpcode;
begin
   var n := Length(FOpcodes);
   if FOpcodeCount = n then
      SetLength(FOpcodes, n+16);
   Result := @FOpcodes[FOpcodeCount];
   Result.StackDepth := StackDepth;
   Inc(FOpcodeCount);
end;

// DeleteLastOpCode
//
procedure TdwsTabularExpression.DeleteLastOpCode;
begin
   Assert(FOpcodeCount > 0);
   Dec(FOpcodeCount);
end;

// StackDelta
//
procedure TdwsTabularExpression.StackDelta(delta : Integer);
begin
   FStackDepth := FStackDepth + delta;
   if FStackDepth <= 0 then
      EdwsTabular.Create('Unbalanced opcode');
   if FStackDepth > FMaxStackDepth then
      FMaxStackDepth := FStackDepth;
end;

// LastOpCode
//
function TdwsTabularExpression.LastOpCode : PdwsTabularOpcode;
begin
   var n := FOpcodeCount-1;
   if n >= 0 then
      Result := @FOpcodes[n]
   else Result := nil;
end;

// LastOpCodeIsConst
//
function TdwsTabularExpression.LastOpCodeIsConst : Boolean;
begin
   var n := FOpcodeCount-1;
   Result := (n >= 0) and (@FOpcodes[n].Method = @DoPushConst);
end;

// LastOpCodeIsMultConst
//
function TdwsTabularExpression.LastOpCodeIsMultConst : Boolean;
begin
   var n := FOpcodeCount-1;
   Result := (n >= 0) and (@FOpcodes[n].Method = @DoMultConst);
end;

// PrevOpCode
//
function TdwsTabularExpression.PrevOpCode : PdwsTabularOpcode;
begin
   var n := FOpcodeCount-2;
   if n >= 0 then
      Result := @FOpcodes[n]
   else Result := nil;
end;

// PrevOpCodeIsMultConst
//
function TdwsTabularExpression.PrevOpCodeIsMultConst : Boolean;
begin
   var n := FOpcodeCount-2;
   Result := (n >= 0) and (@FOpcodes[n].Method = @DoMultConst);
end;

// DoPushConst
//
class procedure TdwsTabularExpression.DoPushConst(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   stack.Push(op.Operand1);
end;

// DoPushNumField
//
class procedure TdwsTabularExpression.DoPushNumField(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   stack.Push(op.NumberPtr[stack.RowIndex]);
end;

// DoPushNumFieldDef
//
class procedure TdwsTabularExpression.DoPushNumFieldDef(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var i := stack.RowIndex;
   if IsEmptyFloat(op.NumberPtr[i]) then
      stack.Push(op.Operand1)
   else stack.Push(op.NumberPtr[i]);
end;

// DoDup
//
class procedure TdwsTabularExpression.DoDup(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   stack.Push(PdwsTabularNumber(IntPtr(stack.PeekPtr)-PInteger(@op.Operand1)^)^);
end;

// DoMultConst
//
class procedure TdwsTabularExpression.DoMultAddConst(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.PeekPtr;
   p^ := p^ * op.Operand1 + op.Operand2;
end;

// DoMultConstAdd
//
class procedure TdwsTabularExpression.DoMultConstAdd(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   stack.PeekPtr^ := stack.Pop * op.Operand1 + stack.PeekPtr^;
end;

// DoAdd
//
class procedure TdwsTabularExpression.DoAdd(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.DecStackPtr;
   p^ := p^ + PdwsTabularNumberArray(p)[1];
end;

// DoAddConst
//
class procedure TdwsTabularExpression.DoAddConst(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   stack.PeekPtr^ := stack.PeekPtr^ + op.Operand1;
end;

// DoSub
//
class procedure TdwsTabularExpression.DoSub(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.DecStackPtr;
   p^ := p^ - PdwsTabularNumberArray(p)[1];
end;

// DoMult
//
class procedure TdwsTabularExpression.DoMult(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   stack.Peek := stack.Pop * stack.Peek;
end;

// DoMultConst
//
class procedure TdwsTabularExpression.DoMultConst(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   stack.PeekPtr^ := stack.PeekPtr^ * op.Operand1;
end;

// DoDiv
//
class procedure TdwsTabularExpression.DoDiv(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.DecStackPtr;
   p^ := p^ / PdwsTabularNumberArray(p)[1];
end;

// DoMin
//
class procedure TdwsTabularExpression.DoMin(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.DecStackPtr;
   p^ := System.Math.Min(p^, PdwsTabularNumberArray(p)[1]);
end;

// DoMax
//
class procedure TdwsTabularExpression.DoMax(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.DecStackPtr;
   p^ := System.Math.Max(p^, PdwsTabularNumberArray(p)[1]);
end;

// DoReLu
//
class procedure TdwsTabularExpression.DoReLu(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.DecStackPtr;
   p^ := System.Math.Max(0, p^ + PdwsTabularNumberArray(p)[1]);
end;

// DoSMAPE
//
class procedure TdwsTabularExpression.DoSMAPE(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p1 := stack.DecStackPtr;
   var p2 : PdwsTabularNumber := @PdwsTabularNumberArray(p1)[1];
   p1^ := System.Abs(p1^ - p2^) / (System.Abs(p1^) + System.Abs(p2^));
end;

// DoLn
//
class procedure TdwsTabularExpression.DoLn(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.PeekPtr;
   p^ := System.Ln(p^);
end;

// DoExp
//
class procedure TdwsTabularExpression.DoExp(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.PeekPtr;
   p^ := System.Exp(p^);
end;

// DoDouble
//
class procedure TdwsTabularExpression.DoDouble(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.PeekPtr;
   p^ := p^ + p^;
end;

// DoSqr
//
class procedure TdwsTabularExpression.DoSqr(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.PeekPtr;
   p^ := p^ * p^;
end;

// DoSqrt
//
class procedure TdwsTabularExpression.DoSqrt(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.PeekPtr;
   p^ := System.Sqrt(p^);
end;

// DoAbs
//
class procedure TdwsTabularExpression.DoAbs(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.PeekPtr;
   p^ := System.Abs(p^);
end;

// DoGTRE
//
class procedure TdwsTabularExpression.DoGTRE(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   stack.Peek := Ord(stack.Pop >= stack.Peek)
end;

// DoGTREConst
//
class procedure TdwsTabularExpression.DoGTREConst(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.PeekPtr;
   p^ := Ord(p^ >= op.Operand1);
end;

// DoEqual
//
class procedure TdwsTabularExpression.DoEqual(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   stack.Peek := Ord(stack.Pop = stack.Peek)
end;

// DoNotEqual
//
class procedure TdwsTabularExpression.DoNotEqual(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   stack.Peek := Ord(stack.Pop <> stack.Peek)
end;

// DoRound
//
class procedure TdwsTabularExpression.DoRound(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.PeekPtr;
   p^ := System.Round(p^);
end;

// DoFloor
//
class procedure TdwsTabularExpression.DoFloor(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.PeekPtr;
   p^ := System.Math.Floor(p^);
end;

// DoCeil
//
class procedure TdwsTabularExpression.DoCeil(stack : TdwsTabularStack; op : PdwsTabularOpcode);
begin
   var p := stack.PeekPtr;
   p^ := System.Math.Ceil(p^);
end;

// PushConst
//
procedure TdwsTabularExpression.PushConst(const c : TdwsTabularNumber);
begin
   var op := AddOpCode;
   op.Method := @DoPushConst;
   op.Operand1 := c;
   StackDelta(1);
end;

// PushNumField
//
procedure TdwsTabularExpression.PushNumField(const name : String);
begin
   var op := AddOpCode;
   op.Method := @DoPushNumField;
   op.NumberPtr := Tabular.NumbersPtrOfColumn(name);
   StackDelta(1);
end;

// PushNumFieldDef
//
procedure TdwsTabularExpression.PushNumFieldDef(const name : String; const default : TdwsTabularNumber);
begin
   var op := AddOpCode;
   op.Method := @DoPushNumFieldDef;
   op.Operand1 := default;
   op.NumberPtr := Tabular.NumbersPtrOfColumn(name);
   op.StringPtr := Tabular.StringsPtrOfColumn(name);
   StackDelta(1);
end;

// PushLookupField
//
procedure TdwsTabularExpression.PushLookupField(const name : String; values : TdwsNameValues; const default : TdwsTabularNumber);
begin
   var op := AddOpCode;

   var column := Tabular.ColumnByName(name);

   var callJIT := True;

   var n := Length(FObjectBag);
   SetLength(FObjectBag, n+1);
   if values.Count <= 30 then begin
      var lnv := TdwsLinearNameValues.Create(values, column.FUnifier);
      if lnv.FUnified then begin
         op.Method := @lnv.DoLookupUnified;
         callJIT := False;
      end else op.Method := @lnv.DoLookup;
      op.Lookup := lnv;
      FObjectBag[n] := lnv;
      FreeAndNil(values);
   end else begin
      op.Method := @values.DoLookup;
      op.Lookup := values;
      FObjectBag[n] := values;
   end;

   op.StringPtr := column.StringsDataPtr;
   op.Operand1 := default;
   StackDelta(1);

   FJITHasCalls := FJITHasCalls or callJIT;
end;

// Dup
//
procedure TdwsTabularExpression.Dup(depth : Integer);
begin
   if FStackDepth <= depth then begin
      if depth = 0 then
         raise EdwsTabular.Create('Dup requires at least one element in the stack')
      else raise EdwsTabular.CreateFmt('Dup(%d) requires at least %d elements in the stack',
                                       [ depth, depth+1 ]);
   end;
   var op := AddOpCode;
   op.Method := @DoDup;
   PInteger(@op.Operand1)^ := depth * SizeOf(TdwsTabularNumber);
   StackDelta(1);
end;

// MultAddConst
//
procedure TdwsTabularExpression.MultAddConst(const m, a : TdwsTabularNumber);
begin
   var op := AddOpCode;
   op.Method := @DoMultAddConst;
   op.Operand1 := m;
   op.Operand2 := a;
end;

// Add
//
procedure TdwsTabularExpression.Add;
begin
   if LastOpCodeIsConst then begin
      if PrevOpCodeIsMultConst then begin
         PrevOpCode.Operand2 := LastOpCode.Operand1;
         PrevOpCode.Method := @DoMultAddConst;
         DeleteLastOpCode;
      end else begin
         LastOpCode.Method := @DoAddConst;
      end;
   end else if LastOpCodeIsMultConst then begin
      LastOpCode.Method := @DoMultConstAdd;
   end else begin
      AddOpCode.Method := @DoAdd;
   end;
   StackDelta(-1);
end;

// Sub
//
procedure TdwsTabularExpression.Sub;
begin
   if LastOpCodeIsConst then begin
      var p := LastOpCode;
      p.Operand1 := -p.Operand1;
      p.Method := @DoAddConst;
   end else begin
      AddOpCode.Method := @DoSub;
   end;
   StackDelta(-1);
end;

// Mult
//
procedure TdwsTabularExpression.Mult;
begin
   if LastOpCodeIsConst then begin
      var p := LastOpCode;
      if p.Operand1 = 2 then
         p.Method := @DoDouble
      else p.Method := @DoMultConst;
   end else begin
      AddOpCode.Method := @DoMult;
   end;
   StackDelta(-1);
end;

// Divide
//
procedure TdwsTabularExpression.Divide;
begin
   if LastOpCodeIsConst then begin
      var p := LastOpCode;
      p.Operand1 := 1 / p.Operand1;
      p.Method := @DoMultConst;
   end else begin
      AddOpCode.Method := @DoDiv;
   end;
   StackDelta(-1);
end;

// Min
//
procedure TdwsTabularExpression.Min;
begin
   AddOpCode.Method := @DoMin;
   StackDelta(-1);
end;

// Max
//
procedure TdwsTabularExpression.Max;
begin
   AddOpCode.Method := @DoMax;
   StackDelta(-1);
end;

// ReLu
//
procedure TdwsTabularExpression.ReLu;
begin
   AddOpCode.Method := @DoReLu;
   StackDelta(-1);
end;

// SMAPE
//
procedure TdwsTabularExpression.SMAPE;
begin
   AddOpCode.Method := @DoSMAPE;
   StackDelta(-1);
end;

// Ln
//
procedure TdwsTabularExpression.Ln;
begin
   AddOpCode.Method := @DoLn;
   FJITHasCalls := True;
end;

// Exp
//
procedure TdwsTabularExpression.Exp;
begin
   AddOpCode.Method := @DoExp;
   FJITHasCalls := True;
end;

// Sqr
//
procedure TdwsTabularExpression.Sqr;
begin
   AddOpCode.Method := @DoSqr;
end;

// Sqrt
//
procedure TdwsTabularExpression.Sqrt;
begin
   AddOpCode.Method := @DoSqrt;
end;

// Abs
//
procedure TdwsTabularExpression.Abs;
begin
   AddOpCode.Method := @DoAbs;
end;

// GreaterOrEqual
//
procedure TdwsTabularExpression.GreaterOrEqual;
begin
   if LastOpCodeIsConst then begin
      LastOpCode.Method := @DoGTREConst;
   end else begin
      AddOpCode.Method := @DoGTRE;
   end;
   StackDelta(-1);
end;

// Equal
//
procedure TdwsTabularExpression.Equal;
begin
   AddOpCode.Method := @DoEqual;
   StackDelta(-1);
end;

// NotEqual
//
procedure TdwsTabularExpression.NotEqual;
begin
   AddOpCode.Method := @DoNotEqual;
   StackDelta(-1);
end;

// Round
//
procedure TdwsTabularExpression.Round;
begin
   AddOpCode.Method := @DoRound;
end;

// Floor
//
procedure TdwsTabularExpression.Floor;
begin
   AddOpCode.Method := @DoFloor;
end;

// Ceil
//
procedure TdwsTabularExpression.Ceil;
begin
   AddOpCode.Method := @DoCeil;
end;

{$ifdef ENABLE_JIT64}
// JITCall
//
procedure TdwsTabularExpression.JITCall(stack : TdwsTabularStack; var result : TdwsTabularBatchResult; ops : PdwsTabularOpcode);
asm
   mov  rax, rcx
   mov  rcx, rdx
   mov  rdx, r8
   mov  r8, r9
   call [rax + OFFSET FCodePtr]
end;
{$endif}

// JITCompile
//
function TdwsTabularExpression.JITCompile(const batchAggregates : TdwsTabularExpressionAggreggates) : Boolean;
begin
   Result := False;
   {$ifdef ENABLE_JIT64}
   if vTabularDisableJIT or not Win64AVX2Supported then Exit;

   var jit := JIT;
   if jit = nil then Exit;

   Result := jit.JITExpression(Self, batchAggregates);

   if Result then begin
      FJITAggregates := Copy(batchAggregates);
      FCodeBlock := jit.CompiledOutput;
      FCodePtr := FCodeBlock.CodePtr;
      jit.Allocator.DetachSubAllocators;
   end else begin
      SetLength(FJITAggregates, 0);
      jit.Fixups.ClearFixups;
   end;
   {$endif}
end;

// Evaluate
//
function TdwsTabularExpression.Evaluate(stack : TdwsTabularStack) : TdwsTabularNumber;
begin
   if FStackDepth <> 1 then
      raise EdwsTabular.Create('Unbalanced opcodes');
   if stack.StackSize < MaxStackDepth then
      raise EdwsTabular.Create('Insufficient stack size');
   stack.Clear;
   var n := FOpcodeCount;
   var p : PdwsTabularOpcode := @FOpcodes[0];
   while n >= 4 do begin
      p.Method(stack, p);
      Inc(p);
      p.Method(stack, p);
      Inc(p);
      p.Method(stack, p);
      Inc(p);
      p.Method(stack, p);
      Inc(p);
      Dec(n, 4);
   end;
   while n > 0 do begin
      p.Method(stack, p);
      Inc(p);
      Dec(n);
   end;
   Result := stack.Peek;
end;

// EvaluateAggregate
//
function TdwsTabularExpression.EvaluateAggregate(aggregate : TdwsTabularExpressionAggreggate;
                                                 fromIndex : NativeInt = 0; toIndex : NativeInt = -1) : Double;

   {$ifdef ENABLE_JIT64}
   function BatchSumAggregate(stack : TdwsTabularStack; batches : Integer) : Double;
   var
      sum1, sum2, sum3 : Double;
      buf : TdwsTabularBatchResult;
   begin
      Result := 0;
      sum1 := 0;
      sum2 := 0;
      sum3 := 0;
      for batches := batches downto 1 do begin
         JITCall(stack, buf, @FOpcodes[0]);
         stack.RowIndex := stack.RowIndex + Length(buf);
         {$ifdef TABULAR_SINGLES}
         Result := Result + buf[0] + buf[1];
         sum1 := sum1 + buf[2] + buf[3];
         sum2 := sum2 + buf[4] + buf[5];
         sum3 := sum3 + buf[6] + buf[7];
         {$else}
         Result := Result + buf[0];
         sum1 := sum1 + buf[1];
         sum2 := sum2 + buf[2];
         sum3 := sum3 + buf[3];
         {$endif}
      end;
      Result := (Result + sum1) + (sum2 + sum3);
   end;

   function BatchJITAggregate(stack : TdwsTabularStack; batches : Integer) : Double;
   var
      buf : array of TdwsTabularBatchResult;
   begin
      SetLength(buf, Length(FJITAggregates));
      for var i := 0 to High(FJITAggregates) do begin
         case FJITAggregates[i] of
            tegSum : buf[i] := Default(TdwsTabularBatchResult);
         else
            Assert(False);
         end;
      end;
      var pbuf : PdwsTabularBatchResult := @buf[0];
      for batches := batches downto 1 do begin
         JITCall(stack, pbuf^, @FOpcodes[0]);
         stack.RowIndex := stack.RowIndex + Length(pbuf^);
      end;
      Result := (pbuf[0] + pbuf[1]) + (pbuf[2] + pbuf[3]);
   end;
   {$endif}

   function SumAggregate(stack : TdwsTabularStack; toIndex : NativeInt) : Double;
   begin
      {$ifdef ENABLE_JIT64}
      if FCodeBlock <> nil then
         if Length(FJITAggregates) > 0 then
            Result := BatchJITAggregate(stack, (toIndex-stack.RowIndex+1) div (High(TdwsTabularBatchResult)+1))
         else Result := BatchSumAggregate(stack, (toIndex-stack.RowIndex+1) div (High(TdwsTabularBatchResult)+1))
      else Result := 0;
      {$else}
      Result := 0;
      {$endif}
      for var i := stack.RowIndex to toIndex do begin
         stack.RowIndex := i;
         Result := Result + Evaluate(stack);
      end;
   end;

begin
   Assert(aggregate = tegSum);
   var stack := TdwsTabularStack.Create(MaxStackDepth);
   try
      if fromIndex > 0 then
         stack.RowIndex := fromIndex;
      if toIndex < 0 then
         toIndex := Tabular.RowCount + toIndex
      else if toIndex >= Tabular.RowCount-1 then
         toIndex := Tabular.RowCount-1;
      Result := SumAggregate(stack, toIndex);
   finally
      stack.Free
   end;
end;

// EvaluateAll
//
function TdwsTabularExpression.EvaluateAll : TdwsTabularNumberArray;

   {$ifdef ENABLE_JIT64}
   function BatchEvaluate(stack : TdwsTabularStack; batches : Integer; p : PdwsTabularNumber) : PdwsTabularNumber;
   begin
      for var i := 1 to batches do begin
         JITCall(stack, PdwsTabularBatchResult(p)^, @FOpcodes[0]);
         stack.RowIndex := stack.RowIndex + (High(TdwsTabularBatchResult)+1);
         Inc(p, (High(TdwsTabularBatchResult)+1));
      end;
      Result := p;
   end;
   {$endif}

   procedure DoEvaluateAll(stack : TdwsTabularStack; rowCount : Integer; p : PdwsTabularNumber);
   begin
      {$ifdef ENABLE_JIT64}
      if FCodeBlock <> nil then
         p := BatchEvaluate(stack, rowCount div (High(TdwsTabularBatchResult)+1), p);
      {$endif}
      for var i := stack.RowIndex to rowCount-1 do begin
         stack.RowIndex := i;
         p^ := Evaluate(stack);
         Inc(p);
      end;
   end;

begin
   var stack := TdwsTabularStack.Create(MaxStackDepth);
   try
      Setlength(Result, Tabular.RowCount);
      DoEvaluateAll(stack, Tabular.RowCount, PdwsTabularNumber(Result));
   finally
      stack.Free
   end;
end;

// ------------------
// ------------------ TdwsTabularJIT ------------------
// ------------------

{$ifdef ENABLE_JIT64}

// Create
//
constructor TdwsTabularJIT.Create;
begin
   inherited;
   Allocator.SubAllocatorProtect := False;
   Allocator.SubAllocatorUseRtFnCallback := True;
end;

procedure DoExp4(p : PdwsTabularBatchResult);
begin
   p[0] := Exp(p[0]);
   p[1] := Exp(p[1]);
   p[2] := Exp(p[2]);
   p[3] := Exp(p[3]);
end;

procedure DoLn4(p : PdwsTabularBatchResult);
begin
   p[0] := Ln(p[0]);
   p[1] := Ln(p[1]);
   p[2] := Ln(p[2]);
   p[3] := Ln(p[3]);
end;

procedure DoLookup4(pResult : PdwsTabularBatchResult; op : PdwsTabularOpcode;
                    scaledRowIndex : Integer);

   function Lookup(pStr : Pointer; op : PdwsTabularOpcode) : TdwsTabularNumber;
   var
      bucket : TdwsNameValueBucket;
   begin
      bucket.Name := String(pStr);
      if TdwsNameValues(op.Lookup).Match(bucket) then
         Result := bucket.Value
      else Result := op.Operand1;
   end;

begin
   {$if SizeOf(TdwsTabularNumber) <> SizeOf(String) }
   var p : PPointerArray := @op.StringPtr[scaledRowIndex div SizeOf(TdwsTabularNumber)];
   {$else}
   var p : PPointerArray := Pointer(IntPtr(op.StringPtr) + scaledRowIndex);
   {$endif}
   pResult[0] := Lookup(p[0], op);
   pResult[1] := Lookup(p[1], op);
   pResult[2] := Lookup(p[2], op);
   pResult[3] := Lookup(p[3], op);
end;

procedure DoLookupUnified4(pResult : PdwsTabularBatchResult; op : PdwsTabularOpcode;
                           scaledRowIndex : Integer);

   function Lookup(pStr : Pointer; op : PdwsTabularOpcode) : TdwsTabularNumber; inline;
   begin
      if pStr <> nil then begin
         var lnv := TdwsLinearNameValues(op.Lookup);
         for var i := lnv.FCount-1 downto 0 do begin
            if Pointer(lnv.FNames[i]) = pStr then begin
               Exit(lnv.FValues[i]);
            end;
         end;
      end;
      Result := op.Operand1;
   end;

begin
   {$if SizeOf(TdwsTabularNumber) <> SizeOf(String) }
   var p : PPointerArray := @op.StringPtr[scaledRowIndex div SizeOf(TdwsTabularNumber)];
   {$else}
   var p : PPointerArray := Pointer(IntPtr(op.StringPtr) + scaledRowIndex);
   {$endif}
   pResult[0] := Lookup(p[0], op);
   pResult[1] := Lookup(p[1], op);
   pResult[2] := Lookup(p[2], op);
   pResult[3] := Lookup(p[3], op);
end;

// JITExpression
//
function TdwsTabularJIT.JITExpression(expr : TdwsTabularExpression; const batchAggregates : TdwsTabularExpressionAggreggates) : Boolean;
{$ifdef TABULAR_SINGLES}
const
   cAbsMask : array [0..SizeOf(TdwsTabularNumber)-1] of Byte = ( $FF, $FF, $FF, $7F );
var
   storeStackOffset : Integer;
   x86 : Tx86_64_WriteOnlyStream;
   destBatchGPR : TgpRegister64;
   rowIndexOffsetGPR : TgpRegister64;
   opcodesGPR : TgpRegister64;
   alterationNotifiedUpTo : Integer;

   procedure SaveStateForFallback(nbYmmToStore : Integer);
   begin
      if storeStackOffset = 0 then
         storeStackOffset := Preamble.AllocateStackSpace((expr.MaxStackDepth + 2) * 4 * SizeOf(TdwsTabularNumber));
      if nbYmmToStore > 0 then begin
         for var k := 0 to nbYmmToStore-1 do
            x86._vmovups_ptr_reg_reg(gprRBP, storeStackOffset + k * 4 * SizeOf(TdwsTabularNumber), TymmRegister(k));
         x86._vzeroupper;
      end;
   end;

   procedure RestoreStateForFallback(nbYmmToStore : Integer);
   begin
      if nbYmmToStore > 0 then begin
         for var k := 0 to nbYmmToStore-1 do
            x86._vmovups_ptr_reg(TymmRegister(k), gprRBP, storeStackOffset + k * 4 * SizeOf(TdwsTabularNumber));
      end;
   end;

   procedure NotifyAlteration(reg : TymmRegister);
   begin
      for var x := alterationNotifiedUpTo+1 to Ord(reg) do
         Preamble.NotifyXMMAlteration(TxmmRegister(x));
      alterationNotifiedUpTo := Ord(reg);
   end;

begin
   Result := False;
   // we reserve extra ymm registers for temp stuff
   if expr.MaxStackDepth >= 12 then Exit;

   Result := True;

   storeStackOffset := 0;

   StartJIT(nil, False);
   Preamble.PreserveExec := False;
   Preamble.NoExecMemRegister := True;

   alterationNotifiedUpTo := -1;
   NotifyAlteration(TYmmRegister(expr.MaxStackDepth));

   x86 := Output as Tx86_64_WriteOnlyStream;

   // call parameters: stack in RCX, dest batch in RDX, base of opcodes in r8

   // RSI <- stack in RCX
   //x86._mov_reg_reg(gprRSI, gprRCX);
   //Preamble.NotifyGPAlteration(gprRSI);

   if expr.FJITHasCalls then begin
      destBatchGPR := gprRBX;
      Preamble.NotifyGPAlteration(destBatchGPR);
      rowIndexOffsetGPR := gprRDI;
      Preamble.NotifyGPAlteration(rowIndexOffsetGPR);
      opcodesGPR := gprRSI;
      Preamble.NotifyGPAlteration(opcodesGPR);
   end else begin
      destBatchGPR := gprRDX;
      rowIndexOffsetGPR := gprRCX;
      opcodesGPR := gprR8;
   end;
   x86._mov_reg_reg(destBatchGPR, gprRDX);
   x86._mov_reg_reg(opcodesGPR, gprR8);

   // rowIndex * SizeOf(Double)
   x86._mov_reg_qword_ptr_reg(rowIndexOffsetGPR, gprRCX, NativeInt(@TdwsTabularStack(nil).FRowIndex));
   x86._shift_reg_imm(gpShl, rowIndexOffsetGPR, 2);

   for var i := 0 to expr.FOpcodeCount-1 do begin
      var op : PdwsTabularOpcode := @expr.FOpcodes[i];
      if @op.Method = @TdwsTabularExpression.DoPushNumField then begin
         x86._mov_reg_qword(gprRAX, NativeUInt(op.NumberPtr));
         x86._vmovups_ptr_indexed(TymmRegister(op.StackDepth), gprRAX, rowIndexOffsetGPR, 1, 0);

      end else if @op.Method = @TdwsTabularExpression.DoPushNumFieldDef then begin
         var regDef := TymmRegister(op.StackDepth+2);
         Assert(regDef <= ymm15);
         NotifyAlteration(regDef);

         var regZeros := TymmRegister(op.StackDepth+1);
         x86._mov_reg_qword(gprRAX, NativeUInt(@cEmptyFloatAsInt));
         x86._vpbroadcastd_ptr_reg(regZeros, gprRAX, 0);

         x86._mov_reg_qword(gprRAX, NativeUInt(op.NumberPtr));
         x86._vmovdqu_ptr_indexed(regDef, gprRAX, rowIndexOffsetGPR, 1, 0);
         x86._vpcmpeqd(regZeros, regDef, regZeros);

         if op.Operand1 = 0 then
            x86._v_op_ps(xmm_xorpd, regDef, regDef, regDef)
         else x86._vbroadcastss_ptr_reg(regDef, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));

         var regDest := TymmRegister(op.StackDepth);
         x86._mov_reg_qword(gprRAX, NativeUInt(op.NumberPtr));
         x86._vmovups_ptr_indexed(regDest, gprRAX, rowIndexOffsetGPR, 1, 0);

         x86._vblendvps(regDest, regDest, regDef, regZeros);

      end else if @op.Method = @TdwsTabularExpression.DoPushConst then begin
         var reg := TymmRegister(op.StackDepth);
         if op.Operand1 = 0 then
            x86._v_op_ps(xmm_xorpd, reg, reg, reg)
         else x86._vbroadcastss_ptr_reg(reg, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));

      end else if @op.Method = @TdwsTabularExpression.DoMultAddConst then begin
         var reg := TymmRegister(op.StackDepth-1);
         var regMul := TymmRegister(op.StackDepth);
         var regAdd := TymmRegister(op.StackDepth+1);
         NotifyAlteration(regAdd);
         x86._vbroadcastss_ptr_reg(regMul, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));
         x86._vbroadcastss_ptr_reg(regAdd, opcodesGPR, IntPtr(@op.Operand2) - IntPtr(expr.FOpcodes));
         x86._vfmadd_ps(213, reg, regMul, regAdd);

      end else if @op.Method = @TdwsTabularExpression.DoMultConstAdd then begin
         var reg := TymmRegister(op.StackDepth-1);
         var regMul := TymmRegister(op.StackDepth);
         var regAdd := TymmRegister(op.StackDepth-2);
         NotifyAlteration(regAdd);
         x86._vbroadcastss_ptr_reg(regMul, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));
         x86._vfmadd_ps(231, regAdd, reg, regMul);

      end else if @op.Method = @TdwsTabularExpression.DoAdd then begin
         x86._v_op_ps(xmm_addpd, TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1));
      end else if @op.Method = @TdwsTabularExpression.DoAddConst then begin
         var reg := TymmRegister(op.StackDepth-1);
         var regOp := TymmRegister(op.StackDepth);
         x86._vbroadcastss_ptr_reg(regOp, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));
         x86._v_op_ps(xmm_addpd, reg, reg, regOp);

      end else if @op.Method = @TdwsTabularExpression.DoMult then begin
         x86._v_op_ps(xmm_mulpd, TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1));
      end else if @op.Method = @TdwsTabularExpression.DoMultConst then begin
         var reg := TymmRegister(op.StackDepth-1);
         var regOp := TymmRegister(op.StackDepth);
         x86._vbroadcastss_ptr_reg(regOp, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));
         x86._v_op_ps(xmm_mulpd, reg, reg, regOp);

      end else if @op.Method = @TdwsTabularExpression.DoSub then begin
         x86._v_op_ps(xmm_subpd, TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1));

      end else if @op.Method = @TdwsTabularExpression.DoDiv then begin
         x86._v_op_ps(xmm_divpd, TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1));

      end else if @op.Method = @TdwsTabularExpression.DoMin then begin
         x86._v_op_ps(xmm_minpd, TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1));
      end else if @op.Method = @TdwsTabularExpression.DoMax then begin
         x86._v_op_ps(xmm_maxpd, TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1));

      end else if @op.Method = @TdwsTabularExpression.DoReLu then begin
         var regDest := TymmRegister(op.StackDepth-2);
         var regOperand := TymmRegister(op.StackDepth-1);
         x86._v_op_ps(xmm_addpd, regDest, regDest, regOperand);
         x86._v_op_ps(xmm_xorpd, regOperand, regOperand, regOperand);
         x86._v_op_ps(xmm_maxpd, regDest, regDest, regOperand);

      end else if @op.Method = @TdwsTabularExpression.DoDouble then begin
         x86._v_op_ps(xmm_addpd, TymmRegister(op.StackDepth-1), TymmRegister(op.StackDepth-1), TymmRegister(op.StackDepth-1));
      end else if @op.Method = @TdwsTabularExpression.DoSqr then begin
         x86._v_op_ps(xmm_mulpd, TymmRegister(op.StackDepth-1), TymmRegister(op.StackDepth-1), TymmRegister(op.StackDepth-1));
      end else if @op.Method = @TdwsTabularExpression.DoSqrt then begin
         x86._v_op_ps(xmm_sqrtpd, TymmRegister(op.StackDepth-1), ymm0, TymmRegister(op.StackDepth-1));

      end else if @op.Method = @TdwsTabularExpression.DoAbs then begin
         Fixups.NewYMMOpRegPSImm(xmm_andpd, TymmRegister(op.StackDepth-1),
                                 TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask));

      end else if @op.Method = @TdwsTabularExpression.DoGTRE then begin
         x86._vcmpps(TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1), $11);
         Fixups.NewYMMOpRegPSImm(xmm_andpd, TymmRegister(op.StackDepth-2), 1.0, 1.0, 1.0, 1.0);
      end else if @op.Method = @TdwsTabularExpression.DoEqual then begin
         x86._vcmpps(TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1), 0);
         Fixups.NewYMMOpRegPDImm(xmm_andpd, TymmRegister(op.StackDepth-2), 1.0, 1.0, 1.0, 1.0);
      end else if @op.Method = @TdwsTabularExpression.DoNotEqual then begin
         x86._vcmpps(TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1), $C);
         Fixups.NewYMMOpRegPDImm(xmm_andpd, TymmRegister(op.StackDepth-2), 1.0, 1.0, 1.0, 1.0);

      end else if @op.Method = @TdwsTabularExpression.DoGTREConst then begin
         var regDest := TymmRegister(op.StackDepth-1);
         var regOperand := TymmRegister(op.StackDepth);
         if op.Operand1 = 0 then
            x86._v_op_ps(xmm_xorpd, regOperand, regOperand, regOperand)
         else x86._vbroadcastss_ptr_reg(regOperand, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));
         x86._vcmpps(regDest, regDest, regOperand, 5);
         Fixups.NewYMMOpRegPSImm(xmm_andpd, regDest, 1.0, 1.0, 1.0, 1.0);

      end else if @op.Method = @TdwsTabularExpression.DoRound then begin
         var reg := TymmRegister(op.StackDepth-1);
         x86._vround_ps(reg, reg, 0);
      end else if @op.Method = @TdwsTabularExpression.DoFloor then begin
         var reg := TymmRegister(op.StackDepth-1);
         x86._vround_ps(reg, reg, 1);
      end else if @op.Method = @TdwsTabularExpression.DoCeil then begin
         var reg := TymmRegister(op.StackDepth-1);
         x86._vround_ps(reg, reg, 2);

      end else if @op.Method = @TdwsLinearNameValues.DoLookupUnified then begin
{         SaveStateForFallback(op.StackDepth);

         x86._lea_reg_reg(gprRCX, gprRBP, storeStackOffset + (op.StackDepth) * 4 * SizeOf(TdwsTabularNumber));
         x86._mov_reg_imm(gprRDX, NativeInt(op));
         x86._mov_reg_reg(gprR8, rowIndexOffsetGPR);

         x86._call_absmem(@DoLookupUnified4);

         RestoreStateForFallback(op.StackDepth+1);   }

         var regDest := TymmRegister(op.StackDepth);
         var regStr  := TymmRegister(op.StackDepth+1);
         var regLookup := TymmRegister(op.StackDepth+2);
         var regOperand := TymmRegister(op.StackDepth+3);
         Assert(regOperand <= ymm15);
         NotifyAlteration(regOperand);

         x86._mov_reg_qword(gprRAX, NativeUInt(@op.Operand1));
         x86._vbroadcastss_ptr_reg(regDest, gprRAX, 0);

         x86._mov_reg_qword(gprRAX, NativeUInt(op.StringPtr));
         x86._vmovdqu_ptr_indexed(regStr, gprRAX, rowIndexOffsetGPR, 1, 0);

         x86._mov_reg_qword(gprR9, NativeUInt(TdwsLinearNameValues(op.Lookup).FNames));
         x86._mov_reg_qword(gprRAX, NativeUInt(TdwsLinearNameValues(op.Lookup).FValues));

         for var k := 0 to TdwsLinearNameValues(op.Lookup).FCount-1 do begin

            x86._vpbroadcastd_ptr_reg(regLookup, gprR9, k * SizeOf(String));
            x86._vpcmpeqd(regOperand, regStr, regLookup);
            x86._vbroadcastss_ptr_reg(regLookup, gprRAX, k * SizeOf(TdwsTabularNumber));
            x86._vblendvps(regDest, regDest, regLookup, regOperand);

         end;

//      end else if @op.Method = @TdwsNameValues.DoLookup then begin
//         SaveStateForFallback(op.StackDepth);
//
//         x86._lea_reg_reg(gprRCX, gprRBP, storeStackOffset + (op.StackDepth) * 4 * SizeOf(TdwsTabularNumber));
//         x86._mov_reg_imm(gprRDX, NativeInt(op));
//         x86._mov_reg_reg(gprR8, rowIndexOffsetGPR);
//
//         x86._call_absmem(@DoLookup4);
//
//         RestoreStateForFallback(op.StackDepth+1);

      end else if @op.Method = @TdwsTabularExpression.DoExp then begin
         SaveStateForFallback(op.StackDepth);

         x86._lea_reg_reg(gprRCX, gprRBP, storeStackOffset + (op.StackDepth-1) * 4 * SizeOf(TdwsTabularNumber));

         x86._call_absmem(@DoExp4);

         RestoreStateForFallback(op.StackDepth);

      end else if @op.Method = @TdwsTabularExpression.DoLn then begin
         SaveStateForFallback(op.StackDepth);

         x86._lea_reg_reg(gprRCX, gprRBP, storeStackOffset + (op.StackDepth-1) * 4 * SizeOf(TdwsTabularNumber));

         x86._call_absmem(@DoLn4);

         RestoreStateForFallback(op.StackDepth);

// lookup field
// lookup unified
      end else begin
         Result := False;
         Break;
      end;
   end;

   x86._vmovups_ptr_reg_reg(destBatchGPR, 0, ymm0);
   x86._vzeroupper;

   EndJIT;

{$else} // TABULAR_SINGLES

const
   cAbsMask : array [0..SizeOf(TdwsTabularNumber)-1] of Byte = ( $FF, $FF, $FF, $FF, $FF, $FF, $FF, $7F );
var
   storeStackOffset : Integer;
   x86 : Tx86_64_WriteOnlyStream;
   destBatchGPR : TgpRegister64;
   rowIndexOffsetGPR : TgpRegister64;
   opcodesGPR : TgpRegister64;
   alterationNotifiedUpTo : Integer;

   procedure SaveStateForFallback(nbYmmToStore : Integer);
   begin
      if storeStackOffset = 0 then
         storeStackOffset := Preamble.AllocateStackSpace((expr.MaxStackDepth + 2) * 4 * SizeOf(TdwsTabularNumber));
      if nbYmmToStore > 0 then begin
         for var k := 0 to nbYmmToStore-1 do
            x86._vmovupd_ptr_reg_reg(gprRBP, storeStackOffset + k * 4 * SizeOf(TdwsTabularNumber), TymmRegister(k));
         x86._vzeroupper;
      end;
   end;

   procedure RestoreStateForFallback(nbYmmToStore : Integer);
   begin
      if nbYmmToStore > 0 then begin
         for var k := 0 to nbYmmToStore-1 do
            x86._vmovupd_ptr_reg(TymmRegister(k), gprRBP, storeStackOffset + k * 4 * SizeOf(TdwsTabularNumber));
      end;
   end;

   procedure NotifyAlteration(reg : TymmRegister);
   begin
      for var x := alterationNotifiedUpTo+1 to Ord(reg) do
         Preamble.NotifyXMMAlteration(TxmmRegister(x));
      alterationNotifiedUpTo := Ord(reg);
   end;

begin
   Result := False;
   // we reserve extra ymm registers for temp stuff
   if expr.MaxStackDepth >= 12 then Exit;

   Result := True;

   storeStackOffset := 0;

   StartJIT(nil, False);
   Preamble.PreserveExec := False;
   Preamble.NoExecMemRegister := True;

   alterationNotifiedUpTo := -1;
   NotifyAlteration(TYmmRegister(expr.MaxStackDepth));

   x86 := Output as Tx86_64_WriteOnlyStream;

   // call parameters: stack in RCX, dest batch in RDX, base of opcodes in r8

   // RSI <- stack in RCX
   //x86._mov_reg_reg(gprRSI, gprRCX);
   //Preamble.NotifyGPAlteration(gprRSI);

   if expr.FJITHasCalls then begin
      destBatchGPR := gprRBX;
      Preamble.NotifyGPAlteration(destBatchGPR);
      rowIndexOffsetGPR := gprRDI;
      Preamble.NotifyGPAlteration(rowIndexOffsetGPR);
      opcodesGPR := gprRSI;
      Preamble.NotifyGPAlteration(opcodesGPR);
   end else begin
      destBatchGPR := gprRDX;
      rowIndexOffsetGPR := gprRCX;
      opcodesGPR := gprR8;
   end;
   x86._mov_reg_reg(destBatchGPR, gprRDX);
   x86._mov_reg_reg(opcodesGPR, gprR8);

   // rowIndex * SizeOf(Double)
   x86._mov_reg_qword_ptr_reg(rowIndexOffsetGPR, gprRCX, NativeInt(@TdwsTabularStack(nil).FRowIndex));
   x86._shift_reg_imm(gpShl, rowIndexOffsetGPR, 3);

   for var i := 0 to expr.FOpcodeCount-1 do begin
      var op : PdwsTabularOpcode := @expr.FOpcodes[i];
      if @op.Method = @TdwsTabularExpression.DoPushNumField then begin
         x86._mov_reg_qword(gprRAX, NativeUInt(op.NumberPtr));
         x86._vmovupd_ptr_indexed(TymmRegister(op.StackDepth), gprRAX, rowIndexOffsetGPR, 1, 0);

      end else if @op.Method = @TdwsTabularExpression.DoPushNumFieldDef then begin
         var regDef := TymmRegister(op.StackDepth+2);
         Assert(regDef <= ymm15);
         NotifyAlteration(regDef);

         var regZeros := TymmRegister(op.StackDepth+1);
         x86._mov_reg_qword(gprRAX, NativeUInt(@cEmptyFloatAsInt));
         x86._vpbroadcastq_ptr_reg(regZeros, gprRAX, 0);

         x86._mov_reg_qword(gprRAX, NativeUInt(op.NumberPtr));
         x86._vmovdqu_ptr_indexed(regDef, gprRAX, rowIndexOffsetGPR, 1, 0);
         x86._vpcmpeqq(regZeros, regDef, regZeros);

         if op.Operand1 = 0 then
            x86._v_op_pd(xmm_xorpd, regDef, regDef, regDef)
         else x86._vbroadcastsd_ptr_reg(regDef, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));

         var regDest := TymmRegister(op.StackDepth);
         x86._mov_reg_qword(gprRAX, NativeUInt(op.NumberPtr));
         x86._vmovupd_ptr_indexed(regDest, gprRAX, rowIndexOffsetGPR, 1, 0);

         x86._vblendvpd(regDest, regDest, regDef, regZeros);

      end else if @op.Method = @TdwsTabularExpression.DoPushConst then begin
         var reg := TymmRegister(op.StackDepth);
         if op.Operand1 = 0 then
            x86._v_op_pd(xmm_xorpd, reg, reg, reg)
         else x86._vbroadcastsd_ptr_reg(reg, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));

      end else if @op.Method = @TdwsTabularExpression.DoDup then begin
         var reg := TymmRegister(op.StackDepth);
         x86._vmovapd_reg_reg(reg, TymmRegister(Ord(reg) - 1 - PInteger(@op.Operand1)^ div SizeOf(TdwsTabularNumber)));

      end else if @op.Method = @TdwsTabularExpression.DoMultAddConst then begin
         var reg := TymmRegister(op.StackDepth-1);
         var regMul := TymmRegister(op.StackDepth);
         var regAdd := TymmRegister(op.StackDepth+1);
         NotifyAlteration(regAdd);
         x86._vbroadcastsd_ptr_reg(regMul, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));
         x86._vbroadcastsd_ptr_reg(regAdd, opcodesGPR, IntPtr(@op.Operand2) - IntPtr(expr.FOpcodes));
         x86._vfmadd_pd(213, reg, regMul, regAdd);

      end else if @op.Method = @TdwsTabularExpression.DoMultConstAdd then begin
         var reg := TymmRegister(op.StackDepth-1);
         var regMul := TymmRegister(op.StackDepth);
         var regAdd := TymmRegister(op.StackDepth-2);
         NotifyAlteration(regAdd);
         x86._vbroadcastsd_ptr_reg(regMul, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));
         x86._vfmadd_pd(231, regAdd, reg, regMul);

      end else if @op.Method = @TdwsTabularExpression.DoAdd then begin
         x86._v_op_pd(xmm_addpd, TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1));
      end else if @op.Method = @TdwsTabularExpression.DoAddConst then begin
         var reg := TymmRegister(op.StackDepth-1);
         var regOp := TymmRegister(op.StackDepth);
         x86._vbroadcastsd_ptr_reg(regOp, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));
         x86._v_op_pd(xmm_addpd, reg, reg, regOp);

      end else if @op.Method = @TdwsTabularExpression.DoMult then begin
         x86._v_op_pd(xmm_mulpd, TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1));
      end else if @op.Method = @TdwsTabularExpression.DoMultConst then begin
         var reg := TymmRegister(op.StackDepth-1);
         var regOp := TymmRegister(op.StackDepth);
         x86._vbroadcastsd_ptr_reg(regOp, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));
         x86._v_op_pd(xmm_mulpd, reg, reg, regOp);

      end else if @op.Method = @TdwsTabularExpression.DoSub then begin
         x86._v_op_pd(xmm_subpd, TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1));

      end else if @op.Method = @TdwsTabularExpression.DoDiv then begin
         x86._v_op_pd(xmm_divpd, TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1));

      end else if @op.Method = @TdwsTabularExpression.DoMin then begin
         x86._v_op_pd(xmm_minpd, TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1));
      end else if @op.Method = @TdwsTabularExpression.DoMax then begin
         x86._v_op_pd(xmm_maxpd, TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1));

      end else if @op.Method = @TdwsTabularExpression.DoReLu then begin
         var regDest := TymmRegister(op.StackDepth-2);
         var regOperand := TymmRegister(op.StackDepth-1);
         x86._v_op_pd(xmm_addpd, regDest, regDest, regOperand);
         x86._v_op_pd(xmm_xorpd, regOperand, regOperand, regOperand);
         x86._v_op_pd(xmm_maxpd, regDest, regDest, regOperand);

      end else if @op.Method = @TdwsTabularExpression.DoSMAPE then begin
         var regDest := TymmRegister(op.StackDepth-2);
         var regOperand := TymmRegister(op.StackDepth-1);
         var regNumer := TymmRegister(op.StackDepth);
         x86._v_op_pd(xmm_subpd, regNumer, regDest, regOperand);
         Fixups.NewYMMOpRegPDImm(xmm_andpd, regNumer,
                                 TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask));
         Fixups.NewYMMOpRegPDImm(xmm_andpd, regDest,
                                 TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask));
         Fixups.NewYMMOpRegPDImm(xmm_andpd, regOperand,
                                 TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask));
         x86._v_op_pd(xmm_addpd, regOperand, regOperand, regDest);
         x86._v_op_pd(xmm_divpd, regDest, regNumer, regOperand);

      end else if @op.Method = @TdwsTabularExpression.DoDouble then begin
         x86._v_op_pd(xmm_addpd, TymmRegister(op.StackDepth-1), TymmRegister(op.StackDepth-1), TymmRegister(op.StackDepth-1));
      end else if @op.Method = @TdwsTabularExpression.DoSqr then begin
         x86._v_op_pd(xmm_mulpd, TymmRegister(op.StackDepth-1), TymmRegister(op.StackDepth-1), TymmRegister(op.StackDepth-1));
      end else if @op.Method = @TdwsTabularExpression.DoSqrt then begin
         x86._v_op_pd(xmm_sqrtpd, TymmRegister(op.StackDepth-1), ymm0, TymmRegister(op.StackDepth-1));

      end else if @op.Method = @TdwsTabularExpression.DoAbs then begin
         Fixups.NewYMMOpRegPDImm(xmm_andpd, TymmRegister(op.StackDepth-1),
                                 TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask), TdwsTabularNumber(cAbsMask));

      end else if @op.Method = @TdwsTabularExpression.DoGTRE then begin
         x86._vcmppd(TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1), $11);
         Fixups.NewYMMOpRegPDImm(xmm_andpd, TymmRegister(op.StackDepth-2), 1.0, 1.0, 1.0, 1.0);
      end else if @op.Method = @TdwsTabularExpression.DoEqual then begin
         x86._vcmppd(TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1), 0);
         Fixups.NewYMMOpRegPDImm(xmm_andpd, TymmRegister(op.StackDepth-2), 1.0, 1.0, 1.0, 1.0);
      end else if @op.Method = @TdwsTabularExpression.DoNotEqual then begin
         x86._vcmppd(TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-2), TymmRegister(op.StackDepth-1), $C);
         Fixups.NewYMMOpRegPDImm(xmm_andpd, TymmRegister(op.StackDepth-2), 1.0, 1.0, 1.0, 1.0);

      end else if @op.Method = @TdwsTabularExpression.DoGTREConst then begin
         var regDest := TymmRegister(op.StackDepth-1);
         var regOperand := TymmRegister(op.StackDepth);
         if op.Operand1 = 0 then
            x86._v_op_pd(xmm_xorpd, regOperand, regOperand, regOperand)
         else x86._vbroadcastsd_ptr_reg(regOperand, opcodesGPR, IntPtr(@op.Operand1) - IntPtr(expr.FOpcodes));
         x86._vcmppd(regDest, regDest, regOperand, 5);
         Fixups.NewYMMOpRegPDImm(xmm_andpd, regDest, 1.0, 1.0, 1.0, 1.0);

      end else if @op.Method = @TdwsTabularExpression.DoRound then begin
         var reg := TymmRegister(op.StackDepth-1);
         x86._vround_pd(reg, reg, 0);
      end else if @op.Method = @TdwsTabularExpression.DoFloor then begin
         var reg := TymmRegister(op.StackDepth-1);
         x86._vround_pd(reg, reg, 1);
      end else if @op.Method = @TdwsTabularExpression.DoCeil then begin
         var reg := TymmRegister(op.StackDepth-1);
         x86._vround_pd(reg, reg, 2);

      end else if @op.Method = @TdwsLinearNameValues.DoLookupUnified then begin
{         SaveStateForFallback(op.StackDepth);

         x86._lea_reg_reg(gprRCX, gprRBP, storeStackOffset + (op.StackDepth) * 4 * SizeOf(TdwsTabularNumber));
         x86._mov_reg_imm(gprRDX, NativeInt(op));
         x86._mov_reg_reg(gprR8, rowIndexOffsetGPR);

         x86._call_absmem(@DoLookupUnified4);

         RestoreStateForFallback(op.StackDepth+1);   }

         var regDest := TymmRegister(op.StackDepth);
         var regStr  := TymmRegister(op.StackDepth+1);
         var regLookup := TymmRegister(op.StackDepth+2);
         var regOperand := TymmRegister(op.StackDepth+3);
         Assert(regOperand <= ymm15);
         NotifyAlteration(regOperand);

         x86._mov_reg_qword(gprRAX, NativeUInt(@op.Operand1));
         x86._vbroadcastsd_ptr_reg(regDest, gprRAX, 0);

         x86._mov_reg_qword(gprRAX, NativeUInt(op.StringPtr));
         x86._vmovdqu_ptr_indexed(regStr, gprRAX, rowIndexOffsetGPR, 1, 0);

         x86._mov_reg_qword(gprR9, NativeUInt(TdwsLinearNameValues(op.Lookup).FNames));
         x86._mov_reg_qword(gprRAX, NativeUInt(TdwsLinearNameValues(op.Lookup).FValues));

         for var k := 0 to TdwsLinearNameValues(op.Lookup).FCount-1 do begin

            x86._vpbroadcastq_ptr_reg(regLookup, gprR9, k * SizeOf(String));
            x86._vpcmpeqq(regOperand, regStr, regLookup);
            x86._vbroadcastsd_ptr_reg(regLookup, gprRAX, k * SizeOf(TdwsTabularNumber));
            x86._vblendvpd(regDest, regDest, regLookup, regOperand);

         end;

//      end else if @op.Method = @TdwsNameValues.DoLookup then begin
//         SaveStateForFallback(op.StackDepth);
//
//         x86._lea_reg_reg(gprRCX, gprRBP, storeStackOffset + (op.StackDepth) * 4 * SizeOf(TdwsTabularNumber));
//         x86._mov_reg_imm(gprRDX, NativeInt(op));
//         x86._mov_reg_reg(gprR8, rowIndexOffsetGPR);
//
//         x86._call_absmem(@DoLookup4);
//
//         RestoreStateForFallback(op.StackDepth+1);

      end else if @op.Method = @TdwsTabularExpression.DoExp then begin
         SaveStateForFallback(op.StackDepth);

         x86._lea_reg_reg(gprRCX, gprRBP, storeStackOffset + (op.StackDepth-1) * 4 * SizeOf(TdwsTabularNumber));

         x86._call_absmem(@DoExp4);

         RestoreStateForFallback(op.StackDepth);

      end else if @op.Method = @TdwsTabularExpression.DoLn then begin
         SaveStateForFallback(op.StackDepth);

         x86._lea_reg_reg(gprRCX, gprRBP, storeStackOffset + (op.StackDepth-1) * 4 * SizeOf(TdwsTabularNumber));

         x86._call_absmem(@DoLn4);

         RestoreStateForFallback(op.StackDepth);

// lookup field
// lookup unified
      end else begin
         Result := False;
         Break;
      end;
   end;

   if Length(batchAggregates) > 0 then begin
      var offset := 0;
      Assert(Length(batchAggregates) < 15);
      for var i := 0 to High(batchAggregates) do begin
         var reg := TymmRegister(i+1);
         x86._vmovdqu_ptr_reg(reg, destBatchGPR, offset);
         case batchAggregates[i] of
            tegSum : x86._vaddpd(reg, reg, ymm0);
            tegMax : x86._v_op_pd(xmm_maxpd, reg, reg, ymm0);
            tegMin : x86._v_op_pd(xmm_minpd, reg, reg, ymm0);
         else
            Assert(False);
         end;
         x86._vmovupd_ptr_reg_reg(destBatchGPR, offset, reg);
         offset := offset + 4 * SizeOf(TdwsTabularNumber);
      end;
   end else begin
      x86._vmovupd_ptr_reg_reg(destBatchGPR, 0, ymm0);
   end;
   x86._vzeroupper;

   EndJIT;
{$endif}
end;

{$endif ENABLE_JIT64}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vDotFormatSettings := FormatSettings;
   vDotFormatSettings.DecimalSeparator := '.';

end.
