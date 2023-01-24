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
unit dwsTimeSeries;

{$I dws.inc}

interface

uses
   System.Classes, System.SysUtils,
   dwsUtils, dwsDynamicArrays, dwsXPlatform, dwsSymbols;

type
   TdwsTimeSeries = class;

   TdwsTimeSeriesSequence = class (TRefCountedObject)
      private
         FOwner : TdwsTimeSeries;
         FName : String;
         FDecimals : Integer;
         FScaleIntToFloat : Double;
         FScaleFloatToInt : Double;
         FIndex : Integer;

      public
         property Owner : TdwsTimeSeries read FOwner;
         property Name : String read FName;
         property Decimals : Integer read FDecimals;
         property ScaleIntToFloat : Double read FScaleIntToFloat;
         property ScaleFloatToInt : Double read FScaleFloatToInt;
         property Index : Integer read FIndex;
   end;
   TdwsTimeSeriesSequences = TObjectList<TdwsTimeSeriesSequence>;

   TdwsTimeSeriesUnpackMode = ( tsumUnpackAndKeep, tsumUnpackAndDelete );

   TdwsTimeSeriesMemoryStatistics = record
      PackedBytes : Int64;
      UnPackedBytes : Int64;
      SampleCount : Int64;
   end;

   TdwsTimeSeriesSequenceData = class (TRefCountedObject)
      private
         FSequence : TdwsTimeSeriesSequence;
         FValuesPtr : Pointer;
         FCount : Integer;
         FCapacity : Integer;
         FPackedData : Pointer;
         FPackedDataSize : Integer;

      protected
         procedure Pack;
         procedure Unpack(nbValues : Integer; unpackMode : TdwsTimeSeriesUnpackMode);

         procedure Grow;
         function GetValues(index : Integer) : Double; inline;
         procedure SetValues(index : Integer; const v : Double); inline;

      public
         constructor Create(aSequence : TdwsTimeSeriesSequence);
         destructor Destroy; override;

         property Sequence : TdwsTimeSeriesSequence read FSequence;
         property Values[index : Integer] : Double read GetValues write SetValues;

         procedure AddValue(const v : Double);
         procedure Insert(index : Integer; const v : Double);
         procedure Delete(index, nb : Integer);
         procedure SetFrom(p : PDoubleArray; nb : Integer);

         procedure MemoryStats(var stats : TdwsTimeSeriesMemoryStatistics);
   end;
   TdwsTimeSeriesSequenceDatas = TObjectList<TdwsTimeSeriesSequenceData>;

   TdwsTimeSeriesExtractionOption = (
      tseoIgnoreNulls = 0
   );
   TdwsTimeSeriesExtractionOptions = set of TdwsTimeSeriesExtractionOption;

   TdwsTimeSeriesBatch = class (TRefCountedObject)
      private
         FStartTime : Int64;
         FStopTime : Int64;
         FTimeStamps : TInt64DynArray;
         FPackedTimeStamps : Pointer;
         FPackedTimeStampsSize : Integer;
         FDatas : TdwsTimeSeriesSequenceDatas;
         FNbSamples : Integer;

      protected
         procedure PackTimeStamps;
         procedure UnpackTimeStamps(unpackMode : TdwsTimeSeriesUnpackMode);

         function FindTimeIndex(const time : Int64; var idx : Integer) : Boolean;
         function SequenceData(seq : TdwsTimeSeriesSequence) : TdwsTimeSeriesSequenceData;
         procedure StoreSample(seq : TdwsTimeSeriesSequence; const time : Int64; value : Double);

         function GetTimeStamps(const fromTime, toTime : Int64; const timeStamps : TScriptDynamicNativeIntegerArray) : Integer;
         function GetSamples(seq : TdwsTimeSeriesSequence; const fromTime, toTime : Int64;
                             timeStamps : TScriptDynamicNativeIntegerArray;
                             values : TScriptDynamicNativeFloatArray;
                             options : TdwsTimeSeriesExtractionOptions) : Integer;

      public
         constructor Create;
         destructor Destroy; override;

         property StartTime : Int64 read FStartTime;
         property StopTime : Int64 read FStopTime;
         property NbSamples : Integer read FNbSamples;

         procedure Pack;
         procedure Unpack(unpackMode : TdwsTimeSeriesUnpackMode);

         procedure ClearSamplesBefore(time : Int64);

         procedure MemoryStats(var stats : TdwsTimeSeriesMemoryStatistics);
   end;
   TdwsTimeSeriesBatches = TObjectList<TdwsTimeSeriesBatch>;

   TdwsTimeSeries = class (TInterfacedSelfObject)
      private
         FNextSequenceID : Int64;
         FSequences : TdwsTimeSeriesSequences;
         FBatches : TdwsTimeSeriesBatches;
         FLock : TMultiReadSingleWrite;
         FUseCount : Integer;
         FPoolNext : TdwsTimeSeries;
         FName : String;
         FLargestBatchSampleCount : Integer;

      protected
         function FindBatchIndex(const time : Int64; var idx : Integer) : Boolean;

      public
         constructor Create(const aName : String);
         destructor Destroy; override;

         property Name : String read FName;

         function CreateSequence(const name : String; decimals : Integer) : TdwsTimeSeriesSequence;
         function SequenceByName(const name : String) : TdwsTimeSeriesSequence;
         function SequenceByIndex(idx : Integer) : TdwsTimeSeriesSequence;
         function SequenceCount : Integer; inline;

         procedure StoreSample(seq : TdwsTimeSeriesSequence; const time : Int64; const value : Double);
         procedure StoreSamples(seq : TdwsTimeSeriesSequence; const times, values : IScriptDynArray);
         procedure ClearSamples;
         procedure ClearSamplesBefore(time : Int64);

         function GetTimeStamps(const fromTime, toTime : Int64; const times : IScriptDynArray) : Integer;
         function GetSample(seq : TdwsTimeSeriesSequence; const timeStamp : Int64) : Double;
         function GetSamples(seq : TdwsTimeSeriesSequence; const fromTime, toTime : Int64;
                             const times, values : IScriptDynArray;
                             options : TdwsTimeSeriesExtractionOptions) : Integer;

         function NextTimeStamp(var timeStamp : Int64) : Boolean;

         procedure SplitLargeBatches(sampleCountTreshold : Integer);

         procedure Pack;
         procedure Unpack(unpackMode : TdwsTimeSeriesUnpackMode);

         function MemoryStats : TdwsTimeSeriesMemoryStatistics;
         property LargestBatchSampleCount : Integer read FLargestBatchSampleCount write FLargestBatchSampleCount;
   end;

   TimeSeriesPool = class sealed
      private
         class procedure Initialize; static;
         class procedure Finalize; static;

      public
         class function Acquire(const aName : String) : TdwsTimeSeries; static;
         class procedure Release(ts : TdwsTimeSeries); static;
         class procedure Delete(const aName : String); static;
         class procedure Cleanup; static;
   end;

function DeltaPackValues(const values : Pointer; const nbValues : Integer; const scale : Double; var packedData : Pointer) : Integer;
procedure DeltaUnpackValues(const packedData : Pointer; nbValues : Integer; const scale : Double; const values : Pointer);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses Math;

const cNaN = 0/0;

(*

Packed delta byte encoding

Direct delta

2 bits scale 1, 10, 100, 1000
5 bits -16 +15

Special values

$80

3 bits -> number of values -1 of that size in sequence
3 bits -> 0 -> int8
		    1 -> int16
          2 -> int24
          3 -> int32
          4 -> int64
          7 -> nulls

$87 -> NaN

3 bits -> number of NaN -1

$C0

6 bytes number of zeroes

*)

// DeltaPackValues
//
function DeltaPackValues(const values : Pointer; const nbValues : Integer; const scale : Double; var packedData : Pointer) : Integer;

   procedure WriteByte(var p : PByte; b : Byte); inline;
   begin
      p^ := b;
      Inc(p);
   end;

   procedure WriteInt8(var p : PByte; i : Int8); inline;
   begin
      PInt8Array(p)[0] := i;
      Inc(p, SizeOf(Int8));
   end;

   procedure WriteInt16(var p : PByte; i : Int16); inline;
   begin
      PInt16Array(p)[0] := i;
      Inc(p, SizeOf(Int16));
   end;

   procedure WriteInt32(var p : PByte; i : Int32); inline;
   begin
      PInt32Array(p)[0] := i;
      Inc(p, SizeOf(Int32));
   end;

   procedure WriteInt64(var p : PByte; i : Int64); inline;
   begin
      PInt64Array(p)[0] := i;
      Inc(p, SizeOf(Int64));
   end;

var
   pSrc, pNextSrc : UIntPtr;
   pDest : PByte;
   current, prev : Int64;
   int64Src : Boolean;
begin
   Assert(SizeOf(Int64) = SizeOf(Double));

   FreeMemory(packedData);
   packedData := nil;
   if nbValues = 0 then Exit(0);

   packedData := GetMemory(nbValues*9); // worst case all 8 bytes values
   pDest := PByte(packedData);
   pSrc := UIntPtr(values);
   int64Src := (scale = 0);

   prev := 0;

   var i := 0;
   while i < nbValues do begin
      if int64Src then
         current := PInt64(pSrc)^
      else begin
         if IsNaN(PDouble(pSrc)^) then begin
            var n := 0;
            while True do begin
               Inc(pSrc, SizeOf(Int64));
               Inc(i);
               Inc(n);
               if (i >= nbValues) or not IsNaN(PDouble(pSrc)^) then begin
                  while n >= 8 do begin
                     WriteByte(pDest, $87 + (7 shl 3));
                     Dec(n, 8);
                  end;
                  if n > 0 then
                     WriteByte(pDest, $87 + ((n-1) shl 3));
                  Break;
               end;
            end;
            continue;
            current := prev;
         end else current := Round(PDouble(pSrc)^*scale);
      end;
      var delta := current - prev;
      if delta = 0 then begin
         var n := 1;
         while i < nbValues-1 do begin
            pNextSrc := pSrc + SizeOf(Int64);
            if int64Src then begin
               if PInt64(pNextSrc)^ <> current then break;
            end else begin
               if Round(PDouble(pNextSrc)^*scale) <> current then break;
            end;
            Inc(n);
            pSrc := pNextSrc;
            Inc(i);
         end;
         while n >= $40 do begin
            WriteByte(pDest, $FF);
            Dec(n, $40);
         end;
         if n > 0 then
            WriteByte(pDest, $C0 + (n-1));
      end else if (delta >= -16) and (delta <= 15) then begin
         WriteByte(pDest, delta and $1F)
      end else if (delta >= -128) and (delta <= 127) then begin
         WriteByte(pDest, $80);
         WriteInt8(pDest, delta);
      end else if (delta >= -32768) and (delta <= 32767) then begin
         WriteByte(pDest, $81);
         WriteInt16(pDest, delta);
      end else if (delta >= -8388608) and (delta <= 8388607) then begin
         WriteByte(pDest, $82);
         WriteInt32(pDest, delta);
         Dec(pDest);
      end else if (delta >= -2147483648) and (delta <= 2147483647) then begin
         WriteByte(pDest, $83);
         WriteInt32(pDest, delta);
      end else begin
         WriteByte(pDest, $84);
         WriteInt64(pDest, delta);
      end;
      prev := current;
      Inc(pSrc, SizeOf(Int64));
      Inc(i);
   end;
   Result := IntPtr(pDest)-IntPtr(packedData);
   packedData := ReallocMemory(packedData, Result);
end;

// DeltaUnpackValues
//
procedure DeltaUnpackValues(const packedData : Pointer; nbValues : Integer; const scale : Double; const values : Pointer);
type
   TInt24 = packed record
      w : Word;
      b : Byte;
   end;
   PInt24 = ^TInt24;
   TInt24Array = packed array [0..MaxInt shr 2] of TInt24;
   PInt24Array = ^TInt24Array;
var
   pSrc, pDest : PByte;
   current, delta : Int64;
   int64Dest : Boolean;

   procedure WriteCurrent;
   begin
      Assert(nbValues > 0);
      if int64Dest then
         PInt64(pDest)^ := current
      else PDouble(pDest)^ := current*scale;
      Inc(pDest, SizeOf(Int64));
      Dec(nbValues);
   end;

   procedure WriteNaN;
   begin
      Assert(nbValues > 0);
      PDouble(pDest)^ := cNaN;
      Inc(pDest, SizeOf(Double));
      Dec(nbValues);
   end;

begin
   int64Dest := (scale = 0);
   pDest := values;
   pSrc := packedData;
   current := 0;

   while nbValues > 0 do begin
      var code := pSrc^;
      Inc(pSrc);
      if (code and $80) = $80 then begin
         if (code and $C0) = $C0 then begin
            // number of zeroes
            for var i := 0 to (code and 7) do
               WriteCurrent;
         end else begin
            // large delta
            var nb := ((code shr 3) and 7);
            case code and 7 of
               0 : for var i := 0 to nb do begin
                  Inc(current, PInt8Array(pSrc)[i]);
                  Inc(pSrc, SizeOf(Int8));
                  WriteCurrent;
               end;
               1 : for var i := 0 to nb do begin
                  Inc(current, PInt16Array(pSrc)[i]);
                  Inc(pSrc, SizeOf(Int16));
                  WriteCurrent;
               end;
               2 : for var i := 0 to nb do begin
                  var pi24 := PInt24(@PInt24Array(pSrc)[i]);
                  if (pi24.b and $80) = 0 then
                     Inc(current, pi24.w + (pi24.b shl 16))
                  else Inc(current, (Int64(-1) xor $FFFFFF) + (pi24.w + (pi24.b shl 16)));
                  Inc(pSrc, SizeOf(TInt24));
                  WriteCurrent;
               end;
               3 : for var i := 0 to nb do begin
                  Inc(current, PInt32Array(pSrc)[i]);
                  Inc(pSrc, SizeOf(Int32));
                  WriteCurrent;
               end;
               4 : for var i := 0 to nb do begin
                  Inc(current, PInt64Array(pSrc)[i]);
                  Inc(pSrc, SizeOf(Int64));
                  WriteCurrent;
               end;
               7 : for var i := 0 to nb do
                  WriteNaN;
            else
               Assert(False);
            end;
         end;
      end else begin
         // small direct delta
         if (code and $10) = 0 then
            delta := code and $F
         else delta := ((Int64(-1) xor $1F) + code);
         case (code shr 5) and 3 of
            1 : delta := delta * 10;
            2 : delta := delta * 100;
            3 : delta := delta * 1000;
         end;
         Inc(current, delta);
         WriteCurrent;
      end;
   end;
end;

// ------------------
// ------------------ TdwsTimeSeriesSequenceData ------------------
// ------------------

// Create
//
constructor TdwsTimeSeriesSequenceData.Create(aSequence : TdwsTimeSeriesSequence);
begin
   inherited Create;
   FSequence := aSequence;
end;

// Destroy
//
destructor TdwsTimeSeriesSequenceData.Destroy;
begin
   inherited;
   FreeMemory(FPackedData);
   FreeMemory(FValuesPtr);
end;

// AddValue
//
procedure TdwsTimeSeriesSequenceData.AddValue(const v : Double);
begin
   if FCount = FCapacity then Grow;
   PDoubleArray(FValuesPtr)[FCount] := v;
   Inc(FCount);
end;

// Insert
//
procedure TdwsTimeSeriesSequenceData.Insert(index : Integer; const v : Double);
begin
   Assert(Cardinal(index) <= Cardinal(FCount));
   if FCount = FCapacity then Grow;
   if index < FCount then
      System.Move(PDoubleArray(FValuesPtr)[index], PDoubleArray(FValuesPtr)[index+1], (FCount-index)*SizeOf(Double));
   PDoubleArray(FValuesPtr)[index] := v;
   Inc(FCount);
end;

// Delete
//
procedure TdwsTimeSeriesSequenceData.Delete(index, nb : Integer);
begin
   Assert(Cardinal(index) < Cardinal(FCount));
   var nTail := FCount - index - nb;
   if nTail > 0 then begin
      System.Move(PDoubleArray(FValuesPtr)[index + nb], PDoubleArray(FValuesPtr)[index], nTail*SizeOf(Double));
   end else Assert(nTail = 0);
   Dec(FCount, nb);
   FValuesPtr := ReallocMemory(FValuesPtr, FCount*SizeOf(Double));
   FCapacity := FCount;
end;

// SetFrom
//
procedure TdwsTimeSeriesSequenceData.SetFrom(p : PDoubleArray; nb : Integer);
begin
   if FPackedData <> nil then begin
      FreeMemory(FPackedData);
      FPackedData := nil;
      FPackedDataSize := 0;
   end;
   var byteSize := nb * SizeOf(Double);
   FValuesPtr := ReallocMemory(FValuesPtr, byteSize);
   FCapacity := nb;
   FCount := nb;
   System.Move(p^, FValuesPtr^, byteSize);
end;

// MemoryStats
//
procedure TdwsTimeSeriesSequenceData.MemoryStats(var stats : TdwsTimeSeriesMemoryStatistics);
begin
   Inc(stats.PackedBytes, FPackedDataSize);
   Inc(stats.UnPackedBytes, FCapacity*SizeOf(Double));
end;

// Grow
//
procedure TdwsTimeSeriesSequenceData.Grow;
begin
   FCapacity := FCapacity + (FCapacity shr 2) + 8;
   FValuesPtr := ReallocMemory(FValuesPtr, FCapacity*SizeOf(Double));
end;

// GetValues
//
function TdwsTimeSeriesSequenceData.GetValues(index : Integer) : Double;
begin
   Assert(Cardinal(index) < Cardinal(FCount));
   Result := PDoubleArray(FValuesPtr)[index];
end;

// SetValues
//
procedure TdwsTimeSeriesSequenceData.SetValues(index : Integer; const v : Double);
begin
   Assert(Cardinal(index) < Cardinal(FCount));
   PDoubleArray(FValuesPtr)[index] := v;
end;

// Pack
//
procedure TdwsTimeSeriesSequenceData.Pack;
begin
   if FCapacity = 0 then Exit;

   FPackedDataSize := DeltaPackValues(FValuesPtr, FCount, Sequence.ScaleFloatToInt, FPackedData);
   FreeMemory(FValuesPtr);
   FValuesPtr := nil;
   FCapacity := 0;
   FCount := 0;
end;

// Unpack
//
procedure TdwsTimeSeriesSequenceData.Unpack(nbValues : Integer; unpackMode : TdwsTimeSeriesUnpackMode);
begin
   if FCapacity > 0 then Exit;

   FCapacity := nbValues;
   FCount := nbValues;
   FValuesPtr := ReallocMemory(FValuesPtr, nbValues*SizeOf(Double));
   DeltaUnpackValues(FPackedData, nbValues, Sequence.ScaleIntToFloat, FValuesPtr);

   if unpackMode = tsumUnpackAndDelete then begin
      FreeMemory(FPackedData);
      FPackedData := nil;
      FPackedDataSize := 0;
   end;
end;

// ------------------
// ------------------ TdwsTimeSeriesBatch ------------------
// ------------------

// Create
//
constructor TdwsTimeSeriesBatch.Create;
begin
   inherited;
   FDatas := TdwsTimeSeriesSequenceDatas.Create;
end;

// Destroy
//
destructor TdwsTimeSeriesBatch.Destroy;
begin
   inherited;
   FDatas.Free;
   FreeMemory(FPackedTimeStamps);
end;

// PackTimeStamps
//
procedure TdwsTimeSeriesBatch.PackTimeStamps;
begin
   if (NbSamples = 0) or (Length(FTimeStamps) = 0) then Exit;

   FPackedTimeStampsSize := DeltaPackValues(FTimeStamps, NbSamples, 0, FPackedTimeStamps);
   SetLength(FTimeStamps, 0);
end;

// UnpackTimeStamps
//
procedure TdwsTimeSeriesBatch.UnpackTimeStamps(unpackMode : TdwsTimeSeriesUnpackMode);
begin
   if (NbSamples = 0) or (Length(FTimeStamps) > 0) then Exit;

   SetLength(FTimeStamps, NbSamples);
   DeltaUnpackValues(FPackedTimeStamps, NbSamples, 0, FTimeStamps);

   if unpackMode = tsumUnpackAndDelete then begin
      FreeMemory(FPackedTimeStamps);
      FPackedTimeStamps := nil;
      FPackedTimeStampsSize := 0;
   end;
end;

// Pack
//
procedure TdwsTimeSeriesBatch.Pack;
begin
   PackTimeStamps;
   for var i := 0 to FDatas.Count-1 do
      FDatas[i].Pack;
end;

// Unpack
//
procedure TdwsTimeSeriesBatch.Unpack(unpackMode : TdwsTimeSeriesUnpackMode);
begin
   UnPackTimeStamps(unpackMode);
   for var i := 0 to FDatas.Count-1 do
      FDatas[i].UnPack(NbSamples, unpackMode);
end;

// ClearSamplesBefore
//
procedure TdwsTimeSeriesBatch.ClearSamplesBefore(time : Int64);
begin
   if FStartTime >= time then Exit;

   UnpackTimeStamps(tsumUnpackAndDelete);
   var n := 0;
   if not FindTimeIndex(time, n) then
      Dec(n);
   if n > 0 then begin
      Delete(FTimeStamps, 0, n);
      for var i := 0 to FDatas.Count-1 do begin
         var d := FDatas[i];
         d.Unpack(FNbSamples, tsumUnpackAndDelete);
         d.Delete(0, n);
      end;
      Dec(FNbSamples);
      if FNbSamples > 0 then
         FStartTime := FTimeStamps[0];
   end;
end;

// MemoryStats
//
procedure TdwsTimeSeriesBatch.MemoryStats(var stats : TdwsTimeSeriesMemoryStatistics);
begin
   Inc(stats.PackedBytes, FPackedTimeStampsSize);
   Inc(stats.UnPackedBytes, Length(FTimeStamps)*SizeOf(Int64));
   Inc(stats.SampleCount, NbSamples);
   for var i := 0 to FDatas.Count-1 do
      FDatas[i].MemoryStats(stats);
end;

// FindTimeIndex
//
function TdwsTimeSeriesBatch.FindTimeIndex(const time : Int64; var idx : Integer) : Boolean;
var
   lower, upper, mid : Integer;
   cmp : Int64;
begin
   Result := False;
   lower := 0;
   upper := NbSamples - 1;
   while lower <= upper do begin
      mid := (lower + upper) shr 1;
      cmp := FTimeStamps[mid] - time;
      if cmp < 0 then
         lower := mid + 1
      else begin
         upper := mid - 1;
         if cmp = 0 then
            Result := True;
      end;
   end;
   idx := lower;
end;

// SequenceData
//
function TdwsTimeSeriesBatch.SequenceData(seq : TdwsTimeSeriesSequence) : TdwsTimeSeriesSequenceData;

   procedure PreparesDatasUptoIndex(timeSeries : TdwsTimeSeries; seqIndex : Integer);
   begin
      for var i := FDatas.Count to seqIndex do begin
         var data := TdwsTimeSeriesSequenceData.Create(timeSeries.FSequences[i]);
         FDatas.Add(data);
         for var j := 0 to NbSamples-1 do
            data.AddValue(cNaN);
      end;
   end;

begin
   if seq.Index >= FDatas.Count then
      PreparesDatasUptoIndex(seq.Owner, seq.Index);
   Result := FDatas[seq.Index];
end;

// StoreSample
//
procedure TdwsTimeSeriesBatch.StoreSample(seq : TdwsTimeSeriesSequence; const time : Int64; value : Double);
var
   timeIndex : Integer;
begin
   if not IsNan(value) then
      value := RoundTo(value, -seq.Decimals);

   if NbSamples = 0 then begin
      // special case of 1st sample in a batch
      SetLength(FTimeStamps, 1);
      FNbSamples := 1;
      FStartTime := time;
      FStopTime := time;
      FTimeStamps[0] := time;
      SequenceData(seq).Values[0] := value;
      Exit;
   end;

   if Length(FTimeStamps) = 0 then
      UnpackTimeStamps(tsumUnpackAndDelete);
   if not FindTimeIndex(time, timeIndex) then begin
      Unpack(tsumUnpackAndDelete);
      if time < FStartTime then
         FStartTime := time
      else if time > FStopTime then
         FStopTime := time;
      if timeIndex = NbSamples then begin
         SetLength(FTimeStamps, timeIndex + 1);
         FTimeStamps[timeIndex] := time;
         for var i := 0 to FDatas.Count-1 do
            FDatas[i].AddValue(cNaN);
      end else begin
         Insert(time, FTimeStamps, timeIndex);
         for var i := 0 to FDatas.Count-1 do
            FDatas[i].Insert(timeIndex, cNaN);
      end;
      Inc(FNbSamples);
   end;
   var data := SequenceData(seq);
   if data.FCapacity = 0 then
      data.Unpack(NbSamples, tsumUnpackAndDelete);
   data.Values[timeIndex] := value;
end;

// GetTimeStamps
//
function TdwsTimeSeriesBatch.GetTimeStamps(
      const fromTime, toTime : Int64;
      const timeStamps : TScriptDynamicNativeIntegerArray
   ) : Integer;
begin
   UnpackTimeStamps(tsumUnpackAndKeep);

   var i := 0;
   var n := Length(FTimeStamps);
   while (i < n) and (FTimeStamps[i] < fromTime) do
      Inc(i);
   Result := timeStamps.ArrayLength;
   while (i < n) and (FTimeStamps[i] <= toTime) do begin
      timeStamps.Add(FTimeStamps[i]);
      Inc(i);
   end;
   Result := timeStamps.ArrayLength - Result;
end;

// GetSamples
//
function TdwsTimeSeriesBatch.GetSamples(seq : TdwsTimeSeriesSequence; const fromTime, toTime : Int64;
                                        timeStamps : TScriptDynamicNativeIntegerArray;
                                        values : TScriptDynamicNativeFloatArray;
                                        options : TdwsTimeSeriesExtractionOptions) : Integer;
begin
   Result := 0;
   if seq.Index >= FDatas.Count then Exit;

   var data := SequenceData(seq);

   UnpackTimeStamps(tsumUnpackAndKeep);
   data.Unpack(NbSamples, tsumUnpackAndKeep);

   var acceptNulls := not (tseoIgnoreNulls in options);

   var i := 0;
   var n := Length(FTimeStamps);
   while (i < n) and (FTimeStamps[i] < fromTime) do
      Inc(i);
   while (i < n) and (FTimeStamps[i] <= toTime) do begin
      if acceptNulls or not IsNan(data.Values[i]) then begin
         timeStamps.Add(FTimeStamps[i]);
         values.Add(data.Values[i]);
         Inc(Result);
      end;
      Inc(i);
   end;
end;

// ------------------
// ------------------ TdwsTimeSeries ------------------
// ------------------

// Create
//
constructor TdwsTimeSeries.Create(const aName : String);
begin
   inherited Create;
   FName := aName;
   FNextSequenceID := 1;
   FSequences := TdwsTimeSeriesSequences.Create;
   FBatches := TdwsTimeSeriesBatches.Create;
   FLock := TMultiReadSingleWrite.Create;
end;

// Destroy
//
destructor TdwsTimeSeries.Destroy;
begin
   inherited;
   FBatches.Free;
   FSequences.Free;
   FLock.Free;
end;

// CreateSequence
//
function TdwsTimeSeries.CreateSequence(const name : String; decimals : Integer) : TdwsTimeSeriesSequence;
begin
   FLock.BeginWrite;
   try
      Result := TdwsTimeSeriesSequence.Create;
      Result.FOwner := Self;
      Result.FDecimals := decimals;
      Result.FScaleFloatToInt := IntPower(10, decimals);
      Result.FScaleIntToFloat := IntPower(0.1, decimals);
      Result.FIndex := FSequences.Count;
      Result.FName := name;
      FSequences.Add(Result);
   finally
      FLock.EndWrite;
   end;
end;

// SequenceByName
//
function TdwsTimeSeries.SequenceByName(const name : String) : TdwsTimeSeriesSequence;
begin
   FLock.BeginRead;
   try
      for var i := 0 to FSequences.Count-1 do begin
         Result := FSequences[i];
         if Result.Name = name then Exit;
      end;
   finally
      FLock.EndRead;
   end;
   Result := nil;
end;

// SequenceByIndex
//
function TdwsTimeSeries.SequenceByIndex(idx : Integer) : TdwsTimeSeriesSequence;
begin
   FLock.BeginRead;
   try
      if Cardinal(idx) >= Cardinal(FSequences.Count) then
         raise ERangeError.CreateFmt('Index out of range (%d)', [ idx ]);
      Result := FSequences[idx];
   finally
      Flock.EndRead;
   end;
end;

// SequenceCount
//
function TdwsTimeSeries.SequenceCount : Integer;
begin
   Result := FSequences.Count;
end;

// StoreSample
//
procedure TdwsTimeSeries.StoreSample(seq : TdwsTimeSeriesSequence; const time : Int64; const value : Double);
begin
   FLock.BeginWrite;
   try
      if FBatches.Count = 0 then begin
         // special case of first batch
         var batch := TdwsTimeSeriesBatch.Create;
         FBatches.Add(batch);
         batch.StoreSample(seq, time, value);
         Exit;
      end;

      var idx := 0;
      FindBatchIndex(time, idx);
      if idx >= FBatches.Count then
         idx := FBatches.Count-1;
      var batch := FBatches[idx];
      batch.StoreSample(seq, time, value);
      if batch.NbSamples > FLargestBatchSampleCount then
         FLargestBatchSampleCount := batch.NbSamples;
   finally
      FLock.EndWrite;
   end;
end;

// StoreSamples
//
procedure TdwsTimeSeries.StoreSamples(seq : TdwsTimeSeriesSequence; const times, values : IScriptDynArray);
begin
   var nbSamples := times.ArrayLength;
   if nbSamples <> values.ArrayLength then
      raise Exception.Create('Mismatched time & value arrays length');
   if nbSamples = 0 then Exit;

   var sampleIndex := 0;
   FLock.BeginWrite;
   try
      if FBatches.Count = 0 then begin
         // special case of first batch
         var batch := TdwsTimeSeriesBatch.Create;
         FBatches.Add(batch);
         batch.StoreSample(seq, times.AsInteger[sampleIndex], values.AsFloat[sampleIndex]);
         sampleIndex := 1;
      end;

      var batchIndex := 0;
      while sampleIndex < nbSamples do begin
         var t := times.AsInteger[sampleIndex];
         var batch := FBatches[batchIndex];
         if (t < batch.StartTime) or (t > batch.FStopTime) then begin
            FindBatchIndex(t, batchIndex);
            if batchIndex >= FBatches.Count then
               batchIndex := FBatches.Count-1;
            batch := FBatches[batchIndex];
         end;
         batch.StoreSample(seq, t, values.AsFloat[sampleIndex]);
         if batch.NbSamples > FLargestBatchSampleCount then
            FLargestBatchSampleCount := batch.NbSamples;
         Inc(sampleIndex);
      end;
   finally
      FLock.EndWrite;
   end;
end;

// ClearSamples
//
procedure TdwsTimeSeries.ClearSamples;
begin
   FLock.BeginWrite;
   try
      FBatches.Clear;
   finally
      FLock.EndWrite;
   end;
end;

// ClearSamplesBefore
//
procedure TdwsTimeSeries.ClearSamplesBefore(time : Int64);
begin
   FLock.BeginWrite;
   try
      // clear whole batches
      while (FBatches.Count > 0) and (FBatches[0].FStopTime < time) do
         FBatches.Extract(0).Free;
      // clear partial batch
      if FBatches.Count > 0 then begin
         FBatches[0].ClearSamplesBefore(time);
         if FBatches[0].NbSamples = 0 then
            FBatches.Extract(0).Free;
      end;
   finally
      FLock.EndWrite;
   end;
end;

// GetTimeStamps
//
function TdwsTimeSeries.GetTimeStamps(const fromTime, toTime : Int64; const times : IScriptDynArray) : Integer;
begin
   var timesArray := times.GetSelf as TScriptDynamicNativeIntegerArray;

   Result := 0;
   FLock.BeginRead;
   try
      for var i := 0 to FBatches.Count-1 do begin
         var batch := FBatches[i];
         if batch.StopTime < fromTime then continue;
         if batch.StartTime > toTime then break;

         Inc(Result, batch.GetTimeStamps(fromTime, toTime, timesArray));
      end;
   finally
      FLock.EndRead;
   end;
end;

// GetSample
//
function TdwsTimeSeries.GetSample(seq : TdwsTimeSeriesSequence; const timeStamp : Int64) : Double;
var
   batchIndex, timeIndex : Integer;
begin
   FLock.BeginRead;
   try
      var n := FBatches.Count;
      if n > 0 then begin
         FindBatchIndex(timeStamp, batchIndex);
         if batchIndex >= n then
            Dec(batchIndex);
         var batch := FBatches[batchIndex];
         if (timeStamp >= batch.StartTime) and (timeStamp <= batch.StopTime) then begin
            batch.UnpackTimeStamps(tsumUnpackAndKeep);
            if batch.FindTimeIndex(timeStamp, timeIndex) then begin
               var data := batch.SequenceData(seq);
               data.Unpack(batch.NbSamples, tsumUnpackAndKeep);
               Exit(data.Values[timeIndex]);
            end;
         end;
      end;
   finally
      FLock.EndRead;
   end;
   Result := cNaN;
end;

// GetSamples
//
function TdwsTimeSeries.GetSamples(seq : TdwsTimeSeriesSequence; const fromTime, toTime : Int64;
                                   const times, values : IScriptDynArray;
                                   options : TdwsTimeSeriesExtractionOptions) : Integer;
begin
   var timesArray := times.GetSelf as TScriptDynamicNativeIntegerArray;
   var valuesArray := values.GetSelf as TScriptDynamicNativeFloatArray;

   Result := 0;
   FLock.BeginRead;
   try
      for var i := 0 to FBatches.Count-1 do begin
         var batch := FBatches[i];
         if batch.StopTime < fromTime then continue;
         if batch.StartTime > toTime then break;

         Inc(Result, batch.GetSamples(seq, fromTime, toTime, timesArray, valuesArray, options));
      end;
   finally
      FLock.EndRead;
   end;
end;

// NextTimeStamp
//
function TdwsTimeSeries.NextTimeStamp(var timeStamp : Int64) : Boolean;
var
   batchIndex, timeIndex : Integer;
begin
   Result := False;
   FLock.BeginRead;
   try
      if not FindBatchIndex(timeStamp, batchIndex) then begin
         if batchIndex > 0 then
            Dec(batchIndex)
         else if FBatches.Count = 0 then
            Exit;
      end;

      var batch := FBatches[batchIndex];
      if batch.StopTime = timeStamp then begin
         if batchIndex >= FBatches.Count-1 then
            Exit;
         batch := FBatches[batchIndex+1];
         if batch.StartTime > timeStamp then
            Exit;
      end;
      if timeStamp < batch.StartTime then begin
         timeStamp := batch.StartTime;
         Exit(True);
      end;
      batch.UnpackTimeStamps(tsumUnpackAndKeep);
      if batch.FindTimeIndex(timeStamp, timeIndex) then begin
         Assert(timeIndex < High(batch.FTimeStamps));
         timeStamp := batch.FTimeStamps[timeIndex + 1];
         Result := True;
      end;
   finally
      FLock.EndRead;
   end;
end;

// SplitLargeBatches
//
procedure TdwsTimeSeries.SplitLargeBatches(sampleCountTreshold : Integer);
begin
   if sampleCountTreshold <= 1 then Exit;
   if SequenceCount <= 0 then Exit;

   var needSplitsFromIndex : Integer := -1;
   FLock.BeginRead;
   try
      for var i := FBatches.Count-1 downto 0 do begin
         var batch := FBatches[i];
         if batch.NbSamples > sampleCountTreshold then begin
            needSplitsFromIndex := i;
            break;
         end;
      end;
   finally
      FLock.EndRead;
   end;

   if needSplitsFromIndex < 0 then Exit;

   FLock.BeginWrite;
   try
      FLargestBatchSampleCount := 0;
      if needSplitsFromIndex > FBatches.Count-1 then
         needSplitsFromIndex := FBatches.Count-1;
      for var i := needSplitsFromIndex downto 0 do begin
         var batch := FBatches[i];
         if batch.NbSamples <= sampleCountTreshold then begin
            if batch.NbSamples > FLargestBatchSampleCount then
               FLargestBatchSampleCount := batch.NbSamples;
            continue;
         end;

         var newBatch := TdwsTimeSeriesBatch.Create;
         FBatches.Insert(i, newBatch);
         var nbToNewBatch := batch.NbSamples div 2;

         batch.UnpackTimeStamps(tsumUnpackAndDelete);
         newBatch.FTimeStamps := Copy(batch.FTimeStamps, 0, nbToNewBatch);
         newBatch.FNbSamples := nbToNewBatch;

         for var k := 0 to batch.FDatas.Count-1 do begin
            var data := batch.FDatas[k];
            data.Unpack(batch.NbSamples, tsumUnpackAndDelete);
            var newData := TdwsTimeSeriesSequenceData.Create(data.Sequence);
            newBatch.FDatas.Add(newData);
            newData.SetFrom(data.FValuesPtr, nbToNewBatch);
            data.Delete(0, nbToNewBatch);
         end;

         Dec(batch.FNbSamples, nbToNewBatch);

         Delete(batch.FTimeStamps, 0, nbToNewBatch);
         newBatch.FStartTime := batch.StartTime;
         batch.FStartTime := batch.FTimeStamps[0];
         newBatch.FStopTime := newBatch.FTimeStamps[nbToNewBatch-1];

         if nbToNewBatch > FLargestBatchSampleCount then
            FLargestBatchSampleCount := nbToNewBatch;
      end;
   finally
      FLock.EndWrite
   end;
end;

// Pack
//
procedure TdwsTimeSeries.Pack;
begin
   FLock.BeginWrite;
   try
      for var i := 0 to FBatches.Count-1 do
         FBatches[i].Pack;
   finally
      FLock.EndWrite;
   end;
end;

// Unpack
//
procedure TdwsTimeSeries.Unpack(unpackMode : TdwsTimeSeriesUnpackMode);
begin
   FLock.BeginWrite;
   try
      for var i := 0 to FBatches.Count-1 do
         FBatches[i].Unpack(unpackMode);
   finally
      FLock.EndWrite;
   end;
end;

// MemoryStats
//
function TdwsTimeSeries.MemoryStats : TdwsTimeSeriesMemoryStatistics;
begin
   Result := Default(TdwsTimeSeriesMemoryStatistics);
   FLock.BeginRead;
   try
      for var i := 0 to FBatches.Count-1 do
         FBatches[i].MemoryStats(Result);
   finally
      FLock.EndRead;
   end;
end;

// FindBatchIndex
//
function TdwsTimeSeries.FindBatchIndex(const time : Int64; var idx : Integer) : Boolean;
var
   lower, upper, mid : Integer;
   cmp : Int64;
begin
   Result := False;
   lower := 0;
   upper := FBatches.Count - 1;
   while lower <= upper do begin
      mid := (lower + upper) shr 1;
      cmp := FBatches[mid].StartTime - time;
      if cmp < 0 then
         lower := mid + 1
      else begin
         upper := mid - 1;
         if cmp = 0 then
            Result := True;
      end;
   end;
   idx := lower;
end;

// ------------------
// ------------------ TimeSeriesPool ------------------
// ------------------

var
   vPoolLock : TMultiReadSingleWrite;
   vPoolFirst : TdwsTimeSeries;

// Initialize
//
class procedure TimeSeriesPool.Initialize;
begin
   vPoolLock := TMultiReadSingleWrite.Create;
end;

// Finalize
//
class procedure TimeSeriesPool.Finalize;
begin
   TimeSeriesPool.Cleanup;
   FreeAndNil(vPoolLock);
end;

// Acquire
//
class function TimeSeriesPool.Acquire(const aName : String) : TdwsTimeSeries;
begin
   // first pass with read lock
   vPoolLock.BeginRead;
   try
      Result := vPoolFirst;
      while (Result <> nil) and (Result.Name <> aName) do
         Result := Result.FPoolNext;
      if Result <> nil then begin
         AtomicIncrement(Result.FUseCount);
         Exit;
      end;
   finally
      vPoolLock.EndRead;
   end;

   // 2nd pass with write lock to avoid duplicates
   vPoolLock.BeginWrite;
   try
      Result := vPoolFirst;
      while (Result <> nil) and (Result.Name <> aName) do
         Result := Result.FPoolNext;
      if Result <> nil then begin
         Inc(Result.FUseCount);
         Exit;
      end;
      Result := TdwsTimeSeries.Create(aName);
      Result.FPoolNext := vPoolFirst;
      vPoolFirst := Result;
   finally
      vPoolLock.EndWrite;
   end;
   Result := TdwsTimeSeries.Create(aName);
end;

// Release
//
class procedure TimeSeriesPool.Release(ts : TdwsTimeSeries);
begin
   if ts <> nil then begin
      Assert(ts.FUseCount > 0);
      AtomicDecrement(ts.FUseCount);
   end;
end;

// Delete
//
class procedure TimeSeriesPool.Delete(const aName : String);
begin
   vPoolLock.BeginWrite;
   try
      var ts := vPoolFirst;
      var prev : TdwsTimeSeries := nil;
      while (ts <> nil) and (ts.Name <> aName) do begin
         prev := ts;
         ts := ts.FPoolNext;
      end;
      if ts <> nil then begin
         if ts.FUseCount = 0 then
            raise Exception.CreateFmt('TimeSeries "%s" is in use, cannot delete', [ aName ]);
         if prev <> nil then
            prev.FPoolNext := ts.FPoolNext
         else vPoolFirst := ts.FPoolNext;
         ts.Free;
      end;
   finally
      vPoolLock.EndWrite;
   end;
end;

// Cleanup
//
class procedure TimeSeriesPool.Cleanup;
begin
   vPoolLock.BeginWrite;
   try
      // ensure that no pool is still in use
      var p := vPoolFirst;
      while (p <> nil) and (p.FUseCount = 0) do
         p := p.FPoolNext;
      if p <> nil then
         raise Exception.CreateFmt('TimeSeries "%s" is in use, cannot cleanup', [ p.Name ]);

      while vPoolFirst <> nil do begin
         p := vPoolFirst.FPoolNext;
         vPoolFirst.Free;
         vPoolFirst := p;
      end;
   finally
      vPoolLock.EndWrite;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TimeSeriesPool.Initialize;

finalization

   TimeSeriesPool.Finalize;

end.
