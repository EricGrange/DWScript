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

uses Classes, SysUtils, dwsUtils, dwsDynamicArrays, dwsXPlatform, dwsSymbols;

type
   TdwsTimeSeries = class;

   TdwsTimeSeriesSequence = class (TRefCountedObject)
      private
         FOwner : TdwsTimeSeries;
         FName : String;
         FDecimals : Integer;
         FIndex : Integer;

      public
         property Owner : TdwsTimeSeries read FOwner;
         property Name : String read FName;
         property Decimals : Integer read FDecimals;
         property Index : Integer read FIndex;
   end;
   TdwsTimeSeriesSequences = TObjectList<TdwsTimeSeriesSequence>;

   TdwsTimeSeriesUnpackMode = ( tsumUnpackAndKeep, tsumUnpackAndDelete );

   TdwsTimeSeriesSequenceData = class (TRefCountedObject)
      private
         FSequence : TdwsTimeSeriesSequence;
         FPackedData : Pointer;
         FPackedDataSize : Integer;
         FValues : TDoubleDynArray;

      protected
         procedure Pack;
         procedure Unpack(nbValues : Integer; unpackMode : TdwsTimeSeriesUnpackMode);

      public
         constructor Create(aSequence : TdwsTimeSeriesSequence);
         destructor Destroy; override;

         property Sequence : TdwsTimeSeriesSequence read FSequence;
         property Values : TDoubleDynArray read FValues;

         procedure MemoryStats(var packedSize, unpackedUse : Int64);
   end;
   TdwsTimeSeriesSequenceDatas = TObjectList<TdwsTimeSeriesSequenceData>;

   TdwsTimeSeriesExtractionOption = (
      tseoIgnoreNulls = 1
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

         procedure MemoryStats(var packedSize, unpackedUse : Int64);
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

         function GetSamples(seq : TdwsTimeSeriesSequence; const fromTime, toTime : Int64;
                             const times, values : IScriptDynArray;
                             options : TdwsTimeSeriesExtractionOptions) : Integer;

         procedure SplitLargeBatches(sampleCountTreshold : Integer);

         procedure Pack;
         procedure Unpack(unpackMode : TdwsTimeSeriesUnpackMode);

         procedure MemoryStats(var packedSize, unpackedUse : Int64);
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

   FreeMem(packedData);
   if nbValues = 0 then Exit(0);

   GetMem(packedData, nbValues*9); // worst case all 8 bytes values
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
   ReallocMem(packedData, Result);
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
   FreeMem(FPackedData);
end;

// MemoryStats
//
procedure TdwsTimeSeriesSequenceData.MemoryStats(var packedSize, unpackedUse : Int64);
begin
   Inc(packedSize, FPackedDataSize);
   Inc(unpackedUse, Length(FValues)*SizeOf(Double));
end;

// Unpack
//
procedure TdwsTimeSeriesSequenceData.Unpack(nbValues : Integer; unpackMode : TdwsTimeSeriesUnpackMode);
begin
   if Length(FValues) > 0 then Exit;

   SetLength(FValues, nbValues);
   DeltaUnpackValues(FPackedData, nbValues, IntPower(0.1, Sequence.Decimals), FValues);

   if unpackMode = tsumUnpackAndDelete then begin
      FreeMem(FPackedData);
      FPackedDataSize := 0;
   end;
end;

// Pack
//
procedure TdwsTimeSeriesSequenceData.Pack;
begin
   if Length(FValues) = 0 then Exit;

   FPackedDataSize := DeltaPackValues(FValues, Length(FValues), IntPower(10, Sequence.Decimals), FPackedData);
   SetLength(FValues, 0);
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
   FreeMem(FPackedTimeStamps);
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
      FreeMem(FPackedTimeStamps);
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
         Delete(d.FValues, 0, n);
      end;
      Dec(FNbSamples);
      if FNbSamples > 0 then
         FStartTime := FTimeStamps[0];
   end;
end;

// MemoryStats
//
procedure TdwsTimeSeriesBatch.MemoryStats(var packedSize, unpackedUse : Int64);
begin
   Inc(packedSize, FPackedTimeStampsSize);
   Inc(unpackedUse, Length(FTimeStamps)*SizeOf(Int64));
   for var i := 0 to FDatas.Count-1 do
      FDatas[i].MemoryStats(packedSize, unpackedUse);
end;

// FindTimeIndex
//
function TdwsTimeSeriesBatch.FindTimeIndex(const time : Int64; var idx : Integer) : Boolean;
var
   lower, upper, mid, cmp: Integer;
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
         SetLength(data.FValues, NbSamples);
         for var j := 0 to NbSamples-1 do
            data.FValues[j] := cNaN;
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
      SequenceData(seq).FValues[0] := value;
      Exit;
   end;

   Unpack(tsumUnpackAndDelete);
   if not FindTimeIndex(time, timeIndex) then begin
      if time < FStartTime then
         FStartTime := time
      else if time > FStopTime then
         FStopTime := time;
      Insert(time, FTimeStamps, timeIndex);
      for var i := 0 to FDatas.Count-1 do
         Insert(cNaN, FDatas[i].FValues, timeIndex);
      Inc(FNbSamples);
   end;
   FDatas[seq.Index].FValues[timeIndex] := value;
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

   var data := FDatas[seq.Index];

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
      FBatches[idx].StoreSample(seq, time, value);
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
      if needSplitsFromIndex > FBatches.Count-1 then
         needSplitsFromIndex := FBatches.Count-1;
      for var i := needSplitsFromIndex downto 0 do begin
         var batch := FBatches[i];
         if batch.NbSamples <= sampleCountTreshold then continue;

         var newBatch := TdwsTimeSeriesBatch.Create;
         FBatches.Insert(i, newBatch);
         var nbToNewBatch := batch.NbSamples div 2;

         batch.UnpackTimeStamps(tsumUnpackAndDelete);
         newBatch.FTimeStamps := Copy(batch.FTimeStamps, 0, nbToNewBatch);
         Delete(batch.FTimeStamps, 0, nbToNewBatch);

         for var k := 0 to batch.FDatas.Count-1 do begin
            var data := batch.FDatas[k];
            data.Unpack(batch.NbSamples, tsumUnpackAndDelete);
            var newData := TdwsTimeSeriesSequenceData.Create(data.Sequence);
            newData.FValues := Copy(data.FValues, 0, nbToNewBatch);
            Delete(data.FValues, 0, nbToNewBatch);
         end;
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
procedure TdwsTimeSeries.MemoryStats(var packedSize, unpackedUse : Int64);
begin
   FLock.BeginRead;
   try
      for var i := 0 to FBatches.Count-1 do
         MemoryStats(packedSize, unpackedUse);
   finally
      FLock.EndRead;
   end;
end;

// FindBatchIndex
//
function TdwsTimeSeries.FindBatchIndex(const time : Int64; var idx : Integer) : Boolean;
var
   lower, upper, mid, cmp: Integer;
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
