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
unit dwsTimeSeriesLibModule;

interface

uses
  System.SysUtils, System.Classes, dwsComp, dwsExprs, dwsExprList, dwsSymbols;

type
  TdwsTimeSeriesLib = class(TDataModule)
    dwsTimeSeries: TdwsUnit;
    procedure dwsTimeSeriesClassesTimeSeriesCleanUp(ExternalObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsAddSequenceEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsSequenceCountEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsGetSequenceNameEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsStoreSampleEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsDisconnectEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesConstructorsConnectEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsStoreSamplesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsOptimizeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsExtractSamplesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsClearSamplesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsMemoryStatisticsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsExtractTimeStampsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsGetSampleEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsNextTimeStampEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTimeSeriesClassesTimeSeriesMethodsStoreSample_IntegerIntegerFloat_FastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
  private
    FScript : TDelphiWebScript;
  protected
    procedure SetScript(const val : TDelphiWebScript);
  public
    property Script : TDelphiWebScript read FScript write SetScript;
  end;

var
   vTimeSeriesLargeBatchTreshold : Integer = 500;
   vTimeSeriesLargeBatchTresholdAutoSplit : Integer = 1000;

implementation

uses
   dwsUtils, dwsTimeSeries;

{$R *.dfm}

function SeqByName(Info: TProgramInfo; extObject : TObject) : TdwsTimeSeriesSequence;
begin
   var name := Info.ParamAsString[0];
   Result := TdwsTimeSeries(extObject).SequenceByName(name);
   if Result = nil then
      raise Exception.CreateFmt('Unknown sequence "%s"', [ name ]);
end;

procedure AutoSplit(ts : TdwsTimeSeries);
begin
   if ts.LargestBatchSampleCount > vTimeSeriesLargeBatchTresholdAutoSplit then
      ts.SplitLargeBatches(vTimeSeriesLargeBatchTreshold);
end;

function ExtractOptionsFromInteger(i : Integer) : TdwsTimeSeriesExtractionOptions;
begin
   Result := [];
   if (i and (1 shl Ord(tseoIgnoreNulls))) <> 0 then Include(Result, tseoIgnoreNulls);
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesCleanUp(
  ExternalObject: TObject);
begin
   var ts := TdwsTimeSeries(ExternalObject);
   if ts.Name = '' then
      ExternalObject.Free
   else TimeSeriesPool.Release(ts);
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject := TdwsTimeSeries.Create('');
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesConstructorsConnectEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   var name := Info.ParamAsString[0];
   if name = '' then
      raise Exception.Create('Pooled TimeSeries cannot have an empty name');
   ExtObject := TimeSeriesPool.Acquire(name);
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsAddSequenceEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var seq := TdwsTimeSeries(ExtObject).CreateSequence(Info.ParamAsString[0], Info.ParamAsInteger[1]);
   Info.ResultAsInteger := seq.Index;
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsClearSamplesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   TdwsTimeSeries(ExtObject).ClearSamples;
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsDisconnectEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   if TdwsTimeSeries(ExtObject).Name = '' then
      raise Exception.Create('Not a pooled TimeSeries, cannot disconnect');
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsExtractSamplesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := TdwsTimeSeries(ExtObject).GetSamples(
      SeqByName(Info, ExtObject),
      Info.ParamAsInteger[1], Info.ParamAsInteger[2],
      Info.ParamAsScriptDynArray[3], Info.ParamAsScriptDynArray[4],
      ExtractOptionsFromInteger(Info.ParamAsInteger[5])
   )
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsExtractTimeStampsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := TdwsTimeSeries(ExtObject).GetTimeStamps(
      Info.ParamAsInteger[0], Info.ParamAsInteger[1],
      Info.ParamAsScriptDynArray[2]
   )
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsGetSampleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat := TdwsTimeSeries(ExtObject).GetSample(
      SeqByName(Info, ExtObject),
      Info.ParamAsInteger[1]
   );
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsGetSequenceNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := TdwsTimeSeries(ExtObject).SequenceByIndex(Info.ParamAsInteger[0]).Name;
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsMemoryStatisticsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var stats := TdwsTimeSeries(ExtObject).MemoryStats;
   var r := Info.ResultVars;
   r.Member['PackedBytes'].ValueAsInteger := stats.PackedBytes;
   r.Member['UnPackedBytes'].ValueAsInteger := stats.UnPackedBytes;
   r.Member['SampleCount'].ValueAsInteger := stats.SampleCount;
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsNextTimeStampEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var t := Info.ParamAsInteger[0];
   var r := TdwsTimeSeries(ExtObject).NextTimeStamp(t);
   Info.ResultAsBoolean := r;
   if r then
      Info.ParamAsInteger[0] := t;
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsOptimizeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   TdwsTimeSeries(ExtObject).SplitLargeBatches(vTimeSeriesLargeBatchTreshold);
   TdwsTimeSeries(ExtObject).Pack;
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsSequenceCountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := TdwsTimeSeries(ExtObject).SequenceCount;
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsStoreSampleEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var ts := TdwsTimeSeries(ExtObject);
   ts.StoreSample(
      SeqByName(Info, ExtObject), Info.ParamAsInteger[1], Info.ParamAsFloat[2]
   );
   if ts.LargestBatchSampleCount > vTimeSeriesLargeBatchTresholdAutoSplit then
      ts.SplitLargeBatches(vTimeSeriesLargeBatchTreshold);
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsStoreSamplesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   var ts := TdwsTimeSeries(ExtObject);
   ts.StoreSamples(
      SeqByName(Info, ExtObject), Info.ParamAsScriptDynArray[1], Info.ParamAsScriptDynArray[2]
   );
   AutoSplit(ts);
end;

procedure TdwsTimeSeriesLib.dwsTimeSeriesClassesTimeSeriesMethodsStoreSample_IntegerIntegerFloat_FastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   tsObj : IScriptObj;
begin
   baseExpr.EvalAsScriptObj(args.Exec, tsObj);
   (args.Expr as TProgramExpr).CheckScriptObject(args.Exec, tsObj);
   var ts := TdwsTimeSeries(tsObj.ExternalObject);
   ts.StoreSample(
      ts.SequenceByIndex(args.AsInteger[0]),
      args.AsInteger[1], args.AsFloat[2]
   );
   AutoSplit(ts);
end;

procedure TdwsTimeSeriesLib.SetScript(const val : TDelphiWebScript);
begin
   dwsTimeSeries.Script := val;
end;

end.
