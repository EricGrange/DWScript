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
unit dwsSampling;

{$I dws.inc}

interface

uses Windows, Classes, dwsDebugger, dwsUtils, dwsErrors, dwsSymbols,
   SysUtils, dwsExprs, dwsJSON, MMSystem, dwsStrings, dwsScriptSource;

type

   TdwsSample = class (TRefCountedObject)
      private
         FSourceName : UnicodeString;
         FFuncName : UnicodeString;
         FLine : Integer;
         FCount : Integer;

      public
         property SourceName : UnicodeString read FSourceName write FSourceName;
         property FuncName : UnicodeString read FFuncName write FFuncName;
         property Line : Integer read FLine write FLine;
         property Count : Integer read FCount write FCount;
   end;

   // TdwsSamplings
   //
   TdwsSamplings = class(TSortedList<TdwsSample>)
      private
         FTemp : TdwsSample;

      protected
         function Compare(const item1, item2 : TdwsSample) : Integer; override;

      public
         constructor Create;
         destructor Destroy; override;

         procedure AddSample(scriptPos : TScriptPos; funcSym : TFuncSymbol);
         procedure Clear;

         procedure ToJSON(writer : TdwsJSONWriter);
         function ToString : UnicodeString; override;
   end;

   // TdwsSamplingDebugger
   //
   TdwsSamplingDebugger = class (TdwsSimpleDebugger)
      private
         FSamplings : TdwsSamplings;
         FSamplingPos : TScriptPos;
         FSamplingFuncStack : TTightStack;
         FTimerID : Integer;
         FSamplingInterval : Integer;
         FCollecting : Boolean;
         FSuspended : Boolean;

      protected
         procedure StartDebug(exec : TdwsExecution); override;
         procedure DoDebug(exec : TdwsExecution; expr : TExprBase); override;
         procedure StopDebug(exec : TdwsExecution); override;
         procedure EnterFunc(exec : TdwsExecution; funcExpr : TExprBase); override;
         procedure LeaveFunc(exec : TdwsExecution; funcExpr : TExprBase); override;

         procedure CollectSample;

      public
         constructor Create(aOwner : TComponent); override;
         destructor Destroy; override;

         property Samplings : TdwsSamplings read FSamplings;
         property SamplingInterval : Integer read FSamplingInterval write FSamplingInterval;
         property Suspended : Boolean read FSuspended write FSuspended;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   // TdwsSamplingsSorter
   //
   TdwsSamplingsSorter = class(TSortedList<TdwsSample>)
      protected
         function Compare(const item1, item2 : TdwsSample) : Integer; override;
   end;

// Compare
//
function TdwsSamplingsSorter.Compare(const item1, item2 : TdwsSample) : Integer;
begin
   Result:=CompareStr(item1.SourceName, item2.SourceName);
   if Result<>0 then Exit;
   Result:=CompareStr(item1.FuncName, item2.FuncName);
   if Result<>0 then Exit;
   Result:=item1.Line-item2.Line;
end;

// ------------------
// ------------------ TdwsSamplings ------------------
// ------------------

// Compare
//
function TdwsSamplings.Compare(const item1, item2 : TdwsSample) : Integer;
begin
   Result:=item1.Line-item2.Line;
   if Result<>0 then Exit;
   Result:=CompareStr(item1.FuncName, item2.FuncName);
   if Result<>0 then Exit;
   Result:=CompareStr(item1.SourceName, item2.SourceName);
end;

// Create
//
constructor TdwsSamplings.Create;
begin
   inherited;
   FTemp:=TdwsSample.Create;
end;

// Destroy
//
destructor TdwsSamplings.Destroy;
begin
   Clean;
   FTemp.Free;
   inherited;
end;

// AddSample
//
procedure TdwsSamplings.AddSample(scriptPos : TScriptPos; funcSym : TFuncSymbol);
var
   idx : Integer;
   added : Boolean;
begin
   if scriptPos.SourceFile<>nil then
      FTemp.FSourceName:=scriptPos.SourceName
   else FTemp.FSourceName:=MSG_MainModule;
   if funcSym<>nil then
      FTemp.FuncName:=funcSym.QualifiedName
   else FTemp.FuncName:=MSG_MainFunction;
   FTemp.Line:=scriptPos.Line;
   idx:=AddOrFind(FTemp, added);
   if added then begin
      FTemp.Count:=1;
      FTemp:=TdwsSample.Create;
   end else begin
      Inc(Items[idx].FCount);
   end;
end;

// Clear
//
procedure TdwsSamplings.Clear;
begin
   Clean;
end;

// ToJSON
//
procedure TdwsSamplings.ToJSON(writer : TdwsJSONWriter);

   procedure BeginFunc(const func : UnicodeString);
   begin
      writer.BeginObject;
      writer.WriteName('Func');
      writer.WriteString(func);
      writer.BeginArray;
   end;

var
   i : Integer;
   sorter : TdwsSamplingsSorter;
   sourceFile : UnicodeString;
   func : UnicodeString;
   sample : TdwsSample;
begin
   writer.BeginArray;
   sorter:=TdwsSamplingsSorter.Create;
   try
      for i:=0 to Count-1 do
         sorter.Add(Items[i]);

      sourceFile:='';
      func:='';
      for i:=0 to Count-1 do begin
         sample:=sorter[i];
         if sourceFile<>sample.SourceName then begin
            if sourceFile<>'' then begin
               writer.EndArray; // func
               writer.EndObject;
               writer.EndArray; // file
               writer.EndObject;
            end;
            sourceFile:=sample.SourceName;
            func:=sample.FuncName;
            writer.BeginObject;
            writer.WriteName('File');
            writer.WriteString(sourceFile);
            writer.BeginArray;

            BeginFunc(func);
         end;
         if func<>sample.FuncName then begin
            if func<>'' then begin
               writer.EndArray;
               writer.EndObject;
            end;
            func:=sample.FuncName;
            BeginFunc(func);
         end;

         writer.BeginObject;
         writer.WriteName('Line');
         writer.WriteNumber(sample.Line);
         writer.EndObject;

      end;
      sorter.Clear;
   finally
      sorter.Free;
   end;
   writer.EndArray;
end;

// ToString
//
function TdwsSamplings.ToString : UnicodeString;
var
   wobs : TWriteOnlyBlockStream;
   wr : TdwsJSONWriter;
begin
   wobs:=TWriteOnlyBlockStream.Create;
   wr:=TdwsJSONWriter.Create(wobs);
   try
      ToJSON(wr);
      Result:=wobs.ToString;
   finally
      wr.Free;
      wobs.Free;
   end;
end;

// ------------------
// ------------------ TdwsSamplingDebugger ------------------
// ------------------

// Create
//
constructor TdwsSamplingDebugger.Create(aOwner : TComponent);
begin
   inherited;
   FSamplings:=TdwsSamplings.Create;
   FSamplingInterval:=10;
end;

// Destroy
//
destructor TdwsSamplingDebugger.Destroy;
begin
   FSamplings.Free;
   FSamplingFuncStack.Free;
   inherited;
end;

// CollectSample
//
procedure TdwsSamplingDebugger.CollectSample;
begin
   if FSuspended then Exit;
   if not FSamplingPos.Defined then Exit; // not started
   if FCollecting then Exit; // ignore sample if too busy
   FCollecting:=True;
   try
      FSamplings.AddSample(FSamplingPos, TFuncSymbol(FSamplingFuncStack.Peek));
   finally
      FCollecting:=False;
   end;
end;

// ProfilerTimeCallBack
//
procedure ProfilerTimeCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); pascal;
begin
   TdwsSamplingDebugger(dwUser).CollectSample;
end;

// StartDebug
//
procedure TdwsSamplingDebugger.StartDebug(exec : TdwsExecution);
begin
   FSamplings.Clean;
   FSamplingFuncStack.Clear;
   FSamplingFuncStack.Push(nil);
   FSamplingPos:=cNullPos;
   if FSamplingInterval<1 then
      FSamplingInterval:=1;
   FTimerID:=timeSetEvent(FSamplingInterval, 0, @ProfilerTimeCallBack,
                          Cardinal(Self), TIME_PERIODIC);
   inherited;
end;

// DoDebug
//
procedure TdwsSamplingDebugger.DoDebug(exec : TdwsExecution; expr : TExprBase);
var
   sample : TScriptPos;
begin
   sample:=expr.ScriptPos;
   if sample.Defined then
      FSamplingPos:=sample;
   inherited;
end;

// StopDebug
//
procedure TdwsSamplingDebugger.StopDebug(exec : TdwsExecution);
begin
   if FTimerID>0 then begin
      TimeKillEvent(FTimerID);
      FTimerID:=0;
   end;
   inherited;
end;

// EnterFunc
//
procedure TdwsSamplingDebugger.EnterFunc(exec : TdwsExecution; funcExpr : TExprBase);
begin
   if (funcExpr is TFuncExprBase) then
      FSamplingFuncStack.Push(TFuncExprBase(funcExpr).FuncSym);
   inherited;
end;

// LeaveFunc
//
procedure TdwsSamplingDebugger.LeaveFunc(exec : TdwsExecution; funcExpr : TExprBase);
begin
   if (funcExpr is TFuncExprBase) then
      FSamplingFuncStack.Pop;
   inherited;
end;

end.
