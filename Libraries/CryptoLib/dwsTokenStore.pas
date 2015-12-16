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
unit dwsTokenStore;

{$I dws.inc}

interface

uses dwsUtils, dwsXPlatform, dwsJSON;

type

   TdwsToken = record
      HashCode : Integer;
      Token : String;
      Expire : TDateTime;
   end;

   TdwsTokenHash = class(TSimpleHash<TdwsToken>)
      protected
         function SameItem(const item1, item2 : TdwsToken) : Boolean; override;
         function GetItemHashCode(const item1 : TdwsToken) : Integer; override;
   end;

   TdwsTokenStore = class
      private
         FLock : TMultiReadSingleWrite;
         FHash : TdwsTokenHash;
         FCollection : ITimer;
         FCollectionIntervalMilliseconds : Integer;

      protected
         procedure ScheduleCollection;
         function CollectToken(const item : TdwsToken) : TSimpleHashAction;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Register(const aToken : String; ttlSeconds : Double);

         function CheckAndKeep(const aToken : String) : Boolean;
         function CheckAndClear(const aToken : String) : Boolean;

         procedure Clear;
         procedure Collect;

         procedure SaveToJSON(writer : TdwsJSONWriter);
         procedure LoadFromJSON(json : TdwsJSONValue);

         procedure SaveToFile(const fileName : String);
         procedure LoadFromFile(const fileName : String);

         property CollectionIntervalMilliseconds : Integer read FCollectionIntervalMilliseconds write FCollectionIntervalMilliseconds;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cCollectionIntervalMilliseconds = 5000;

// ------------------
// ------------------ TdwsTokenHash ------------------
// ------------------

// SameItem
//
function TdwsTokenHash.SameItem(const item1, item2 : TdwsToken) : Boolean;
begin
   Result:=(item1.HashCode=item2.HashCode) and (item1.Token=item2.Token);
end;

// GetItemHashCode
//
function TdwsTokenHash.GetItemHashCode(const item1 : TdwsToken) : Integer;
begin
   Result:=item1.HashCode;
end;

// ------------------
// ------------------ TdwsTokenStore ------------------
// ------------------

// Create
//
constructor TdwsTokenStore.Create;
begin
   inherited;
   FHash:=TdwsTokenHash.Create;
   FLock:=TMultiReadSingleWrite.Create;
   FCollectionIntervalMilliseconds:=cCollectionIntervalMilliseconds;
end;

// Destroy
//
destructor TdwsTokenStore.Destroy;
begin
   FCollection:=nil;
   FLock.Free;
   FHash.Free;
   inherited;
end;

// Register
//
procedure TdwsTokenStore.Register(const aToken : String; ttlSeconds : Double);
var
   token : TdwsToken;
begin
   token.HashCode:=SimpleStringHash(aToken);
   token.Token:=aToken;
   token.Expire:=UTCDateTime+ttlSeconds*(1/86400);
   FLock.BeginWrite;
   try
      FHash.Add(token);
      if FCollection=nil then
         ScheduleCollection;
   finally
      FLock.EndWrite;
   end;
end;

// CheckAndKeep
//
function TdwsTokenStore.CheckAndKeep(const aToken : String) : Boolean;
var
   token : TdwsToken;
   t : TDateTime;
begin
   token.HashCode:=SimpleStringHash(aToken);
   token.Token:=aToken;
   t:=UTCDateTime;
   FLock.BeginRead;
   try
      if FHash.Match(token) then
         Result:=(token.Expire>t)
      else Result:=False;
   finally
      FLock.EndRead;
   end;

end;

// CheckAndClear
//
function TdwsTokenStore.CheckAndClear(const aToken : String) : Boolean;
var
   token : TdwsToken;
   t : TDateTime;
begin
   token.HashCode:=SimpleStringHash(aToken);
   token.Token:=aToken;
   t:=UTCDateTime;
   FLock.BeginWrite;
   try
      if FHash.Match(token) then begin
         Result:=(token.Expire>t);
         FHash.Remove(token);
      end else Result:=False;
   finally
      FLock.EndWrite;
   end;
end;

// Clear
//
procedure TdwsTokenStore.Clear;
begin
   FLock.BeginWrite;
   try
      FHash.Clear;
   finally
      FLock.EndWrite;
   end;
end;

// Collect
//
procedure TdwsTokenStore.Collect;
begin
   FLock.BeginWrite;
   try
      if FHash.Count>0 then begin
         FHash.Enumerate(CollectToken);
         if FHash.Count>0 then
            ScheduleCollection
         else FHash.Clear;
      end;
   finally
      FLock.EndWrite;
   end;
end;

// CollectToken
//
function TdwsTokenStore.CollectToken(const item : TdwsToken) : TSimpleHashAction;
begin
   Result:=shaRemove;
end;

type
   TJSONStreamer = class
      Writer : TdwsJSONWriter;
      T : TDateTime;
      function Write(const item : TdwsToken) : TSimpleHashAction;
   end;

function TJSONStreamer.Write(const item : TdwsToken) : TSimpleHashAction;
begin
   if item.Expire>T then begin
      Writer.WriteName(item.Token);
      Writer.WriteNumber(item.Expire);
      Result:=shaNone;
   end else Result:=shaRemove;
end;

// SaveToJSON
//
procedure TdwsTokenStore.SaveToJSON(writer : TdwsJSONWriter);
var
   streamer : TJSONStreamer;
begin
   streamer:=TJSONStreamer.Create;
   try
      streamer.Writer:=writer;
      streamer.T:=UTCDateTime;
      writer.BeginObject;
      FLock.BeginRead;
      try
         FHash.Enumerate(streamer.Write);
      finally
         FLock.EndRead;
      end;
      writer.EndObject;
   finally
      streamer.Free;
   end;
end;

// LoadFromJSON
//
procedure TdwsTokenStore.LoadFromJSON(json : TdwsJSONValue);
var
   i : Integer;
   token : TdwsToken;
   t : TDateTime;
begin
   t:=UTCDateTime;
   FLock.BeginWrite;
   try
      FHash.Clear;
      for i:=0 to json.ElementCount-1 do begin
         token.Expire:=json.Elements[i].AsNumber;
         if token.Expire>t then begin
            token.Token:=json.Names[i];
            token.HashCode:=SimpleStringHash(token.Token);
            FHash.Add(token);
         end;
      end;
   finally
      FLock.EndWrite;
   end;
end;

// SaveToFile
//
procedure TdwsTokenStore.SaveToFile(const fileName : String);
var
   wr : TdwsJSONWriter;
begin
   wr:=TdwsJSONWriter.Create(nil);
   try
      SaveToJSON(wr);
      SaveTextToUTF8File(fileName, wr.ToString);
   finally
      wr.Free;
   end;
end;

// LoadFromFile
//
procedure TdwsTokenStore.LoadFromFile(const fileName : String);
var
   json : TdwsJSONValue;
begin
   json:=TdwsJSONValue.ParseFile(fileName);
   try
      LoadFromJSON(json);
   finally
      json.Free;
   end;
end;

// ScheduleCollection
//
procedure TdwsTokenStore.ScheduleCollection;
begin
   FCollection:=TTimerTimeout.Create(cCollectionIntervalMilliseconds, Collect);
end;

end.
