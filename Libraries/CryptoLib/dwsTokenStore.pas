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
{$R-}

interface

uses
   dwsUtils, dwsXPlatform, dwsCryptoXPlatform,
   dwsJSON, dwsXXHash;

type

   TdwsToken = record
      HashCode : Integer;
      Token : String;
      DataHashCode : Integer;
      Data : String;
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
         FCollectData : String;
         FCollectDataHash : Integer;
         FHashSalt : Cardinal;

      protected
         procedure ScheduleCollection;
         function CollectTokenByData(const item : TdwsToken) : TSimpleHashAction;

         function GetTokenData(const aToken : String) : String;

         function HashToken(const aToken : String) : Cardinal; inline;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Register(const aToken : String; ttlSeconds : Double; const aData : String);

         function CheckAndKeep(const aToken, aData : String) : Boolean;
         function CheckAndRemove(const aToken, aData : String) : Boolean;

         property TokenData[const aToken : String] : String read GetTokenData;

         procedure Remove(const aToken : String);
         procedure RemoveByData(const aData : String);
         procedure Clear;
         procedure Collect;
         function  Count : Integer;

         procedure SaveToJSON(writer : TdwsJSONWriter);
         procedure LoadFromJSON(json : TdwsJSONValue);

         procedure SaveToFile(const fileName : String);
         procedure LoadFromFile(const fileName : String);

         property CollectionIntervalMilliseconds : Integer read FCollectionIntervalMilliseconds write FCollectionIntervalMilliseconds;
   end;

   TdwsTokenKeyStore = class
      private
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
   CryptographicRandom(@FHashSalt, SizeOf(FHashSalt));
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

// HashToken
//
function TdwsTokenStore.HashToken(const aToken : String) : Cardinal;
begin
   Result := xxHash32.Full(Pointer(aToken), Length(aToken)*SizeOf(Char), FHashSalt);
end;

// Register
//
procedure TdwsTokenStore.Register(const aToken : String; ttlSeconds : Double; const aData : String);
var
   token : TdwsToken;
begin
   if aToken='' then Exit;
   token.HashCode:=HashToken(aToken);
   token.Token:=aToken;
   token.Expire:=UTCDateTime+ttlSeconds*(1/86400);
   token.DataHashCode:=HashToken(aData);
   token.Data:=aData;
   FLock.BeginWrite;
   try
      FHash.Replace(token);
      if FCollection=nil then
         ScheduleCollection;
   finally
      FLock.EndWrite;
   end;
end;

// CheckAndKeep
//
function TdwsTokenStore.CheckAndKeep(const aToken, aData : String) : Boolean;
var
   token : TdwsToken;
   t : TDateTime;
begin
   if aToken='' then Exit(False);
   token.HashCode:=HashToken(aToken);
   token.Token:=aToken;
   t:=UTCDateTime;
   FLock.BeginRead;
   try
      if FHash.Match(token) then
         Result:=(token.Expire>t) and (token.Data=aData)
      else Result:=False;
   finally
      FLock.EndRead;
   end;
end;

// CheckAndRemove
//
function TdwsTokenStore.CheckAndRemove(const aToken, aData : String) : Boolean;
var
   token : TdwsToken;
   t : TDateTime;
begin
   if aToken='' then Exit(False);
   token.HashCode:=HashToken(aToken);
   token.Token:=aToken;
   t:=UTCDateTime;
   FLock.BeginRead;
   try
      if FHash.Match(token) then begin
         Result := (token.Expire>t) and (token.Data=aData);
      end else Result := False;
   finally
      FLock.EndRead;
   end;
   if Result then begin
      FLock.BeginWrite;
      try
         if FHash.Match(token) then begin
            token.Data := '';
            token.Expire := 0;
            FHash.Replace(token);
         end;
      finally
         FLock.EndWrite;
      end;
   end;
end;

// Remove
//
procedure TdwsTokenStore.Remove(const aToken : String);
var
   token : TdwsToken;
begin
   if aToken='' then Exit;
   token.HashCode:=HashToken(aToken);
   token.Token:=aToken;
   FLock.BeginWrite;
   try
      if FHash.Match(token) then begin
            token.Data := '';
            token.Expire := 0;
         FHash.Replace(token);
      end;
   finally
      FLock.EndWrite;
   end;
end;

// RemoveByData
//
procedure TdwsTokenStore.RemoveByData(const aData : String);
begin
   FLock.BeginWrite;
   try
      FCollectData:=aData;
      FCollectDataHash:=HashToken(aData);
      FHash.Enumerate(CollectTokenByData);
   finally
      FCollectData:='';
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
type TCollector = class
   FUTCDateTime : TDateTime;
   function CollectToken(const item : TdwsToken) : TSimpleHashAction;
end;
function TCollector.CollectToken(const item : TdwsToken) : TSimpleHashAction;
begin
   if item.Expire>FUTCDateTime then
      Result:=shaNone
   else Result:=shaRemove;
end;

procedure TdwsTokenStore.Collect;
var
   collector : TCollector;
begin
   collector:=TCollector.Create;
   collector.FUTCDateTime:=UTCDateTime;
   FLock.BeginWrite;
   try
      if FHash.Count>0 then begin
         FHash.Enumerate(collector.CollectToken);
         if FHash.Count>0 then
            ScheduleCollection
         else FHash.Clear;
      end;
   finally
      FLock.EndWrite;
      collector.Free;
   end;
end;

// Count
//
function TdwsTokenStore.Count : Integer;
begin
   Result := FHash.Count;
end;

// CollectTokenByData
//
function TdwsTokenStore.CollectTokenByData(const item : TdwsToken) : TSimpleHashAction;
begin
   if (item.DataHashCode=FCollectDataHash) and (item.Data=FCollectData) then
      Result:=shaRemove
   else Result:=shaNone;
end;

// GetTokenData
//
function TdwsTokenStore.GetTokenData(const aToken : String) : String;
var
   token : TdwsToken;
   t : TDateTime;
begin
   Result:='';
   token.HashCode:=HashToken(aToken);
   token.Token:=aToken;
   t:=UTCDateTime;
   FLock.BeginRead;
   try
      if FHash.Match(token) and (token.Expire>t) then
         Result:=token.Data
      else Result:='';
   finally
      FLock.EndRead;
   end;
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
      if item.Data<>'' then begin
         Writer.BeginObject;
         Writer.WriteName('data').WriteString(item.Data);
         Writer.WriteName('expire').WriteNumber(item.Expire);
         Writer.EndObject;
      end else begin
         Writer.WriteNumber(item.Expire);
      end;
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
   elem : TdwsJSONValue;
begin
   t:=UTCDateTime;
   FLock.BeginWrite;
   try
      FHash.Clear;
      for i:=0 to json.ElementCount-1 do begin
         elem:=json.Elements[i];
         if elem.ValueType=jvtObject then begin
            token.Expire:=elem['expire'].AsNumber;
            token.Data:=elem['data'].AsString;
         end else begin
            token.Expire:=elem.AsNumber;
            token.Data:='';
         end;
         if token.Expire>t then begin
            token.Token:=json.Names[i];
            token.HashCode:=HashToken(token.Token);
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
