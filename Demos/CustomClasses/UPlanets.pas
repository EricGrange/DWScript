{
   Small pointless class hierarchy with a virtual method and a virtual
   constructor to illustrate one case of exposing objects in DWS.

   The relevant part is the ScriptObj variant field in TPlanet that is used
   to store the reference to the script-side object, which in our case can
   be considered as an adapter. This allows returning the same adapter to the
   script upon each request.

   If you don't care about memory, and if your users won't be comparing
   script-side instances, you can ignore all that and create a new adapter
   each time (resulting in potentially multiple script-side objects pointing
   to the same Delphi-side object).
}
unit UPlanets;

interface

uses Variants, dwsSymbols;

type

   // TPlanet
   //
   TPlanet = class
      private
         FScriptObj : Variant;
         FSatellites: array of TPlanet;

      protected
         function GetName : String; virtual;
         function GetSatellites(position : Integer) : TPlanet; virtual;

      public
         constructor Create; virtual;
         destructor Destroy; override;

         property Name : String read GetName;
         property Satellites[position : Integer] : TPlanet read GetSatellites;
         function SatelliteCount : Integer;

         property ScriptObj : Variant read FScriptObj write FScriptObj;
   end;

   // TEarth
   //
   TEarth = class (TPlanet)
      protected
         function GetName : String; override;

      public
         constructor Create; override;
   end;

   // TMoon
   //
   TMoon = class (TPlanet)
      protected
         function GetName : String; override;
   end;

implementation

// ------------------
// ------------------ TPlanet ------------------
// ------------------

// Create
//
constructor TPlanet.Create;
begin
   inherited Create;
end;

// Destroy
//
destructor TPlanet.Destroy;
var
   p : TPlanet;
begin
   for p in FSatellites do
      p.Free;
   inherited;
end;

// GetName
//
function TPlanet.GetName : String;
begin
   Result:='undefined';
end;

// GetSatellites
//
function TPlanet.GetSatellites(position : Integer) : TPlanet;
begin
   Result:=FSatellites[position];
end;

// SatelliteCount
//
function TPlanet.SatelliteCount : Integer;
begin
   Result:=Length(FSatellites);
end;

// ------------------
// ------------------ TEarth ------------------
// ------------------

// Create
//
constructor TEarth.Create;
begin
   inherited;
   SetLength(FSatellites, 1);
   FSatellites[0]:=TMoon.Create;
end;

// GetName
//
function TEarth.GetName : String;
begin
   Result:='Earth';
end;

// ------------------
// ------------------ TMoon ------------------
// ------------------

// GetName
//
function TMoon.GetName : String;
begin
   Result:='Moon';
end;

end.
