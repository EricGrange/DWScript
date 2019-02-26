unit UnitWithImplemVar;

interface

function GetValue(name : String) : String;

implementation

var v : array [String] of String;

function GetValue(name : String) : String;
begin
   Result := v[name];
   v[name] := Result + '!';
end;
