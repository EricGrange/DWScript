unit Init1;

interface

uses
  Init3,
  Init2;

implementation

initialization
	PrintLn('Init Init1');
finalization
	PrintLn('Init1');
end;