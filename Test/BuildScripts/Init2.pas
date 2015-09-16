unit Init2;

interface

uses
  Init3;

implementation

initialization
	PrintLn('Init Init2');
finalization
	PrintLn('Init2');
end;