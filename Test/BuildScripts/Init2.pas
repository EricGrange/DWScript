unit Init2;

interface

uses
  Init3;

implementation

initialization
	PrintLn('Init '+CurrentSourceCodeLocation.File);
finalization
	PrintLn(CurrentSourceCodeLocation.File);
end;