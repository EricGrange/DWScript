unit Init3;

interface

implementation

initialization
  PrintLn('Init '+CurrentSourceCodeLocation.File);
finalization
	PrintLn(CurrentSourceCodeLocation.File);
end;