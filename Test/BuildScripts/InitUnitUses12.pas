unit InitUnitUses12;

interface

uses InitUnit1;

implementation

uses InitUnit2;

initialization

PrintLn("Init'ing InitUnitUses12");

finalization

PrintLn("Final'ing InitUnitUses12");

end.