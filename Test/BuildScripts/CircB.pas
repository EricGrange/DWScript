//
// New Script
//
 unit CircB;

 interface

 procedure PB;
 procedure PC;

 implementation

 uses
    CircA;

 procedure PB;
 begin
    PrintLn('PB');
 end;

 procedure PC;
 begin
    PrintLn('PC');
    PA;
 end;

 end.
