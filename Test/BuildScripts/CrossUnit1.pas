unit CrossUnit1;

interface

type 
   TClass1 = class
      procedure Stuff; virtual;
   end;
   
implementation

uses CrossUnit2;

procedure TClass1.Stuff;
begin
   PrintLn(ClassName+' Stuff1');
   new TClass2.Stuff;
end;

