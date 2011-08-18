unit CrossUnit2;

interface

uses CrossUnit1;

type 
   TClass2 = class(TClass1)
      procedure Stuff; override;
   end;
   
implementation

procedure TClass2.Stuff;
begin
   PrintLn(ClassName+' Stuff2');
end;

