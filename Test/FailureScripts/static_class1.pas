type
   TStatic = class static
      class function Func : Integer;
      procedure Stuff;
      function SomeFunc : Integer;
      constructor Create;
	  class method Virt; virtual; abstract;

      property Prop : Integer read Func;
   end;

type
   TSub = class (TStatic)
      class method Virt; override;
	  function Oops : String;
   end;

var o : TStatic;

PrintLn(TStatic.Func);
TStatic.Virt;
PrintLn(TSub.Func);
PrintLn(TSub.Prop);

TStatic.Create;

