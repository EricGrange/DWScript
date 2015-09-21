type
   TBase = class
      constructor Create; overload; virtual; 
	  begin PrintLn(1); end;
      constructor Create(a : Integer); overload;
	  begin PrintLn(a); end;
   end;

type   
   TChild1 = class (TBase)
      constructor Create; override;
	  begin PrintLn(3); end;
      constructor Create(a : Integer); overload;
	  begin PrintLn(a); end;
   end;
   
type   
   TChild2 = class (TBase)
      constructor Create; overload; override;
	  begin PrintLn(5); end;
      constructor Create(a : Integer); overload;
	  begin PrintLn(a); end;
   end;   
   
TBase.Create;
TBase.Create(2);
TChild1.Create;
TChild1.Create(4);
TChild2.Create;
TChild2.Create(6);