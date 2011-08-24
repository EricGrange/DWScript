type
  IMyInterface = interface 
    procedure A;
  end;
  
type
  TMyImplemenation = class(TObject, IMyInterface)
    procedure A; begin PrintLn('A'); end;
    destructor Destroy; override; begin PrintLn('Destroy'); end;
  end;

procedure Test;
begin
  var IntfRef: IMyInterface;

  IntfRef := TMyImplemenation.Create;  
  IntfRef.A;
end; // Reference not niled but runs out of scope -> Destroy

Test;
PrintLn('end');