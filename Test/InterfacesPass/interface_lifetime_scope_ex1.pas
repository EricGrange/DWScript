type
  IMyInterface = interface 
    procedure A;
  end;
  
type
  TMyImplemenation = class(TObject, IMyInterface)
    procedure A; begin PrintLn('A'); end;
    destructor Destroy; override; begin PrintLn('Destroy'); end;
  end;

procedure Test(var Result: IMyInterface);
begin
  var IntfRef: IMyInterface;

  IntfRef := TMyImplemenation.Create;  
  IntfRef.A;
  Result := IntfRef;
end; 

var R: IMyInterface;

Test(R);
PrintLn('---');
R := nil;
PrintLn('end');
