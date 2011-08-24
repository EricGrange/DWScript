type
  IMyInterface = interface 
    procedure A;
  end;
  
type
  TMyImplemenation = class(TObject, IMyInterface)
    procedure A; begin PrintLn('A'); end;
    destructor Destroy; override; begin PrintLn('Destroy'); end;
  end;

function Test: IMyInterface;
begin
  var IntfRef: IMyInterface;

  IntfRef := TMyImplemenation.Create;  
  IntfRef.A;
  Result := IntfRef;
end; 

var R: IMyInterface;

R := Test;
PrintLn('---');
R := nil;
PrintLn('end');
