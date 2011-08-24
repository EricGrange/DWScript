type
  IMyInterface = interface 
    procedure A;
  end;
  
type
  TMyImplemenation = class(TObject, IMyInterface)
    procedure A; begin PrintLn('A'); end;
    destructor Destroy; override; begin PrintLn('Destroy'); end;
  end;

var IntfRef: IMyInterface;

IntfRef := TMyImplemenation.Create;
IntfRef.A;
IntfRef := nil; // -> Destroy
PrintLn('end');
