type
  IMyInterface = interface 
    procedure A;
  end;
  
type
  TMyImplemenation = class(TObject, IMyInterface)
    procedure A; begin PrintLn('A'); end;
    destructor Destroy; override; begin PrintLn('Destroy'); end;
  end;

var ObjRef: TMyImplemenation;
var IntfRef: IMyInterface;

ObjRef := TMyImplemenation.Create;
IntfRef := ObjRef;
IntfRef.A;
ObjRef.A;
ObjRef := nil;
IntfRef.A;
IntfRef := nil; // -> Destroy
PrintLn('end');
