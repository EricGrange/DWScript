type
  IMyInterface = interface 
    procedure A;
  end;
  
type
  TMyImplementation = class(TObject, IMyInterface)
    procedure A; begin PrintLn('A'); end;
  end;

var
  IntfRef: IMyInterface;
  
var obj := TMyImplementation.Create;

if obj implements IMyInterface then 
   PrintLn('Ok');
if (new TObject) implements IMyInterface then 
   PrintLn('Bug TObject');
   
if TMyImplementation implements IMyInterface then 
   PrintLn('Ok');
if TObject implements IMyInterface then 
   PrintLn('Bug TObject');   