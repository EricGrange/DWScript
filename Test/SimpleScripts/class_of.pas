type
   TBase = class
      Field : String;
      constructor Create; virtual;
   end;

constructor TBase.Create;
begin
   Field:='Hello';
end;

type
   TChild = class (TBase)
      constructor Create; override;
   end;

constructor TChild.Create;
begin
   Field:='World';
end;

type TBaseClass = class of TBase;

var meta : TBaseClass;

if Assigned(meta) then PrintLn('Assigned bug');

meta:=TBase;

if not Assigned(meta) then PrintLn('Not Assigned bug');

var obj1 : TBase;
obj1:=meta.Create;

meta:=TChild;
var obj2 = meta.Create;

if meta=TBase then PrintLn('Bug Equal TBase');
if meta<>TChild then PrintLn('Bug Not Equal TChild');

PrintLn(obj1.Field);
PrintLn(obj2.Field);

meta:=nil;

if Assigned(meta) then PrintLn('Clear Assigned bug');


