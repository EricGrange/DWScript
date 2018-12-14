type TSub = class(TObject);
type TObjectArray = array of TObject;
type VariantArray = array of Variant;

function Func(APar1: TObject; APar2: Variant): Boolean; overload;
begin
   Print('Object ');
   Print(APar1.ClassName);
   Print(', Variant ');
   PrintLn(APar2);
end;

function Func(APar1: TObject; APar2: Integer): Boolean; overload;
begin
   Print('Object ');
   Print(APar1.ClassName);
   Print(', Integer ');
   PrintLn(APar2);
end;

function Func(APar1: TObjectArray; APar2: VariantArray): Boolean; overload;
begin
   Print('ObjectArray ');
   Print(APar1.Length);
   Print(', VariantArray ');
   PrintLn(APar2.Length);
end;

function Func(APar1: TObjectArray; APar2: Variant): Boolean; overload;
begin
   Print('ObjectArray ');
   Print(APar1.Length);
   Print(', Variant ');
   PrintLn(APar2);
end;

function GetValue1(): Integer;
begin
  Result := 2; // some calculation
end;

var LObject1 := new TObject;
var LObject2 := new TSub;

Func(LObject1, 123);
Func(LObject1, 'hello');
Func([LObject1, LObject2], 123);

{$ifdef JS_CODEGEN}
var va : VariantArray; // ambiguous in JS Codegen as [] and Variant are compatible
va.Add(GetValue1(), 0);
Func([LObject1, LObject2], va);
va.Clear;
Func([], va);
{$else}
Func([LObject1, LObject2], [GetValue1(), 0]);
Func([], []);
{$endif}

