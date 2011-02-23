// partial "leaks" and other weirdness intentional

type TMyClass = class
      Field : TStrings;
      constructor Create;
      destructor Destroy; override;
      procedure Print;
   end;

constructor TMyClass.Create;
begin
   Field:=TStrings.Create;
end;

destructor TMyClass.Destroy;
begin
   Field.Free;
end;

procedure TMyClass.Print;
var
   i : Integer;
begin
   if Self=nil then begin
      PrintLn('nil');
      Exit;
   end;
   PrintLn(Field.Count);
   for i:=0 to Field.Count-1 do begin
      Default.Print(Field[i]);
      TMyClass(Field.Objects[i]).Print;
   end;
end;

var o := TMyClass.Create;
var so := TMyClass.Create;

o.Field.AddObject('First ', nil);
o.Field.AddObject('Second ', TMyClass.Create);
o.Field.AddObject('Third ', so);

so.Field.Add('Alpha ');
so.Field.Add('Beta ');

o.Print;