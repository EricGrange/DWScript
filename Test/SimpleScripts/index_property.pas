type
   TTest = class
      FBase : Integer;
      constructor Create(aBase : Integer);

      function Get(i : Integer) : String;
      procedure Set(i : Integer; v : String);
      function GetD(i : Integer) : String;
      procedure SetD(i : Integer; v : String);

      property Prop[i : Integer] : String read Get write Set;
      property PropD[i : Integer] : String read GetD write SetD; default;
   end;

constructor TTest.Create(aBase : Integer);
begin
   FBase:=aBase;
end;

function TTest.Get(i : Integer) : String;
begin
   Result:='Get '+IntToStr(i+FBase);
end;

procedure TTest.Set(i : Integer; v : String);
begin
   PrintLn('Set '+IntToStr(i+FBase)+': '+v);
end;

function TTest.GetD(i : Integer) : String;
begin
   Result:='GetD '+IntToStr(i+FBase);
end;

procedure TTest.SetD(i : Integer; v : String);
begin
   PrintLn('SetD '+IntToStr(i+FBase)+': '+v);
end;

var t = TTest.Create(10);

t.Prop[0]:='hello';
t.Prop[1]:='world';

PrintLn(t.Prop[100]);
PrintLn(t.Prop[200]);

t[0]:='hello';
t[1]:='world';

PrintLn(t[100]);
PrintLn(t[200]);

