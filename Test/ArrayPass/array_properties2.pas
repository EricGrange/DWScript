type
   TSub = class
      Field : Integer;
      procedure Proc;
      procedure SetProp(v: String);
      property Prop : String write SetProp;
   end;

type
   TTest = class
      function GetSub(x: Integer): TSub;
      property Sub[x: Integer]: TSub read GetSub; default;
   end;

procedure TSub.Proc;
begin
   PrintLn('Sub '+IntToStr(Field));
end;

procedure TSub.SetProp(v: String);
begin
   PrintLn('SetProp '+IntToStr(Field)+': '+v);
end;

function TTest.GetSub(x: Integer): TSub;
begin
   Result := TSub.Create;
   Result.Field := x;
end;

var t := TTest.Create;

(t).Sub[0].Proc;
(t.Sub[1]).Proc;
t.Sub[2].Proc;

(t).Sub[0].Prop:='a';
(t.Sub[1]).Prop:='b';
t.Sub[2].Prop:='c';

(t).GetSub(10).Proc;
(t.GetSub(11)).Proc;
t.GetSub(12).Proc;

(t).GetSub(10).Prop:='a';
(t.GetSub(11)).Prop:='b';
t.Sub[12].Prop:='c';

