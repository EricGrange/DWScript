type
	TBase = class
       Field : Integer;
	   procedure PrintBase;
	end;
	
type
    TChild = class(TBase)
	    Field : Integer;
        procedure PrintChild;
	end;
	
procedure TBase.PrintBase;
begin
   PrintLn(Field);
end;

procedure TChild.PrintChild;
begin
   PrintLn(Field);
end;

var c := TChild.Create;
var b : TBase := c;

b.Field:=1;
c.Field:=2;

b.PrintBase;
c.PrintBase;
c.PrintChild;