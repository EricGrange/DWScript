type TA = array of Integer;

procedure Test(data: TA = []);
begin
   PrintLn(Length(data));
end;

Test;

Test([1]);

Test([]);

var a : TA := [2, 3];
Test(a);
