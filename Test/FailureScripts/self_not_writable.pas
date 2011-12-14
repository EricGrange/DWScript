Type

 TObj = Class

    Procedure Test;
    
    class Procedure ClassTest;
    Begin
      Self := NIL;
    End;
 end;

procedure VarTest(var o : TObj);
begin
end; 
 
procedure TObj.Test;
begin
   VarTest(Self);
   Self := NIL;
end;