var f : function : Integer;

function Test123 : Integer;
begin
    Result := 123;
end;

f := Test123;

PrintLn(f);

procedure Proc;
begin

    PrintLn(f);

    f := lambda => 456;
    
    PrintLn(f);

end;

Proc;

PrintLn(f);
