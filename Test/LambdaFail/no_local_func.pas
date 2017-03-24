var f : function : Integer;

function Test123 : Integer;
begin
    Result := 123;
end;

f := Test123;

procedure Proc;
begin

    function Test456 : Integer;
    begin
        Result := 456;
    end;
    
    f := Test456;
    
    f := lambda => Test456;
    
    var g := lambda => 789;
    
    f := @g;

end;