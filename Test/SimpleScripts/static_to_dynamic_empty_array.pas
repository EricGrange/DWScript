procedure Test1(const AParams : Array Of Variant);
begin
   PrintLn(Length(AParams));
end;

test1([]); 

procedure Test2(const AParams : Array Of Integer);
begin
   PrintLn(Low(AParams));
   PrintLn(High(AParams));
end;

test2([]); 