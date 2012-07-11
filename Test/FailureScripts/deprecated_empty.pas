type ttest = record
   field : Integer;
   property prop : Integer write field; deprecated "";
end;

Procedure test; Deprecated '';
begin
end;

test;

var t: ttest;
t.prop:=1;
