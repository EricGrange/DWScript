procedure Test(v : JSONVariant);
begin
   PrintLn(if v.undefined then 'bug udefined!' else 'undefined is falsy');
   PrintLn(if v.boolTrue then 'true is true' else 'true is false!');
   PrintLn(if v.boolFalse then 'false is true!' else 'false is false');
   PrintLn(if v.intZero then 'intZero is true!' else 'intZero is false');
   PrintLn(if v.intOne then 'intOne is true' else 'intOne is false!');
   PrintLn(if v.floatZero then 'floatZero is true!' else 'floatZero is false');
   PrintLn(if v.floatMinus2dot5 then '-2.5 is true' else '-2.5 is false!');
   PrintLn(if v.strEmpty then 'strEmpty is true!' else 'strEmpty is false');
   PrintLn(if v.strHello then 'strHello is true' else 'strHello is false!');
   PrintLn(if v.nullField then 'nullField is true!' else 'nullField is false');
   PrintLn(if v.arrayEmpty then 'arrayEmpty is true' else 'arrayEmpty is false!');
   PrintLn(if v.arrayOf1 then 'arrayOf1 is true' else 'arrayOf1 is false!');
   PrintLn(if v.objEmpty then 'objEmpty is true' else 'objEmpty is false!');
   PrintLn(if v.objWithField then 'objWithField is true' else 'objWithField is false!');
   PrintLn(if v.objWithField.Field then 'objWithField.Field is true' else 'objWithField.Field is false!');
   PrintLn(if v.objWithField.abc then 'objWithField.abc is true!' else 'objWithField.abc is false');
end;

var v := JSON.Serialize(record
   'boolTrue' := True;
   'boolFalse' := False;
   'intZero' := 0;
   'intOne' := 1;
   'floatZero' := 0.0;
   'floatMinus2dot5' := -2.5;
   'strEmpty' := '';
   'strHello' := 'hello';
   'nullField' := Null;
   'arrayEmpty' := [];
   'arrayOf1' := [ 1 ];
   'objEmpty' := JSON.NewObject;
   'objWithField' := record Field := 123 end;
end);

PrintLn('Serialized ------');
Test(v);

PrintLn('Stringified + Parsed ------');
Test(JSON.Parse(v.ToString()));
