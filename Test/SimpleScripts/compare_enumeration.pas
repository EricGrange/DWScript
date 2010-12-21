type
   TEnum = (enOne, enTwo, enThree);

procedure PrintBool(msg : String; b : Boolean);
begin
   Print(msg);
   if b then
      PrintLn('True')
   else PrintLn('False');
end;

PrintBool('1 = 1 is ', enOne=enOne);
PrintBool('1 <> 1 is ', enOne<>enOne);

PrintBool('1 = 2 is ', enOne=enTwo);
PrintBool('1 <> 2 is ', enOne<>enTwo);

PrintBool('1 in [2, 3] is ', enOne in [enTwo, enThree]);
PrintBool('1 not in [2, 3] is ', enOne not in [enTwo, enThree]);

PrintBool('2 in [2, 3] is ', enTwo in [enTwo, enThree]);
PrintBool('2 not in [2, 3] is ', enTwo not in [enTwo, enThree]);
