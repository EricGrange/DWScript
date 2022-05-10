
procedure NumbersAndVowels(s : String);
begin
   var nums, vows, accented : Integer;
   for var i := 1 to Length(s) do begin
      if s[i] in ['a', 'e', 'i', 'o', 'u', 'y'] then
         vows += 1;
      if s[i] in ['0'..'9'] then
         nums += 1;
      if s[i] in ['à', 'é'] then
         accented += 1;
   end;
   PrintLn(nums.ToString + ', ' + vows.ToString + ', ' + accented.ToString);
end;

NumbersAndVowels('');
NumbersAndVowels('hello world');
NumbersAndVowels('42 is the answer');
NumbersAndVowels('éléphant 3');



