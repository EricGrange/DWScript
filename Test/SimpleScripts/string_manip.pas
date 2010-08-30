const empty = '';
var str : String = empty;

PrintLn(Length(str));
SetLength(str, 1);
PrintLn(Length(str));
str[1]:='z';
PrintLn(str);
str[1]:=Chr($5A); // 'Z'
str:=str+'a';
str:='o'+str;
PrintLn(str);

PrintLn('df'+'01'+str+'13'+'45'+str+empty+str);
