type TEnum = (one = 1, ten = 10, hundred = 100, thousandth = 1000);
type TSet = set of TEnum;

procedure PrintSet(s : TSet);
begin
    if TEnum(16) in s then Print('bug');
    Print(if thousandth in s then '1' else '0');
    Print(if hundred in s then '1' else '0');
    if TEnum(128) in s then Print('bug');
    Print(if ten in s then '1' else '0');
    PrintLn(if one in s then '1' else '0');
end;

var s1 : TSet = [one];
var s2 : TSet = [ten];
var s3 : TSet = [hundred];
var s4 : TSet = [thousandth];
var s : TSet;

PrintSet(s1 + s1);

s := s1 + s2;
PrintSet(s);

s := s + s3;
PrintSet(s);

s := s + s4;
PrintSet(s);

s := s1 + s3 + s4;
PrintSet(s);

PrintSet(s2 + s4);

PrintSet(s3 + s4);
