var s: String = 'a';
var i : Integer;

try
   i:=0;
   s[i]:='!';
   PrintLn('doh!');
except
end;
try
   i:=2;
   s[i]:='!';
   PrintLn('doh!');
except
end;

try
   i:=0;
   s[i]:=Chr(32);
   PrintLn('doh!');
except
end;
try
   i:=2;
   s[i]:=Chr(32);
   PrintLn('doh!');
except
end;