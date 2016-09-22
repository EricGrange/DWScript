PrintLn(DateTimeToRFC822(0));
PrintLn(DateTimeToRFC822(42000.4242));

var t := 42000.4242;
var s := DateTimeToRFC822(t);

PrintLn(s);

PrintLn(RFC822ToDateTime(s));

s := 'Wed, 12 Feb 1997 16:29:51 -0500';
t := RFC822ToDateTime(s);
PrintLn(t);
PrintLn(DateTimeToRFC822(t));




