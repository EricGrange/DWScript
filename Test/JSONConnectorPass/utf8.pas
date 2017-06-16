var v := JSON.NewObject;

v['é'] := 'à';

PrintLn(JSON.Stringify(v));

var u := JSON.StringifyUTF8(v);
PrintLn(u);

PrintLn(JSON.Parse(u).ToString());
PrintLn(JSON.ParseUTF8(u).ToString());
