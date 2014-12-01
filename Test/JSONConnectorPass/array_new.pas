var a:=JSON.NewArray;
PrintLn(Length(a)); // cast as string, length 2!
PrintLn(a.Length());
PrintLn(a.length);  // special property
PrintLn(JSON.Stringify(a));