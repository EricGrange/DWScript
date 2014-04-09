var a := JSON.NewArray;

PrintLn(a.Add(1, 2));
PrintLn(a.Push("hello"));
PrintLn(a.Push(True, "world", False, Null));

PrintLn(a.ToString());
