var strings := TStrings.Create;

strings.CommaText := 'ab,b';

PrintLn(strings.Count);
PrintLn(strings.Text);