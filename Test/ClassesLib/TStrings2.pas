var strings := TStrings.Create;
strings += 'Hello';
strings += 'World';

PrintLn(strings.Text);

strings.Delete(0);

PrintLn(strings.CommaText);

strings -= 'dummy';

PrintLn(strings.CommaText);

strings -= 'World';

PrintLn(strings.Count);
