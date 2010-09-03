var strings: TStrings;

strings := TStrings.Create;
strings.Add('Hello');
strings.Add('World');

// Without default property
PrintLn(strings.Get(0) + ' ' + strings.Get(1));

// With default property
PrintLn(strings[0] + ' ' + strings[1]);

PrintLn(strings.CommaText);

strings.Exchange(0, 1);

PrintLn(strings.CommaText);

PrintLn(IntToStr(strings.IndexOf('Hello')));

