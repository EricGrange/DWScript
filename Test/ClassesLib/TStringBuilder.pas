var sb = TStringBuilder.Create;

PrintLn('<'+sb.ToString+'>');
sb.Append('hello');
PrintLn('<'+sb.ToString+'>');
sb += ' world';
PrintLn('<'+sb.ToString+'>');

sb.Clear;
sb.Append(123);
PrintLn('<'+sb.ToString+'>');
sb.Append(',').Append(3.14);
PrintLn('<'+sb.ToString+'>');

