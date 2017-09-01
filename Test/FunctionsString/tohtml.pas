var s := '<script>alert("xss")</script>';

PrintLn(s.ToHTML);

PrintLn(s.ToHtmlAttribute);