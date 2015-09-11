
PrintLn(HashRIPEMD160.HashData(''));

PrintLn(HashRIPEMD160.HashData('a'));

PrintLn(HashRIPEMD160.HashData('message digest'));

PrintLn(HashRIPEMD160.HashData(StringOfString('1234567890', 8)));

PrintLn(HashRIPEMD160.HashData(StringOfChar('a', 1000000)));

PrintLn(HashRIPEMD160.HMAC('', ''));

PrintLn(HashRIPEMD160.HMAC('key', 'The quick brown fox jumps over the lazy dog'));

PrintLn(HashRIPEMD160.HMAC('The quick brown fox jumps over the lazy dogThe quick brown fox jumps over the lazy dog',
						   'The quick brown fox jumps over the lazy dog'));

