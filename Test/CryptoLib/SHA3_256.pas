
PrintLn(HashSHA3_256.HashData(''));

PrintLn(HashSHA3_256.HashData('a'));

PrintLn(HashSHA3_256.HashData('12345678901234567890123456789012345678901234567890123456789012345678901234567890'));

PrintLn(HashSHA3_256.HMAC('', ''));

PrintLn(HashSHA3_256.HMAC('key', 'The quick brown fox jumps over the lazy dog'));

PrintLn(HashSHA3_256.HMAC('The quick brown fox jumps over the lazy dogThe quick brown fox jumps over the lazy dog',
						  'The quick brown fox jumps over the lazy dog'));

