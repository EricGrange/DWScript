
PrintLn(HashSHA1.HashData(''));

PrintLn(HashSHA1.HashData('The quick brown fox jumps over the lazy dog'));

PrintLn(HashSHA1.HashData('The quick brown fox jumps over the lazy dog.'));

PrintLn(HashSHA1.HMAC('', ''));

PrintLn(HashSHA1.HMAC('key', 'The quick brown fox jumps over the lazy dog'));

PrintLn(HashSHA1.HMAC('The quick brown fox jumps over the lazy dogThe quick brown fox jumps over the lazy dog',
			 		  'The quick brown fox jumps over the lazy dog'));
