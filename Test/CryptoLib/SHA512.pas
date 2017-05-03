
PrintLn(HashSHA512.HashData(''));

PrintLn(HashSHA512.HashData('The quick brown fox jumps over the lazy dog'));

PrintLn(HashSHA512.HashData('The quick brown fox jumps over the lazy dog.'));


PrintLn(HashSHA512.HMAC('', ''));

PrintLn(HashSHA512.HMAC('key', 'The quick brown fox jumps over the lazy dog'));

PrintLn(HashSHA512.HMAC('The quick brown fox jumps over the lazy dogThe quick brown fox jumps over the lazy dog',
						'The quick brown fox jumps over the lazy dog'));

