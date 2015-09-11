
PrintLn(HashSHA256.HashData(''));

PrintLn(HashSHA256.HashData('The quick brown fox jumps over the lazy dog'));

PrintLn(HashSHA256.HashData('The quick brown fox jumps over the lazy dog.'));


PrintLn(HashSHA256.HMAC('', ''));

PrintLn(HashSHA256.HMAC('key', 'The quick brown fox jumps over the lazy dog'));

PrintLn(HashSHA256.HMAC('The quick brown fox jumps over the lazy dogThe quick brown fox jumps over the lazy dog',
						'The quick brown fox jumps over the lazy dog'));

