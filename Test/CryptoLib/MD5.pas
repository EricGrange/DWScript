
PrintLn(HashMD5.HashData(''));

PrintLn(HashMD5.HashData('The quick brown fox jumps over the lazy dog'));

PrintLn(HashMD5.HashData('The quick brown fox jumps over the lazy dog.'));

PrintLn(HashMD5.HMAC('', ''));

PrintLn(HashMD5.HMAC('key', 'The quick brown fox jumps over the lazy dog'));

PrintLn(HashMD5.HMAC('The quick brown fox jumps over the lazy dogThe quick brown fox jumps over the lazy dog',
					'The quick brown fox jumps over the lazy dog'));
