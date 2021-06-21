
PrintLn(PBKDF2_HMAC_SHA256('password', 'salt', 4096).Left(40));

PrintLn(PBKDF2_HMAC_SHA256('this is some password longer than 32 bytes', 'this is some salt longer than 32 bytes', 20));

