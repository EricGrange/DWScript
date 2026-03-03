var revokedUrl := 'https://revoked.badssl.com';
var dummyData: String;

// Test 1: Should succeed (Default)
HttpQuery.EnableSSLRevocation := False;
PrintLn('Testing without revocation check...');
try
   HttpQuery.GetText(revokedUrl, dummyData);
   PrintLn('Success: Revocation ignored (as expected).');
except
   on E: Exception do PrintLn('Error: ' + E.Message);
end;

// Test 2: Should fail (Enabled)
HttpQuery.EnableSSLRevocation := True;
PrintLn('Testing WITH revocation check...');
try
   HttpQuery.GetText(revokedUrl, dummyData);
   PrintLn('Error: Request succeeded but should have been blocked!');
except
   on E: Exception do PrintLn('Caught Expected Error: ' + E.Message);
end;
