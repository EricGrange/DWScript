var msg := 'hello';
var key := 'world';

var crypted := EncryptionAESSHA256Full.EncryptData(msg, key);

PrintLn(crypted.Length);

PrintLn(EncryptionAESSHA256Full.DecryptData(crypted, key));

if EncryptionAESSHA256Full.DecryptData(crypted, 'wrong')=msg then
	PrintLn('bug');

msg := ''; 
key := '';

crypted := EncryptionAESSHA256Full.EncryptData(msg, key);

PrintLn(crypted.Length);
PrintLn('<'+EncryptionAESSHA256Full.DecryptData(crypted, key)+'>');
