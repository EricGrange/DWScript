var cipherText128 := HexadecimalEncoder.Decode(
       '874d6191b620e3261bef6864990db6ce'
      +'9806f66b7970fdff8617187bb9fffdff'
      +'5ae4df3edbd5d35e5b4f09020db03eab'
      +'1e031dda2fbe03d1792170a0f3009cee'
   );
var cipherText256 := HexadecimalEncoder.Decode(
       '601ec313775789a5b7a7f504bbf3d228'
      +'f443e3ca4d62b59aca84e990cacaf5c5'
      +'2b0930daa23de94ce87017ba2d84988d'
      +'dfc9c58db67aada613c2dd08457941a6'
   );
var plainText := HexadecimalEncoder.Decode(
       '6bc1bee22e409f96e93d7e117393172a'
      +'ae2d8a571e03ac9c9eb76fac45af8e51'
      +'30c81c46a35ce411e5fbc1191a0a52ef'
      +'f69f2445df4f9b17ad2b417be66c3710'
   );
var key128 := HexadecimalEncoder.Decode(
      '2b7e151628aed2a6abf7158809cf4f3c'
   );
var key256 := HexadecimalEncoder.Decode(
      '603deb1015ca71be2b73aef0857d7781'
     +'1f352c073b6108d72d9810a30914dff4'
   );
var iv := HexadecimalEncoder.Decode(
      'f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff'
   );

PrintLn('--- AES 128');

var encrypted := EncryptionAESnistCTR.EncryptData(plainText, key128, iv);

PrintLn(HexadecimalEncoder.Encode(encrypted));
PrintLn(encrypted = cipherText128);

var decrypted := EncryptionAESnistCTR.DecryptData(encrypted, key128, iv);

PrintLn(HexadecimalEncoder.Encode(decrypted));
PrintLn(decrypted = plainText);

PrintLn('--- AES 256');

encrypted := EncryptionAESnistCTR.EncryptData(plainText, key256, iv);

PrintLn(HexadecimalEncoder.Encode(encrypted));
PrintLn(encrypted = cipherText256);

decrypted := EncryptionAESnistCTR.DecryptData(encrypted, key256, iv);

PrintLn(HexadecimalEncoder.Encode(decrypted));
PrintLn(decrypted = plainText);
