var pixmap := CreatePixmap(2, 1);

pixmap.SetPixel(0, 0, $12345671);
pixmap.SetPixel(1, 0, $17654231);

PrintLn(pixmap.GetPixel(0, 0).ToHexString(8));
PrintLn(pixmap.GetData(1).ToHexString(8));

pixmap.SetData(0, $12345678);
PrintLn(pixmap.GetPixel(0, 0).ToHexString(8));
PrintLn(pixmap.GetData(0).ToHexString(8));

PrintLn(pixmap.ToHexString);
