uses System.Encoding;

var pixmap : array of Integer;

pixmap.Add(0);

var r := PixmapToJPEGData(pixmap, 1, 1, 90, [TJPEGOption.Optimize, TJPEGOption.NoJFIFHeader]);

PrintLn(Base64Encoder.Encode(r));