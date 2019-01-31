var Oggetto := JSON.NewObject;
Oggetto.Dati := JSON.Parse('{"Campo":{"IDValue":5}}');
PrintLn(Oggetto);
Oggetto.SottoOggetto := Oggetto.Dati.Campo;
PrintLn(Oggetto);

Oggetto := JSON.NewObject;
Oggetto.Dati := JSON.Parse('{"Campo":{"IDValue":5}}');
Oggetto.SottoOggetto := Oggetto.Dati.Campo.Clone();
PrintLn(Oggetto);