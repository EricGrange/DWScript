type TMyStream = class end;

type TStreamSpecial = (BACKSPACE, CRLF);

function StreamString(s : TMyStream; str : String) : TMyStream;
begin
   Print(str);
   Result:=s;
end;

operator << (TMyStream, String) : TMyStream uses StreamString;

function StreamSpecial(s : TMyStream; spec : TStreamSpecial) : TMyStream;
begin
   if spec=BACKSPACE then
      s << '<<'
   else s << #13#10;
   Result:=s;
end;

operator << (TMyStream, TStreamSpecial) : TMyStream uses StreamSpecial;

function StreamDummy(s : TMyStream; str : String) : TMyStream;
begin
   s << 'Dummy ' << str << CRLF;
end;

operator >> (TMyStream, String) : TMyStream uses StreamDummy;

var o := new TMyStream;

o << "Hello world!" << CRLF 
  >> "Hey"
  << BACKSPACE << "Wicked stuff..." << CRLF;