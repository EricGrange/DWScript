program sha3;

{$APPTYPE CONSOLE}
{$SetPEFlags $0001}

{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
   Windows,
   dwsSHA3;

procedure HashFile(index : Integer);
var
   h : THandle;
   err, nb : Cardinal;
   state : TSHA3State;
   buffer : array [0..32767] of Byte;
   digest : TSHA3_256_Hash;
begin
   h := CreateFileW(PWideChar(ParamStr(index)), GENERIC_READ, FILE_SHARE_READ+FILE_SHARE_WRITE,
                    nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
   if h = INVALID_HANDLE_VALUE then begin
      err := GetLastError;
      case err of
         ERROR_FILE_NOT_FOUND : Writeln('File not found');
         ERROR_ACCESS_DENIED : Writeln('File access denied');
      else
         Writeln('Error ', err, ' while opening file');
      end;
      Exit;
   end;

   SHA3_Init(state, SHA3_256);
   while ReadFile(h, buffer, SizeOf(buffer), nb, nil) do begin
      SHA3_Update(state, @buffer, nb);
      if nb < SizeOf(buffer) then break;
   end;
   SHA3_FinalHash(state, @digest);
   Writeln(SHA3_DigestToString(@digest, SizeOf(digest)));
end;

procedure HashString(const s : String);
begin
   Writeln(HashSHA3_256(UTF8Encode(s)));
end;

var
   n, i : Integer;
   arg : String;
begin
   n := ParamCount;
   if n > 0 then begin
      arg := ParamStr(1);
      if arg = '-s' then begin
         i := 2;
         repeat
            HashString(ParamStr(i));
            Inc(i);
         until i > n;
      end else begin
         if arg = '-f' then
            i := 2
         else i := 1;
         for i := i to n do
            HashFile(i);
      end;
   end else begin
      Writeln('DWScript SHA3-256 utility - https://www.dwscript.net'#13#10);
      Writeln('sha3 [-f] <filename> [<other filename>...]');
      Writeln('sha3 -s "string as utf-8" ["other string"...]');
   end;
end.
