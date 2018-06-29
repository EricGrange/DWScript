

if not ApplicationInfo.ExeName.EndsWith('.exe') then
   PrintLn('ExeName failed');

if ApplicationInfo.ExeLinkTimeStamp not in [1520000000..1900000000] then // 2018-2031 range
   PrintLn('LinkTimeStamp looks incorrect');

if ApplicationInfo.EnvironmentVariable['TEMP'] = '' then
   PrintLn('TEMP env variable looks empty?');

const cEnvVarName = '5gt7l7m17l1t7U3lIoZJK';
var r := Now.ToString;
ApplicationInfo.EnvironmentVariable[cEnvVarName] := r;
if ApplicationInfo.EnvironmentVariable[cEnvVarName] <> r then
   PrintLn('Failed to set env var');

PrintLn(ApplicationInfo.UserName <> '');
