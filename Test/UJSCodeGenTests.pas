unit UJSCodeGenTests;

{$I dws.inc}

interface

uses
   Forms, Classes, SysUtils, TestFrameWork, Windows, Messages,
   UJSTestChromium,
   dwsComp, dwsCompiler, dwsExprs, dwsUtils, dwsXPlatform, dwsUnitSymbols,
   dwsCodeGen, dwsJSCodeGen, dwsJSLibModule, dwsFunctions, dwsCompilerContext,
   dwsErrors, ClipBrd;

type

   TJSCodeGenTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FCodeGen : TdwsJSCodeGen;
         FASMModule : TdwsJSLibModule;
         FChromium : ITestChromium;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoInclude(const scriptName: string; var scriptSource: string);
         function  DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;

         function GetExpectedResult(const fileName : String) : String;

         procedure Compilation;
         procedure Execution;

      published

         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionNonOptimizedWithInlineMagics;
         procedure ExecutionOptimized;
         procedure ExecutionOptimizedWithInlineMagics;
         procedure ExecutionOptimizedAndSmartLinked;
         procedure ExecutionOptimizedAndObfuscated;
         procedure ExecutionOptimizedObfuscatedSmartLinked;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cCompilerOptions = cDefaultCompilerOptions
         + [coSymbolDictionary, coContextMap, coAllowClosures];

// ------------------
// ------------------ TJSCodeGenTests ------------------
// ------------------

// SetUp
//
procedure TJSCodeGenTests.SetUp;
const
   cBaseFilter = '*';
var
   pasFilter : String;
   dwsFilter : String;
   i : Integer;
begin
   SetDecimalSeparator('.');

   FTests:=TStringList.Create;

   pasFilter:=cBaseFilter+'.pas';
   dwsFilter:=cBaseFilter+'.dws';

   CollectFiles(ExtractFilePath(ParamStr(0))+'Algorithms'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'SimpleScripts'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'BuildScripts'+PathDelim, dwsFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'InterfacesPass'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'OverloadsPass'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'HelpersPass'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'LambdaPass'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'PropertyExpressionsPass'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'SetOfPass'+PathDelim, pasFilter, FTests);

   CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsMath'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsString'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsTime'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsVariant'+PathDelim, pasFilter, FTests);       // *)
   //CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsRTTI'+PathDelim, pasFilter, FTests);

   {$ifdef JS_BIGINTEGER}
   CollectFiles(ExtractFilePath(ParamStr(0))+'BigInteger'+PathDelim, pasFilter, FTests);
   {$endif}

   for i := FTests.Count-1 downto 0 do
      if Pos('array_element_var', FTests[i]) > 0 then
         FTests.Delete(i);

   FCompiler:=TDelphiWebScript.Create(nil);
   FCompiler.OnInclude:=DoInclude;
   FCompiler.OnNeedUnit:=DoNeedUnit;
   FCompiler.Config.HintsLevel:=hlPedantic;
   FCompiler.Config.Conditionals.Add('CONDITION');
   FCompiler.Config.Conditionals.Add('JS_CODEGEN');

   FCodeGen:=TdwsJSCodeGen.Create;

   FASMModule:=TdwsJSLibModule.Create(nil);
   FASMModule.Script:=FCompiler;

   if FChromium = nil then
      FChromium := CreateTestChromium;
end;

// TearDown
//
procedure TJSCodeGenTests.TearDown;
begin
   FASMModule.Free;

   FCodeGen.Free;

   FCompiler.Free;

   FTests.Free;
end;

// DoInclude
//
procedure TJSCodeGenTests.DoInclude(const scriptName: string; var scriptSource: string);
begin
   scriptSource := LoadTextFromFile('SimpleScripts\'+scriptName);
   if scriptSource = '' then
      scriptSource := LoadTextFromFile('BuildScripts\'+scriptName);
end;

// DoNeedUnit
//
function TJSCodeGenTests.DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;
var
   fName : String;
begin
   Result := nil;
   fName := 'BuildScripts\' + unitName + '.pas';
   if FileExists(fName) then
      unitSource := LoadTextFromFile(fName);
end;

// Compilation
//
procedure TJSCodeGenTests.Compilation;
var
   source : TStringList;
   i, ignored : Integer;
   prog : IdwsProgram;
   diagnostic : TStringList;
begin
   ignored:=0;
   diagnostic:=TStringList.Create;
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text, ExtractFileName(FTests[i]));

         if prog.Msgs.HasErrors then begin
            CheckEquals(GetExpectedResult(FTests[i]),
                         'Errors >>>>'#13#10
                        +prog.Msgs.AsInfo,
                        FTests[i]);
            Inc(ignored);
            continue;
         end;

         FCodeGen.Clear;
         try
            FCodeGen.CompileProgram(prog);
         except
            on e: Exception do begin
               diagnostic.Add(ExtractFileName(FTests[i])+': '+e.Message);
            end;
         end;

      end;

      CheckEquals(0, diagnostic.Count,
                  Format('%d / %d tests passed (%d ignored)'#13#10'%s',
                         [FTests.Count-diagnostic.Count-ignored, FTests.Count-ignored, ignored,
                          diagnostic.Text]));
   finally
      diagnostic.Free;
      source.Free;
   end;
end;

// GetNodeOutput
//
function GetNodeOutput(const jsFileName : String) : String;
var
   securityAttribs: TSecurityAttributes;
   startupInfo: TStartupInfo;
   processInfo: TProcessInformation;
   stdOutPipeRead, stdOutPipeWrite: THandle;
   wasOK: Boolean;
   buffer: array[0..255] of AnsiChar;
   bytesRead: Cardinal;
   cmd : string;
   handle: Boolean;
begin
   Result := '';
   with securityAttribs do begin
      nLength := SizeOf(securityAttribs);
      bInheritHandle := True;
      lpSecurityDescriptor := nil;
   end;
   CreatePipe(stdOutPipeRead, stdOutPipeWrite, @securityAttribs, 0);
   try
      FillChar(startupInfo, SizeOf(startupInfo), 0);
      startupInfo.cb := SizeOf(startupInfo);
      startupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      startupInfo.wShowWindow := SW_HIDE;
      startupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      startupInfo.hStdOutput := stdOutPipeWrite;
      startupInfo.hStdError := stdOutPipeWrite;

      cmd:='node "'+jsFileName+'"';
      handle := CreateProcess(nil, PChar(cmd),
                              nil, nil, True, 0, nil,
                              nil, startupInfo, processInfo);
      CloseHandle(stdOutPipeWrite);
      if handle then
         try
            repeat
               wasOK := ReadFile(stdOutPipeRead, buffer, 255, bytesRead, nil);
               if bytesRead > 0 then begin
                  buffer[bytesRead]:=#0;
                  if (bytesRead>1) and (buffer[bytesRead-1]=#10) then
                     buffer[bytesRead-1]:=#0;
                  Result:=Result+UTF8ToString(buffer);
               end;
            until not wasOK or (bytesRead = 0);
            WaitForSingleObject(processInfo.hProcess, INFINITE);
         finally
            CloseHandle(processInfo.hThread);
            CloseHandle(processInfo.hProcess);
         end
      else RaiseLastOSError;
   finally
      CloseHandle(stdOutPipeRead);
   end;
end;

function GetTempFileName(const APrefix: String): String;
var
   pathBuffer, fileBuffer: array[0..MAX_PATH] of Char;
begin
   GetTempPath(MAX_PATH, pathBuffer);
   Windows.GetTempFileName(pathBuffer, PChar(APrefix), 0, fileBuffer);
   Result := PChar(@fileBuffer);
end;

// Execution
//
procedure TJSCodeGenTests.Execution;
var
   source : TStringList;
   i, ignored : Integer;
   prog : IdwsProgram;
   jscode : String;
//   fileName : String;
   output, expectedResult : String;
   diagnostic : TStringList;
begin
   for i := 1 to 100 do
      if not FChromium.Initialized then begin
         FChromium.LoadURL('about:blank');
         Application.ProcessMessages;
         Sleep(50);
      end;
   Assert(FChromium.Initialized);


   i := FCompiler.Config.Conditionals.IndexOf('INLINE_MAGICS');
   if not (cgoNoInlineMagics in FCodeGen.Options) then begin
      if i < 0 then
         FCompiler.Config.Conditionals.Add('INLINE_MAGICS')
   end else begin
      if i >= 0 then
         FCompiler.Config.Conditionals.Delete(i);
   end;

   ignored:=0;
   diagnostic:=TStringList.Create;
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         if    (Pos('Algorithms', FTests[i])>1)
            or (Pos('Functions', FTests[i])>1) then
            FCompiler.Config.HintsLevel:=hlStrict
         else FCompiler.Config.HintsLevel:=hlPedantic;

         prog:=FCompiler.Compile(source.Text, ExtractFileName(FTests[i]));

         if prog.Msgs.HasErrors then begin
            CheckEquals(GetExpectedResult(FTests[i]),
                         'Errors >>>>'#13#10
                        +prog.Msgs.AsInfo,
                        FTests[i]);
            Inc(ignored);
            continue;
         end;

         FCodeGen.Clear;
         try
            FCodeGen.CompileProgram(prog);

            FCodeGen.FlushedDependencies.Add('Print');
            FCodeGen.FlushedDependencies.Add('PrintLn');

            jsCode:= '(function () {'#13#10
                    +'var $testResult = [];'#13#10
                    +'function Print(s) { if (s===true) $testResult.push("True"); '
                                        +'else if (s===false) $testResult.push("False"); '
                                        +'else $testResult.push(s); }'#13#10
                    +'function PrintLn(s) { Print(s); $testResult.push("\r\n"); }'#13#10
                    +'try {'#13#10
                    +#13#10
                    +FCodeGen.CompiledOutput(prog)
                    +#13#10
                    +'} catch(e) {$testResult.splice(0,0,"Errors >>>>\r\nRuntime Error: "+((e.ClassType)?e.FMessage:e.message)+"\r\nResult >>>>\r\n")};'#13#10
                    +'console.log($testResult.join(""));'#13#10
                    +'})();';
            FChromium.ClearLastResult;

            //SaveTextToUTF8File('c:\temp\test.js', UTF8Encode(jscode));

            {// execute via node
            fileName:=GetTempFileName('dws');
            SaveTextToUTF8File(fileName, jscode);
            try
               FLastJSResult:=GetNodeOutput(fileName);
            finally
               DeleteFile(PChar(fileName));
            end;
            }

            // execute via chromium
            FChromium.ExecuteAndWait(jsCode, 'about:blank');
            {
            FChromium.ExecuteJavaScript(jsCode, 'about:blank');
            for k := 1 to 300 do begin
               if FChromium.LastJSResult <> '*no result*' then break;
               Application.ProcessMessages;
//               GlobalCEFApp.RunMessageLoop;
               case k of
                  0..99 : Sleep(1);
                  100..199 : Sleep(10);
               else
                  Sleep(20);
               end;
            end;
            //}

            output := FChromium.LastResult;
            if prog.Msgs.Count > 0 then begin
               output := 'Errors >>>>'#13#10
                       + prog.Msgs.AsInfo
                       + 'Result >>>>'#13#10
                       + output;
            end;

            expectedResult := GetExpectedResult(FTests[i]);
            FastStringReplace(expectedResult, #13#10, #10);
            FastStringReplace(output, #13#10, #10);
            if not (expectedResult=output) then begin
//               Clipboard.AsText:=jsCode;
               diagnostic.Add( ExtractFileName(FTests[i])
                              +': expected <'+expectedResult
                              +'> but got <'+output+'>');//+jsCode);
            end;
         except
            on e : Exception do begin
               diagnostic.Add(ExtractFileName(FTests[i])+': '+e.Message);
            end;
         end;

      end;

      CheckEquals(0, diagnostic.Count,
                  Format('%d / %d tests passed (%d ignored)'#13#10'%s',
                         [FTests.Count-diagnostic.Count-ignored, FTests.Count-ignored, ignored,
                          diagnostic.Text]));
   finally
      diagnostic.Free;
      source.Free;
   end;
end;

// CompilationNormal
//
procedure TJSCodeGenTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=cCompilerOptions+[coVariablesAsVarOnly];
   FCodeGen.Options:=FCodeGen.Options-[cgoNoInlineMagics];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TJSCodeGenTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=cCompilerOptions+[coSymbolDictionary, coContextMap, coVariablesAsVarOnly];
   FCodeGen.Options:=FCodeGen.Options+[cgoNoInlineMagics];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TJSCodeGenTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=cCompilerOptions-[coOptimize]+[coVariablesAsVarOnly];
   FCodeGen.Options:=FCodeGen.Options+[cgoNoInlineMagics];
   FCodeGen.Verbosity:=cgovVerbose;
   Execution;
   FCodeGen.Verbosity:=cgovNone;
end;

// ExecutionNonOptimizedWithInlineMagics
//
procedure TJSCodeGenTests.ExecutionNonOptimizedWithInlineMagics;
begin
   FCompiler.Config.CompilerOptions:=cCompilerOptions-[coOptimize]+[coVariablesAsVarOnly];
   FCodeGen.Options:=FCodeGen.Options-[cgoNoInlineMagics];
   Execution;
end;

// ExecutionOptimized
//
procedure TJSCodeGenTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=cCompilerOptions+[coOptimize, coVariablesAsVarOnly, coSymbolDictionary];
   FCodeGen.Options:=FCodeGen.Options+[cgoNoInlineMagics];
   Execution;
end;

// ExecutionOptimizedWithInlineMagics
//
procedure TJSCodeGenTests.ExecutionOptimizedWithInlineMagics;
begin
   FCompiler.Config.CompilerOptions:=cCompilerOptions+[coOptimize, coVariablesAsVarOnly, coSymbolDictionary];
   FCodeGen.Options:=FCodeGen.Options-[cgoNoInlineMagics];
   Execution;
end;

// ExecutionOptimizedAndSmartLinked
//
procedure TJSCodeGenTests.ExecutionOptimizedAndSmartLinked;
begin
   FCompiler.Config.CompilerOptions:=cCompilerOptions+[coOptimize, coVariablesAsVarOnly, coSymbolDictionary];
   FCodeGen.Options:=FCodeGen.Options-[cgoNoInlineMagics]+[cgoSmartLink, cgoDeVirtualize];
   Execution;
end;

// ExecutionOptimizedAndObfuscated
//
procedure TJSCodeGenTests.ExecutionOptimizedAndObfuscated;
begin
   FCompiler.Config.CompilerOptions:=cCompilerOptions+[coOptimize, coVariablesAsVarOnly];
   FCodeGen.Options:=FCodeGen.Options+[cgoObfuscate];
   try
      Execution;
   finally
      FCodeGen.Options:=FCodeGen.Options-[cgoObfuscate];
   end;
end;

// ExecutionOptimizedObfuscatedSmartLinked
//
procedure TJSCodeGenTests.ExecutionOptimizedObfuscatedSmartLinked;
begin
   FCompiler.Config.CompilerOptions:=
      cCompilerOptions+[coOptimize, coVariablesAsVarOnly, coSymbolDictionary, coContextMap];
   FCodeGen.Options:=FCodeGen.Options+[cgoObfuscate, cgoSmartLink, cgoOptimizeForSize];
   try
      Execution;
   finally
      FCodeGen.Options:=FCodeGen.Options-[cgoObfuscate, cgoSmartLink, cgoOptimizeForSize];
   end;
end;

// GetExpectedResult
//
function TJSCodeGenTests.GetExpectedResult(const fileName : String) : String;
var
   expectedResult : TStringList;
   resultsFileName : String;
begin
   expectedResult:=TStringList.Create;
   try
      if coOptimize in FCompiler.Config.CompilerOptions then begin
         resultsFileName:=ChangeFileExt(fileName, '.optimized.jstxt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            Result:=expectedResult.Text;
            exit;
         end;
      end;
      resultsFileName:=ChangeFileExt(fileName, '.jstxt');
      if FileExists(resultsFileName) then
         expectedResult.LoadFromFile(resultsFileName)
      else begin
         resultsFileName:=ChangeFileExt(fileName, '.txt');
         if FileExists(resultsFileName) then
            expectedResult.LoadFromFile(resultsFileName)
         else expectedResult.Clear;
      end;
      Result:=expectedResult.Text;
   finally
      expectedResult.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('JSCodeGenTests', TJSCodeGenTests.Suite);

end.
