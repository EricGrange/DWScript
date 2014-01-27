unit dwsExternalFunctionJit;

interface
uses
   SysUtils,
   dwsUtils, dwsSymbols;

type
   TFunctionCall = record
      call: nativeUInt;
      offset: integer;
   end;

   TFunctionCallArray = array of TFunctionCall;

   TTryFrame = array[0..3] of integer;

   IExternalFunctionJit = interface
      procedure BeginProcedure(params: TParamsSymbolTable);
      procedure BeginFunction(retval: TTypeSymbol; params: TParamsSymbolTable);
      procedure PassParam(param: TParamSymbol);
      procedure Call;
      procedure PostCall;
      function GetBytes: TBytes;
      function GetCalls: TFunctionCallArray;
      function HasTryFrame: boolean;
      function GetTryFrame: TTryFrame;
   end;

implementation

{ TFunctionCall }

end.
