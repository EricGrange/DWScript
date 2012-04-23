type
   TDummy = class 
      class procedure Test; static;
      constructor Bug; static;
      destructor Bug2; static;
      procedure ReBug; static;
   end;
   
type
   TRec = record
      class procedure Test; static;
      procedure Bug; static;
   end;   
   
 {$FATAL 'aborted'}