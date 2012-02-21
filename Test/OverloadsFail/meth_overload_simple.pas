type
   TMyRecord = record
      procedure Test; overload;
      procedure Test; overload;
      procedure Test(a, b : Integer); overload;
      procedure Test(a : Integer); overload;
      procedure Test(a : String); overload;
      function  Test(a : String) : Integer; overload;
   end;
   
type 
   TMyClass = class
      procedure Test; overload;
      procedure Test; overload;
      procedure Test(a, b : Integer); overload;
      procedure Test(a : Integer); overload;
      procedure Test(a : String); overload;
      function  Test(a : String) : Integer; overload;
      procedure Virt; virtual; overload;
   end;

