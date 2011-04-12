type
   TBase = class
      procedure StaticProc;
      procedure VirtualProc1; virtual;
      procedure VirtualProc2(a : Integer); virtual;
      function VirtualMeth1 : Integer; virtual;
      function VirtualMeth2 : String; virtual;
   end;

type
   TChild1 = class (TBase)
      procedure StaticProc; override;
      procedure VirtualProc1; override;
      procedure VirtualProc2(a : Integer); override;
      function VirtualMeth1 : Integer; override;
      function VirtualMeth2 : String; override;
   end;

type
   TChild2 = class (TBase)
      procedure StaticProc; virtual;
      procedure VirtualProc1; virtual;
      procedure VirtualProc2(a : Integer); virtual;
      function VirtualMeth1 : Integer; virtual;
      function VirtualMeth2 : String; virtual;
   end;

type
   TChild3 = class (TBase)
      procedure VirtualProc2; override;
      procedure VirtualProc1(a : Integer); override;
      function VirtualMeth2 : Integer; override;
      function VirtualMeth1 : String; override;
   end;

type
   TChild4 = class (TBase)
      constructor VirtualProc1; override;
      procedure VirtualProc2(a : String); override;
      method VirtualMeth1 : Integer; override;
      class function VirtualMeth2 : String; override;
   end;

{$FATAL 'Done'}
