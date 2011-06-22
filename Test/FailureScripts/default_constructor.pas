type
   TMyClass = class
      constructor Hello; default;
      constructor World; default;
   end;

type
   TMyClass2 = class (TMyClass)
      constructor World; default;
      procedure Dummy; default;
   end;

{$FATAL 'done'}