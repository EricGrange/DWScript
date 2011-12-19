type
   TTest = class
      function Get(a : Integer) : String;
      property Test[a : Integer] : String read Get;
      property TestVar[var a : Integer] : String read Get;
      property TestConst[const a : Integer] : String read Get;
      property TestUnfinished[a : Integer