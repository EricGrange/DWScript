type 
   TBase = class
      procedure Stuff; virtual;
      begin
         ///
      ensure
         1=0;
      end;
   end;
   
type 
   TChild = class(TBase)
      procedure Stuff; override;
      require
          1=1;
      begin
         ///
      ensure
          1=1;
      end;   
   end;   
   


