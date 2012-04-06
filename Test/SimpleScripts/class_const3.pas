const cTOTO = 'toto';

type maclasse1 = class
   private
      const cTOTO = 'titi';
   public
      class procedure Test; begin PrintLn(cTOTO); end;
end;

type maclasse2 = class
   private
      const cTOTO = 'tata';
   public
      class procedure Test; begin PrintLn(cTOTO); end;
end;

PrintLn(cToto);
maclasse1.Test;
maclasse2.Test;
