unit HelperLib;

interface

type
   TMyArray = array [0..0] of Integer;

   TMyArrayHelper = helper for TMyArray
      class const Hello : String = 'Hello';
   end;

