unit JS_JSON;

// JS-based JSON for JS CodeGen tests

type

   JJSON = class external

      function Parse(s : String) : Variant; external 'parse';
      function Stringify(v : Variant) : String; overload; external 'stringify';
      function Stringify(v, replacer, space : Variant) : String; overload; external 'stringify';

   end;

function JSON : JJSON; external 'JSON' property;   
