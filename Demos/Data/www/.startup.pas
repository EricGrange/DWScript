// this script will be executed when the server starts

WebServer.SetURLRewriteRulesJSON(#'[
   {
      "pattern" : "/helloworldxyz",
      "rewrite" : "/index.dws"
   }
  ]');
