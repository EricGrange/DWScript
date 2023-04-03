{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsWebEnvironmentTypes;

interface

type
   TWebRequestAuthentication = (
      wraNone,
      wraFailed,
      wraBasic,
      wraDigest,
      wraNTLM,
      wraNegotiate,
      wraKerberos,
      wraAuthorization
   );
   TWebRequestAuthentications = set of TWebRequestAuthentication;

   TWebRequestMethodVerb = (
      wrmvUnknown,
      wrmvOPTIONS,
      wrmvGET,
      wrmvHEAD,
      wrmvPOST,
      wrmvPUT,
      wrmvDELETE,
      wrmvTRACE,
      wrmvCONNECT,
      wrmvTRACK,
      wrmvMOVE,
      wrmvCOPY,
      wrmvPROPFIND,
      wrmvPROPPATCH,
      wrmvMKCOL,
      wrmvLOCK,
      wrmvUNLOCK,
      wrmvSEARCH
   );
   TWebRequestMethodVerbs = set of TWebRequestMethodVerb;

implementation

end.
