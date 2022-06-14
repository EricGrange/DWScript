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
unit dwsWebUtils;

{$I dws.inc}

interface

uses
   Classes, SysUtils, StrUtils,
   dwsUtils, dwsXPlatform, dwsXXHash;

type
   TMIMEBodyPart = class;

   IMIMEBodyPart = interface
      ['{5B77EB7D-7794-42AA-AD72-0713477EC405}']
      function GetSelf : TMIMEBodyPart;
      function GetRawHeaders : RawByteString;
      function GetRawData : RawByteString;
      function GetHeaders : TStrings;

      property RawHeaders : RawByteString read GetRawHeaders;
      property RawData : RawByteString read GetRawData;
      property Headers : TStrings read GetHeaders;

      function ContentDisposition : String;
      function ContentType : String;

      function Name : String;
      function FileName : String;
   end;

   TMIMEBodyPart = class (TInterfacedObject, IMIMEBodyPart)
      private
         FRawHeaders : RawByteString;
         FRawData : RawByteString;
         FHeaders : TStrings;
         FContentDisposition  : TStrings;
         FName : String;

      protected
         function GetSelf : TMIMEBodyPart;
         function GetRawHeaders : RawByteString;
         function GetRawData : RawByteString;
         procedure PrepareHeaders;
         function GetHeaders : TStrings;
         procedure PrepareContentDisposition;

      public
         destructor Destroy; override;

         property RawHeaders : RawByteString read FRawHeaders;
         property RawData : RawByteString read FRawData;
         property Headers : TStrings read GetHeaders;

         function ContentDisposition : String;
         function ContentType : String;

         function Name : String;
         function FileName : String;
   end;

   TIMIMEBodyParts = array of IMIMEBodyPart;

   WebUtils = class
      public
         class procedure ParseURLEncoded(const data : RawByteString; dest : TStrings); static;
         class function DecodeURLEncoded(const src : RawByteString; start, count : Integer) : String; overload; static;
         class function DecodeURLEncoded(const src : RawByteString; start : Integer) : String; overload; static;
         class function EncodeURLEncoded(const src : String) : String; static;

         class procedure ParseMIMEHeaderValue(const src : RawByteString; dest : TStrings); static;

         class procedure ParseMultiPartFormData(const src, dashBoundary : RawByteString; var dest : TIMIMEBodyParts); static;

         class function DecodeHex2(p : PAnsiChar) : Integer; static;
         class function HasFieldName(const list : TStrings; const name : String) : Boolean; static;

         class function EncodeEncodedWord(const s : String) : String; static;

         class function DateTimeToRFC822(const dt : TdwsDateTime) : String; overload; static; inline;
         class function DateTimeToRFC822(const dt : TDateTime) : String; overload; static;
         class function RFC822ToDateTime(const str : String) : TDateTime; static;

         class function HTMLTextEncode(const s : UnicodeString) : UnicodeString; static;
         class function HTMLTextDecode(const s : UnicodeString) : UnicodeString; static;
         class function HTMLCharacterDecode(p : PWideChar) : WideChar; static;

         class function HTMLAttributeEncode(const s : UnicodeString) : UnicodeString; static;
         class function HTMLAttributeDecode(const s : UnicodeString) : UnicodeString; static;

         class function CSSTextEncode(const s : UnicodeString) : UnicodeString; static;

         class function XMLTextEncode(const s : UnicodeString; unsupportedXML10CharactersMode : Integer = 0) : UnicodeString; static;
         class function XMLTextDecode(const s : UnicodeString) : UnicodeString; static;

         class function IsValidCookieName(const s : String) : Boolean; static;
         class function IsValidCookieValue(const s : String) : Boolean; static;
   end;

   EXMLDecodeError = class (Exception);
   EXMLEncodeError = class (Exception);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cToHex : String = '0123456789ABCDEF';

   // based on http://www.w3.org/TR/html5/entities.json
   cAllNamedEntities =
       'Aacute=C1,aacute=E1,Abreve=102,abreve=103,ac=223E,acd=223F,acE=223E,Acirc=C2,acirc=E2,acute=B4,Acy=410,acy=430,AElig=C6,aelig=E6,'
      +'af=2061,Afr=1D504,afr=1D51E,Agrave=C0,agrave=E0,alefsym=2135,aleph=2135,Alpha=391,alpha=3B1,Amacr=100,amacr=101,amalg=2A3F,'
      +'AMP=26,amp=26,And=2A53,and=2227,andand=2A55,andd=2A5C,andslope=2A58,andv=2A5A,ang=2220,ange=29A4,angle=2220,angmsd=2221,angmsdaa=29A8,'
      +'angmsdab=29A9,angmsdac=29AA,angmsdad=29AB,angmsdae=29AC,angmsdaf=29AD,angmsdag=29AE,angmsdah=29AF,angrt=221F,angrtvb=22BE,'
      +'angrtvbd=299D,angsph=2222,angst=C5,angzarr=237C,Aogon=104,aogon=105,Aopf=1D538,aopf=1D552,ap=2248,apacir=2A6F,apE=2A70,ape=224A,'
      +'apid=224B,apos=27,ApplyFunction=2061,approx=2248,approxeq=224A,Aring=C5,aring=E5,Ascr=1D49C,ascr=1D4B6,Assign=2254,ast=2A,'
      +'asymp=2248,asympeq=224D,Atilde=C3,atilde=E3,Auml=C4,auml=E4,awconint=2233,awint=2A11,backcong=224C,backepsilon=3F6,backprime=2035,'
      +'backsim=223D,backsimeq=22CD,Backslash=2216,Barv=2AE7,barvee=22BD,Barwed=2306,barwed=2305,barwedge=2305,bbrk=23B5,bbrktbrk=23B6,'
      +'bcong=224C,Bcy=411,bcy=431,bdquo=201E,becaus=2235,Because=2235,because=2235,bemptyv=29B0,bepsi=3F6,bernou=212C,Bernoullis=212C,'
      +'Beta=392,beta=3B2,beth=2136,between=226C,Bfr=1D505,bfr=1D51F,bigcap=22C2,bigcirc=25EF,bigcup=22C3,bigodot=2A00,bigoplus=2A01,'
      +'bigotimes=2A02,bigsqcup=2A06,bigstar=2605,bigtriangledown=25BD,bigtriangleup=25B3,biguplus=2A04,bigvee=22C1,bigwedge=22C0,'
      +'bkarow=290D,blacklozenge=29EB,blacksquare=25AA,blacktriangle=25B4,blacktriangledown=25BE,blacktriangleleft=25C2,blacktriangleright=25B8,'
      +'blank=2423,blk12=2592,blk14=2591,blk34=2593,block=2588,bne=3D,bnequiv=2261,bNot=2AED,bnot=2310,Bopf=1D539,bopf=1D553,bot=22A5,'
      +'bottom=22A5,bowtie=22C8,boxbox=29C9,boxDL=2557,boxDl=2556,boxdL=2555,boxdl=2510,boxDR=2554,boxDr=2553,boxdR=2552,boxdr=250C,'
      +'boxH=2550,boxh=2500,boxHD=2566,boxHd=2564,boxhD=2565,boxhd=252C,boxHU=2569,boxHu=2567,boxhU=2568,boxhu=2534,boxminus=229F,'
      +'boxplus=229E,boxtimes=22A0,boxUL=255D,boxUl=255C,boxuL=255B,boxul=2518,boxUR=255A,boxUr=2559,boxuR=2558,boxur=2514,boxV=2551,'
      +'boxv=2502,boxVH=256C,boxVh=256B,boxvH=256A,boxvh=253C,boxVL=2563,boxVl=2562,boxvL=2561,boxvl=2524,boxVR=2560,boxVr=255F,boxvR=255E,'
      +'boxvr=251C,bprime=2035,Breve=2D8,breve=2D8,brvbar=A6,Bscr=212C,bscr=1D4B7,bsemi=204F,bsim=223D,bsime=22CD,bsol=5C,bsolb=29C5,'
      +'bsolhsub=27C8,bull=2022,bullet=2022,bump=224E,bumpE=2AAE,bumpe=224F,Bumpeq=224E,bumpeq=224F,Cacute=106,cacute=107,Cap=22D2,'
      +'cap=2229,capand=2A44,capbrcup=2A49,capcap=2A4B,capcup=2A47,capdot=2A40,CapitalDifferentialD=2145,caps=2229,caret=2041,caron=2C7,'
      +'Cayleys=212D,ccaps=2A4D,Ccaron=10C,ccaron=10D,Ccedil=C7,ccedil=E7,Ccirc=108,ccirc=109,Cconint=2230,ccups=2A4C,ccupssm=2A50,'
      +'Cdot=10A,cdot=10B,cedil=B8,Cedilla=B8,cemptyv=29B2,cent=A2,CenterDot=B7,centerdot=B7,Cfr=212D,cfr=1D520,CHcy=427,chcy=447,'
      +'check=2713,checkmark=2713,Chi=3A7,chi=3C7,cir=25CB,circ=2C6,circeq=2257,circlearrowleft=21BA,circlearrowright=21BB,circledast=229B,'
      +'circledcirc=229A,circleddash=229D,CircleDot=2299,circledR=AE,circledS=24C8,CircleMinus=2296,CirclePlus=2295,CircleTimes=2297,'
      +'cirE=29C3,cire=2257,cirfnint=2A10,cirmid=2AEF,cirscir=29C2,ClockwiseContourIntegral=2232,CloseCurlyDoubleQuote=201D,CloseCurlyQuote=2019,'
      +'clubs=2663,clubsuit=2663,Colon=2237,colon=3A,Colone=2A74,colone=2254,coloneq=2254,comma=2C,commat=40,comp=2201,compfn=2218,'
      +'complement=2201,complexes=2102,cong=2245,congdot=2A6D,Congruent=2261,Conint=222F,conint=222E,ContourIntegral=222E,Copf=2102,'
      +'copf=1D554,coprod=2210,Coproduct=2210,COPY=A9,copy=A9,copysr=2117,CounterClockwiseContourIntegral=2233,crarr=21B5,Cross=2A2F,'
      +'cross=2717,Cscr=1D49E,cscr=1D4B8,csub=2ACF,csube=2AD1,csup=2AD0,csupe=2AD2,ctdot=22EF,cudarrl=2938,cudarrr=2935,cuepr=22DE,'
      +'cuesc=22DF,cularr=21B6,cularrp=293D,Cup=22D3,cup=222A,cupbrcap=2A48,CupCap=224D,cupcap=2A46,cupcup=2A4A,cupdot=228D,cupor=2A45,'
      +'cups=222A,curarr=21B7,curarrm=293C,curlyeqprec=22DE,curlyeqsucc=22DF,curlyvee=22CE,curlywedge=22CF,curren=A4,curvearrowleft=21B6,'
      +'curvearrowright=21B7,cuvee=22CE,cuwed=22CF,cwconint=2232,cwint=2231,cylcty=232D,Dagger=2021,dagger=2020,daleth=2138,Darr=21A1,'
      +'dArr=21D3,darr=2193,dash=2010,Dashv=2AE4,dashv=22A3,dbkarow=290F,dblac=2DD,Dcaron=10E,dcaron=10F,Dcy=414,dcy=434,DD=2145,'
      +'dd=2146,ddagger=2021,ddarr=21CA,DDotrahd=2911,ddotseq=2A77,deg=B0,Del=2207,Delta=394,delta=3B4,demptyv=29B1,dfisht=297F,Dfr=1D507,'
      +'dfr=1D521,dHar=2965,dharl=21C3,dharr=21C2,DiacriticalAcute=B4,DiacriticalDot=2D9,DiacriticalDoubleAcute=2DD,DiacriticalGrave=60,'
      +'DiacriticalTilde=2DC,diam=22C4,Diamond=22C4,diamond=22C4,diamondsuit=2666,diams=2666,die=A8,DifferentialD=2146,digamma=3DD,'
      +'disin=22F2,div=F7,divide=F7,divideontimes=22C7,divonx=22C7,DJcy=402,djcy=452,dlcorn=231E,dlcrop=230D,dollar=24,Dopf=1D53B,'
      +'dopf=1D555,Dot=A8,dot=2D9,DotDot=20DC,doteq=2250,doteqdot=2251,DotEqual=2250,dotminus=2238,dotplus=2214,dotsquare=22A1,doublebarwedge=2306,'
      +'DoubleContourIntegral=222F,DoubleDot=A8,DoubleDownArrow=21D3,DoubleLeftArrow=21D0,DoubleLeftRightArrow=21D4,DoubleLeftTee=2AE4,'
      +'DoubleLongLeftArrow=27F8,DoubleLongLeftRightArrow=27FA,DoubleLongRightArrow=27F9,DoubleRightArrow=21D2,DoubleRightTee=22A8,'
      +'DoubleUpArrow=21D1,DoubleUpDownArrow=21D5,DoubleVerticalBar=2225,DownArrow=2193,Downarrow=21D3,downarrow=2193,DownArrowBar=2913,'
      +'DownArrowUpArrow=21F5,DownBreve=311,downdownarrows=21CA,downharpoonleft=21C3,downharpoonright=21C2,DownLeftRightVector=2950,'
      +'DownLeftTeeVector=295E,DownLeftVector=21BD,DownLeftVectorBar=2956,DownRightTeeVector=295F,DownRightVector=21C1,DownRightVectorBar=2957,'
      +'DownTee=22A4,DownTeeArrow=21A7,drbkarow=2910,drcorn=231F,drcrop=230C,Dscr=1D49F,dscr=1D4B9,DScy=405,dscy=455,dsol=29F6,Dstrok=110,'
      +'dstrok=111,dtdot=22F1,dtri=25BF,dtrif=25BE,duarr=21F5,duhar=296F,dwangle=29A6,DZcy=40F,dzcy=45F,dzigrarr=27FF,Eacute=C9,eacute=E9,'
      +'easter=2A6E,Ecaron=11A,ecaron=11B,ecir=2256,Ecirc=CA,ecirc=EA,ecolon=2255,Ecy=42D,ecy=44D,eDDot=2A77,Edot=116,eDot=2251,edot=117,'
      +'ee=2147,efDot=2252,Efr=1D508,efr=1D522,eg=2A9A,Egrave=C8,egrave=E8,egs=2A96,egsdot=2A98,el=2A99,Element=2208,elinters=23E7,'
      +'ell=2113,els=2A95,elsdot=2A97,Emacr=112,emacr=113,empty=2205,emptyset=2205,EmptySmallSquare=25FB,emptyv=2205,EmptyVerySmallSquare=25AB,'
      +'emsp=2003,emsp13=2004,emsp14=2005,ENG=14A,eng=14B,ensp=2002,Eogon=118,eogon=119,Eopf=1D53C,eopf=1D556,epar=22D5,eparsl=29E3,'
      +'eplus=2A71,epsi=3B5,Epsilon=395,epsilon=3B5,epsiv=3F5,eqcirc=2256,eqcolon=2255,eqsim=2242,eqslantgtr=2A96,eqslantless=2A95,'
      +'Equal=2A75,equals=3D,EqualTilde=2242,equest=225F,Equilibrium=21CC,equiv=2261,equivDD=2A78,eqvparsl=29E5,erarr=2971,erDot=2253,'
      +'Escr=2130,escr=212F,esdot=2250,Esim=2A73,esim=2242,Eta=397,eta=3B7,ETH=D0,eth=F0,Euml=CB,euml=EB,euro=20AC,excl=21,exist=2203,'
      +'Exists=2203,expectation=2130,ExponentialE=2147,exponentiale=2147,fallingdotseq=2252,Fcy=424,fcy=444,female=2640,ffilig=FB03,'
      +'fflig=FB00,ffllig=FB04,Ffr=1D509,ffr=1D523,filig=FB01,FilledSmallSquare=25FC,FilledVerySmallSquare=25AA,fjlig=66,flat=266D,'
      +'fllig=FB02,fltns=25B1,fnof=192,Fopf=1D53D,fopf=1D557,ForAll=2200,forall=2200,fork=22D4,forkv=2AD9,Fouriertrf=2131,fpartint=2A0D,'
      +'frac12=BD,frac13=2153,frac14=BC,frac15=2155,frac16=2159,frac18=215B,frac23=2154,frac25=2156,frac34=BE,frac35=2157,frac38=215C,'
      +'frac45=2158,frac56=215A,frac58=215D,frac78=215E,frasl=2044,frown=2322,Fscr=2131,fscr=1D4BB,gacute=1F5,Gamma=393,gamma=3B3,'
      +'Gammad=3DC,gammad=3DD,gap=2A86,Gbreve=11E,gbreve=11F,Gcedil=122,Gcirc=11C,gcirc=11D,Gcy=413,gcy=433,Gdot=120,gdot=121,gE=2267,'
      +'ge=2265,gEl=2A8C,gel=22DB,geq=2265,geqq=2267,geqslant=2A7E,ges=2A7E,gescc=2AA9,gesdot=2A80,gesdoto=2A82,gesdotol=2A84,gesl=22DB,'
      +'gesles=2A94,Gfr=1D50A,gfr=1D524,Gg=22D9,gg=226B,ggg=22D9,gimel=2137,GJcy=403,gjcy=453,gl=2277,gla=2AA5,glE=2A92,glj=2AA4,'
      +'gnap=2A8A,gnapprox=2A8A,gnE=2269,gne=2A88,gneq=2A88,gneqq=2269,gnsim=22E7,Gopf=1D53E,gopf=1D558,grave=60,GreaterEqual=2265,'
      +'GreaterEqualLess=22DB,GreaterFullEqual=2267,GreaterGreater=2AA2,GreaterLess=2277,GreaterSlantEqual=2A7E,GreaterTilde=2273,'
      +'Gscr=1D4A2,gscr=210A,gsim=2273,gsime=2A8E,gsiml=2A90,GT=3E,Gt=226B,gt=3E,gtcc=2AA7,gtcir=2A7A,gtdot=22D7,gtlPar=2995,gtquest=2A7C,'
      +'gtrapprox=2A86,gtrarr=2978,gtrdot=22D7,gtreqless=22DB,gtreqqless=2A8C,gtrless=2277,gtrsim=2273,gvertneqq=2269,gvnE=2269,Hacek=2C7,'
      +'hairsp=200A,half=BD,hamilt=210B,HARDcy=42A,hardcy=44A,hArr=21D4,harr=2194,harrcir=2948,harrw=21AD,Hat=5E,hbar=210F,Hcirc=124,'
      +'hcirc=125,hearts=2665,heartsuit=2665,hellip=2026,hercon=22B9,Hfr=210C,hfr=1D525,HilbertSpace=210B,hksearow=2925,hkswarow=2926,'
      +'hoarr=21FF,homtht=223B,hookleftarrow=21A9,hookrightarrow=21AA,Hopf=210D,hopf=1D559,horbar=2015,HorizontalLine=2500,Hscr=210B,'
      +'hscr=1D4BD,hslash=210F,Hstrok=126,hstrok=127,HumpDownHump=224E,HumpEqual=224F,hybull=2043,hyphen=2010,Iacute=CD,iacute=ED,'
      +'ic=2063,Icirc=CE,icirc=EE,Icy=418,icy=438,Idot=130,IEcy=415,iecy=435,iexcl=A1,iff=21D4,Ifr=2111,ifr=1D526,Igrave=CC,igrave=EC,'
      +'ii=2148,iiiint=2A0C,iiint=222D,iinfin=29DC,iiota=2129,IJlig=132,ijlig=133,Im=2111,Imacr=12A,imacr=12B,image=2111,ImaginaryI=2148,'
      +'imagline=2110,imagpart=2111,imath=131,imof=22B7,imped=1B5,Implies=21D2,in=2208,incare=2105,infin=221E,infintie=29DD,inodot=131,'
      +'Int=222C,int=222B,intcal=22BA,integers=2124,Integral=222B,intercal=22BA,Intersection=22C2,intlarhk=2A17,intprod=2A3C,InvisibleComma=2063,'
      +'InvisibleTimes=2062,IOcy=401,iocy=451,Iogon=12E,iogon=12F,Iopf=1D540,iopf=1D55A,Iota=399,iota=3B9,iprod=2A3C,iquest=BF,Iscr=2110,'
      +'iscr=1D4BE,isin=2208,isindot=22F5,isinE=22F9,isins=22F4,isinsv=22F3,isinv=2208,it=2062,Itilde=128,itilde=129,Iukcy=406,iukcy=456,'
      +'Iuml=CF,iuml=EF,Jcirc=134,jcirc=135,Jcy=419,jcy=439,Jfr=1D50D,jfr=1D527,jmath=237,Jopf=1D541,jopf=1D55B,Jscr=1D4A5,jscr=1D4BF,'
      +'Jsercy=408,jsercy=458,Jukcy=404,jukcy=454,Kappa=39A,kappa=3BA,kappav=3F0,Kcedil=136,kcedil=137,Kcy=41A,kcy=43A,Kfr=1D50E,'
      +'kfr=1D528,kgreen=138,KHcy=425,khcy=445,KJcy=40C,kjcy=45C,Kopf=1D542,kopf=1D55C,Kscr=1D4A6,kscr=1D4C0,lAarr=21DA,Lacute=139,'
      +'lacute=13A,laemptyv=29B4,lagran=2112,Lambda=39B,lambda=3BB,Lang=27EA,lang=27E8,langd=2991,langle=27E8,lap=2A85,Laplacetrf=2112,'
      +'laquo=AB,Larr=219E,lArr=21D0,larr=2190,larrb=21E4,larrbfs=291F,larrfs=291D,larrhk=21A9,larrlp=21AB,larrpl=2939,larrsim=2973,'
      +'larrtl=21A2,lat=2AAB,lAtail=291B,latail=2919,late=2AAD,lates=2AAD,lBarr=290E,lbarr=290C,lbbrk=2772,lbrace=7B,lbrack=5B,lbrke=298B,'
      +'lbrksld=298F,lbrkslu=298D,Lcaron=13D,lcaron=13E,Lcedil=13B,lcedil=13C,lceil=2308,lcub=7B,Lcy=41B,lcy=43B,ldca=2936,ldquo=201C,'
      +'ldquor=201E,ldrdhar=2967,ldrushar=294B,ldsh=21B2,lE=2266,le=2264,LeftAngleBracket=27E8,LeftArrow=2190,Leftarrow=21D0,leftarrow=2190,'
      +'LeftArrowBar=21E4,LeftArrowRightArrow=21C6,leftarrowtail=21A2,LeftCeiling=2308,LeftDoubleBracket=27E6,LeftDownTeeVector=2961,'
      +'LeftDownVector=21C3,LeftDownVectorBar=2959,LeftFloor=230A,leftharpoondown=21BD,leftharpoonup=21BC,leftleftarrows=21C7,LeftRightArrow=2194,'
      +'Leftrightarrow=21D4,leftrightarrow=2194,leftrightarrows=21C6,leftrightharpoons=21CB,leftrightsquigarrow=21AD,LeftRightVector=294E,'
      +'LeftTee=22A3,LeftTeeArrow=21A4,LeftTeeVector=295A,leftthreetimes=22CB,LeftTriangle=22B2,LeftTriangleBar=29CF,LeftTriangleEqual=22B4,'
      +'LeftUpDownVector=2951,LeftUpTeeVector=2960,LeftUpVector=21BF,LeftUpVectorBar=2958,LeftVector=21BC,LeftVectorBar=2952,lEg=2A8B,'
      +'leg=22DA,leq=2264,leqq=2266,leqslant=2A7D,les=2A7D,lescc=2AA8,lesdot=2A7F,lesdoto=2A81,lesdotor=2A83,lesg=22DA,lesges=2A93,'
      +'lessapprox=2A85,lessdot=22D6,lesseqgtr=22DA,lesseqqgtr=2A8B,LessEqualGreater=22DA,LessFullEqual=2266,LessGreater=2276,lessgtr=2276,'
      +'LessLess=2AA1,lesssim=2272,LessSlantEqual=2A7D,LessTilde=2272,lfisht=297C,lfloor=230A,Lfr=1D50F,lfr=1D529,lg=2276,lgE=2A91,'
      +'lHar=2962,lhard=21BD,lharu=21BC,lharul=296A,lhblk=2584,LJcy=409,ljcy=459,Ll=22D8,ll=226A,llarr=21C7,llcorner=231E,Lleftarrow=21DA,'
      +'llhard=296B,lltri=25FA,Lmidot=13F,lmidot=140,lmoust=23B0,lmoustache=23B0,lnap=2A89,lnapprox=2A89,lnE=2268,lne=2A87,lneq=2A87,'
      +'lneqq=2268,lnsim=22E6,loang=27EC,loarr=21FD,lobrk=27E6,LongLeftArrow=27F5,Longleftarrow=27F8,longleftarrow=27F5,LongLeftRightArrow=27F7,'
      +'Longleftrightarrow=27FA,longleftrightarrow=27F7,longmapsto=27FC,LongRightArrow=27F6,Longrightarrow=27F9,longrightarrow=27F6,'
      +'looparrowleft=21AB,looparrowright=21AC,lopar=2985,Lopf=1D543,lopf=1D55D,loplus=2A2D,lotimes=2A34,lowast=2217,lowbar=5F,LowerLeftArrow=2199,'
      +'LowerRightArrow=2198,loz=25CA,lozenge=25CA,lozf=29EB,lpar=28,lparlt=2993,lrarr=21C6,lrcorner=231F,lrhar=21CB,lrhard=296D,'
      +'lrm=200E,lrtri=22BF,lsaquo=2039,Lscr=2112,lscr=1D4C1,Lsh=21B0,lsh=21B0,lsim=2272,lsime=2A8D,lsimg=2A8F,lsqb=5B,lsquo=2018,'
      +'lsquor=201A,Lstrok=141,lstrok=142,LT=3C,Lt=226A,lt=3C,ltcc=2AA6,ltcir=2A79,ltdot=22D6,lthree=22CB,ltimes=22C9,ltlarr=2976,'
      +'ltquest=2A7B,ltri=25C3,ltrie=22B4,ltrif=25C2,ltrPar=2996,lurdshar=294A,luruhar=2966,lvertneqq=2268,lvnE=2268,macr=AF,male=2642,'
      +'malt=2720,maltese=2720,Map=2905,map=21A6,mapsto=21A6,mapstodown=21A7,mapstoleft=21A4,mapstoup=21A5,marker=25AE,mcomma=2A29,'
      +'Mcy=41C,mcy=43C,mdash=2014,mDDot=223A,measuredangle=2221,MediumSpace=205F,Mellintrf=2133,Mfr=1D510,mfr=1D52A,mho=2127,micro=B5,'
      +'mid=2223,midast=2A,midcir=2AF0,middot=B7,minus=2212,minusb=229F,minusd=2238,minusdu=2A2A,MinusPlus=2213,mlcp=2ADB,mldr=2026,'
      +'mnplus=2213,models=22A7,Mopf=1D544,mopf=1D55E,mp=2213,Mscr=2133,mscr=1D4C2,mstpos=223E,Mu=39C,mu=3BC,multimap=22B8,mumap=22B8,'
      +'nabla=2207,Nacute=143,nacute=144,nang=2220,nap=2249,napE=2A70,napid=224B,napos=149,napprox=2249,natur=266E,natural=266E,naturals=2115,'
      +'nbsp=A0,nbump=224E,nbumpe=224F,ncap=2A43,Ncaron=147,ncaron=148,Ncedil=145,ncedil=146,ncong=2247,ncongdot=2A6D,ncup=2A42,Ncy=41D,'
      +'ncy=43D,ndash=2013,ne=2260,nearhk=2924,neArr=21D7,nearr=2197,nearrow=2197,nedot=2250,NegativeMediumSpace=200B,NegativeThickSpace=200B,'
      +'NegativeThinSpace=200B,NegativeVeryThinSpace=200B,nequiv=2262,nesear=2928,nesim=2242,NestedGreaterGreater=226B,NestedLessLess=226A,'
      +'NewLine=A,nexist=2204,nexists=2204,Nfr=1D511,nfr=1D52B,ngE=2267,nge=2271,ngeq=2271,ngeqq=2267,ngeqslant=2A7E,nges=2A7E,nGg=22D9,'
      +'ngsim=2275,nGt=226B,ngt=226F,ngtr=226F,nGtv=226B,nhArr=21CE,nharr=21AE,nhpar=2AF2,ni=220B,nis=22FC,nisd=22FA,niv=220B,NJcy=40A,'
      +'njcy=45A,nlArr=21CD,nlarr=219A,nldr=2025,nlE=2266,nle=2270,nLeftarrow=21CD,nleftarrow=219A,nLeftrightarrow=21CE,nleftrightarrow=21AE,'
      +'nleq=2270,nleqq=2266,nleqslant=2A7D,nles=2A7D,nless=226E,nLl=22D8,nlsim=2274,nLt=226A,nlt=226E,nltri=22EA,nltrie=22EC,nLtv=226A,'
      +'nmid=2224,NoBreak=2060,NonBreakingSpace=A0,Nopf=2115,nopf=1D55F,Not=2AEC,not=AC,NotCongruent=2262,NotCupCap=226D,NotDoubleVerticalBar=2226,'
      +'NotElement=2209,NotEqual=2260,NotEqualTilde=2242,NotExists=2204,NotGreater=226F,NotGreaterEqual=2271,NotGreaterFullEqual=2267,'
      +'NotGreaterGreater=226B,NotGreaterLess=2279,NotGreaterSlantEqual=2A7E,NotGreaterTilde=2275,NotHumpDownHump=224E,NotHumpEqual=224F,'
      +'notin=2209,notindot=22F5,notinE=22F9,notinva=2209,notinvb=22F7,notinvc=22F6,NotLeftTriangle=22EA,NotLeftTriangleBar=29CF,'
      +'NotLeftTriangleEqual=22EC,NotLess=226E,NotLessEqual=2270,NotLessGreater=2278,NotLessLess=226A,NotLessSlantEqual=2A7D,NotLessTilde=2274,'
      +'NotNestedGreaterGreater=2AA2,NotNestedLessLess=2AA1,notni=220C,notniva=220C,notnivb=22FE,notnivc=22FD,NotPrecedes=2280,NotPrecedesEqual=2AAF,'
      +'NotPrecedesSlantEqual=22E0,NotReverseElement=220C,NotRightTriangle=22EB,NotRightTriangleBar=29D0,NotRightTriangleEqual=22ED,'
      +'NotSquareSubset=228F,NotSquareSubsetEqual=22E2,NotSquareSuperset=2290,NotSquareSupersetEqual=22E3,NotSubset=2282,NotSubsetEqual=2288,'
      +'NotSucceeds=2281,NotSucceedsEqual=2AB0,NotSucceedsSlantEqual=22E1,NotSucceedsTilde=227F,NotSuperset=2283,NotSupersetEqual=2289,'
      +'NotTilde=2241,NotTildeEqual=2244,NotTildeFullEqual=2247,NotTildeTilde=2249,NotVerticalBar=2224,npar=2226,nparallel=2226,nparsl=2AFD,'
      +'npart=2202,npolint=2A14,npr=2280,nprcue=22E0,npre=2AAF,nprec=2280,npreceq=2AAF,nrArr=21CF,nrarr=219B,nrarrc=2933,nrarrw=219D,'
      +'nRightarrow=21CF,nrightarrow=219B,nrtri=22EB,nrtrie=22ED,nsc=2281,nsccue=22E1,nsce=2AB0,Nscr=1D4A9,nscr=1D4C3,nshortmid=2224,'
      +'nshortparallel=2226,nsim=2241,nsime=2244,nsimeq=2244,nsmid=2224,nspar=2226,nsqsube=22E2,nsqsupe=22E3,nsub=2284,nsubE=2AC5,'
      +'nsube=2288,nsubset=2282,nsubseteq=2288,nsubseteqq=2AC5,nsucc=2281,nsucceq=2AB0,nsup=2285,nsupE=2AC6,nsupe=2289,nsupset=2283,'
      +'nsupseteq=2289,nsupseteqq=2AC6,ntgl=2279,Ntilde=D1,ntilde=F1,ntlg=2278,ntriangleleft=22EA,ntrianglelefteq=22EC,ntriangleright=22EB,'
      +'ntrianglerighteq=22ED,Nu=39D,nu=3BD,num=23,numero=2116,numsp=2007,nvap=224D,nVDash=22AF,nVdash=22AE,nvDash=22AD,nvdash=22AC,'
      +'nvge=2265,nvgt=3E,nvHarr=2904,nvinfin=29DE,nvlArr=2902,nvle=2264,nvlt=3C,nvltrie=22B4,nvrArr=2903,nvrtrie=22B5,nvsim=223C,'
      +'nwarhk=2923,nwArr=21D6,nwarr=2196,nwarrow=2196,nwnear=2927,Oacute=D3,oacute=F3,oast=229B,ocir=229A,Ocirc=D4,ocirc=F4,Ocy=41E,'
      +'ocy=43E,odash=229D,Odblac=150,odblac=151,odiv=2A38,odot=2299,odsold=29BC,OElig=152,oelig=153,ofcir=29BF,Ofr=1D512,ofr=1D52C,'
      +'ogon=2DB,Ograve=D2,ograve=F2,ogt=29C1,ohbar=29B5,ohm=3A9,oint=222E,olarr=21BA,olcir=29BE,olcross=29BB,oline=203E,olt=29C0,'
      +'Omacr=14C,omacr=14D,Omega=3A9,omega=3C9,Omicron=39F,omicron=3BF,omid=29B6,ominus=2296,Oopf=1D546,oopf=1D560,opar=29B7,OpenCurlyDoubleQuote=201C,'
      +'OpenCurlyQuote=2018,operp=29B9,oplus=2295,Or=2A54,or=2228,orarr=21BB,ord=2A5D,order=2134,orderof=2134,ordf=AA,ordm=BA,origof=22B6,'
      +'oror=2A56,orslope=2A57,orv=2A5B,oS=24C8,Oscr=1D4AA,oscr=2134,Oslash=D8,oslash=F8,osol=2298,Otilde=D5,otilde=F5,Otimes=2A37,'
      +'otimes=2297,otimesas=2A36,Ouml=D6,ouml=F6,ovbar=233D,OverBar=203E,OverBrace=23DE,OverBracket=23B4,OverParenthesis=23DC,par=2225,'
      +'para=B6,parallel=2225,parsim=2AF3,parsl=2AFD,part=2202,PartialD=2202,Pcy=41F,pcy=43F,percnt=25,period=2E,permil=2030,perp=22A5,'
      +'pertenk=2031,Pfr=1D513,pfr=1D52D,Phi=3A6,phi=3C6,phiv=3D5,phmmat=2133,phone=260E,Pi=3A0,pi=3C0,pitchfork=22D4,piv=3D6,planck=210F,'
      +'planckh=210E,plankv=210F,plus=2B,plusacir=2A23,plusb=229E,pluscir=2A22,plusdo=2214,plusdu=2A25,pluse=2A72,PlusMinus=B1,plusmn=B1,'
      +'plussim=2A26,plustwo=2A27,pm=B1,Poincareplane=210C,pointint=2A15,Popf=2119,popf=1D561,pound=A3,Pr=2ABB,pr=227A,prap=2AB7,'
      +'prcue=227C,prE=2AB3,pre=2AAF,prec=227A,precapprox=2AB7,preccurlyeq=227C,Precedes=227A,PrecedesEqual=2AAF,PrecedesSlantEqual=227C,'
      +'PrecedesTilde=227E,preceq=2AAF,precnapprox=2AB9,precneqq=2AB5,precnsim=22E8,precsim=227E,Prime=2033,prime=2032,primes=2119,'
      +'prnap=2AB9,prnE=2AB5,prnsim=22E8,prod=220F,Product=220F,profalar=232E,profline=2312,profsurf=2313,prop=221D,Proportion=2237,'
      +'Proportional=221D,propto=221D,prsim=227E,prurel=22B0,Pscr=1D4AB,pscr=1D4C5,Psi=3A8,psi=3C8,puncsp=2008,Qfr=1D514,qfr=1D52E,'
      +'qint=2A0C,Qopf=211A,qopf=1D562,qprime=2057,Qscr=1D4AC,qscr=1D4C6,quaternions=210D,quatint=2A16,quest=3F,questeq=225F,QUOT=22,'
      +'quot=22,rAarr=21DB,race=223D,Racute=154,racute=155,radic=221A,raemptyv=29B3,Rang=27EB,rang=27E9,rangd=2992,range=29A5,rangle=27E9,'
      +'raquo=BB,Rarr=21A0,rArr=21D2,rarr=2192,rarrap=2975,rarrb=21E5,rarrbfs=2920,rarrc=2933,rarrfs=291E,rarrhk=21AA,rarrlp=21AC,'
      +'rarrpl=2945,rarrsim=2974,Rarrtl=2916,rarrtl=21A3,rarrw=219D,rAtail=291C,ratail=291A,ratio=2236,rationals=211A,RBarr=2910,'
      +'rBarr=290F,rbarr=290D,rbbrk=2773,rbrace=7D,rbrack=5D,rbrke=298C,rbrksld=298E,rbrkslu=2990,Rcaron=158,rcaron=159,Rcedil=156,'
      +'rcedil=157,rceil=2309,rcub=7D,Rcy=420,rcy=440,rdca=2937,rdldhar=2969,rdquo=201D,rdquor=201D,rdsh=21B3,Re=211C,real=211C,realine=211B,'
      +'realpart=211C,reals=211D,rect=25AD,REG=AE,reg=AE,ReverseElement=220B,ReverseEquilibrium=21CB,ReverseUpEquilibrium=296F,rfisht=297D,'
      +'rfloor=230B,Rfr=211C,rfr=1D52F,rHar=2964,rhard=21C1,rharu=21C0,rharul=296C,Rho=3A1,rho=3C1,rhov=3F1,RightAngleBracket=27E9,'
      +'RightArrow=2192,Rightarrow=21D2,rightarrow=2192,RightArrowBar=21E5,RightArrowLeftArrow=21C4,rightarrowtail=21A3,RightCeiling=2309,'
      +'RightDoubleBracket=27E7,RightDownTeeVector=295D,RightDownVector=21C2,RightDownVectorBar=2955,RightFloor=230B,rightharpoondown=21C1,'
      +'rightharpoonup=21C0,rightleftarrows=21C4,rightleftharpoons=21CC,rightrightarrows=21C9,rightsquigarrow=219D,RightTee=22A2,'
      +'RightTeeArrow=21A6,RightTeeVector=295B,rightthreetimes=22CC,RightTriangle=22B3,RightTriangleBar=29D0,RightTriangleEqual=22B5,'
      +'RightUpDownVector=294F,RightUpTeeVector=295C,RightUpVector=21BE,RightUpVectorBar=2954,RightVector=21C0,RightVectorBar=2953,'
      +'ring=2DA,risingdotseq=2253,rlarr=21C4,rlhar=21CC,rlm=200F,rmoust=23B1,rmoustache=23B1,rnmid=2AEE,roang=27ED,roarr=21FE,robrk=27E7,'
      +'ropar=2986,Ropf=211D,ropf=1D563,roplus=2A2E,rotimes=2A35,RoundImplies=2970,rpar=29,rpargt=2994,rppolint=2A12,rrarr=21C9,Rrightarrow=21DB,'
      +'rsaquo=203A,Rscr=211B,rscr=1D4C7,Rsh=21B1,rsh=21B1,rsqb=5D,rsquo=2019,rsquor=2019,rthree=22CC,rtimes=22CA,rtri=25B9,rtrie=22B5,'
      +'rtrif=25B8,rtriltri=29CE,RuleDelayed=29F4,ruluhar=2968,rx=211E,Sacute=15A,sacute=15B,sbquo=201A,Sc=2ABC,sc=227B,scap=2AB8,'
      +'Scaron=160,scaron=161,sccue=227D,scE=2AB4,sce=2AB0,Scedil=15E,scedil=15F,Scirc=15C,scirc=15D,scnap=2ABA,scnE=2AB6,scnsim=22E9,'
      +'scpolint=2A13,scsim=227F,Scy=421,scy=441,sdot=22C5,sdotb=22A1,sdote=2A66,searhk=2925,seArr=21D8,searr=2198,searrow=2198,sect=A7,'
      +'semi=3B,seswar=2929,setminus=2216,setmn=2216,sext=2736,Sfr=1D516,sfr=1D530,sfrown=2322,sharp=266F,SHCHcy=429,shchcy=449,SHcy=428,'
      +'shcy=448,ShortDownArrow=2193,ShortLeftArrow=2190,shortmid=2223,shortparallel=2225,ShortRightArrow=2192,ShortUpArrow=2191,'
      +'shy=AD,Sigma=3A3,sigma=3C3,sigmaf=3C2,sigmav=3C2,sim=223C,simdot=2A6A,sime=2243,simeq=2243,simg=2A9E,simgE=2AA0,siml=2A9D,'
      +'simlE=2A9F,simne=2246,simplus=2A24,simrarr=2972,slarr=2190,SmallCircle=2218,smallsetminus=2216,smashp=2A33,smeparsl=29E4,'
      +'smid=2223,smile=2323,smt=2AAA,smte=2AAC,smtes=2AAC,SOFTcy=42C,softcy=44C,sol=2F,solb=29C4,solbar=233F,Sopf=1D54A,sopf=1D564,'
      +'spades=2660,spadesuit=2660,spar=2225,sqcap=2293,sqcaps=2293,sqcup=2294,sqcups=2294,Sqrt=221A,sqsub=228F,sqsube=2291,sqsubset=228F,'
      +'sqsubseteq=2291,sqsup=2290,sqsupe=2292,sqsupset=2290,sqsupseteq=2292,squ=25A1,Square=25A1,square=25A1,SquareIntersection=2293,'
      +'SquareSubset=228F,SquareSubsetEqual=2291,SquareSuperset=2290,SquareSupersetEqual=2292,SquareUnion=2294,squarf=25AA,squf=25AA,'
      +'srarr=2192,Sscr=1D4AE,sscr=1D4C8,ssetmn=2216,ssmile=2323,sstarf=22C6,Star=22C6,star=2606,starf=2605,straightepsilon=3F5,straightphi=3D5,'
      +'strns=AF,Sub=22D0,sub=2282,subdot=2ABD,subE=2AC5,sube=2286,subedot=2AC3,submult=2AC1,subnE=2ACB,subne=228A,subplus=2ABF,subrarr=2979,'
      +'Subset=22D0,subset=2282,subseteq=2286,subseteqq=2AC5,SubsetEqual=2286,subsetneq=228A,subsetneqq=2ACB,subsim=2AC7,subsub=2AD5,'
      +'subsup=2AD3,succ=227B,succapprox=2AB8,succcurlyeq=227D,Succeeds=227B,SucceedsEqual=2AB0,SucceedsSlantEqual=227D,SucceedsTilde=227F,'
      +'succeq=2AB0,succnapprox=2ABA,succneqq=2AB6,succnsim=22E9,succsim=227F,SuchThat=220B,Sum=2211,sum=2211,sung=266A,Sup=22D1,'
      +'sup=2283,sup1=B9,sup2=B2,sup3=B3,supdot=2ABE,supdsub=2AD8,supE=2AC6,supe=2287,supedot=2AC4,Superset=2283,SupersetEqual=2287,'
      +'suphsol=27C9,suphsub=2AD7,suplarr=297B,supmult=2AC2,supnE=2ACC,supne=228B,supplus=2AC0,Supset=22D1,supset=2283,supseteq=2287,'
      +'supseteqq=2AC6,supsetneq=228B,supsetneqq=2ACC,supsim=2AC8,supsub=2AD4,supsup=2AD6,swarhk=2926,swArr=21D9,swarr=2199,swarrow=2199,'
      +'swnwar=292A,szlig=DF,Tab=9,target=2316,Tau=3A4,tau=3C4,tbrk=23B4,Tcaron=164,tcaron=165,Tcedil=162,tcedil=163,Tcy=422,tcy=442,'
      +'tdot=20DB,telrec=2315,Tfr=1D517,tfr=1D531,there4=2234,Therefore=2234,therefore=2234,Theta=398,theta=3B8,thetasym=3D1,thetav=3D1,'
      +'thickapprox=2248,thicksim=223C,ThickSpace=205F,thinsp=2009,ThinSpace=2009,thkap=2248,thksim=223C,THORN=DE,thorn=FE,Tilde=223C,'
      +'tilde=2DC,TildeEqual=2243,TildeFullEqual=2245,TildeTilde=2248,times=D7,timesb=22A0,timesbar=2A31,timesd=2A30,tint=222D,toea=2928,'
      +'top=22A4,topbot=2336,topcir=2AF1,Topf=1D54B,topf=1D565,topfork=2ADA,tosa=2929,tprime=2034,TRADE=2122,trade=2122,triangle=25B5,'
      +'triangledown=25BF,triangleleft=25C3,trianglelefteq=22B4,triangleq=225C,triangleright=25B9,trianglerighteq=22B5,tridot=25EC,'
      +'trie=225C,triminus=2A3A,TripleDot=20DB,triplus=2A39,trisb=29CD,tritime=2A3B,trpezium=23E2,Tscr=1D4AF,tscr=1D4C9,TScy=426,'
      +'tscy=446,TSHcy=40B,tshcy=45B,Tstrok=166,tstrok=167,twixt=226C,twoheadleftarrow=219E,twoheadrightarrow=21A0,Uacute=DA,uacute=FA,'
      +'Uarr=219F,uArr=21D1,uarr=2191,Uarrocir=2949,Ubrcy=40E,ubrcy=45E,Ubreve=16C,ubreve=16D,Ucirc=DB,ucirc=FB,Ucy=423,ucy=443,udarr=21C5,'
      +'Udblac=170,udblac=171,udhar=296E,ufisht=297E,Ufr=1D518,ufr=1D532,Ugrave=D9,ugrave=F9,uHar=2963,uharl=21BF,uharr=21BE,uhblk=2580,'
      +'ulcorn=231C,ulcorner=231C,ulcrop=230F,ultri=25F8,Umacr=16A,umacr=16B,uml=A8,UnderBar=5F,UnderBrace=23DF,UnderBracket=23B5,'
      +'UnderParenthesis=23DD,Union=22C3,UnionPlus=228E,Uogon=172,uogon=173,Uopf=1D54C,uopf=1D566,UpArrow=2191,Uparrow=21D1,uparrow=2191,'
      +'UpArrowBar=2912,UpArrowDownArrow=21C5,UpDownArrow=2195,Updownarrow=21D5,updownarrow=2195,UpEquilibrium=296E,upharpoonleft=21BF,'
      +'upharpoonright=21BE,uplus=228E,UpperLeftArrow=2196,UpperRightArrow=2197,Upsi=3D2,upsi=3C5,upsih=3D2,Upsilon=3A5,upsilon=3C5,'
      +'UpTee=22A5,UpTeeArrow=21A5,upuparrows=21C8,urcorn=231D,urcorner=231D,urcrop=230E,Uring=16E,uring=16F,urtri=25F9,Uscr=1D4B0,'
      +'uscr=1D4CA,utdot=22F0,Utilde=168,utilde=169,utri=25B5,utrif=25B4,uuarr=21C8,Uuml=DC,uuml=FC,uwangle=29A7,vangrt=299C,varepsilon=3F5,'
      +'varkappa=3F0,varnothing=2205,varphi=3D5,varpi=3D6,varpropto=221D,vArr=21D5,varr=2195,varrho=3F1,varsigma=3C2,varsubsetneq=228A,'
      +'varsubsetneqq=2ACB,varsupsetneq=228B,varsupsetneqq=2ACC,vartheta=3D1,vartriangleleft=22B2,vartriangleright=22B3,Vbar=2AEB,'
      +'vBar=2AE8,vBarv=2AE9,Vcy=412,vcy=432,VDash=22AB,Vdash=22A9,vDash=22A8,vdash=22A2,Vdashl=2AE6,Vee=22C1,vee=2228,veebar=22BB,'
      +'veeeq=225A,vellip=22EE,Verbar=2016,verbar=7C,Vert=2016,vert=7C,VerticalBar=2223,VerticalLine=7C,VerticalSeparator=2758,VerticalTilde=2240,'
      +'VeryThinSpace=200A,Vfr=1D519,vfr=1D533,vltri=22B2,vnsub=2282,vnsup=2283,Vopf=1D54D,vopf=1D567,vprop=221D,vrtri=22B3,Vscr=1D4B1,'
      +'vscr=1D4CB,vsubnE=2ACB,vsubne=228A,vsupnE=2ACC,vsupne=228B,Vvdash=22AA,vzigzag=299A,Wcirc=174,wcirc=175,wedbar=2A5F,Wedge=22C0,'
      +'wedge=2227,wedgeq=2259,weierp=2118,Wfr=1D51A,wfr=1D534,Wopf=1D54E,wopf=1D568,wp=2118,wr=2240,wreath=2240,Wscr=1D4B2,wscr=1D4CC,'
      +'xcap=22C2,xcirc=25EF,xcup=22C3,xdtri=25BD,Xfr=1D51B,xfr=1D535,xhArr=27FA,xharr=27F7,Xi=39E,xi=3BE,xlArr=27F8,xlarr=27F5,xmap=27FC,'
      +'xnis=22FB,xodot=2A00,Xopf=1D54F,xopf=1D569,xoplus=2A01,xotime=2A02,xrArr=27F9,xrarr=27F6,Xscr=1D4B3,xscr=1D4CD,xsqcup=2A06,'
      +'xuplus=2A04,xutri=25B3,xvee=22C1,xwedge=22C0,Yacute=DD,yacute=FD,YAcy=42F,yacy=44F,Ycirc=176,ycirc=177,Ycy=42B,ycy=44B,yen=A5,'
      +'Yfr=1D51C,yfr=1D536,YIcy=407,yicy=457,Yopf=1D550,yopf=1D56A,Yscr=1D4B4,yscr=1D4CE,YUcy=42E,yucy=44E,Yuml=178,yuml=FF,Zacute=179,'
      +'zacute=17A,Zcaron=17D,zcaron=17E,Zcy=417,zcy=437,Zdot=17B,zdot=17C,zeetrf=2128,ZeroWidthSpace=200B,Zeta=396,zeta=3B6,Zfr=2128';

type
   TNamedEntity = record
      Name : String;
      Code : Integer;
   end;

   TNamedEntities = class(TSimpleHash<TNamedEntity>)
      function SameItem(const item1, item2 : TNamedEntity) : Boolean; override;
      function GetItemHashCode(const item1 : TNamedEntity) : Cardinal; override;
   end;

function TNamedEntities.SameItem(const item1, item2 : TNamedEntity) : Boolean;
begin
   Result := (item1.Name = item2.Name);
end;

function TNamedEntities.GetItemHashCode(const item1 : TNamedEntity) : Cardinal;
begin
   Result := SimpleStringHash(item1.Name);
end;

var
   vAllNamedEntities : TNamedEntities;

// PrepareAllNamedEntities
//
procedure PrepareAllNamedEntities;
var
   i, p, e : Integer;
   s : String;
   entity : TNamedEntity;
begin
   vAllNamedEntities := TNamedEntities.Create;
   vAllNamedEntities.PreallocateCapacity(4096);

   s := cAllNamedEntities;
   i := 1;
   repeat
      p := Pos(',', s, i);
      if p <= 0 then
         p := Length(s)+1;
      e := Pos('=', s, i);
      entity.Name := Copy(s, i, e-i);
      entity.Code := StrToInt('$' + Copy(s, e+1, p-e-1));
      vAllNamedEntities.Add(entity);
      i := p + 1;
   until i >= Length(s);
end;

// ------------------
// ------------------ WebUtils ------------------
// ------------------

// ParseURLEncoded
//
class procedure WebUtils.ParseURLEncoded(const data : RawByteString; dest : TStrings);
var
   base, next, last : Integer;
begin
   last:=Length(data);
   base:=1;
   while True do begin
      next:=base;
      repeat
         if next>last then begin
            next:=-1;
            break;
         end else case data[next] of
            '&', ';' :
               break
         else
            Inc(next);
         end;
      until False;
      if next > base then begin
         dest.Add(DecodeURLEncoded(data, base, next-base));
         base := next + 1;
      end else if next > 0 then begin
         base := next + 1;
      end else begin
         if base<Length(data) then
            dest.Add(DecodeURLEncoded(data, base));
         Break;
      end;
   end;
end;

// DecodeURLEncoded
//
class function WebUtils.DecodeURLEncoded(const src : RawByteString; start, count : Integer) : String;
var
   raw : UTF8String;
   pSrc, pDest : PAnsiChar;
   c : AnsiChar;
begin
   if count = 0 then Exit;
   SetLength(raw, count);
   pSrc:=@src[start];
   pDest:=PAnsiChar(Pointer(raw));
   while count>0 do begin
      Dec(count);
      c:=AnsiChar(pSrc^);
      case c of
         '+' :
            pDest^:=' ';
         '%' : begin
            if count<2 then break;
            pDest^:=AnsiChar(DecodeHex2(@pSrc[1]));
            Inc(pSrc, 2);
            Dec(count, 2);
         end;
      else
         pDest^:=c;
      end;
      Inc(pDest);
      Inc(pSrc);
   end;
   SetLength(raw, NativeUInt(pDest)-NativeUInt(Pointer(raw)));
   Result := UTF8ToUnicodeString(raw);
end;

// EncodeURLEncoded
//
class function WebUtils.EncodeURLEncoded(const src : String) : String;
var
   raw : UTF8String;
   pSrc : PAnsiChar;
   pDest : PChar;
begin
   if src='' then Exit('');

   raw := UTF8Encode(src);
   SetLength(Result, Length(raw)*3); // worst-case all special chars

   pSrc := Pointer(raw);
   pDest := Pointer(Result);

   // we are slightly more aggressive on the special characters than strictly required
   repeat
      case pSrc^ of
         #0 : break;
         #1..'/',  '['..']', ':'..'@', #127..AnsiChar(255) : begin
            pDest[0] := '%';
            pDest[1] := cToHex[1+(Ord(pSrc^) shr 4)];
            pDest[2] := cToHex[1+(Ord(pSrc^) and 15)];
            Inc(pDest, 3);
         end;
      else
         pDest^ := Char(pSrc^);
         Inc(pDest);
      end;
      Inc(pSrc);
   until False;

   SetLength(Result, (NativeUInt(PDest)-NativeUInt(Pointer(Result))) div SizeOf(Char));
end;

// ParseMIMEHeaderValue
//
class procedure WebUtils.ParseMIMEHeaderValue(const src : RawByteString; dest : TStrings);

   procedure AddValue(p, pEnd : PAnsiChar);
   var
      buf : String;
      pBuf : PChar;
      inQuote : Boolean;
   begin
      SetLength(buf, NativeUInt(pEnd) - NativeUInt(p));
      pBuf := Pointer(buf);
      inQuote := False;
      while p < pEnd do begin
         if inQuote then begin
            case p^ of
               '"' : begin
                  inQuote := False;
               end;
               '\' : begin
                  if p < pEnd-1 then begin
                     pBuf^ := Char(p[1]);
                     Inc(pBuf);
                     Inc(p);
                  end else break;
               end;
            else
               pBuf^ := Char(p^);
               Inc(pBuf);
            end;
         end else begin
            case p^ of
               '"' : begin
                  inQuote := True;
               end;
            else
               pBuf^ := Char(p^);
               Inc(pBuf);
            end;
         end;
         Inc(p);
      end;
      SetLength(buf, (NativeUInt(pBuf) - NativeUInt(Pointer(buf))) div SizeOf(Char));
      dest.Add(buf);
   end;

var
   pSrc, pStart : PAnsiChar;
   inString, inLeadingSpaces : Boolean;
begin
   if src = '' then Exit;

   pSrc := Pointer(src);
   pStart := pSrc;
   inString := False;
   inLeadingSpaces := True;
   while pSrc^ <> #0 do begin
      if inString then begin
         if pSrc^ = '"' then
            inString := False;
         Inc(pSrc);
      end else begin
         case pSrc^ of
            ';' : begin
               AddValue(pStart, pSrc);
               Inc(pSrc);
               pStart := pSrc;
               inLeadingSpaces := True;
            end;
            '"' : begin
               inString := True;
               inLeadingSpaces := False;
               Inc(pSrc);
            end;
            ' ' : begin
               Inc(pSrc);
               if inLeadingSpaces then
                  pStart := pSrc;
            end;
         else
            inLeadingSpaces := False;
            Inc(pSrc);
         end;
      end;
   end;
   AddValue(pStart, pSrc);
end;

// ParseMultiPartFormData
//
class procedure WebUtils.ParseMultiPartFormData(const src, dashBoundary : RawByteString; var dest : TIMIMEBodyParts);
const
   cCRLFCRLF : RawByteString = #13#10#13#10;
var
   p, pBoundary, pBody : Integer;
   lenBoundary, n : Integer;
   part : TMIMEBodyPart;
begin
   if (src = '') or (dashBoundary = '') then Exit;

   lenBoundary := Length(dashBoundary);
   p := 1;
   pBoundary := PosExA(dashBoundary, src, p);
   while pBoundary > 0 do begin
      p := pBoundary + lenBoundary;
      if (src[p] <> #13) or (src[p+1] <> #10) then Exit;
      Inc(p, 2);
      pBody := PosExA(cCRLFCRLF, src, p);
      if pBody <= 0 then Exit;
      Inc(pBody, 4);
      pBoundary := PosExA(dashBoundary, src, pBody);
      if pBoundary > 0 then begin
         if (src[pBoundary-2] <> #13) or (src[pBoundary-1] <> #10) then Exit;
         part := TMIMEBodyPart.Create;
         n := Length(dest);
         SetLength(dest, n+1);
         dest[n] := part;
         part.FRawHeaders := Copy(src, p, pBody-p-4);
         part.FRawData := Copy(src, pBody, pBoundary-2-pBody);
      end;
   end;
end;

// DecodeURLEncoded
//
class function WebUtils.DecodeURLEncoded(const src : RawByteString; start : Integer) : String;
var
   n : Integer;
begin
   n := Length(src)-start+1;
   if n > 0 then
      Result:=DecodeURLEncoded(src, start, n)
   else Result:='';
end;

// DecodeHex2
//
class function WebUtils.DecodeHex2(p : PAnsiChar) : Integer;
var
   c : AnsiChar;
begin
   c:=p[0];
   case c of
      '0'..'9' : Result:=Ord(c)-Ord('0');
      'A'..'F' : Result:=Ord(c)+(10-Ord('A'));
      'a'..'f' : Result:=Ord(c)+(10-Ord('a'));
   else
      Exit(-1);
   end;
   c:=p[1];
   case c of
      '0'..'9' : Result:=(Result shl 4)+Ord(c)-Ord('0');
      'A'..'F' : Result:=(Result shl 4)+Ord(c)+(10-Ord('A'));
      'a'..'f' : Result:=(Result shl 4)+Ord(c)+(10-Ord('a'));
   else
      Exit(-1);
   end;
end;


// HasFieldName
//
class function WebUtils.HasFieldName(const list : TStrings; const name : String) : Boolean;
var
   i, n : Integer;
   elem : String;
begin
   for i:=0 to list.Count-1 do begin
      elem:=list[i];
      if StrBeginsWith(elem, name) then begin
         n:=Length(name);
         if (Length(elem)=n) or (elem[n+1]='=') then
            Exit(True);
      end;
   end;
   Result:=False;
end;

// EncodeEncodedWord
//
class function WebUtils.EncodeEncodedWord(const s : String) : String;
var
   p, n : Integer;
   line : array [0..100] of WideChar;
   buf : String;
   c : AnsiChar;

   procedure FlushLine;
   begin
      SetString(buf, PChar(@line[0]), p);
      Result:=Result+'=?utf-8?Q?'+buf+'?='#13#10#9;
   end;

begin
   Result:='';
   p:=0;
   n:=0;
   for c in UTF8Encode(s) do begin
      case Ord(c) of
         Ord(' ') : begin
            line[p]:='_';
         end;
         Ord('?'), Ord('='), 128..255 : begin
            line[p]:='=';
            line[p+1]:=cToHex[(Ord(c) shr 4)+1];
            line[p+2]:=cToHex[(Ord(c) and 15)+1];
            Inc(p, 2);
            Inc(n, 2);
         end;
      else
         line[p]:=WideChar(c);
      end;
      Inc(p);
      if n>64 then begin
         FlushLine;
         p:=0;
         n:=0;
      end else Inc(n);
   end;
   if p>0 then
      FlushLine;
   if StrEndsWith(Result, #13#10#9) then
      SetLength(Result, Length(Result)-3);
end;

const
   cRFC822Months : array[1..12] of String = (
      'Jan','Feb','Mar','Apr', 'May','Jun','Jul','Aug', 'Sep','Oct','Nov','Dec'
   );
   cRFC822Days : array[1..7] of String = (
      'Sun','Mon','Tue','Wed','Thu', 'Fri','Sat'
   );

// DateTimeToRFC822
//
class function WebUtils.DateTimeToRFC822(const dt : TdwsDateTime) : String;
begin
   Result := WebUtils.DateTimeToRFC822(dt.AsUTCDateTime);
end;

// DateTimeToRFC822
//
class function WebUtils.DateTimeToRFC822(const dt : TDateTime) : String;

   procedure Copy3(src, dest : PChar); inline;
   begin
      PCardinal(dest)^ := PCardinal(src)^;
      dest[2] := src[2];
   end;

var
   a, m, j, hh, mn, ss, ms : Word;
   dow : Word;
   aHigh, aLow : Cardinal;
   p : PChar;
begin
   DecodeDateFully(dt, a, m, j, dow);
   DecodeTime(dt, hh, mn, ss, ms);
   SetLength(Result, 29); // 'ddd, jj mmm yyyy hh:nn:ss GMT';
   p := Pointer(Result);
   Copy3(Pointer(cRFC822Days[dow]), p);
   p[3] := ',';
   p[4] := ' ';
   PTwoChars(@p[5])^ := cTwoDigits[j];
   p[7] := ' ';
   Copy3(Pointer(cRFC822Months[m]), @p[8]);
   p[11] := ' ';
   aHigh := a;
   if aHigh > 9999 then aHigh := 999;
   aLow := DivMod100(aHigh);
   PTwoChars(@p[12])^ := cTwoDigits[aHigh];
   PTwoChars(@p[14])^ := cTwoDigits[aLow];
   p[16] := ' ';
   PTwoChars(@p[17])^ := cTwoDigits[hh];
   p[19] := ':';
   PTwoChars(@p[20])^ := cTwoDigits[mn];
   p[22] := ':';
   PTwoChars(@p[23])^ := cTwoDigits[ss];
   p[25] := ' ';
   p[26] := 'G';
   p[27] := 'M';
   p[28] := 'T';
end;

// RFC822ToDateTime
//
class function WebUtils.RFC822ToDateTime(const str : String) : TDateTime;
const
   cMaxItems = 6;
var
   list : array [0..cMaxItems+1] of String;
   count : Integer;
   y, mo, d : Word;
   h, mi, s : Word;
   deltaHours, deltaDays, p : Integer;
   deltaTime : TDateTime;

   procedure SplitStr(const str : String; const delim : WideChar; start : Integer);
   var
      lookup : integer;
   begin
      count:=0;
      if str='' then Exit;
      lookup:=start;
      while lookup<=Length(str) do begin
         if str[lookup]=delim then begin
            if lookup>start then begin
               list[count]:=Copy(str, start, lookup-start);
               Inc(count);
               if count>=cMaxItems then break;
            end;
            start:=lookup+1;
         end;
         Inc(lookup);
      end;
      if lookup>start then begin
         list[count]:=Copy(str, start, lookup-start);
         Inc(count);
      end;
   end;

   function ParseTwoDigits(p : PChar; offset : Integer) : Integer; inline;
   begin
      Result:=Ord(p[offset])*10+Ord(p[offset+1])-11*Ord('0')
   end;

   function ParseFourDigits(p : PChar; offset : Integer) : Integer; inline;
   begin
      Result:=ParseTwoDigits(p, 0)*100+ParseTwoDigits(p, 2);
   end;

   procedure ParseHMS(const str : String);
   var
      p : PChar;
   begin
      p:=PChar(Pointer(str));
      h:=65535;
      case Length(str) of
         5 : begin // hh:nn
            if p[2]<>':' then exit;
            h:=ParseTwoDigits(p, 0);
            mi:=ParseTwoDigits(p, 3);
            s:=0;
         end;
         8 : begin // hh:nn:ss
            if p[2]<>':' then exit;
            if p[5]<>':' then exit;
            h:=ParseTwoDigits(p, 0);
            mi:=ParseTwoDigits(p, 3);
            s:=ParseTwoDigits(p, 6);
         end;
      end;
   end;

   procedure ParseYear(const str : String);
   begin
      case Length(str) of
         2 : y:=ParseTwoDigits(Pointer(str), 0)+2000;
         4 : y:=ParseFourDigits(Pointer(str), 0);
      else
         y:=65535;
      end;
   end;

   procedure ParseMonth(const str : String);
   begin
      mo:=1;
      while (mo<=12) and not SameText(str, cRFC822Months[mo]) do
         Inc(mo);
   end;

begin
   Result:=0;
   if str='' then Exit;

   p:=Pos(',', str);
   if p>0 then
      SplitStr(str, ' ', p+1)
   else SplitStr(str, ' ', 1);
   if count<5 then // invalid date
      Exit;
   if (count > 5) and (Pos(':', list[4])>0) and StrBeginsWith(list[5], 'GMT+') then begin
      // Fri Feb 11 2022 10:25:55 GMT+0100 (Central European Standard Time)
      ParseMonth(list[1]);
      if Length(list[2])=2 then
         d:=ParseTwoDigits(Pointer(list[2]), 0)
      else d:=0;
      ParseYear(list[3]);
      ParseHMS(list[4]);
      deltaHours := StrToIntDef(Copy(list[5], 4), 0);
   end else begin
      // Thu, 08 Oct 2009 00:00:00 GMT
      if Length(list[0])=2 then
         d:=ParseTwoDigits(Pointer(list[0]), 0)
      else d:=0;
      ParseMonth(list[1]);
      ParseYear(list[2]);
      ParseHMS(list[3]);
      deltaHours:=StrToIntDef(list[4], 0);
   end;
   deltaDays:=0;
   while h>=24 do begin
      Dec(h, 24);
      Inc(deltaDays);
   end;
   if not TryEncodeDate(y, mo, d, Result) then
      Result:=0
   else if TryEncodeTime(h, mi, s, 0, deltaTime) then
      Result:=Result+deltaTime-deltaHours*(1/100/24)+deltaDays
   else Result:=0;
end;

// HTMLTextEncode
//
class function WebUtils.HTMLTextEncode(const s : UnicodeString) : UnicodeString;
var
   capacity : Integer;
   pSrc, pDest : PWideChar;

   procedure Grow;
   var
      nr, dnr : Integer;
      k : NativeUInt;
   begin
      k := NativeUInt(pDest)-NativeUInt(Pointer(Result));
      nr := Length(Result);
      dnr := (nr div 4) + 8;
      SetLength(Result, nr + dnr);
      Inc(capacity, dnr);
      pDest := Pointer(NativeUInt(Pointer(Result))+k);
   end;

   procedure Append(const a : UnicodeString);
   var
      n : Integer;
   begin
      n := Length(a);
      if n>capacity then Grow;
      System.Move(Pointer(a)^, pDest^, n*SizeOf(WideChar));
      Inc(pDest, n);
      Dec(capacity, n);
   end;

begin
   if s='' then exit;
   capacity:=Length(s);
   SetLength(Result, capacity);
   pSrc:=Pointer(s);
   pDest:=Pointer(Result);
   repeat
      case pSrc^ of
         #0 : break;
         '<' : Append('&lt;');
         '>' : Append('&gt;');
         '&' : Append('&amp;');
         '"' : Append('&quot;');
         '''' : Append('&#39;');
         #$00A0 : Append('&nbsp;');
      else
         if capacity=0 then
            Grow;
         pDest^ := pSrc^;
         Inc(pDest);
         Dec(capacity);
      end;
      Inc(pSrc);
   until False;
   if capacity>0 then
      SetLength(Result, Length(Result)-capacity);
end;

// HTMLTextDecode
//
class function WebUtils.HTMLTextDecode(const s : UnicodeString) : UnicodeString;
type
   TDecoderState = ( dsText, dsTag, dsSingleQuote, dsDoubleQuote, dsCharacter );
var
   pSrc, pDest, pAmp : PWideChar;
   c : WideChar;
   state : TDecoderState;
begin
   if s='' then exit;
   SetLength(Result, Length(s));
   state:=dsText;
   pAmp:=nil;
   pSrc:=Pointer(s);
   pDest:=Pointer(Result);
   repeat
      c := pSrc^;
      if c = #0 then break;
      case state of
         dsText : begin
            case c of
               '<' : state := dsTag;
               '&' : begin
                  state := dsCharacter;
                  pAmp := pSrc;
               end;
            else
               pDest^ := c;
               Inc(pDest);
            end;
         end;
         dsTag : begin
            case c of
               '>' : state := dsText;
               '''' : state := dsSingleQuote;
               '"' : state := dsDoubleQuote;
            end;
         end;
         dsSingleQuote : begin
            case c of
               '''' : state := dsTag;
            end;
         end;
         dsDoubleQuote : begin
            case c of
               '"' : state := dsTag;
            end;
         end;
         dsCharacter : begin
            case c of
               '0'..'9', 'a'..'z', 'A'..'Z', '#' : ; // alowed characters
            else
               if c = ';' then
                  c := HTMLCharacterDecode(pAmp)
               else c := #0;
               if c = #0 then begin
                  // broken entity, pass as is
                  while pAmp <> pSrc do begin
                     pDest^ := pAmp^;
                     Inc(pDest);
                     Inc(pAmp);
                  end;
                  pDest^ := pAmp^;
                  Inc(pDest);
                  pAmp := nil;
               end else begin
                  pDest^ := c;
                  Inc(pDest);
               end;
               state := dsText;
            end;
         end;
      else
         Assert(False);
      end;
      Inc(pSrc);
   until False;

   SetLength(Result, (NativeUInt(pDest)-NativeUInt(Pointer(Result))) div SizeOf(WideChar));
end;

// HTMLCharacterDecode
//
class function WebUtils.HTMLCharacterDecode(p : PWideChar) : WideChar;

   function AsString(p : PWideChar) : UnicodeString;
   var
      n : Integer;
   begin
      n:=1;
      while True do begin
         case p[n] of
            #0, ';' : break;
         else
            Inc(n);
         end;
      end;
      SetString(Result, p, n);
   end;

   function DecodeNumeric(p : PWideChar) : WideChar;
   begin
      Result:=WideChar(StrToIntDef(AsString(p), 0)); // UCS-2 only !
   end;

   function Check(p : PWideChar; const aBegin : UnicodeString) : Boolean; overload;
   var
      n : Integer;
   begin
      n:=Length(aBegin);
      Result:=CompareMem(p, Pointer(aBegin), n*SizeOf(WideChar));
      if Result then
         Result:=(p[n]=';');
   end;

   function TryAllEntities(p : PWideChar) : WideChar;
   var
      entity : TNamedEntity;
   begin
      entity.Name := String(AsString(p));
      if vAllNamedEntities.Match(entity) then begin
         Result := WideChar(entity.Code);  // UCS-2 only !
      end else Result:=#0;
   end;

begin
   Assert(p[0]='&');
   Result:=#0;
   // special case handling for common entities
   Inc(p);
   case p^ of
      'a' : begin
         if Check(p, 'amp') then Exit('&')
         else if Check(p, 'apos') then Exit('''');
      end;
      'g' : begin
         if Check(p, 'gt') then Exit('>');
      end;
      'l' : begin
         if Check(p, 'lt') then Exit('<');
      end;
      'n' : begin
         if Check(p, 'nbsp') then Exit(#$00A0);
      end;
      'q' : begin
         if Check(p, 'quot') then Exit('"');
      end;
      '#' : Exit(DecodeNumeric(@p[1]));
   end;
   if Result=#0 then
      Result:=TryAllEntities(p);
end;

// HTMLAttributeEncode
//
class function WebUtils.HTMLAttributeEncode(const s : UnicodeString) : UnicodeString;
// as per OWASP XSS Rule#2
var
   capacity : Integer;
   pSrc, pDest : PWideChar;

   procedure Grow;
   var
      nr, dnr : Integer;
      k : NativeUInt;
   begin
      k := NativeUInt(pDest)-NativeUInt(Pointer(Result));
      nr := Length(Result);
      dnr := (nr div 4) + 8;
      SetLength(Result, nr + dnr);
      Inc(capacity, dnr);
      pDest := Pointer(NativeUInt(Pointer(Result))+k);
   end;

   procedure Append(c : Byte); overload;
   begin
      if capacity < 6 then Grow;
      pDest[0] := '&';
      pDest[1] := '#';
      if c in [10..99] then begin
         pDest[2] := WideChar( Ord('0')+(c div 10) );
         pDest[3] := WideChar( Ord('0')+(c mod 10) );
         pDest[4] := ';';
         Inc(pDest, 5);
         Dec(capacity, 5);
      end else begin
         pDest[2] := 'x';
         pDest[3] := cToHex[((c shr 4) and $F)+1];
         pDest[4] := cToHex[(c and $F)+1];
         pDest[5] := ';';
         Inc(pDest, 6);
         Dec(capacity, 6);
      end;
   end;

begin
   if s='' then exit;
   capacity:=Length(s);
   SetLength(Result, capacity);
   pSrc  := Pointer(s);
   pDest := Pointer(Result);
   repeat
      case pSrc^ of
         #0 : break;
         'A'..'Z', 'a'..'z', '0'..'9', #256..#$FFFF : begin
            if capacity=0 then
               Grow;
            pDest^ := pSrc^;
            Inc(pDest);
            Dec(capacity);
         end;
      else
         Append(Ord(pSrc^));
      end;
      Inc(pSrc);
   until False;
   if capacity>0 then
      SetLength(Result, Length(Result)-capacity);
end;

// HTMLAttributeDecode
//
class function WebUtils.HTMLAttributeDecode(const s : UnicodeString) : UnicodeString;
begin
   Result:=WebUtils.HTMLTextDecode(s);
end;

// CSSTextEncode
//
class function WebUtils.CSSTextEncode(const s : UnicodeString) : UnicodeString;
var
   pSrc, pDest : PWideChar;
begin
   if s = '' then Exit;
   SetLength(Result, Length(s)*2); // worst case all characters escaped
   pSrc  := Pointer(s);
   pDest := Pointer(Result);
   repeat
      case pSrc^ of
         #0 : break;
         'A'..'Z', 'a'..'z', '0'..'9', #256..#$FFFF : begin
            pDest^ := pSrc^;
            Inc(pDest);
         end;
      else
         pDest[0] := '\';
         pDest[1] := pSrc^;
         Inc(pDest, 2);
      end;
      Inc(pSrc);
   until False;
   SetLength(Result, (NativeUInt(pDest)-NativeUInt(Pointer(Result))) div SizeOf(Char));
end;

// XMLTextEncode
//
class function WebUtils.XMLTextEncode(const s : UnicodeString; unsupportedXML10CharactersMode : Integer = 0) : UnicodeString;
var
   capacity : Integer;
   pSrc, pDest : PWideChar;

   procedure Grow;
   var
      nr, dnr : Integer;
      k : NativeUInt;
   begin
      k := NativeUInt(pDest)-NativeUInt(Pointer(Result));
      nr := Length(Result);
      dnr := (nr div 4) + 8;
      SetLength(Result, nr + dnr);
      Inc(capacity, dnr);
      pDest := Pointer(NativeUInt(Pointer(Result))+k);
   end;

   procedure Append(const a : UnicodeString);
   var
      n : Integer;
   begin
      n := Length(a);
      if n>capacity then Grow;
      System.Move(Pointer(a)^, pDest^, n*SizeOf(WideChar));
      Inc(pDest, n);
      Dec(capacity, n);
   end;

   procedure AppendEncoded(const c : Char);
   begin
      if capacity < 5 then Grow;
      pDest[0] := '&';
      pDest[1] := '#';
      pDest[2] := Char(Ord('0') + Ord(c) div 10);
      pDest[3] := Char(Ord('0') + Ord(c) mod 10);
      pDest[4] := ';';
      Inc(pDest, 5);
      Dec(capacity, 5);
   end;

begin
   if s='' then exit;
   capacity:=Length(s);
   SetLength(Result, capacity);
   pSrc:=Pointer(s);
   pDest:=Pointer(Result);
   repeat
      case pSrc^ of
         #0 : break;
         #1..#8, #11..#12, #14..#31 :
            case unsupportedXML10CharactersMode of
               0 : ; // ignore
               1 : AppendEncoded(pSrc^);
            else
               raise EXMLEncodeError.CreateFmt('Unsupported character #%.2d', [ Ord(pSrc^) ]);
            end;
         '<' : Append('&lt;');
         '>' : Append('&gt;');
         '&' : Append('&amp;');
         '"' : Append('&quot;');
         '''' : Append('&apos;');
      else
         if capacity=0 then
            Grow;
         pDest^ := pSrc^;
         Inc(pDest);
         Dec(capacity);
      end;
      Inc(pSrc);
   until False;
   if capacity>0 then
      SetLength(Result, Length(Result)-capacity);
end;

// XMLTextDecode
//
class function WebUtils.XMLTextDecode(const s : UnicodeString) : UnicodeString;
var
   pSrc, pDest, pAmp : PWideChar;
   c : WideChar;
begin
   if s = '' then exit;
   SetLength(Result, Length(s));
   pAmp := nil;
   pSrc := Pointer(s);
   pDest := Pointer(Result);
   repeat
      c := pSrc^;
      if c = #0 then break;
      if pAmp = nil then begin
         case c of
            '&' :
               pAmp := pSrc;
         else
            pDest^ := c;
            Inc(pDest);
         end;
      end else begin
         case c of
            'a'..'z' : ; // alowed characters
         else
            if c = ';' then begin
               c := #0;
               case pAmp[1] of
                  'a' : case pAmp[2] of
                     'm' :
                        if (pAmp[3] = 'p') and (pAmp[4] = ';') then
                           c := '&';
                     'p' :
                        if (pAmp[3] = 'o') and (pAmp[4] = 's') and (pAmp[5] = ';') then
                           c := '''';
                  end;
                  'g' :
                     if (pAmp[2] = 't') and (pAmp[3] = ';') then
                        c := '>';
                  'l' :
                     if (pAmp[2] = 't') and (pAmp[3] = ';') then
                        c := '<';
                  'q' :
                     if (pAmp[2] = 'u') and (pAmp[3] = 'o') and (pAmp[4] = 't') and (pAmp[5] = ';') then
                        c := '"';
               end;
            end else c := #0;
            if c = #0 then
               raise EXMLDecodeError.CreateFmt('Invalid entity at position %d',
                                               [1 + (NativeUInt(pAmp)-NativeUInt(Pointer(s))) div 2]);
            pDest^ := c;
            Inc(pDest);
            pAmp := nil;
         end;
      end;
      Inc(pSrc);
   until False;

   SetLength(Result, (NativeUInt(pDest)-NativeUInt(Pointer(Result))) div SizeOf(WideChar));
end;

// IsValidCookieName
//
class function WebUtils.IsValidCookieName(const s : String) : Boolean;
var
   p : PChar;
   i : Integer;
begin
   if s = '' then Exit(False);
   // check for RFC 6265 set
   p := PChar(s);
   for i := 0 to Length(s)-1 do begin
      case p[i] of
         #00..' ', #$007F..#$FFFF,
         '(', ')', '<', '>', '@', ',', ';', ':', '\', '"',
         '/', '[', ']', '?', '=', '{', '}' : Exit(False);
      end;
   end;
   Result := True;
end;

// IsValidCookieValue
//
class function WebUtils.IsValidCookieValue(const s : String) : Boolean;
var
   p : PChar;
   i : Integer;
begin
   // check for RFC 6265 set
   p := PChar(s);
   for i := 0 to Length(s)-1 do begin
      case Ord(p[i]) of
         $21, $23..$2B, $2D..$3A, $3C..$5B, $5D..$7E : ;
      else
         Exit(False);
      end;
   end;
   Result := True;
end;

// ------------------
// ------------------ TMIMEBodyPart ------------------
// ------------------

// Destroy
//
destructor TMIMEBodyPart.Destroy;
begin
   FHeaders.Free;
   FContentDisposition.Free;
   inherited;
end;

// GetSelf
//
function TMIMEBodyPart.GetSelf : TMIMEBodyPart;
begin
   Result := Self;
end;

// GetRawHeaders
//
function TMIMEBodyPart.GetRawHeaders : RawByteString;
begin
   Result := FRawHeaders;
end;

// GetRawData
//
function TMIMEBodyPart.GetRawData : RawByteString;
begin
   Result := FRawData;
end;

// PrepareHeaders
//
procedure TMIMEBodyPart.PrepareHeaders;
var
   i, p : Integer;
   buf : String;
begin
   Assert(FHeaders = nil);
   FHeaders := TFastCompareTextList.Create;
   FHeaders.Text := RawByteStringToScriptString(RawHeaders);
   for i := 0 to FHeaders.Count-1 do begin
      buf := FHeaders[i];
      p := Pos(':', buf);
      if p > 0 then begin
         FHeaders[i] := TrimRight(Copy(buf, 1, p-1)) + '=' + TrimLeft(Copy(buf, p+1));
      end;
   end;
end;

// GetHeaders
//
function TMIMEBodyPart.GetHeaders : TStrings;
begin
   if FHeaders = nil then
      PrepareHeaders;
   Result := FHeaders;
end;

// ContentDisposition
//
function TMIMEBodyPart.ContentDisposition : String;
begin
   Result := Headers.Values['Content-Disposition'];
end;

// ContentType
//
function TMIMEBodyPart.ContentType : String;
begin
   Result := Headers.Values['Content-Type'];
end;

// PrepareContentDisposition
//
procedure TMIMEBodyPart.PrepareContentDisposition;
begin
   Assert(FContentDisposition = nil);
   FContentDisposition := TFastCompareTextList.Create;
   WebUtils.ParseMIMEHeaderValue(ScriptStringToRawByteString(ContentDisposition), FContentDisposition);
end;

// Name
//
function TMIMEBodyPart.Name : String;

   procedure PrepareName(part : TMIMEBodyPart);
   begin
      part.PrepareContentDisposition;
      part.FName := part.FContentDisposition.Values['name'];
   end;

begin
   if FContentDisposition = nil then
      PrepareName(Self);
   Result := FName;
end;

// FileName
//
function TMIMEBodyPart.FileName : String;
begin
   if FContentDisposition = nil then
      PrepareContentDisposition;
   Result := FContentDisposition.Values['filename'];
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   PrepareAllNamedEntities;

finalization

   FreeAndNil(vAllNamedEntities);

end.
