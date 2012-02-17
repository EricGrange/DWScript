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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsJSRTL;

interface

uses Classes, SysUtils, dwsUtils, dwsJSCodeGen, dwsCodeGen, dwsExprs,
   dwsSymbols, dwsMagicExprs, dwsCoreExprs;

type
   TJSRTLDependency = record
      Name, Code, Dependency : String;
   end;
   PJSRTLDependency = ^TJSRTLDependency;

   TJSMagicFuncExpr = class (TJSFuncBaseExpr)
      private
         FMagicCodeGens : TStringList;
      public
         constructor Create;
         destructor Destroy; override;
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
         procedure CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol); override;
   end;

   TJSFloatToStrExpr = class (TJSFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSFormatExpr = class (TJSFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

function FindJSRTLDependency(const name : String) : PJSRTLDependency;
function All_RTL_JS : String;
procedure IgnoreJSRTLDependencies(dependencies : TStrings);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R dwsJSRTL.res dwsJSRTL.rc}

const
   cJSRTLDependencies : array [1..145] of TJSRTLDependency = (
      // codegen utility functions
      (Name : '$CheckStep';
       Code : 'function $CheckStep(s,z) { if (s>0) return s; throw Exception.Create$1($New(Exception),"FOR loop STEP should be strictly positive: "+s.toString()+z); }';
       Dependency : 'Exception' ),
      (Name : '$New';
       Code : 'function $New(c) { var i={ClassType:c}; c.$Init(i); return i }'),
      (Name : '$NewDyn';
       Code : 'function $NewDyn(c,z) {'#13#10
              +#9'if (c==null) throw Exception.Create$1($New(Exception),"ClassType is nil"+z);'#13#10
              +#9'var i={ClassType:c};'#13#10
              +#9'c.$Init(i);'#13#10
              +#9'return i'#13#10
              +'}';
       Dependency : 'Exception' ),
      (Name : '$NewArrayFn';
       Code : 'function $NewArrayFn(n,d) { var r=new Array(n); for(var i=0;i<n;i++) r[i]=d(); return r }'),
      (Name : '$NewArray';
       Code : 'function $NewArray(n,v) { var r=new Array(n); for(var i=0;i<n;i++) r[i]=v; return r }'),
      (Name : '$ArrayInsert';
       Code : 'function $ArrayInsert(a,i,v,z) {'#13#10
              +#9'if (i==a.length) a.push(v); else a.splice($Idx(i,0,a.length-1,z),0,v)'#13#10
              +'}';
       Dependency : '$Idx' ),
      (Name : '$ArraySetLength';
       Code : 'function $ArraySetLength(a,n,d) {'#13#10
              +#9'var o=a.length;'#13#10
              +#9'if (o==n) return;'#13#10
              +#9'if (o>n) { a.length=n; return };'#13#10
              +#9'for (;o<n;o++) a.push(d());'#13#10
              +'}'),
      (Name : '$ArrayCopy';
       Code : 'function $ArrayCopy(a,i,z) { return a.slice($Idx(i,0,a.length-1,z)) }';
       Dependency : '$Idx' ),
      (Name : '$ArrayCopyLen';
       Code : 'function $ArrayCopyLen(a,i,l,z) {'#13#10
              +#9'if (l<0) throw Exception.Create$1($New(Exception),"Positive count expected (got "+l.toString()+")"+z);'#13#10
              +#9'return a.slice($Idx(i,0,a.length-1,z),$Idx(i+l-1,0,a.length-1,z)-i+1,z)'#13#10
              +'}';
       Dependency : '$Idx' ),
      (Name : '$ArraySwap';
       Code : 'function $ArraySwap(a,i1,i2) { var t=a[i1]; a[i1]=a[i2]; a[i2]=t }' ),
      (Name : '$ArraySwapChk';
       Code : 'function $ArraySwapChk(a,i1,i2,z) {'#13#10
              +#9'var n=a.length-1, t=a[$Idx(i1,0,n,z)];'#13#10
              +#9'a[i1]=a[$Idx(i2,0,n,z)]'#13#10
              +#9'a[i2]=t;'#13#10
              +'}';
       Dependency : '$Idx' ),
      (Name : '$Check';
       Code : 'function $Check(i,z) { if (i) return i; throw Exception.Create$1($New(Exception),"Object not instantiated"+z); }'),
      (Name : '$CheckIntf';
       Code : 'function $CheckIntf(i,z) { if (i) return i; throw Exception.Create$1($New(Exception),"Interface is nil"+z); }'),
      (Name : '$CheckFunc';
       Code : 'function $CheckFunc(i,z) { if (i) return i; throw Exception.Create$1($New(Exception),"Function pointer is nil"+z); }'),
      (Name : '$Assert';
       Code : 'function $Assert(b,m,z) { if (!b) throw Exception.Create$1($New(EAssertionFailed),"Assertion failed"+z+((m=="")?"":" : ")+m); }';
       Dependency : 'EAssertionFailed' ),
      (Name : '$CondFailed';
       Code : 'function $CondFailed(z,m) { throw Exception.Create$1($New(EAssertionFailed),z+m); }';
       Dependency : 'EAssertionFailed' ),
      (Name : '$Inh';
       Code : 'function $Inh(s,c) {'#13#10
               +#9'if (s===null) return false;'#13#10
               +#9'while ((s)&&(s!==c)) s=s.$Parent;'#13#10
               +#9'return (s)?true:false;'#13#10
               +'}'#13#10 ),
      (Name : '$Is';
       Code : 'function $Is(o,c) {'#13#10
               +#9'if (o===null) return false;'#13#10
               +#9'return $Inh(o.ClassType,c);'#13#10
               +'}'#13#10;
       Dependency : '$Inh' ),
      (Name : '$As';
       Code : 'function $As(o,c) {'#13#10
               +#9'if ((o!==null)&&$Is(o,c)) return o;'#13#10
               +#9'throw Exception.Create$1($New(Exception),"Can''t cast instance of type \""+o.ClassType.$ClassName+"\" to class \""+c.$ClassName+"\"");'#13#10
               +'}';
       Dependency : '$Is' ),
      (Name : '$AsClass';
       Code : 'function $AsClass(s,c) {'#13#10
               +#9'if ((s===null)||$Inh(s,c)) return s;'#13#10
               +#9'throw Exception.Create$1($New(Exception),"Can''t cast class \""+s.$ClassName+"\" to class \""+c.$ClassName+"\"");'#13#10
               +'}';
       Dependency : '$Inh' ),
      (Name : '$AsIntf';
       Code : 'function $AsIntf(o,i) {'#13#10
               +#9'if (o===null) return null;'#13#10
               +#9'var r = o.ClassType.$Intf[i].map(function (e) {'#13#10
                  +#9#9'return function () {'#13#10
                     +#9#9#9'var arg=Array.prototype.slice.call(arguments);'#13#10
                     +#9#9#9'arg.splice(0,0,o);'#13#10
                     +#9#9#9'return e.apply(o, arg);'#13#10
                  +#9#9'}'#13#10
               +#9'});'#13#10
               +#9'r.O = o;'#13#10
               +#9'return r;'#13#10
               +'}'#13#10),
      (Name : '$Implements';
       Code : 'function $Implements(o,i) {'#13#10
               +#9'if (o===null) return false;'#13#10
               +#9'var cti=o.ClassType.$Intf;'#13#10
               +#9'return ((cti!=undefined)&&(cti[i]!=undefined));'#13#10
               +'}'#13#10),
      (Name : '$ClassImplements';
       Code : 'function $ClassImplements(c,i) {'#13#10
               +#9'if (c===null) return false;'#13#10
               +#9'var cti=c.$Intf;'#13#10
               +#9'return ((cti!=undefined)&&(cti[i]!=undefined));'#13#10
               +'}'#13#10),
      (Name : '$IntfAsClass';
       Code : 'function $IntfAsClass(i,c) {'#13#10
               +#9'if (i===null) return null;'#13#10
               +#9'if ($Is(i.O,c)) return i.O;'#13#10
               +#9'else throw Exception.Create$1($New(Exception),"Can''t cast interface of \""+i.O.ClassType.$ClassName+"\" to class \""+c.$ClassName+"\"");'#13#10
               +'}'#13#10;
       Dependency : '$Is' ),
      (Name : '$Idx';
       Code : 'function $Idx(i,l,h,z) {'#13#10
               +#9'if (i<l) throw Exception.Create$1($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>h) throw Exception.Create$1($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return i-l;'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$DIdxR';
       Code : 'function $DIdxR(a,i,z) {'#13#10
               +#9'if (i<0) throw Exception.Create$1($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>a.length) throw Exception.Create$1($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return a[i];'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$DIdxW';
       Code : 'function $DIdxW(a,i,v,z) {'#13#10
               +#9'if (i<0) throw Exception.Create$1($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>a.length) throw Exception.Create$1($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'a[i]=v;'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$SIdx';
       Code : 'function $SIdx(s,i,z) {'#13#10
               +#9'if (i<1) throw Exception.Create$1($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>s.length) throw Exception.Create$1($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return s.charAt(i-1);'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$IndexOfRecord';
       Code : 'function $IndexOfRecord(a,i,f) {'#13#10
               +#9'var ij = JSON.stringify(i);'#13#10
               +#9'for (var k=f,n=a.length;k<n;k++)'#13#10
               +#9#9'if (JSON.stringify(a[k])==ij) return k;'#13#10
               +#9'return -1'#13#10
               +'}'),
      (Name : '$StrSet';
       Code : 'function $StrSet(s,i,v,z) {'#13#10
               +#9'if (i<1) throw Exception.Create$1($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>s.length) throw Exception.Create$1($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return s.substring(0,i-1)+v+s.substring(i);'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$Event';
       Code : 'function $Event(i,f) {'#13#10
               +#9'var li=i,lf=f;'#13#10
               +#9'return function(){'#13#10
                  +#9#9'var arg=Array.prototype.slice.call(arguments);'#13#10
                  +#9#9'arg.unshift(li);'#13#10
                  +#9#9'return lf.apply(li,arg)'#13#10
               +#9'}'#13#10
               +'}'),
      (Name : '$Event0';
       Code : 'function $Event0(i,f) {'#13#10
               +#9'var li=i,lf=f;'#13#10
               +#9'return function() {'#13#10
                  +#9#9'return lf.call(li,li)'#13#10
               +#9'}'#13#10
               +'}'),
      (Name : '$Event1';
       Code : 'function $Event1(i,f) {'#13#10
               +#9'var li=i,lf=f;'#13#10
               +#9'return function(a) {'#13#10
                  +#9#9'return lf.call(li,li,a)'#13#10
               +#9'}'#13#10
               +'}'),
      (Name : '$Event2';
       Code : 'function $Event2(i,f) {'#13#10
               +#9'var li=i,lf=f;'#13#10
               +#9'return function(a,b) {'#13#10
                  +#9#9'return lf.call(li,li,a,b)'#13#10
               +#9'}'#13#10
               +'}'),
      (Name : '$Event3';
       Code : 'function $Event3(i,f) {'#13#10
               +#9'var li=i,lf=f;'#13#10
               +#9'return function(a,b,c) {'#13#10
                  +#9#9'return lf.call(li,li,a,b,c)'#13#10
               +#9'}'#13#10
               +'}'),
      (Name : '$OrdS';
       Code : 'function $OrdS(s) { return (s.length>0)?s.charCodeAt(0):0 }'),
      (Name : '$Ord';
       Code : 'function $Ord(s,z) {'#13#10
               +#9'switch (Object.prototype.toString.call(s)) {'#13#10
                  +#9#9'case "[object Number]": return parseInt(s);'#13#10
                  +#9#9'case "[object Boolean]": return s?1:0;'#13#10
                  +#9#9'case "[object String]": return (s.length>0)?s.charCodeAt(0):0;'#13#10
               +#9'}'#13#10
               +#9'throw Exception.Create$1($New(Exception),"Not an ordinal! "+z);'#13#10
               +'}';
       Dependency : 'Exception' ),
      // RTL functions
      (Name : 'AnsiCompareStr';
       Code : 'function AnsiCompareStr(a,b) { return a.localeCompare(b) }'),
      (Name : 'AnsiCompareText';
       Code : 'function AnsiCompareText(a,b) { return AnsiCompareStr(a.toLocaleUpperCase(), b.toLocaleUpperCase()) }';
       Dependency : 'AnsiCompareStr'),
      (Name : 'AnsiLowerCase';
       Code : 'function AnsiLowerCase(v) { return v.toLocaleLowerCase() }'),
      (Name : 'AnsiUpperCase';
       Code : 'function AnsiUpperCase(v) { return v.toLocaleUpperCase() }'),
      (Name : 'ArcCos';
       Code : 'function ArcCos(v) { return Math.acos(v) }'),
      (Name : 'ArcCosh';
       Code : 'function ArcCosh(v) { return Math.log(v+Math.sqrt((v-1)/(v+1))*(v+1)) }'),
      (Name : 'ArcSin';
       Code : 'function ArcSin(v) { return Math.asin(v) }'),
      (Name : 'ArcSinh';
       Code : 'function ArcSinh(v) { return Math.log(v+Math.sqrt(v*v+1)) }'),
      (Name : 'ArcTan';
       Code : 'function ArcTan(v) { return Math.atan(v) }'),
      (Name : 'ArcTan2';
       Code : 'function ArcTan2(y,x) { return Math.atan2(y,x) }'),
      (Name : 'ArcTanh';
       Code : 'function ArcTanh(v) { return 0.5*Math.log((1+v)/(1-v)) }'),
      (Name : 'BoolToStr';
       Code : 'function BoolToStr(b) { return b?"True":"False" }'),
      (Name : 'Ceil';
       Code : 'function Ceil(v) { return Math.ceil(v) }'),
      (Name : 'CharAt';
       Code : 'function CharAt(s,p) { return s.charAt(p-1) }'),
      (Name : 'Chr';
       Code : 'function Chr(c) {'#13#10
              +#9'if (c<=0xFFFF)'#13#10
                 +#9#9'return String.fromCharCode(c);'#13#10
              +#9'c-=0x10000;'#13#10
              +#9'return String.fromCharCode(0xD800+(c>>10))+String.fromCharCode(0xDC00+(c&0x3FF));'#13#10
              +'}'),
      (Name : 'Clamp';
       Code : 'function Clamp(v,mi,ma) { return v<mi ? mi : v>ma ? ma : v }'),
      (Name : 'ClampInt';
       Code : 'function ClampInt(v,mi,ma) { return v<mi ? mi : v>ma ? ma : v }'),
      (Name : 'CompareStr';
       Code : 'function CompareStr(a,b) { if (a<b) return -1; else return (a==b)?0:1 }'),
      (Name : 'CompareText';
       Code : 'function CompareText(a,b) { return CompareStr(a.toUpperCase(), b.toUpperCase()) }';
       Dependency : 'CompareStr'),
      (Name : 'Copy';
       Code : 'function Copy(s,f,n) { return s.substr(f-1,n) }'),
      (Name : 'Cos';
       Code : 'function Cos(v) { return Math.cos(v) }'),
      (Name : 'Cosh';
       Code : 'function Cosh(v) { return (v==0)?1:0.5*(Math.exp(v)+Math.exp(-v)) }'),
      (Name : 'Cotan';
       Code : 'function Cotan(v) { return 1/Math.tan(v) }'),
      (Name : 'DegToRad';
       Code : 'function DegToRad(v) { return v*(Math.PI/180) }'),
      (Name : 'Delete';
       Code : 'function Delete(s,i,n) { var v=s.'+TdwsJSCodeGen.cBoxFieldName+'; if ((i<=0)||(i>v.length)||(n<=0)) return;'
                                     +' s.'+TdwsJSCodeGen.cBoxFieldName+'=v.substr(0,i-1)+v.substr(i+n-1); }'),
      (Name : 'EncodeDate';
       Code : 'function EncodeDate(y,m,d) { return (new Date(y,m,d)).getTime()/864e5+25569 }'),
      (Name : 'Even';
       Code : 'function Even(v) { return (v&1)==0 }'),
      (Name : 'Exp';
       Code : 'function Exp(v) { return Math.exp(v) }'),
      (Name : 'Factorial';
       Code : 'function Factorial(i) { var r=1; while (i>1) { r*=i; i--; } return r }'),
      (Name : 'FloatToStr';
       Code : 'function FloatToStr(i,p) { return (p==99)?i.toString():i.toFixed(p) }'),
      (Name : 'Floor';
       Code : 'function Floor(v) { return Math.floor(v) }'),
      (Name : 'Format';
       Code : 'function Format(f,a) { a.unshift(f); return sprintf.apply(null,a) }';
       Dependency : '!sprintf_js'),
      (Name : 'FormatDateTime';
       Code : 'function FormatDateTime(f,a) { return formatDateTime(f,new Date((a-25569)*864e5)) }';
       Dependency : '!formatDateTime_js'),
      (Name : 'Frac';
       Code : 'function Frac(v) { return v-((v>0)?Math.floor(v):Math.ceil(v)) }'),
      (Name : 'FindDelimiter';
       Code : 'function FindDelimiter(d,s,x) { var n=d.length,r=ns=s.length,i,p; for (i=0;i<n;i++) { p=s.indexOf(d.charAt(i),x-1); if (p>=0&&p<r) r=p; } return (r==ns)?-1:r+1; }'),
      (Name : 'Gcd';
       Code : 'function Gcd(a, b) { var r; while (b!=0) { r=a%b; a=b; b=r; } return a }'),
      (Name : 'HexToInt';
       Code : 'function HexToInt(v) { return parseInt(v,16) }'),
      (Name : 'Hypot';
       Code : 'function Hypot(x,y) { return Math.sqrt(x*x+y*y) }'),
      (Name : 'Insert';
       Code : 'function Insert(s,d,i) { var v=d.'+TdwsJSCodeGen.cBoxFieldName+'; if (s=="") return; if (i<1) i=1; if (i>v.length) i=v.length+1;'
               +'d.'+TdwsJSCodeGen.cBoxFieldName+'=v.substr(0,i-1)+s+v.substr(i-1); }'),
      (Name : 'Int';
       Code : 'function Int(v) { return (v>0)?Math.floor(v):Math.ceil(v) }'),
      (Name : 'IntPower';
       Code : 'function IntPower(x,y) { return Math.pow(x,y) }'),
      (Name : 'IntToBin';
       Code : 'function IntToBin(v,d) { var r=v.toString(2).toUpperCase(); while (r.length<d) r="0"+r; return r }'),
      (Name : 'IntToHex';
       Code : 'function IntToHex(v,d) { var hex=v.toString(16).toUpperCase(); return "00000000".substr(0, 8-d-hex.length)+hex }'),
      (Name : 'IntToStr';
       Code : 'function IntToStr(i) { return i.toString() }'),
      (Name : 'IsDelimiter';
       Code : 'function IsDelimiter(d,s,i) { if ((i<=0)||(i>s.length)) return false; else return d.indexOf(s.charAt(i-1))>=0; }'),
      (Name : 'IsPrime';
       Code : 'function IsPrime(n) { if (n<=3) { return (n>=2) } else return ((n&1)&&(LeastFactor(n)==n)) }';
       Dependency : 'LeastFactor' ),
      (Name : 'Lcm';
       Code : 'function Lcm(a, b) { var g=Gcd(a,b); return (g!=0)?(Math.floor(a/g)*b):0 }';
       Dependency : 'Gcd' ),
      (Name : 'LeastFactor';
       Code : 'function LeastFactor(n) {'#13#10
               +#9'if (n<=1) return (n==1)?1:0;'#13#10
               +#9'if (!(n&1)) return 2;'#13#10
               +#9'if (!(n%3)) return 3;'#13#10
               +#9'var lim=Math.sqrt(n);'#13#10
               +#9'for (var i=5; i<=lim; i+=4) {'#13#10
                  +#9#9'if (!(n%i)) return i;'#13#10
                  +#9#9'i+=2;'#13#10
                  +#9#9'if (!(n%i)) return i;'#13#10
               +#9'}'#13#10
               +'return n'#13#10
               +'}'),
      (Name : 'Ln';
       Code : 'function Ln(v) { return Math.log(v) }'),
      (Name : 'LastDelimiter';
       Code : 'function LastDelimiter(d,s) { var r=-1,n=d.length,i,p; for (i=0;i<n;i++) { p=s.lastIndexOf(d.charAt(i)); if (p>r) r=p; } return r+1;}'),
      (Name : 'LeftStr';
       Code : 'function LeftStr(s,n) { return s.substr(0,n) }'),
      (Name : 'Log10';
       Code : 'function Log10(x) { return Math.log(x)/Math.LN10 }'),
      (Name : 'Log2';
       Code : 'function Log2(x) { return Math.log(x)/Math.LN2 }'),
      (Name : 'LogN';
       Code : 'function LogN(n,x) { return Math.log(x)/Math.log(n) }'),
      (Name : 'LowerCase';
       Code : 'function LowerCase(v) { return v.toLowerCase() }'),
      (Name : 'Max$_Float_Float_';
       Code : 'function Max$_Float_Float_(a,b) { return (a>b)?a:b }'),
      (Name : 'Max$_Integer_Integer_';
       Code : 'function Max$_Integer_Integer_(a,b) { return (a>b)?a:b }'),
      (Name : 'MaxInt';
       Code : 'function MaxInt(a,b) { return (a>b)?a:b }'),
      (Name : 'Min$_Float_Float_';
       Code : 'function Min$_Float_Float_(a,b) { return (a<b)?a:b }'),
      (Name : 'Min$_Integer_Integer_';
       Code : 'function Min$_Integer_Integer_(a,b) { return (a<b)?a:b }'),
      (Name : 'MinInt';
       Code : 'function MinInt(a,b) { return (a<b)?a:b }'),
      (Name : 'Now';
       Code : 'function Now() { var d=new Date(); return d.getTime()/8.64e7+25569-d.getTimezoneOffset()/1440 }'),
      (Name : 'Odd';
       Code : 'function Odd(v) { return (v&1)==1 }'),
      (Name : 'Pi';
       Code : 'function Pi() { return Math.PI }'),
      (Name : 'Pos';
       Code : 'function Pos(a,b) { return b.indexOf(a)+1 }'),
      (Name : 'PosEx';
       Code : 'function PosEx(a,b,o) { return b.indexOf(a,o-1)+1 }'),
      (Name : 'Power';
       Code : 'function Power(x,y) { return Math.pow(x,y) }'),
      (Name : 'QuotedStr';
       Code : 'function QuotedStr(s,q) { if (!q) q="''"; return q+s.replace(q, q+q)+q }'),
      (Name : 'RadToDeg';
       Code : 'function RadToDeg(v) { return v*(180/Math.PI) }'),
      (Name : 'Random';
       Code : 'var Random = $alea()';
       Dependency : '!alea_js' ),
      (Name : 'RandG';
       Code : 'function RandG(m, s) {'#13#10
              +#9'var u, r, n;'#13#10
              +#9'do {'#13#10
                  +#9#9'u=2*Random()-1;'#13#10
                  +#9#9'r=2*Random()-1;'#13#10
                  +#9#9'n=u*u+r*r;'#13#10
              +#9'} while (n<1);'#13#10
              +#9'return m+Math.sqrt(-2*Math.log(n)/n)*u*s;'#13#10
              +'}';
       Dependency : 'Random'),
      (Name : 'RandomInt';
       Code : 'function RandomInt(i) { return Math.floor(Random()*i) }';
       Dependency : 'Random'),
      (Name : 'Randomize';
       Code : 'function Randomize() { Random = $alea() }';
       Dependency : 'Random'),
      (Name : 'RandSeed';
       Code : 'function RandSeed() { return 0 }'), // deprecated
      (Name : 'ReverseString';
       Code : 'function ReverseString(s) { return s.split("").reverse().join("") }'),
      (Name : 'RevPos';
       Code : 'function RevPos(a,b) { return (a=="")?0:(b.lastIndexOf(a)+1) }'),
      (Name : 'RightStr';
       Code : 'function RightStr(s,n) { return s.substr(s.length-n) }'),
      (Name : 'Round';
       Code : 'function Round(v) { return Math.round(v) }'),
      (Name : 'SameText';
       Code : 'function SameText(a,b) { return a.toUpperCase()==b.toUpperCase() }'),
      (Name : 'SetLength';
       Code : 'function SetLength(s,n) { if (s.'+TdwsJSCodeGen.cBoxFieldName+'.length>n) s.'+TdwsJSCodeGen.cBoxFieldName+'=s.'+TdwsJSCodeGen.cBoxFieldName+'.substring(0,n);'
                                       +'else while (s.'+TdwsJSCodeGen.cBoxFieldName+'.length<n) s.'+TdwsJSCodeGen.cBoxFieldName+'+=" "; }'),
      (Name : 'SetRandSeed';
       Code : 'function SetRandSeed(v) { Random = $alea(v) }';
       Dependency : 'Random'),
      (Name : 'Sin';
       Code : 'function Sin(v) { return Math.sin(v) }'),
      (Name : 'Sinh';
       Code : 'function Sinh(v) { return (v==0)?0:(0.5*(Math.exp(v)-Math.exp(-v))) }'),
      (Name : 'StrAfter';
       Code : 'function StrAfter(s,d) { if (!d) return ""; var p=s.indexOf(d); return (p<0)?"":s.substr(p+d.length) }'),
      (Name : 'StrBefore';
       Code : 'function StrBefore(s,d) { if (!d) return s; var p=s.indexOf(d); return (p<0)?s:s.substr(0, p) }'),
      (Name : 'StrBeginsWith';
       Code : 'function StrBeginsWith(s,b) { return s.substr(0, b.length)==b }'),
      (Name : 'StrEndsWith';
       Code : 'function StrEndsWith(s,e) { return s.substr(s.length-e.length)==e }'),
      (Name : 'StringOfChar';
       Code : 'function StringOfChar(c,n) { return stringRepeat(c?c.charAt(0):" ",n) }';
       Dependency : '!stringRepeat_js'),
      (Name : 'StringOfString';
       Code : 'function StringOfString(s,n) { return stringRepeat(s,n) }';
       Dependency : '!stringRepeat_js'),
      (Name : 'StrToFloat';
       Code : 'function StrToFloat(v) { return parseFloat(v) }'),
      (Name : 'StrToFloatDef';
       Code : 'function StrToFloatDef(v,d) { var r=parseFloat(v); return isNaN(r)?d:r }'),
      (Name : 'StrToInt';
       Code : 'function StrToInt(v) { return parseInt(v,10) }'),
      (Name : 'StrToIntDef';
       Code : 'function StrToIntDef(v,d) { var r=parseInt(v,10); return isNaN(r)?d:r }'),
      (Name : 'SubStr';
       Code : 'function SubStr(s,f) { return s.substr(f-1) }'),
      (Name : 'SubString';
       Code : 'function SubString(s,f,t) { return s.substr(f-1, t-2) }'),
      (Name : 'Sqrt';
       Code : 'function Sqrt(v) { return Math.sqrt(v) }'),
      (Name : 'Tan';
       Code : 'function Tan(v) { return Math.tan(v) }'),
      (Name : 'Tanh';
       Code : 'function Tanh(v) { return (v==0)?0:(Math.exp(v)-Math.exp(-v))/(Math.exp(v)+Math.exp(-v)) }'),
      (Name : 'Trim';
       Code : 'function Trim(s) { return s.replace(/^\s\s*/, "").replace(/\s\s*$/, "") }'),
      (Name : 'TrimLeft';
       Code : 'function TrimLeft(s) { return s.replace(/^\s\s*/, "") }'),
      (Name : 'TrimRight';
       Code : 'function TrimRight(s) { return s.replace(/\s\s*$/, "") }'),
      (Name : 'Trunc';
       Code : 'function Trunc(v) { return (v>=0)?Math.floor(v):Math.ceil(v) }'),
      (Name : 'UpperCase';
       Code : 'function UpperCase(v) { return v.toUpperCase() }'),
      (Name : 'VarToStr';
       Code : 'function VarToStr(v) { return (typeof v === "undefined")?"":v.toString() }'),
      (Name : 'VarType';
       Code : 'function VarType(v) {'#13#10
               +#9'switch (Object.prototype.toString.call(v)) {'#13#10
                  +#9#9'case "[object Undefined]": return 0;'#13#10 // varEmpty
                  +#9#9'case "[object String]": return 0x100;'#13#10 // varString
                  +#9#9'case "[object Number]": return (parseInt(v)==v)?3:5;'#13#10 // varInteger/VarDouble
                  +#9#9'case "[object Boolean]": return 0xB;'#13#10 // varBoolean
                  +#9#9'default : return 0xC;'#13#10 // varVariant
               +#9'}'#13#10
               +'}'),
      // RTL classes
      (Name : 'TObject';
       Code : 'var TObject={'#13#10
               +#9'$ClassName: "TObject",'#13#10
               +#9'ClassName: function (s) { return s.$ClassName },'#13#10
               +#9'ClassType: function (s) { return s },'#13#10
               +#9'$Init: function () {},'#13#10
               +#9'Create: function (s) { return s; },'#13#10
               +#9'Destroy: function (s) { for (var prop in s) { if (s.hasOwnProperty(prop)) delete s.prop; } },'#13#10
               +#9'Destroy$v: function(s) { return s.ClassType.Destroy(s) },'#13#10
               +#9'Free: function (s) { if (s!==null) s.ClassType.Destroy(s) }'#13#10
               +'}';
       Dependency : '$New'),
      (Name : 'Exception';
       Code : 'var Exception={'#13#10
               +#9'$ClassName: "Exception",'#13#10
               +#9'$Parent: TObject,'#13#10
               +#9'$Init: function () { FMessage=""; },'#13#10
               +#9'Create$1: function (s,Msg) { s.FMessage=Msg; return s; }'#13#10
               +'}';
       Dependency : 'TObject'),
      (Name : 'EAssertionFailed';
       Code : 'var EAssertionFailed={'#13#10
               +#9'$ClassName: "EAssertionFailed",'#13#10
               +#9'$Parent: Exception,'#13#10
               +#9'$Init: Exception.$Init'#13#10
               +'}';
       Dependency : 'Exception')
   );

// FindJSRTLDependency
//
function FindJSRTLDependency(const name : String) : PJSRTLDependency;
var
   i : Integer;
begin
   for i:=Low(cJSRTLDependencies) to High(cJSRTLDependencies) do begin
      Result:=@cJSRTLDependencies[i];
      if Result.Name=name then Exit;
   end;
   Result:=nil;
end;

// All_RTL_JS
//
function All_RTL_JS : String;
var
   i : Integer;
   wobs : TWriteOnlyBlockStream;
begin
   wobs:=TWriteOnlyBlockStream.Create;
   try
      for i:=Low(cJSRTLDependencies) to High(cJSRTLDependencies) do begin
         wobs.WriteString(cJSRTLDependencies[i].Code);
         wobs.WriteString(#13#10);
      end;
      Result:=wobs.ToString;
   finally
      wobs.Free;
   end;
end;

// IgnoreJSRTLDependencies
//
procedure IgnoreJSRTLDependencies(dependencies : TStrings);
var
   i, k : Integer;
begin
   for i:=Low(cJSRTLDependencies) to High(cJSRTLDependencies) do begin
      k:=Dependencies.IndexOf(cJSRTLDependencies[i].Name);
      if k>=0 then
         Dependencies.Delete(k);
   end;
end;

// ------------------
// ------------------ TJSMagicFuncExpr ------------------
// ------------------

// Create
//
constructor TJSMagicFuncExpr.Create;
begin
   inherited;
   FMagicCodeGens:=TStringList.Create;
   FMagicCodeGens.CaseSensitive:=False;
   FMagicCodeGens.Sorted:=True;
   FMagicCodeGens.Duplicates:=dupError;

   FMagicCodeGens.AddObject('AnsiLowerCase', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.toLocaleLowerCase()']));
   FMagicCodeGens.AddObject('AnsiUpperCase', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.toLocaleUpperCase()']));
   FMagicCodeGens.AddObject('ArcCos', TdwsExprGenericCodeGen.Create(['Math.acos', '(', 0, ')']));
   FMagicCodeGens.AddObject('ArcSin', TdwsExprGenericCodeGen.Create(['Math.asin', '(', 0, ')']));
   FMagicCodeGens.AddObject('ArcTan', TdwsExprGenericCodeGen.Create(['Math.atan', '(', 0, ')']));
   FMagicCodeGens.AddObject('ArcTan2', TdwsExprGenericCodeGen.Create(['Math.atan2', '(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Ceil', TdwsExprGenericCodeGen.Create(['Math.ceil', '(', 0, ')']));
   FMagicCodeGens.AddObject('Cos', TdwsExprGenericCodeGen.Create(['Math.cos', '(', 0, ')']));
   FMagicCodeGens.AddObject('Copy', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.substr(', '(', 1, ')', '-1,', 2, ')']));
   FMagicCodeGens.AddObject('Exp', TdwsExprGenericCodeGen.Create(['Math.exp', '(', 0, ')']));
   FMagicCodeGens.AddObject('FloatToStr', TJSFloatToStrExpr.Create);
   FMagicCodeGens.AddObject('Floor', TdwsExprGenericCodeGen.Create(['Math.floor', '(', 0, ')']));
   FMagicCodeGens.AddObject('Format', TJSFormatExpr.Create);
   FMagicCodeGens.AddObject('HexToInt', TdwsExprGenericCodeGen.Create(['parseInt', '(', 0, ',','16)']));
   FMagicCodeGens.AddObject('IntPower', TdwsExprGenericCodeGen.Create(['Math.pow', '(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('IntToStr', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.toString()']));
   FMagicCodeGens.AddObject('LeftStr', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.substr(0,', 1, ')']));
   FMagicCodeGens.AddObject('Ln', TdwsExprGenericCodeGen.Create(['Math.log', '(', 0, ')']));
   FMagicCodeGens.AddObject('LowerCase', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.toLowerCase()']));
   FMagicCodeGens.AddObject('Max$_Float_Float_', TdwsExprGenericCodeGen.Create(['Math.max','(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Max$_Integer_Integer_', TdwsExprGenericCodeGen.Create(['Math.max', '(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Min$_Float_Float_', TdwsExprGenericCodeGen.Create(['Math.min','(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Min$_Integer_Integer_', TdwsExprGenericCodeGen.Create(['Math.min', '(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('MaxInt', TdwsExprGenericCodeGen.Create(['Math.max','(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('MinInt', TdwsExprGenericCodeGen.Create(['Math.min', '(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Pi', TdwsExprGenericCodeGen.Create(['Math.PI']));
   FMagicCodeGens.AddObject('Pos', TdwsExprGenericCodeGen.Create(['(', 1, '.indexOf', '(', 0, ')', '+1)']));
   FMagicCodeGens.AddObject('PosEx', TdwsExprGenericCodeGen.Create(['(', 1, '.indexOf', '(', 0, ',', '(', 2, ')', '-1)+1)']));
   FMagicCodeGens.AddObject('Power', TdwsExprGenericCodeGen.Create(['Math.pow', '(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Round', TdwsExprGenericCodeGen.Create(['Math.round', '(', 0, ')']));
   FMagicCodeGens.AddObject('Sin', TdwsExprGenericCodeGen.Create(['Math.sin', '(', 0, ')']));
   FMagicCodeGens.AddObject('Sqrt', TdwsExprGenericCodeGen.Create(['Math.sqrt', '(', 0, ')']));
   FMagicCodeGens.AddObject('StrToFloat', TdwsExprGenericCodeGen.Create(['parseFloat', '(', 0, ')']));
   FMagicCodeGens.AddObject('StrToInt', TdwsExprGenericCodeGen.Create(['parseInt', '(', 0, ',', '10)']));
   FMagicCodeGens.AddObject('SubStr', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.substr(', '(', 1, ')', '-1)']));
   FMagicCodeGens.AddObject('SubString', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.substr(', '(', 1, ')', '-1,', '(', 2, ')', '-2)']));
   FMagicCodeGens.AddObject('Tan', TdwsExprGenericCodeGen.Create(['Math.tan', '(', 0, ')']));
   FMagicCodeGens.AddObject('UpperCase', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.toUpperCase()']));
end;

// Destroy
//
destructor TJSMagicFuncExpr.Destroy;
var
   i : Integer;
begin
   inherited;
   for i:=0 to FMagicCodeGens.Count-1 do
      FMagicCodeGens.Objects[i].Free;
   FMagicCodeGens.Free;
end;

// CodeGen
//
procedure TJSMagicFuncExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);

   function GetSignature(funcSym : TFuncSymbol) : String;
   var
      i : Integer;
   begin
      Result:=funcSym.QualifiedName+'$_';
      for i:=0 to funcSym.Params.Count-1 do
         Result:=Result+funcSym.GetParamType(i).Name+'_';
   end;

var
   e : TMagicFuncExpr;
   name : String;
   i : Integer;
begin
   e:=TMagicFuncExpr(expr);
   if e.FuncSym.IsOverloaded then
      name:=GetSignature(e.FuncSym)
   else name:=e.FuncSym.QualifiedName;
   if cgoNoInlineMagics in codeGen.Options then
      i:=-1
   else i:=FMagicCodeGens.IndexOf(name);
   if i>=0 then
      TdwsExprCodeGen(FMagicCodeGens.Objects[i]).CodeGen(codeGen, expr)
   else begin
      codeGen.Dependencies.Add(name);
      inherited;
   end;
end;

// CodeGenFunctionName
//
procedure TJSMagicFuncExpr.CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol);

   function GetSignature(funcSym : TFuncSymbol) : String;
   var
      i : Integer;
   begin
      Result:=funcSym.QualifiedName+'$_';
      for i:=0 to funcSym.Params.Count-1 do
         Result:=Result+funcSym.GetParamType(i).Name+'_';
   end;

var
   e : TMagicFuncExpr;
   name : String;
begin
   e:=TMagicFuncExpr(expr);
   if e.FuncSym.IsOverloaded then
      name:=GetSignature(e.FuncSym)
   else name:=e.FuncSym.QualifiedName;
   codeGen.WriteString(name);
   codeGen.Dependencies.Add(name);
end;

// ------------------
// ------------------ TJSFloatToStrExpr ------------------
// ------------------

// CodeGen
//
procedure TJSFloatToStrExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMagicFuncExpr;
   i : Integer;
begin
   e:=TMagicFuncExpr(expr);

   if e.Args[1] is TConstIntExpr then begin

      codeGen.WriteString('(');
      codeGen.Compile(e.Args[0]);

      i:=e.Args[1].EvalAsInteger(nil);
      if i=99 then
         codeGen.WriteString(').toString()')
      else begin
         codeGen.WriteString(').toFixed(');
         codeGen.WriteString(IntToStr(i));
         codeGen.WriteString(')');
      end;

   end else begin

      codeGen.Dependencies.Add('FloatToStr');

      codeGen.WriteString('FloatToStr(');
      codeGen.CompileNoWrap(e.Args[0] as TTypedExpr);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.Args[1] as TTypedExpr);
      codeGen.WriteString(')');

   end;
end;

// ------------------
// ------------------ TJSFormatExpr ------------------
// ------------------

// CodeGen
//
procedure TJSFormatExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMagicFuncExpr;
begin
   e:=TMagicFuncExpr(expr);

   if e.Args[0] is TConstStringExpr then begin
   end;

   codeGen.Dependencies.Add('Format');

   inherited CodeGen(codeGen, expr);
end;

end.
