{**********************************************************************}
{                                                                      }
{    The contents of this file are subject to the GNU General          }
{    Public License version 3 (the "License") as published by          }
{    the Free Software Foundation. You may obtain a copy of            }
{    the License at https://www.gnu.org/licenses/gpl-3.0.txt           }
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

uses
   Classes, SysUtils,
   dwsUtils, dwsUnicode, dwsJSCodeGen, dwsTextCodeGen, dwsCodeGen, dwsExprList,
   dwsExprs, dwsSymbols, dwsMagicExprs, dwsCoreExprs, dwsConstExprs;

type
   TJSRTLDependency = record
      Name, Code, Dependency, Dependency2 : String;
   end;
   PJSRTLDependency = ^TJSRTLDependency;

   TJSMagicFuncExpr = class (TJSFuncBaseExpr)
      private
         FMagicCodeGens : TStringList;

         class var vAliases : TUnicodeStringList;
         class procedure RegisterAlias(const aliasName, canonicalName : String); static;

      public
         constructor Create(const codeGen : TdwsCodeGen);
         destructor Destroy; override;

         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
         procedure CodeGenFunctionName(codeGen : TdwsCodeGen; expr : TFuncExprBase; funcSym : TFuncSymbol); override;

         class function CanonicalName(const aName : String) : String; static;
   end;

   TJSSqrMagicExpr = class (TJSFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSFloatToStrExpr = class (TJSFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSIntToHexExpr = class (TJSFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSIntToStrExpr = class (TJSFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSStrBeginsWithExpr = class (TJSFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSStrFindExpr = class (TJSFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TJSStrCopyFuncExpr = class (TJSFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSStrMatchesFuncExpr = class (TJSFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSStrReplaceFuncExpr = class (TJSFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSGetTextFuncExpr = class (TJSFuncBaseExpr)
      private
         FCodeGen : TdwsCodeGen;
      public
         constructor Create(codeGen : TdwsCodeGen);
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TFormatSplitInfoDetail = (fsidIndex, fsidLeftAligned, fsidWidth, fsidPrecision, fsidType, fsidError);
   TFormatSplitInfoDetails = set of TFormatSplitInfoDetail;

   TFormatSplitInfo = class (TRefCountedObject)
      public
         Details : TFormatSplitInfoDetails;
         Index : Integer;
         Width : Integer;
         Precision : Integer;
         Typ : Char;
         Str : String;
   end;

   TFormatSplitInfos = class (TObjectList<TFormatSplitInfo>)
      private
         FIsValid : Boolean;

      public
         constructor Create(const fmtString : String);

         property IsValid : Boolean read FIsValid;
   end;

   TJSFormatExpr = class (TJSFuncBaseExpr)
      private
         function FormatIsSimpleEnough(splitInfos : TFormatSplitInfos) : Boolean;

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

{$R dwsJSRTL.res}

const
   cJSRTLDependencies : array [1..290] of TJSRTLDependency = (
      // codegen utility functions
      (Name : '$CheckStep';
       Code : 'function $CheckStep(s,z) { if (s>0) return s; throw Exception.Create($New(Exception),"FOR loop STEP should be strictly positive: "+s.toString()+z); }';
       Dependency : 'Exception' ),
      (Name : '$W';  // only invoked from try..except codegen, which handles dependencies
       Code : 'function $W(e) { return e.ClassType?e:Exception.Create($New(Exception),(typeof e == "string") ? e : e.constructor.name+", "+e.message) }'),
      (Name : '$New';
       Code : 'function $New(c) { var i={ClassType:c}; c.$Init(i); return i }'),
      (Name : '$NewDyn';
       Code : 'function $NewDyn(c,z) {'#13#10
              +#9'if (c==null) throw Exception.Create($New(Exception),"ClassType is nil"+z);'#13#10
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
      (Name : '$ArraySetLenC';
       Code : 'function $ArraySetLenC(a,n,d) {'#13#10
              +#9'var o=a.length;'#13#10
              +#9'if (o==n) return;'#13#10
              +#9'if (o>n) a.length=n; else for (;o<n;o++) a.push(d());'#13#10
              +'}'),
      (Name : '$ArraySetLen';
       Code : 'function $ArraySetLen(a,n,v) {'#13#10
              +#9'var o=a.length;'#13#10
              +#9'if (o==n) return;'#13#10
              +#9'if (o>n) a.length=n; else for (;o<n;o++) a.push(v);'#13#10
              +'}'),
      (Name : '$ArrayCopy';
       Code : 'function $ArrayCopy(a,i,z) { return a.slice($Idx(i,0,a.length-1,z)) }';
       Dependency : '$Idx' ),
      (Name : '$ArrayCopyLen';
       Code : 'function $ArrayCopyLen(a,i,l,z) {'#13#10
              +#9'if (l<0) throw Exception.Create($New(Exception),"Positive count expected (got "+l.toString()+")"+z);'#13#10
              +#9'return a.slice($Idx(i,0,a.length-1,z),$Idx(i+l-1,0,a.length-1,z)-i+1,z)'#13#10
              +'}';
       Dependency : '$Idx' ),
      (Name : '$ArrayMove';
       Code : 'function $ArrayMove(a,s,d) { var e=a[s]; a.splice(s, 1); a.splice(d, 0, e) }' ),
      (Name : '$ArrayMoveChk';
       Code : 'function $ArrayMoveChk(a,s,d,z) {'#13#10
              +#9'var n=a.length-1;'#13#10
              +#9'$ArrayMove(a,$Idx(s,0,n,z),$Idx(d,0,n,z));'#13#10
              +'}';
       Dependency : '$Idx'; Dependency2 : '$ArrayMove' ),
      (Name : '$ArraySwap';
       Code : 'function $ArraySwap(a,i1,i2) { var t=a[i1]; a[i1]=a[i2]; a[i2]=t; return a }' ),
      (Name : '$ArraySwapChk';
       Code : 'function $ArraySwapChk(a,i1,i2,z) {'#13#10
              +#9'var n=a.length-1, t=a[$Idx(i1,0,n,z)];'#13#10
              +#9'a[i1]=a[$Idx(i2,0,n,z)]'#13#10
              +#9'a[i2]=t;'#13#10
              +#9'return a;'#13#10
              +'}';
       Dependency : '$Idx' ),
      (Name : '$CmpNum';
       Code : 'function $CmpNum(a,b) { return a-b }' ),
      (Name : '$Check';
       Code : 'function $Check(i,z) { if (i) return i; throw Exception.Create($New(Exception),"Object not instantiated"+z); }';
       Dependency : 'Exception' ),
      (Name : '$CheckIntf';
       Code : 'function $CheckIntf(i,z) { if (i) return i; throw Exception.Create($New(Exception),"Interface is nil"+z); }';
       Dependency : 'Exception' ),
      (Name : '$CheckFunc';
       Code : 'function $CheckFunc(i,z) { if (i) return i; throw Exception.Create($New(Exception),"Function pointer is nil"+z); }';
       Dependency : 'Exception' ),
      (Name : '$Assert';
       Code : 'function $Assert(b,m,z) { if (!b) throw Exception.Create($New(EAssertionFailed),"Assertion failed"+z+((m=="")?"":" : ")+m); }';
       Dependency : 'EAssertionFailed' ),
      (Name : '$CondFailed';
       Code : 'function $CondFailed(z,m) { throw Exception.Create($New(EAssertionFailed),z+m); }';
       Dependency : 'EAssertionFailed' ),
      (Name : '$Delete';
       Code : 'function $Delete(o) { for (var m in o) delete o[m]; }' ),
      (Name : '$Inh';
       Code : 'function $Inh(s,c) {'#13#10
               +#9'if (s===null) return false;'#13#10
               +#9'while ((s)&&(s!==c)) s=s.$Parent;'#13#10
               +#9'return (s)?true:false;'#13#10
               +'}'#13#10 ),
      (Name : '$ToClassType';
       Code : 'function $ToClassType(o) {'#13#10
               +#9'if (o===null) return o;'#13#10
               +#9'return o.ClassType'#13#10
               +'}'#13#10;
       Dependency : 'TObject' ),
      (Name : '$Is';
       Code : 'function $Is(o,c) {'#13#10
               +#9'if (o===null) return false;'#13#10
               +#9'return $Inh(o.ClassType,c);'#13#10
               +'}'#13#10;
       Dependency : '$Inh' ),
      (Name : '$As';
       Code : 'function $As(o,c) {'#13#10
               +#9'if ((o===null)||$Is(o,c)) return o;'#13#10
               +#9'throw Exception.Create($New(Exception),"Cannot cast instance of type \""+o.ClassType.$ClassName+"\" to class \""+c.$ClassName+"\"");'#13#10
               +'}';
       Dependency : '$Is';
       Dependency2 : 'Exception' ),
      (Name : '$AsClass';
       Code : 'function $AsClass(s,c) {'#13#10
               +#9'if ((s===null)||$Inh(s,c)) return s;'#13#10
               +#9'throw Exception.Create($New(Exception),"Cannot cast class \""+s.$ClassName+"\" to class \""+c.$ClassName+"\"");'#13#10
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
               +#9'else throw Exception.Create($New(Exception),"Cannot cast interface of \""+i.O.ClassType.$ClassName+"\" to class \""+c.$ClassName+"\"");'#13#10
               +'}'#13#10;
       Dependency : '$Is' ),
      (Name : '$Peek';
       Code : 'function $Peek(a,z) {'#13#10
               +#9'var n=a.length;'#13#10
               +#9'if (n>0) return a[n-1];'#13#10
               +#9'throw Exception.Create($New(Exception),"Upper bound exceeded! Index 0"+z);'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$Pop';
       Code : 'function $Pop(a,z) {'#13#10
               +#9'if (a.length>0) return a.pop();'#13#10
               +#9'throw Exception.Create($New(Exception),"Upper bound exceeded! Index 0"+z);'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$Pusha';
       Code : 'Array.prototype.pusha = function (e) { this.push.apply(this, e); return this }' ),
      (Name : '$Idx';
       Code : 'function $Idx(i,l,h,z) {'#13#10
               +#9'if (i<l) throw Exception.Create($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>h) throw Exception.Create($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return i-l;'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$DIdxR';
       Code : 'function $DIdxR(a,i,z) {'#13#10
               +#9'if (i<0) throw Exception.Create($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>=a.length) throw Exception.Create($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return a[i];'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$DIdxW';
       Code : 'function $DIdxW(a,i,v,z) {'#13#10
               +#9'if (i<0) throw Exception.Create($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>=a.length) throw Exception.Create($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'a[i]=v;'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$DIdxAdd';
       Code : 'function $DIdxAdd(a,i,v,z) {'#13#10
               +#9'if (i<0) throw Exception.Create($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>=a.length) throw Exception.Create($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'a[i]+=v;'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$DIdxMinus';
       Code : 'function $DIdxMinus(a,i,v,z) {'#13#10
               +#9'if (i<0) throw Exception.Create($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>=a.length) throw Exception.Create($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'a[i]-=v;'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$DIdxMult';
       Code : 'function $DIdxMult(a,i,v,z) {'#13#10
               +#9'if (i<0) throw Exception.Create($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>=a.length) throw Exception.Create($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'a[i]*=v;'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$DIdxDiv';
       Code : 'function $DIdxMult(a,i,v,z) {'#13#10
               +#9'if (i<0) throw Exception.Create($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>=a.length) throw Exception.Create($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'a[i]/=v;'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$SIdx';
       Code : 'function $SIdx(s,i,z) {'#13#10
               +#9'if (i<1) throw Exception.Create($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>s.length) throw Exception.Create($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return s.charAt(i-1);'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$SCodeIdx';
       Code : 'function $SCodeIdx(s,i,z) {'#13#10
               +#9'if (i<1) throw Exception.Create($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>s.length) throw Exception.Create($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return s.charCodeAt(i-1);'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$IndexOffset';
       Code : 'function $IndexOffset(i,b) { return i >= 0 ? i+b : i }'),
      (Name : '$IndexOfRecord';
       Code : 'function $IndexOfRecord(a,i,f,b) {'#13#10
               +#9'var ij = JSON.stringify(i);'#13#10
               +#9'for (var k=f,n=a.length;k<n;k++)'#13#10
               +#9#9'if (JSON.stringify(a[k])==ij) return k+b;'#13#10
               +#9'return -1'#13#10
               +'}'),
      (Name : '$RemoveRecord';
       Code : 'function $RemoveRecord(a,i,f) {'#13#10
               +#9'var j = $IndexOfRecord(a,i,f);'#13#10
               +#9'if (j>=0) a.splice(j,1);'#13#10
               +'}';
       Dependency : 'IndexOfRecord' ),
      (Name : '$Remove';
       Code : 'function $Remove(a,i,f) {'#13#10
               +#9'var j = a.indexOf(i,f);'#13#10
               +#9'if (j>=0) a.splice(j,1);'#13#10
               +'}'),
      (Name : '$StrSet';
       Code : 'function $StrSet(s,i,v,z) {'#13#10
               +#9'if (i<1) throw Exception.Create($New(Exception),"Lower bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'if (i>s.length) throw Exception.Create($New(Exception),"Upper bound exceeded! Index "+i.toString()+z);'#13#10
               +#9'return s.substring(0,i-1)+v+s.substring(i);'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$StrSetF';
       Code : 'function $StrSetF(o,f,i,v,z) {'#13#10
               +#9'o[f]=$StrSet(o[f],i,v,z);'#13#10
               +'}';
       Dependency : '$StrSet' ),
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
       Code : 'function $OrdS(s) { return (s.length>0)?$uniCharCodeAt(s,0):0 }';
       Dependency : '$uniCharCodeAt' ),
      (Name : '$Ord';
       Code : 'function $Ord(s,z) {'#13#10
               +#9'switch (Object.prototype.toString.call(s)) {'#13#10
                  +#9#9'case "[object Number]": return parseInt(s);'#13#10
                  +#9#9'case "[object Boolean]": return s?1:0;'#13#10
                  +#9#9'case "[object String]": return (s.length>0)?s.charCodeAt(0):0;'#13#10
               +#9'}'#13#10
               +#9'throw Exception.Create($New(Exception),"Not an ordinal! "+z);'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : '$EnumToName';
       Code : 'function $EnumToName(s,v) { var r=s[v]; return (r!==undefined)?r:"?" }'),
      (Name : '$EnumToOrd';
       Code : 'function $EnumToOrd(s,n,d) {'#13#10
               +#9'var $ = s.$; if (!$) {'#13#10
                  +#9#9'$ = {};'#13#10
                  +#9#9'for (var t in s) $[s[t]]=1*t;'#13#10
                  +#9#9's.$ = $;'#13#10
                  +#9'}'#13#10
               +#9'var r = $[n];'#13#10
               +#9'return (r!==undefined)?r:d;'#13#10
               +'}'),
      (Name : '$uniCharAt';
       Code : '';
       Dependency : '!unicharat_js' ),
      (Name : '$uniCharCodeAt';
       Code : '';
       Dependency : '!unicharcodeat_js' ),
      (Name : '$Xor';
       Code : 'function $Xor(a,b) { return typeof a === "number" ? a^b : a != b  && a || b }'),
      (Name : '$Div';
       Code : 'function $Div(a,b) { var r=a/b; return (r>=0)?Math.floor(r):Math.ceil(r) }'),
      (Name : '$Sign';
       Code : 'function $Sign(v) { return v>0?1:v<0?-1:v===v?0:NaN }'),
      (Name : '$GetName';
       Code : 'function $GetName(obj) {'#13#10
              +#9'if (obj && obj.constructor && obj.constructor.toString) {'#13#10
                 +#9#9'var arr = obj.constructor.toString().match(/function\s*(\w+)/);'#13#10
                 +#9#9'if (arr && arr.length == 2) return arr[1];'#13#10
              +#9'}'#13#10
              +#9'return undefined;'#13#10
              +'}'),
      (Name : '$Extend';
       code : 'function $Extend(base, sub, props) {'#13#10
              +#9'function F() {};'#13#10
              +#9'F.prototype = base.prototype;'#13#10
              +#9'sub.prototype = new F();'#13#10
              +#9'sub.prototype.constructor = sub;'#13#10
              +#9'for (var n in props) {'#13#10
                 +#9#9'if (props.hasOwnProperty(n)) {'#13#10
                    +#9#9#9'sub.prototype[n]=props[n];'#13#10
                 +#9#9'}'#13#10
              +#9'}'#13#10
              +'}'),
      (Name : '_';
       code : 'function _(t) { return t }'),
      (Name : '$SetInc';
       code : 'function $SetInc(s,v,m,n) { v-=m; if (v>=0 && v<n) s[v>>5]|=1<<(v&31) }'),
      (Name : '$SetExc';
       code : 'function $SetExc(s,v,m,n) { v-=m; if (v>=0 && v<n) s[v>>5]&=~(1<<(v&31)) }'),
      (Name : '$SetIn';
       code : 'function $SetIn(s,v,m,n) { v-=m; return (v<0 && v>=n)?false:(s[v>>5]&(1<<(v&31)))!=0 }'),
      (Name : '$SetEqual';
       code : 'function $SetEqual(a,b) { for(var i=0;i<a.length;i++) if (a[i]!==b[i]) return false; return true }'),
      (Name : '$SetLiR';
       code : 'function $SetLiR(a,b) { for(var i=0;i<a.length;i++) if ((a[i]&b[i])!=a[i]) return false; return true }'),
      (Name : '$SetAdd';
       code : 'function $SetAdd(a,b) { var r=[]; for(var i=0;i<a.length;i++) r.push(a[i]|b[i]); return r }'),
      (Name : '$SetSub';
       code : 'function $SetSub(a,b) { var r=[]; for(var i=0;i<a.length;i++) r.push(a[i]&(~b[i])); return r }'),
      (Name : '$SetMul';
       code : 'function $SetMul(a,b) { var r=[]; for(var i=0;i<a.length;i++) r.push(a[i]&b[i]); return r }'),
      (Name : '$TZ';
       code : 'var $TZ = 1, $fmt = { '#13#10
              +#9'ShortDayNames : [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ],'#13#10
              +#9'LongDayNames : [ "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" ],'#13#10
              +#9'ShortMonthNames : [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" ],'#13#10
              +#9'LongMonthNames : [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" ],'#13#10
              +#9'ShortDateFormat : "yyyy-mm-dd",'#13#10
              +#9'ShortTimeFormat : "hh:nn",'#13#10
              +#9'LongTimeFormat : "hh:nn:ss",'#13#10
              +#9'TimeAMString : "AM",'#13#10
              +#9'TimePMString : "PM"'#13#10
              +'}'),

      // RTL classes

      (Name : 'TObject';
       Code : 'var TObject={'#13#10
               +#9'$ClassName: "TObject",'#13#10
               +#9'$Parent: null,'#13#10
               +#9'ClassName: function (s) { return s.$ClassName },'#13#10
               +#9'ClassType: function (s) { return s },'#13#10
               +#9'ClassParent: function (s) { return s.$Parent },'#13#10
               +#9'$Init: function (s) {},'#13#10
               +#9'Create: function (s) { return s },'#13#10
               +#9'Destroy: function (s) { for (var prop in s) if (s.hasOwnProperty(prop)) delete s[prop] },'#13#10
               +#9'Destroy'+TdwsJSCodeGen.cVirtualPostfix+': function(s) { return s.ClassType.Destroy(s) },'#13#10
               +#9'Free: function (s) { if (s!==null) s.ClassType.Destroy(s) }'#13#10
               +'}';
       Dependency : '$New'),
      (Name : 'Exception';
       Code : 'var Exception={'#13#10
               +#9'$ClassName: "Exception",'#13#10
               +#9'$Parent: TObject,'#13#10
               +#9'$Init: function (s) { FMessage="" },'#13#10
               +#9'Create: function (s,Msg) { s.FMessage=Msg; return s }'#13#10
               +'}';
       Dependency : 'TObject'),
      (Name : 'EAssertionFailed';
       Code : 'var EAssertionFailed={'#13#10
               +#9'$ClassName: "EAssertionFailed",'#13#10
               +#9'$Parent: Exception,'#13#10
               +#9'$Init: Exception.$Init'#13#10
               +'}';
       Dependency : 'Exception'),

      // RTTI functions

      (Name : 'RTTIPropertyAttribute';
       Code : 'var RTTIPropertyAttribute={'#13#10
               +#9'$ClassName: "RTTIPropertyAttribute",'#13#10
               +#9'$Parent: TCustomAttribute,'#13#10
               +#9'$Init: function () { },'#13#10
               +#9'Name: function (s) { return s.N },'#13#10
               +#9'Typ: function (s) { return s.T },'#13#10
               +#9'Capabilities: function (s) { return s.C },'#13#10
               +#9'Getter: function (s,h) { return s.G(h) },'#13#10
               +#9'Setter: function (s,h,v) { return s.S(h,v) }'#13#10
               +'}';
       Dependency : 'TObject'),
      (Name : 'RTTIMethodAttribute';
       Code : 'var RTTIMethodAttribute={'#13#10
               +#9'$ClassName: "RTTIMethodAttribute",'#13#10
               +#9'$Parent: TCustomAttribute,'#13#10
               +#9'$Init: function () { },'#13#10
               +#9'Name: function (s) { return s.N },'#13#10
               +#9'Typ: function (s) { return s.T?s.T:{ID:0} },'#13#10
               +#9'Info: function (s) { return s.I },'#13#10
               +#9'VMTIndex: function (s,h) { return s.V?s.V:-1 },'#13#10
               +#9'Call: function (s,i,a) { if (!(s.I&4)) a.unshift(i); return s.F.apply(i,a) }'#13#10        // infoStatic = 4
               +'}';
       Dependency : 'TObject'),

      (Name : 'TypeOf$_TClass_';
       Code : 'function TypeOf$_TClass_(v) { return {ID:v} }'),
      (Name : 'RTTIRawAttributes';
       Code :  'var $RTTI_rdy = false;'#13#10
              +'function RTTIRawAttributes() {'#13#10
               +#9'if (!$RTTI_rdy) {'#13#10
                  +#9#9'for (var i=0; i<$RTTI.length; i++) {'#13#10
                     +#9#9#9'var r = $RTTI[i];'#13#10
                     +#9#9#9'if (r.A instanceof Function) r.A = r.A();'#13#10
                  +#9#9'}'#13#10
                  +#9#9'$RTTI_rdy = true;'#13#10
               +#9'}'#13#10
               +#9'return $RTTI'#13#10
               +'}'),
      (Name : 'SameRTTITypeInfo$_TRTTITypeInfo_TRTTITypeInfo_';
       Code : 'function SameRTTITypeInfo$_TRTTITypeInfo_TRTTITypeInfo_(a,b) { return a.ID==b.ID }'),
      (Name : 'TRTTITypeInfo.Name';
       Code : 'var TRTTITypeInfo$TRTTITypeInfo = { Name : function (s) { return s.ID?s.ID.name:"" } }';
       Dependency : '$GetName'),

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
       Code : 'var ArcCos = Math.acos'),
      (Name : 'ArcCosh';
       Code : 'function ArcCosh(v) { return Math.log(v+Math.sqrt((v-1)/(v+1))*(v+1)) }'),
      (Name : 'ArcSin';
       Code : 'var ArcSin = Math.asin'),
      (Name : 'ArcSinh';
       Code : 'function ArcSinh(v) { return Math.log(v+Math.sqrt(v*v+1)) }'),
      (Name : 'ArcTan';
       Code : 'var ArcTan = Math.atan'),
      (Name : 'ArcTan2';
       Code : 'var ArcTan2 = Math.atan2'),
      (Name : 'ArcTanh';
       Code : 'function ArcTanh(v) { return 0.5*Math.log((1+v)/(1-v)) }'),
      (Name : 'BoolToStr';
       Code : 'function BoolToStr(b) { return b?"True":"False" }'),
      (Name : 'ByteSizeToStr';
       Code : '';
       Dependency : '!byteSizeToStr_js' ),
      (Name : 'Ceil';
       Code : 'var Ceil = Math.ceil'),
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
      (Name : 'CompareNum$_Integer_Integer_';
       Code : 'function CompareNum$_Integer_Integer_(a,b) { return a>b?1:a<b?-1:0 }'),
      (Name : 'CompareNum$_Float_Integer_';
       Code : 'function CompareNum$_Float_Integer_(a,b) { return a>b?1:a<b?-1:0 }'),
      (Name : 'CompareNum$_Integer_Float_';
       Code : 'function CompareNum$_Integer_Float_(a,b) { return a>b?1:a<b?-1:0 }'),
      (Name : 'CompareNum$_Float_Float_';
       Code : 'function CompareNum$_Float_Float_(a,b) { return a>b?1:a<b?-1:0 }'),
      (Name : 'Copy';
       Code : 'function Copy(s,f,n) { return s.substr(f-1,n) }'),
      (Name : 'Cos';
       Code : 'var Cos = Math.cos'),
      (Name : 'Cosh';
       Code : 'function Cosh(v) { return (v==0)?1:0.5*(Math.exp(v)+Math.exp(-v)) }'),
      (Name : 'Cotan';
       Code : 'function Cotan(v) { return 1/Math.tan(v) }'),
      (Name : 'Date';
       Code : 'function $Date(v) { return Math.round(Now()-0.5) }';
       Dependency : 'Now' ),
      (Name : 'DateTimeToDate';
       Code : 'function DateTimeToDate(v) {'#13#10
               +#9'return new Date(Math.round((v-25569)*864e5));'#13#10
               +'}';
       Dependency : 'FormatDateTime' ),
      (Name : 'DateTimeToISO8601';
       Code : 'function DateTimeToISO8601(v) {'#13#10
               +#9'return FormatDateTime("yyyy-mm-dd", v, 2)+"T"+FormatDateTime("hh:nn:ss", v, 2)+"Z"'#13#10
               +'}';
       Dependency : 'FormatDateTime' ),
      (Name : 'DateTimeToRFC822';
       Code : 'function DateTimeToRFC822(v) {'#13#10
               +#9'return (new Date(Math.round((v-25569)*864e5))).toUTCString();'#13#10
               +'}' ),
      (Name : 'DateTimeToStr';
       Code : 'function DateTimeToStr(v, u) { return FormatDateTime($fmt.ShortDateFormat+" "+$fmt.LongTimeFormat, v, u) }';
       Dependency : 'FormatDateTime' ),
      (Name : 'UnixTimeToLocalDateTime';
       Code : 'function UnixTimeToLocalDateTime(t) { return t/86400+25569-(new Date(t*1e3)).getTimezoneOffset()/1440 }' ),
      (Name : 'LocalDateTimeToUnixTime';
       Code : 'function LocalDateTimeToUnixTime(t) { var ut = (t-25569)*86400; return ut+(new Date(ut*1e3)).getTimezoneOffset()*60 }' ),
      (Name : 'LocalDateTimeToUTCDateTime';
       Code : 'function LocalDateTimeToUTCDateTime(t) { return t+(new Date((t-25569)*864e5)).getTimezoneOffset()/1440 }' ),
      (Name : 'UTCDateTimeToLocalDateTime';
       Code : 'function UTCDateTimeToLocalDateTime(t) { return t-(new Date((t-25569)*864e5)).getTimezoneOffset()/1440 }' ),
      (Name : 'DateTimeToUnixTime';
       Code : 'function DateTimeToUnixTime(t) { return Math.floor((t-25569)*86400) }' ),
      (Name : 'DateToISO8601';
       Code : 'function DateToISO8601(v) {'#13#10
               +#9'return FormatDateTime("yyyy-mm-dd", v)'#13#10
               +'}';
       Dependency : 'FormatDateTime' ),
      (Name : 'DateToStr';
       Code : 'function DateToStr(v, u) { return FormatDateTime($fmt.ShortDateFormat, v, u) }';
       Dependency : 'FormatDateTime' ),
      (Name : 'DateToWeekNumber';
       Code : 'var DateToWeekNumber = WeekNumber';
       Dependency : 'WeekNumber'),
      (Name : 'DayOfMonth';
       Code : 'function DayOfMonth(v) {'#13#10
               +#9'var o=(v==0)?new Date():DateTimeToDate(v);'#13#10
               +#9'return o.getDate();'#13#10
               +'}';
       Dependency: 'DateTimeToDate'),
      (Name : 'DayOfYear';
       Code : 'function DayOfYear(v) {'#13#10
               +#9'var o=(v==0)?new Date():DateTimeToDate(v);'#13#10
               +#9'var y=o.getFullYear(), m=o.getMonth(), d=o.getDate();'#13#10
               +#9'return 1+Math.round((Date.UTC(y,m,d)-Date.UTC(y,0,1))/864e5);'#13#10
               +'}';
       Dependency: 'DateTimeToDate'),
      (Name : 'DayOfWeek';
       Code : 'function DayOfWeek(v) {'#13#10
               +#9'return DateTimeToDate(v).getDay()+1;'#13#10
               +'}';
       Dependency : 'DateTimeToDate'),
      (Name : 'DayOfTheWeek';
       Code : 'function DayOfTheWeek(v) { return ((DayOfWeek(v)+5)%7)+1 };';
       Dependency : 'DayOfWeek' ),
      (Name : 'DecodeDate';
       Code : 'function DecodeDate(dt,y,m,d,u) {'#13#10
               +#9'var o=DateTimeToDate(dt);'#13#10
               +#9'if ((u||$TZ)==2) {'#13#10
               +#9#9'y.'+TdwsJSCodeGen.cBoxFieldName+'=o.getUTCFullYear();'#13#10
               +#9#9'm.'+TdwsJSCodeGen.cBoxFieldName+'=o.getUTCMonth()+1;'#13#10
               +#9#9'd.'+TdwsJSCodeGen.cBoxFieldName+'=o.getUTCDate();'#13#10
               +#9'} else {'#13#10
               +#9#9'y.'+TdwsJSCodeGen.cBoxFieldName+'=o.getFullYear();'#13#10
               +#9#9'm.'+TdwsJSCodeGen.cBoxFieldName+'=o.getMonth()+1;'#13#10
               +#9#9'd.'+TdwsJSCodeGen.cBoxFieldName+'=o.getDate();'#13#10
               +#9'}'#13#10
               +'}';
       Dependency : 'DateTimeToDate,$TZ'),
      (Name : 'DecodeTime';
       Code : 'function DecodeTime(dt,h,m,s,z,u) {'#13#10
               +#9'var o=DateTimeToDate(dt);'#13#10
               +#9'if ((u||$TZ)==2) {'#13#10
               +#9'h.'+TdwsJSCodeGen.cBoxFieldName+'=o.getUTCHours();'#13#10
               +#9'm.'+TdwsJSCodeGen.cBoxFieldName+'=o.getUTCMinutes();'#13#10
               +#9's.'+TdwsJSCodeGen.cBoxFieldName+'=o.getUTCSeconds();'#13#10
               +#9'z.'+TdwsJSCodeGen.cBoxFieldName+'=o.getUTCMilliseconds();'#13#10
               +#9'} else {'#13#10
               +#9'h.'+TdwsJSCodeGen.cBoxFieldName+'=o.getHours();'#13#10
               +#9'm.'+TdwsJSCodeGen.cBoxFieldName+'=o.getMinutes();'#13#10
               +#9's.'+TdwsJSCodeGen.cBoxFieldName+'=o.getSeconds();'#13#10
               +#9'z.'+TdwsJSCodeGen.cBoxFieldName+'=o.getMilliseconds();'#13#10
               +#9'}'#13#10
               +'}';
       Dependency : 'DateTimeToDate'),
      (Name : 'DegToRad';
       Code : 'function DegToRad(v) { return v*(Math.PI/180) }'),
      (Name : 'Delete';
       Code : 'function Delete(s,i,n) { var v=s.'+TdwsJSCodeGen.cBoxFieldName+'; if ((i<=0)||(i>v.length)||(n<=0)) return;'
                                     +' s.'+TdwsJSCodeGen.cBoxFieldName+'=v.substr(0,i-1)+v.substr(i+n-1); }'),
      (Name : 'DivMod$_Integer_Integer_Integer_Integer_';
       Code : 'function DivMod$_Integer_Integer_Integer_Integer_(d1,d2,r1,r2) {'
               +'var r=$Div(d1, d2); r1.'+TdwsJSCodeGen.cBoxFieldName+'=r;'
               +'r2.'+TdwsJSCodeGen.cBoxFieldName+'=d1-r*d2; }';
       Dependency : '$Div' ),
      (Name : 'DupeString';
       Code : 'function DupeString(s,n) { return StringOfString(s,n) }';
       Dependency : 'StringOfString'),
      (Name : 'EncodeDate';
       Code : 'function EncodeDate(y,m,d,u) { return ( (u||$TZ)===2 ? Date.UTC(y,m-1,d) : new Date(y,m-1,d).getTime() )/864e5+25569 }';
       Dependency : '$TZ'),
      (Name : 'EncodeTime';
       Code : 'function EncodeTime(h,m,s,z) { return (h+(m+(s+z*0.001)/60)/60)/24 }'),
      (Name : 'Even';
       Code : 'function Even(v) { return (v&1)==0 }'),
      (Name : 'Exp';
       Code : 'var Exp = Math.exp'),
      (Name : 'Factorial';
       Code : 'function Factorial(i) { var r=1; while (i>1) { r*=i; i--; } return r }'),
      (Name : 'FirstDayOfMonth';
       Code : 'function FirstDayOfMonth(v, u) {'#13#10
               +#9'var o=(v===0)?new Date():DateTimeToDate(v);'#13#10
               +#9'var y=((u||$TZ)===2) ? o.getUTCFullYear() : o.getFullYear();'#13#10
               +#9'var m=((u||$TZ)===2) ? o.getUTCMonth() : o.getMonth();'#13#10
               +#9'return EncodeDate(y, m+1, 1, u)'#13#10
               +'}';
       Dependency : 'DateTimeToDate,EncodeDate' ),
      (Name : 'FirstDayOfNextMonth';
       Code : 'function FirstDayOfNextMonth(v,u) {'#13#10
               +#9'var o=(v==0)?new Date():DateTimeToDate(v);'#13#10
               +#9'var y=((u||$TZ)===2) ? o.getUTCFullYear() : o.getFullYear();'#13#10
               +#9'var m=((u||$TZ)===2) ? o.getUTCMonth() : o.getMonth();'#13#10
               +#9'if (m==11) { m=0; y++ } else m++;'#13#10
               +#9'return EncodeDate(y, m+1, 1, u)'#13#10
               +'}';
       Dependency : 'EncodeDate,DateTimeToDate' ),
      (Name : 'FirstDayOfNextYear';
       Code : 'function FirstDayOfNextYear(v,u) {'#13#10
               +#9'var o=(v==0)?new Date():DateTimeToDate(v);'#13#10
               +#9'var y=((u||$TZ)===2) ? o.getUTCFullYear() : o.getFullYear();'#13#10
               +#9'return EncodeDate(y+1,1,1,u)'#13#10
               +'}';
       Dependency : 'EncodeDate,DateTimeToDate' ),
      (Name : 'FirstDayOfYear';
       Code : 'function FirstDayOfYear(v,u) {'#13#10
               +#9'var o=(v==0)?new Date():DateTimeToDate(v);'#13#10
               +#9'var y=((u||$TZ)===2) ? o.getUTCFullYear() : o.getFullYear();'#13#10
               +#9'return EncodeDate(y,1,1)'#13#10
               +'}';
       Dependency : 'EncodeDate,DateTimeToDate' ),
      (Name : 'FirstDayOfWeek';
       Code : 'function FirstDayOfWeek(v,u) {'#13#10
               +#9'var o=DateTimeToDate(v);'#13#10
               +#9'var d=((u||$TZ)===2) ? o.getUTCDay() : o.getDay();'#13#10
               +#9'return (d==0)?v-6:v-(d-1)'#13#10
               +'}';
       Dependency : 'DateTimeToDate' ),
      (Name : 'FloatToStr';
       Code : 'function FloatToStr$_Float_(i) { return i.toString() }'#13#10
              +'function FloatToStr$_Float_Integer_(i,p) { return (p==99)?i.toString():i.toFixed(p) }'),
      (Name : 'FloatToStr$_Float_';
       Code : '';
       Dependency : 'FloatToStr'),
      (Name : 'FloatToStr$_Float_Integer_';
       Code : '';
       Dependency : 'FloatToStr'),
      (Name : 'Floor';
       Code : 'var Floor = Math.floor'),
      (Name : 'Format';
       Code : 'function Format(f,a) { a.unshift(f); return sprintf.apply(null,a) }';
       Dependency : '!sprintf_js'),
      (Name : 'FormatDateTime';
       Code : '';
       Dependency : '!formatDateTime_js'; Dependency2 : '$TZ' ),
      (Name : 'Frac';
       Code : 'function Frac(v) { return v-((v>0)?Math.floor(v):Math.ceil(v)) }'),
      (Name : 'FindDelimiter';
       Code : 'function FindDelimiter(d,s,x) { var n=d.length,r=ns=s.length,i,p; for (i=0;i<n;i++) { p=s.indexOf(d.charAt(i),x-1); if (p>=0&&p<r) r=p; } return (r==ns)?-1:r+1; }'),
      (Name : 'Gcd$_Integer_Integer_';
       Code : 'function Gcd$_Integer_Integer_(a, b) { var r; while (b!=0) { r=a%b; a=b; b=r; } return a }'),
      (Name : 'HexToInt';
       Code : 'function HexToInt(v) {'#13#10
               +#9'var r=parseInt(v,16);'#13#10
               +#9'if (isFinite(r)) return r;'#13#10
               +#9'throw Exception.Create($New(Exception),"''"+v.toString()+"'' is not a valid hexadecimal value");'#13#10
               +'}';
       Dependency : 'Exception' ),
      (Name : 'Hypot';
       Code : 'function Hypot(x,y) { return Math.sqrt(x*x+y*y) }'),
      (Name : 'IncMonth';
       Code : 'function IncMonth(v, n, u) {'#13#10
               +#9'var utc=(u||$TZ)===2, o=DateTimeToDate(v), y, m, b;'#13#10
               +#9'if (utc) {'#13#10
               +#9#9'y=o.getUTCFullYear(); m=o.getUTCMonth(); b=Date.UTC(y,m,1);'#13#10
               +#9'} else {'#13#10
               +#9#9'y=o.getFullYear(); m=o.getMonth(); b=new Date(y,m,1).getTime();'#13#10
               +#9'}'#13#10
               +#9'm+=n;'#13#10
               +#9'while (m>=12) { m-=12; y++ };'#13#10
               +#9'while (m<0) { m+=12; y-- };'#13#10
               +#9'return v+((utc?Date.UTC(y,m,1):new Date(y,m,1).getTime())-b)/864e5'#13#10
               +'}';
       Dependency : 'DateTimeToDate,$TZ' ),
      (Name : 'Insert';
       Code : 'function Insert(s,d,i) { var v=d.'+TdwsJSCodeGen.cBoxFieldName+'; if (s=="") return; if (i<1) i=1; if (i>v.length) i=v.length+1;'
               +'d.'+TdwsJSCodeGen.cBoxFieldName+'=v.substr(0,i-1)+s+v.substr(i-1); }'),
      (Name : 'Int';
       Code : 'function Int(v) { return (v>0)?Math.floor(v):Math.ceil(v) }'),
      (Name : 'IntPower$_Float_Integer_';
       Code : 'var IntPower$_Float_Integer_ = Math.pow'),
      (Name : 'ISO8601ToDateTime';
       Code : 'function ISO8601ToDateTime(s) {'#13#10
              +#9'var r = Date.parse(s)/864e5+25569;'#13#10
              +#9'if (isNaN(r)) throw Exception.Create($New(Exception),"Invalid ISO8601")'#13#10
              +#9'return r;'#13#10
              +'}';
       Dependency : 'Exception' ),
      (Name : 'IntToBin';
       Code : 'function IntToBin(v,d) { var r=v.toString(2); while (r.length<d) r="0"+r; return r }'),
      (Name : 'IntToHex';
       Code : 'function IntToHex(v,d) { var r=v.toString(16); return "00000000".substr(0, d-r.length)+r }'),
      (Name : 'IntToHex2';
       Code : 'function IntToHex2(v) { var r=v.toString(16); return (r.length==1)?"0"+r:r }'),
      (Name : 'IntToStr';
       Code : 'function IntToStr(i) { return i.toString() }'),
      (Name : 'IsDelimiter';
       Code : 'function IsDelimiter(d,s,i) { if ((i<=0)||(i>s.length)) return false; else return d.indexOf(s.charAt(i-1))>=0; }'),
      (Name : 'IsLeapYear';
       Code : 'function IsLeapYear(y) { return !(y % 4) && (y % 100) || !(y % 400) ? true : false; }'),
      (Name : 'IsPrime$_Integer_';
       Code : 'function IsPrime$_Integer_(n) { if (n<=3) { return (n>=2) } else return ((n&1)&&(LeastFactor(n)==n)) }';
       Dependency : 'LeastFactor' ),
      (Name : 'Lcm$_Integer_Integer_';
       Code : 'function Lcm$_Integer_Integer_(a, b) { var g=Gcd$_Integer_Integer_(a,b); return (g!=0)?(Math.floor(a/g)*b):0 }';
       Dependency : 'Gcd$_Integer_Integer_' ),
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
       Code : 'var Ln = Math.log'),
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
      (Name : 'MaxInt$_';
       Code : 'function MaxInt$_() { return 9007199254740991 };'),
      (Name : 'Max$_Float_Float_';
       Code : 'function Max$_Float_Float_(a,b) { return (a>b)?a:b }'),
      (Name : 'Max$_Integer_Integer_';
       Code : 'function Max$_Integer_Integer_(a,b) { return (a>b)?a:b }'),
      (Name : 'MaxInt$_Integer_Integer_';
       Code : 'function MaxInt$_Integer_Integer_(a,b) { return (a>b)?a:b }'),
      (Name : 'MidStr';
       Code : 'function MidStr(s,f,n) { return s.substr(f-1,n) }'),
      (Name : 'Min$_Float_Float_';
       Code : 'function Min$_Float_Float_(a,b) { return (a<b)?a:b }'),
      (Name : 'Min$_Integer_Integer_';
       Code : 'function Min$_Integer_Integer_(a,b) { return (a<b)?a:b }'),
      (Name : 'MinInt';
       Code : 'function MinInt(a,b) { return (a<b)?a:b }'),
      (Name : 'MonthOfYear';
       Code : 'function MonthOfYear(v) {'#13#10
               +#9'var o=(v==0)?new Date():DateTimeToDate(v);'#13#10
               +#9'return o.getUTCMonth()+1;'#13#10
               +'}';
       Dependency : 'DateTimeToDate'),
      (Name : 'NormalizeString';
       Code : 'function NormalizeString(s,f) { return s.normalize(f) }'),
      (Name : 'Now';
       Code : 'function Now() { var d=new Date(); return d.getTime()/864e5+25569 }'),
      (Name : 'Odd$_Integer_';
       Code : 'function Odd$_Integer_(v) { return (v&1)==1 }'),
      (Name : 'Pi';
       Code : 'function Pi() { return Math.PI }'),
      (Name : 'NaN';
       Code : 'function $NaN() { return NaN }'),
      (Name : 'Infinity';
       Code : 'function $Infinity() { return Infinity }'),
      (Name : 'IsNaN';
       Code : 'function IsNaN(v) { return isNaN(v) }'),
      (Name : 'IsFinite';
       Code : 'function IsFinite(v) { return isFinite(v) }'),
      (Name : 'IsInfinite';
       Code : 'function IsInfinite(v) { return !(isNaN(v)||isFinite(v)) }'),
      (Name : 'PadLeft';
       Code : 'function PadLeft(s,n,c) { n-=s.length; return n>0 ? StringOfString(c||" ", n)+s : s }';
       Dependency : 'StringOfString' ),
      (Name : 'PadRight';
       Code : 'function PadRight(s,n,c) { n-=s.length; return n>0 ? s+StringOfString(c||" ", n) : s }';
       Dependency : 'StringOfString' ),
      (Name : 'ParseDateTime';
       Code : 'function ParseDateTime(f, s, u) { return strToDateTimeDef(f, s, 0, u) }';
       Dependency : '!strToDateTimeDef_js'; Dependency2 : 'FormatDateTime'),
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
      (Name : 'RFC822ToDateTime';
       Code : 'function RFC822ToDateTime(v) { return Date.parse(s)/864e5+25569 }'),
      (Name : 'RightStr';
       Code : 'function RightStr(s,n) { return s.substr(s.length-n) }'),
      (Name : 'Round';
       Code : 'var Round = Math.round'),
      (Name : 'SameText';
       Code : 'function SameText(a,b) { return a.toUpperCase()==b.toUpperCase() }'),
      (Name : 'SetLength';
       Code : 'function SetLength(s,n) { if (s.'+TdwsJSCodeGen.cBoxFieldName+'.length>n) s.'+TdwsJSCodeGen.cBoxFieldName+'=s.'+TdwsJSCodeGen.cBoxFieldName+'.substring(0,n);'
                                       +'else while (s.'+TdwsJSCodeGen.cBoxFieldName+'.length<n) s.'+TdwsJSCodeGen.cBoxFieldName+'+=" "; }'),
      (Name : 'SetRandSeed';
       Code : 'function SetRandSeed(v) { Random = $alea(v) }';
       Dependency : 'Random'),
      (Name : 'Sign$_Float_';
       Code : 'var Sign$_Float_ = $Sign';
       Dependency : '$Sign'),
      (Name : 'Sign$_Integer_';
       Code : 'var Sign$_Integer_ = $Sign';
       Dependency : '$Sign'),
      (Name : 'Sin';
       Code : 'var Sin = Math.sin'),
      (Name : 'Sinh';
       Code : 'function Sinh(v) { return (v==0)?0:(0.5*(Math.exp(v)-Math.exp(-v))) }'),
      (Name : 'Sleep';
       Code : 'function Sleep(v) { for(v+=Date.now();Date.now()>v;); }'),
      (Name : 'StrAfter';
       Code : 'function StrAfter(s,d) { if (!d) return ""; var p=s.indexOf(d); return (p<0)?"":s.substr(p+d.length) }'),
      (Name : 'StrAfterLast';
       Code : 'function StrAfterLast(s,d) { if (!d) return ""; var p=s.lastIndexOf(d); return (p<0)?"":s.substr(p+d.length) }'),
      (Name : 'StrBefore';
       Code : 'function StrBefore(s,d) { if (!d) return s; var p=s.indexOf(d); return (p<0)?s:s.substr(0, p) }'),
      (Name : 'StrBeforeLast';
       Code : 'function StrBeforeLast(s,d) { if (!d) return s; var p=s.lastIndexOf(d); return (p<0)?s:s.substr(0, p) }'),
      (Name : 'StrBetween';
       Code : 'function StrBetween(s,d,f) { return StrBefore(StrAfter(s, d), f) }';
       Dependency: 'StrAfter'; Dependency2: 'StrBefore'),
      (Name : 'StrBeginsWith';
       Code : 'function StrBeginsWith(s,b) { return (b.length > 0) ? s.substr(0, b.length)==b : false }'),
      (Name : 'StrContains';
       Code : 'function StrContains(s,b) { return s.indexOf(b)>=0 }'),
      (Name : 'StrDeleteLeft';
       Code : 'function StrDeleteLeft(s,n) { return s.substring(n) }'),
      (Name : 'StrDeleteRight';
       Code : 'function StrDeleteRight(s,n) { return s.substr(0, s.length-n) }'),
      (Name : 'StrEndsWith';
       Code : 'function StrEndsWith(s,e) { return s.substr(s.length-e.length)==e }'),
      (Name : 'StrFind';
       Code : 'function StrFind(s,e,i) { return e ? s.indexOf(e, i-1)+1 : 0 }'),
      (Name : 'StringOfChar';
       Code : 'function StringOfChar(c,n) { return StringOfString(c?c.charAt(0):" ",n) }';
       Dependency : 'StringOfString'),
      (Name : 'StringOfString';
       Code : 'function StringOfString(s,n) {'
                  + 'if (n<1) return "";'
                  + 'var r="";'
                  + 'while (n>0) {'
                     + 'if (n&1) r+=s;'
                     + 'n>>=1; s+=s;'
                  + '}'
                  + 'return r'
               + '}'),
      (Name : 'StrJoin';
       Code : 'function StrJoin(a,d) { return a.join(d) }'),
      (Name : 'StrRegExp';
       Code : 'function StrRegExp(s) { return s.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&") }'),
      (Name : 'StrReplace';
       Code : 'function StrReplace(s,o,n) { return o?s.replace(new RegExp(StrRegExp(o), "g"), n):s }';
       Dependency : 'StrRegExp'),
      (Name : 'StrSplit';
       Code : 'function StrSplit(s,d) { return s.split(d) }'),
      (Name : 'StrToBool';
       Code : 'function StrToBool(s) { return (/^(t|y|1|true|yes)$/i).test(s) }'),
      (Name : 'StrToDate';
       Code : 'function StrToDate(s,u) { return strToDateTimeDef($fmt.ShortDateFormat, s, 0, u) }';
       Dependency : '!strToDateTimeDef_js'; Dependency2 : 'FormatDateTime'),
      (Name : 'StrToDateDef';
       Code : 'function StrToDateDef(s,d,u) { return strToDateTimeDef($fmt.ShortDateFormat, s, d, u) }';
       Dependency : '!strToDateTimeDef_js'; Dependency2 : 'FormatDateTime'),
      (Name : 'StrToDateTime';
       Code : 'function StrToDateTime(s,u) { return strToDateTimeDef($fmt.ShortDateFormat+" "+$fmt.LongTimeFormat, s, 0, u) || strToDateTimeDef($fmt.ShortDateFormat, s, 0, u) }';
       Dependency : '!strToDateTimeDef_js'; Dependency2 : 'FormatDateTime'),
      (Name : 'StrToDateTimeDef';
       Code : 'function StrToDateTimeDef(s,d,u) { return strToDateTimeDef($fmt.ShortDateFormat+" "+$fmt.LongTimeFormat, s, 0, u) || strToDateTimeDef($fmt.ShortDateFormat, s, d, u) }';
       Dependency : '!strToDateTimeDef_js'; Dependency2 : 'FormatDateTime'),
      (Name : 'StrToFloat';
       Code : 'function StrToFloat(v) { return parseFloat(v) }'),
      (Name : 'StrToFloatDef';
       Code : 'function StrToFloatDef(v,d) { var r=parseFloat(v); return isNaN(r)?d:r }'),
      (Name : 'StrToHtml';
       Code : 'function StrToHtml(v) { return v.replace(/[&<>"'']/g, StrToHtml.e) }'#13#10
              +'StrToHtml.e = function(c) { return { "&":"&amp;", "<":"&lt;", ">":"&gt;", ''"'':"&quot;", "''":"&#39;" }[c] }' ),
      (Name : 'StrToHtmlAttribute';
       Code : 'function StrToHtmlAttribute(v) {'#13#10
              + #9'for (var r = "", i = 0; i < v.length; i++) {'#13#10
                 + #9#9'var c = v.charCodeAt(i);'#13#10
                 + #9#9'if ((c >= 64 && c < 91) || (c > 96 && c < 123) || (c > 47 && c < 58) || c > 127) {'#13#10
                    + #9#9#9'r += v.charAt(i);'#13#10
                 + #9#9'} else {'#13#10
                    + #9#9#9'r += "&#" + c + ";";'#13#10
                 + #9#9'}'#13#10
              + #9'}'#13#10
              + #9'return r;'#13#10
              + '}';
       Dependency : 'StrToHtml' ),
      (Name : 'StrToInt';
       Code : 'function StrToInt(v) { return parseInt(v,10) }'),
      (Name : 'StrToIntDef';
       Code : 'function StrToIntDef(v,d) { var r=parseInt(v,10); return isNaN(r)?d:r }'),
      (Name : 'StrToJSON';
       Code : 'function StrToJSON(v) { return JSON.stringify(v) }'),
      (Name : 'StrMatches';
       Code : 'function StrMatches(v,r) {'#13#10
                  +#9'if (!r) return false;'#13#10
                  +#9'var f = r.split("");'#13#10
                  +#9'for (var i=0; i<f.length; i++) {'#13#10
                     +#9#9'if (/\\|\^|\$|\+|\.|\(|\)|\!|\{|\}|\[|\]|\|/g.test(f[i])) {'#13#10
                        +#9#9#9'f[i]="\\"+f[i]'#13#10
                     +#9#9'} else if (f[i]=="*") {'#13#10
                        +#9#9#9'f[i]="[\\s\\S]*"'#13#10
                     +#9#9'} else if (f[i]=="?") {'#13#10
                        +#9#9#9'f[i]="[\\s\\S]"'#13#10
                     +#9#9'}'#13#10
                  +#9'}'#13#10
                  +#9'return (new RegExp(f.join(""), "g")).test(v)'#13#10
               +'}'#13#10),
      (Name : 'StrToTime';
       Code : 'function StrToTime(s) { return strToDateTimeDef($fmt.LongTimeFormat, s, 0)||strToDateTimeDef($fmt.ShortTimeFormat, s, 0) }';
       Dependency : '!strToDateTimeDef_js'; Dependency2 : '$TZ'),
      (Name : 'StrToTimeDef';
       Code : 'function StrToTimeDef(s,d) { return strToDateTimeDef($fmt.LongTimeFormat, s, 0)||strToDateTimeDef($fmt.ShortTimeFormat, s, 0)||d }';
       Dependency : '!strToDateTimeDef_js'; Dependency2 : '$TZ'),
      (Name : 'StrToXML';
       Code : 'function StrToXML(v) { return v.replace(/[&<>"'']/g, StrToXML.e) }'#13#10
              +'StrToXML.e = function(c) { return { "&":"&amp;", "<":"&lt;", ">":"&gt;", ''"'':"&quot;", "''":"&apos;" }[c] }' ),
      (Name : 'SubStr';
       Code : 'function SubStr(s,f) { return s.substr(f-1) }'),
      (Name : 'SubString';
       Code : 'function SubString(s,f,t) { return s.substr(f-1, t-2) }'),
      (Name : 'Sqr$_Integer_';
       Code : 'function Sqr$_Integer_(v) { return v*v }'),
      (Name : 'Sqr$_Float_';
       Code : 'function Sqr$_Float_(v) { return v*v }'),
      (Name : 'Sqrt';
       Code : 'var Sqrt = Math.sqrt'),
      (Name : 'StripAccents';
       Code : 'function StripAccents(s) { return s.normalize("NFD").replace(/[\u0300-\u036f]/g,"") }'),
      (Name : 'Tan';
       Code : 'var Tan = Math.tan'),
      (Name : 'Tanh';
       Code : 'function Tanh(v) { return (v==0)?0:(Math.exp(v)-Math.exp(-v))/(Math.exp(v)+Math.exp(-v)) }'),
      (Name : 'Time';
       Code : 'function Time() { var r=Now(); return r-Math.floor(r) }';
       Dependency : 'Now'),
      (Name : 'TimeToStr';
       Code : 'function TimeToStr(v) { return FormatDateTime($fmt.LongTimeFormat, v) }';
       Dependency : 'FormatDateTime' ),
      (Name : 'Trim$_String_';
       Code : 'function Trim$_String_(s) { return s.replace(/^\s\s*/, "").replace(/\s\s*$/, "") }'),
      (Name : 'Trim$_String_Integer_Integer_';
       Code : 'function Trim$_String_Integer_Integer_(s,a,b) { if (a<0) a=0; if (b<0) b=0; return s.substr(a,s.length-a-b) }'),
      (Name : 'TrimLeft';
       Code : 'function TrimLeft(s) { return s.replace(/^\s\s*/, "") }'),
      (Name : 'TrimRight';
       Code : 'function TrimRight(s) { return s.replace(/\s\s*$/, "") }'),
      (Name : 'Trunc';
       Code : 'function Trunc(v) { return (v>=0)?Math.floor(v):Math.ceil(v) }'),
      (Name : 'UnixTime';
       Code : 'function UnixTime() { return Math.floor(Date.now()*1e-3) }'),
      (Name : 'UnixTimeToDateTime';
       Code : 'function UnixTimeToDateTime(t) { return t/86400+25569 }'),
      (Name : 'Unsigned32';
       Code : 'function Unsigned32(v) { return v>>>0 }'),
      (Name : 'UpperCase';
       Code : 'function UpperCase(v) { return v.toUpperCase() }'),
      (Name : 'ASCIIUpperCase';
       Code : 'function ASCIIUpperCase(v) { return v.toUpperCase() }'),
      (Name : 'ASCIILowerCase';
       Code : 'function ASCIILowerCase(v) { return v.toLowerCase() }'),
      (Name : 'UTCDateTime';
       Code : 'function UTCDateTime() { var d=new Date(); return d.getTime()/8.64e7+25569 }'),
      (Name : 'VarAsType';
       Code : 'function VarAsType(v,t) {'#13#10
               +#9'switch (t) {'#13#10
                  +#9#9'case 258: return String(v);'#13#10 // varUString
                  +#9#9'case 3: case 20: return parseInt(v);'#13#10 // varInteger, varInt64
                  +#9#9'case 5: return parseFloat(v);'#13#10 // varDouble
                  +#9#9'case 0xB: return v?true:false;'#13#10 // varBoolean
                  +#9#9'default : return v;'#13#10
               +#9'}'#13#10
               +'}'),
      (Name : 'VarClear$_Variant_';
       Code : 'function VarClear$_Variant_(v) { var u; v.v=u }'),
      (Name : 'VarIsArray';
       Code : 'function VarIsArray(v) { return Array.isArray(v) }'),
      (Name : 'VarIsClear';
       Code : 'function VarIsClear(v) { return (v===undefined) }'),
      (Name : 'VarIsEmpty';
       Code : 'function VarIsEmpty(v) { return (typeof v === "undefined") }'),
      (Name : 'VarIsNull';
       Code : 'function VarIsNull(v) { return (v===null) }'),
      (Name : 'VarIsStr';
       Code : 'function VarIsStr(v) { return typeof v === "string" }'),
      (Name : 'VarToStr';
       Code : 'function VarToStr(v) { return (typeof v === "undefined")?"":v.toString() }'),
      (Name : '$VarToBool';
       Code : 'function $VarToBool(v) { return !!(typeof v == "string" ? {"1":1,"t":1,"y":1,"true":1}[v.toLowerCase()] : v) }'),
      (Name : 'VarToFloatDef';
       Code : 'function VarToFloatDef(v, d) {'#13#10
               + #9'if (v == null) return v === undefined ? d : 0;'#13#10
               + #9'var r = parseFloat(v);'#13#10
               + #9'if (!isNaN(r)) return r;'#13#10
               + #9'if (v === true) return 1;'#13#10
               + #9'if (v === false) return 0;'#13#10
               + #9'return d;'#13#10
               + '}'),
      (Name : 'VarType';
       Code : 'function VarType(v) {'#13#10
               +#9'switch (Object.prototype.toString.call(v)) {'#13#10
                  +#9#9'case "[object Undefined]": return 0;'#13#10 // varEmpty
                  +#9#9'case "[object String]": return 258;'#13#10 // varUString
                  +#9#9'case "[object Number]": return (parseInt(v)==v)?20:5;'#13#10 // varInteger/varDouble
                  +#9#9'case "[object Boolean]": return 0xB;'#13#10 // varBoolean
                  +#9#9'default : return 0xC;'#13#10 // varVariant
               +#9'}'#13#10
               +'}'),
      (Name : 'YearOf';
       Code : 'function YearOf(v) {'#13#10
               +#9'var o=(v==0)?new Date():DateTimeToDate(v);'#13#10
               +#9'return o.getFullYear();'#13#10
               +'}';
       Dependency : 'WeekNumber,DateTimeToDate'),
      (Name : 'YearOfWeek';
       Code : 'function YearOfWeek(v) {'#13#10
               +#9'var o=(v==0)?new Date():DateTimeToDate(v);'#13#10
               +#9'var y=o.getFullYear();'#13#10
               +#9'var m=o.getMonth()+1;'#13#10
               +#9'var d=o.getDate();'#13#10
               +#9'if ((m==1)&&(d<4)) {'#13#10
                  +#9#9'return (WeekNumber(v)==1)?y:y-1;'#13#10
               +#9'} else if ((m==12)&&(d>=29)) {'#13#10
                  +#9#9'return (WeekNumber(v)==1)?y+1:y;'#13#10
               +#9'} else return y;'#13#10
               +'}';
       Dependency : 'WeekNumber,DateTimeToDate'),
      (Name : 'WeekNumber';
       Code : 'function WeekNumber(v) {'#13#10
               +#9'if (v==0) v=Now();'#13#10
               +#9'var weekDay=((DayOfWeek(v)+5)%7)+1;'#13#10
               +#9'v+=4-weekDay;'#13#10
               +#9'var o=DateTimeToDate(v);'#13#10
               +#9'return Math.floor((Math.floor(v)-EncodeDate(o.getFullYear(), 1, 1))/7)+1;'#13#10
               +'}';
       Dependency : 'DayOfWeek,Now,EncodeDate,DateTimeToDate')

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
      Result:=wobs.ToUnicodeString;
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
constructor TJSMagicFuncExpr.Create(const codeGen : TdwsCodeGen);
begin
   inherited Create;
   FMagicCodeGens:=TStringList.Create;
   FMagicCodeGens.CaseSensitive:=False;
   FMagicCodeGens.Sorted:=True;
   FMagicCodeGens.Duplicates:=dupError;

   FMagicCodeGens.AddObject('_', TJSGetTextFuncExpr.Create(codeGen));
   FMagicCodeGens.AddObject('AnsiLowerCase', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.toLocaleLowerCase()']));
   FMagicCodeGens.AddObject('AnsiUpperCase', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.toLocaleUpperCase()']));
   FMagicCodeGens.AddObject('ArcCos', TdwsExprGenericCodeGen.Create(['Math.acos', '(', 0, ')']));
   FMagicCodeGens.AddObject('ArcSin', TdwsExprGenericCodeGen.Create(['Math.asin', '(', 0, ')']));
   FMagicCodeGens.AddObject('ArcTan', TdwsExprGenericCodeGen.Create(['Math.atan', '(', 0, ')']));
   FMagicCodeGens.AddObject('ArcTan2', TdwsExprGenericCodeGen.Create(['Math.atan2', '(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Ceil', TdwsExprGenericCodeGen.Create(['Math.ceil', '(', 0, ')']));
   FMagicCodeGens.AddObject('Copy', TJSStrCopyFuncExpr.Create);
   FMagicCodeGens.AddObject('Cos', TdwsExprGenericCodeGen.Create(['Math.cos', '(', 0, ')']));
   FMagicCodeGens.AddObject('MidStr', TJSStrCopyFuncExpr.Create);
   FMagicCodeGens.AddObject('Exp', TdwsExprGenericCodeGen.Create(['Math.exp', '(', 0, ')']));
   FMagicCodeGens.AddObject('FloatToStr$_Float_', TJSFloatToStrExpr.Create);
   FMagicCodeGens.AddObject('FloatToStr$_Float_Integer_', TJSFloatToStrExpr.Create);
   FMagicCodeGens.AddObject('Floor', TdwsExprGenericCodeGen.Create(['Math.floor', '(', 0, ')']));
   FMagicCodeGens.AddObject('Format', TJSFormatExpr.Create);
   FMagicCodeGens.AddObject('HexToInt', TdwsExprGenericCodeGen.Create(['parseInt', '(', 0, ',','16)']));
   FMagicCodeGens.AddObject('Infinity', TdwsExprGenericCodeGen.Create(['Infinity']));
   FMagicCodeGens.AddObject('IntPower', TdwsExprGenericCodeGen.Create(['Math.pow', '(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('IntToHex', TJSIntToHexExpr.Create);
   FMagicCodeGens.AddObject('IntToStr', TJSIntToStrExpr.Create);
   FMagicCodeGens.AddObject('IsFinite', TdwsExprGenericCodeGen.Create(['isFinite', '(', 0, ')']));
   FMagicCodeGens.AddObject('IsNaN', TdwsExprGenericCodeGen.Create(['isNaN', '(', 0, ')']));
   FMagicCodeGens.AddObject('LeftStr', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.substr(0,', 1, ')']));
   FMagicCodeGens.AddObject('Ln', TdwsExprGenericCodeGen.Create(['Math.log', '(', 0, ')']));
   FMagicCodeGens.AddObject('LowerCase', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.toLowerCase()']));
   FMagicCodeGens.AddObject('Max$_Float_Float_', TdwsExprGenericCodeGen.Create(['Math.max','(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Max$_Integer_Integer_', TdwsExprGenericCodeGen.Create(['Math.max', '(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Min$_Float_Float_', TdwsExprGenericCodeGen.Create(['Math.min','(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Min$_Integer_Integer_', TdwsExprGenericCodeGen.Create(['Math.min', '(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('MaxInt', TdwsExprGenericCodeGen.Create(['Math.max','(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('MinInt', TdwsExprGenericCodeGen.Create(['Math.min', '(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('NaN', TdwsExprGenericCodeGen.Create(['NaN']));
   FMagicCodeGens.AddObject('NormalizeString', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.normalize', '(', 1, ')']));
   FMagicCodeGens.AddObject('Pi', TdwsExprGenericCodeGen.Create(['Math.PI']));
   FMagicCodeGens.AddObject('Pos', TdwsExprGenericCodeGen.Create(['(', 1, '.indexOf', '(', 0, ')', '+1)']));
   FMagicCodeGens.AddObject('PosEx', TdwsExprGenericCodeGen.Create(['(', 1, '.indexOf', '(', 0, ',', '(', 2, ')', '-1)+1)']));
   FMagicCodeGens.AddObject('Power', TdwsExprGenericCodeGen.Create(['Math.pow', '(', 0, ',', 1, ')']));
   FMagicCodeGens.AddObject('Round', TdwsExprGenericCodeGen.Create(['Math.round', '(', 0, ')']));
   FMagicCodeGens.AddObject('Sign$_Float_', TdwsExprGenericCodeGen.Create(['$Sign', '(', 0, ')'], gcgExpression, '$Sign'));
   FMagicCodeGens.AddObject('Sign$_Integer_', TdwsExprGenericCodeGen.Create(['$Sign', '(', 0, ')'], gcgExpression, '$Sign'));
   FMagicCodeGens.AddObject('Sin', TdwsExprGenericCodeGen.Create(['Math.sin', '(', 0, ')']));
   FMagicCodeGens.AddObject('Sqr$_Integer_', TJSSqrMagicExpr.Create);
   FMagicCodeGens.AddObject('Sqr$_Float_', TJSSqrMagicExpr.Create);
   FMagicCodeGens.AddObject('Sqrt', TdwsExprGenericCodeGen.Create(['Math.sqrt', '(', 0, ')']));
   FMagicCodeGens.AddObject('StrBeginsWith', TJSStrBeginsWithExpr.Create);
   FMagicCodeGens.AddObject('StrDeleteLeft', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.substring', '(', 1, ')']));
   // slice not very efficient in browsers right now
   // FMagicCodeGens.AddObject('StrDeleteRight', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.slice', '(0,-', '(', 1, ')', ')']));
   FMagicCodeGens.AddObject('StrContains', TdwsExprGenericCodeGen.Create(['(', '(', 0, ')', '.indexOf', '(', 1, ')', '>=0)']));
   FMagicCodeGens.AddObject('StrFind', TJSStrFindExpr.Create);
   FMagicCodeGens.AddObject('StrJoin', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.join', '(', 1, ')']));
   FMagicCodeGens.AddObject('StrMatches', TJSStrMatchesFuncExpr.Create);
   FMagicCodeGens.AddObject('StrReplace', TJSStrReplaceFuncExpr.Create);
   FMagicCodeGens.AddObject('StrSplit', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.split', '(', 1, ')']));
   FMagicCodeGens.AddObject('StrToFloat', TdwsExprGenericCodeGen.Create(['parseFloat', '(', 0, ')']));
   FMagicCodeGens.AddObject('StrToInt', TdwsExprGenericCodeGen.Create(['parseInt', '(', 0, ',', '10)']));
   FMagicCodeGens.AddObject('StrToJSON', TdwsExprGenericCodeGen.Create(['JSON.stringify', '(', 0, ')']));
   FMagicCodeGens.AddObject('SubStr', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.substr(', '(', 1, ')', '-1)']));
   FMagicCodeGens.AddObject('SubString', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.substr(', '(', 1, ')', '-1,', '(', 2, ')', '-2)']));
   FMagicCodeGens.AddObject('Tan', TdwsExprGenericCodeGen.Create(['Math.tan', '(', 0, ')']));
   FMagicCodeGens.AddObject('TypeOf$_TClass_', TdwsExprGenericCodeGen.Create([0]));
   FMagicCodeGens.AddObject('Unsigned32', TdwsExprGenericCodeGen.Create(['(', 0, '>>>0', ')']));
   FMagicCodeGens.AddObject('UpperCase', TdwsExprGenericCodeGen.Create(['(', 0, ')', '.toUpperCase()']));
   FMagicCodeGens.AddObject('VarIsArray', TdwsExprGenericCodeGen.Create(['Array.isArray', '(', 0 , ')']));
   FMagicCodeGens.AddObject('VarIsClear', TdwsExprGenericCodeGen.Create(['(', 0 , '===undefined', ')']));
   FMagicCodeGens.AddObject('VarIsNull', TdwsExprGenericCodeGen.Create(['(', 0 , '===null', ')']));
   FMagicCodeGens.AddObject('VarIsStr', TdwsExprGenericCodeGen.Create(['(', 'typeof ', '(', 0 , ')', '==="string"', ')']));
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
var
   e : TMagicFuncExpr;
   name : String;
   i : Integer;
begin
   e:=TMagicFuncExpr(expr);
   if e.FuncSym.IsOverloaded then
      name:=TJSFuncBaseExpr.GetSignature(e.FuncSym)
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
   name:=CanonicalName(name);
   codeGen.WriteString(name);
   codeGen.Dependencies.Add(name);
end;

// RegisterAlias
//
class procedure TJSMagicFuncExpr.RegisterAlias(const aliasName, canonicalName : String);
begin
   vAliases.Values[aliasName]:=canonicalName;
end;

// CanonicalName
//
class function TJSMagicFuncExpr.CanonicalName(const aName : String) : String;
var
   i : Integer;
begin
   i:=vAliases.IndexOfName(aName);
   if i>=0 then
      Result:=vAliases.ValueFromIndex[i]
   else Result:=aName;
end;

// ------------------
// ------------------ TJSSqrMagicExpr ------------------
// ------------------

// CodeGen
//
procedure TJSSqrMagicExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMagicFuncExpr;
begin
   e:=TMagicFuncExpr(expr);
   codeGen.WriteString('(');
   TJSSqrExpr.CodeGenSqr(codeGen, e.Args[0] as TTypedExpr);
   codeGen.WriteString(')');
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

   if e.Args.Count=1 then begin

      codeGen.Compile(e.Args[0] as TTypedExpr);
      codeGen.WriteString('.toString()');

   end else if e.Args[1] is TConstIntExpr then begin

      codeGen.WriteString('(');
      codeGen.CompileNoWrap(e.Args[0] as TTypedExpr);

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

      codeGen.WriteString('FloatToStr$_Float_Integer_(');
      codeGen.CompileNoWrap(e.Args[0] as TTypedExpr);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.Args[1] as TTypedExpr);
      codeGen.WriteString(')');

   end;
end;

// ------------------
// ------------------ TJSIntToHexExpr ------------------
// ------------------

// CodeGen
//
procedure TJSIntToHexExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMagicFuncExpr;
   a : TExprBase;
begin
   e:=TMagicFuncExpr(expr);

   a:=e.Args[1];
   if a is TConstIntExpr then begin

      case TConstIntExpr(a).Value of

         0..1 : begin
            codeGen.Compile(e.Args[0]);
            codeGen.WriteString('.toString(16)');
            exit;
         end;
         2 : begin
            codeGen.Dependencies.Add('IntToHex2');
            
            codeGen.WriteString('IntToHex2(');
            codeGen.CompileNoWrap(e.Args[0] as TTypedExpr);
            codeGen.WriteString(')');
            exit;
         end;
         
      end;

   end;
      
   codeGen.Dependencies.Add('IntToHex');

   codeGen.WriteString('IntToHex(');
   codeGen.CompileNoWrap(e.Args[0] as TTypedExpr);
   codeGen.WriteString(',');
   codeGen.CompileNoWrap(e.Args[1] as TTypedExpr);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSIntToStrExpr ------------------
// ------------------

// CodeGen
//
procedure TJSIntToStrExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMagicFuncExpr;
   a : TExprBase;
begin
   e:=TMagicFuncExpr(expr);

   a:=e.Args[0];
   if (a is TVarExpr) or (a is TFuncExpr) or (a is TFieldExpr) then
      codeGen.Compile(a)
   else begin
      Assert(a is TTypedExpr);
      codeGen.WriteString('(');
      codeGen.CompileNoWrap(TTypedExpr(a));
      codeGen.WriteString(')');
   end;
   codeGen.WriteString('.toString()');
end;

// ------------------
// ------------------ TJSStrBeginsWithExpr ------------------
// ------------------

// CodeGen
//
procedure TJSStrBeginsWithExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMagicFuncExpr;
   a : TExprBase;
   c : TConstStringExpr;
begin
   e:=TMagicFuncExpr(expr);

   a:=e.Args[1];
   if a is TConstStringExpr then begin

      c:=TConstStringExpr(a);
      case Length(c.Value) of
         0 : codeGen.WriteString('false');
         1 : begin
            codeGen.WriteString('(');
            codeGen.Compile(e.Args[0]);
            if cgoObfuscate in codeGen.Options then begin
               // slightly faster but less readable, so activate only under obfuscation
               codeGen.WriteString('.charCodeAt(0)==');
               codeGen.WriteString(IntToStr(Ord(c.Value[1])));
            end else begin
               codeGen.WriteString('.charAt(0)==');
               codeGen.WriteLiteralString(c.Value);
            end;
            codeGen.WriteString(')');
         end;
      else
         codeGen.WriteString('(');
         codeGen.Compile(e.Args[0]);
         codeGen.WriteString('.substr(0,');
         codeGen.WriteString(IntToStr(Length(c.Value)));
         codeGen.WriteString(')==');
         codeGen.WriteLiteralString(c.Value);
         codeGen.WriteString(')');
      end;

   end else begin

      codeGen.Dependencies.Add('StrBeginsWith');

      codeGen.WriteString('StrBeginsWith(');
      codeGen.CompileNoWrap(e.Args[0] as TTypedExpr);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.Args[1] as TTypedExpr);
      codeGen.WriteString(')');

   end;
end;


// ------------------
// ------------------ TJSStrFindExpr ------------------
// ------------------

// CodeGen
//
procedure TJSStrFindExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMagicFuncExpr;
   needWrap : Boolean;
begin
   e:=TMagicFuncExpr(expr);

   needWrap:=(e.Args[1] is TConstStringExpr);

   if needWrap then codeGen.WriteString('(');
   CodeGenNoWrap(codeGen, e);
   if needWrap then codeGen.WriteString(')');
end;

// CodeGenNoWrap
//
procedure TJSStrFindExpr.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
var
   e : TMagicFuncExpr;
   offset : TTypedExpr;
   element : TTypedExpr;
begin
   e:=TMagicFuncExpr(expr);

   element:=e.Args[1] as TTypedExpr;
   offset:=e.Args[2] as TTypedExpr;

   if element is TConstStringExpr then begin

      if TConstStringExpr(element).Value = '' then begin
         codeGen.WriteString('0');
      end else begin
         codeGen.CompileValue(e.Args[0] as TTypedExpr);
         codeGen.WriteString('.indexOf(');
         codeGen.WriteLiteralString(TConstStringExpr(element).Value);
         if offset is TConstIntExpr then begin
            if TConstIntExpr(offset).Value<>1 then begin
               codeGen.WriteString(',');
               codeGen.WriteInteger(TConstIntExpr(offset).Value-1);
            end;
         end else begin
            codeGen.WriteString(',');
            codeGen.CompileValue(offset);
            codeGen.WriteString('-1');
         end;
         codeGen.WriteString(')+1');
      end;

   end else begin

      codeGen.Dependencies.Add('StrFind');

      codeGen.WriteString('StrFind(');
      codeGen.CompileNoWrap(e.Args[0] as TTypedExpr);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(element);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(offset);
      codeGen.WriteString(')');

   end;
end;

// ------------------
// ------------------ TJSStrCopyFuncExpr ------------------
// ------------------

// CodeGen
//
procedure TJSStrCopyFuncExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMagicFuncExpr;
   strArg : TExprBase;
   lenArg : TExprBase;
begin
   e:=TMagicFuncExpr(expr);

   strArg:=e.Args[0];
   codeGen.Compile(strArg);
   codeGen.WriteString('.substr(');
   if e.Args[1] is TConstIntExpr then begin
      codeGen.WriteInteger(TConstIntExpr(e.Args[1]).Value-1);
   end else begin
      codeGen.Compile(e.Args[1]);
      codeGen.WriteString('-1');
   end;
   lenArg:=TStringLengthExpr(e.Args[2]);
   if     (lenArg is TStringLengthExpr)
      and (strArg is TTypedExpr)
      and (TStringLengthExpr(lenArg).Expr.SameDataExpr(TTypedExpr(strArg))) then begin
      // to end of string
   end else begin
      codeGen.WriteString(',');
      codeGen.Compile(lenArg);
   end;
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSStrMatchesFuncExpr ------------------
// ------------------

// CodeGen
//
procedure TJSStrMatchesFuncExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);

   function SimpleFilterToRegExp(const filter : String) : String;
   var
      i : Integer;
   begin
      Result:='/^';
      for i:=1 to Length(filter) do begin
         case filter[i] of
            '\', '^', '$', '+', '.', '(', ')', '!', '{', '}', '[', ']', '|' :
               Result:=Result+'\'+filter[i];
            '*' :
               Result:=Result+'[\s\S]*';
            '?' :
               Result:=Result+'[\s\S]'
         else
            Result:=Result+filter[i];
         end;
      end;
      Result:=Result+'$/g';
   end;

var
   e : TMagicFuncExpr;
   a : TExprBase;
   c : TConstStringExpr;
begin
   e:=TMagicFuncExpr(expr);

   a:=e.Args[1];
   if a is TConstStringExpr then begin

      c:=TConstStringExpr(a);
      case Length(c.Value) of
         0 : codeGen.WriteString('false');
      else
         codeGen.WriteString(SimpleFilterToRegExp(c.Value));
         codeGen.WriteString('.test(');
         codeGen.Compile(e.Args[0]);
         codeGen.WriteString(')');
      end;

   end else begin

      codeGen.Dependencies.Add('StrMatches');

      codeGen.WriteString('StrMatches(');
      codeGen.CompileNoWrap(e.Args[0] as TTypedExpr);
      codeGen.WriteString(',');
      codeGen.CompileNoWrap(e.Args[1] as TTypedExpr);
      codeGen.WriteString(')');

   end;
end;

// ------------------
// ------------------ TJSStrReplaceFuncExpr ------------------
// ------------------

// CodeGen
//
procedure TJSStrReplaceFuncExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);

   function EscapeRegExpSpecials(const s : String) : String;
   var
      pSrc, pDest : PChar;
   begin
      SetLength(Result, Length(s));
      pSrc := Pointer(s);
      pDest := Pointer(Result);
      repeat
         case pSrc^ of
            #0 : begin
               SetLength(Result, (NativeUInt(pDest)-NativeUInt(Result)) div SizeOf(Char));
               Exit;
            end;
            '-', '!', '[', ']', '(', ')', '?', '+', '*', '{', '}', '.', '^', '$', '|' : begin
               pDest[0] := '\';
               pDest[1] := pSrc^;
               Inc(pDest, 2);
            end;
            '0'..'>', '@'..'Z', #$005F..'z' : begin
               pDest^ := pSrc^;
               Inc(pDest);
            end;
         else
            Exit('');
         end;
         Inc(pSrc);
      until False;
//       Code : 'function StrRegExp(s) { return s.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&") }'),
   end;

var
   e : TMagicFuncExpr;
   pattern : TExprBase;
   s  : String;
begin
   e := TMagicFuncExpr(expr);

   codeGen.Compile(e.Args[0]);

   pattern := e.Args[1];
   if pattern is TConstStringExpr then begin
      if TConstStringExpr(pattern).Value = '' then Exit;
      s := EscapeRegExpSpecials(TConstStringExpr(pattern).Value);
      if s <> '' then begin
         codeGen.WriteString('.replace(/');
         codeGen.WriteString(s);
         codeGen.WriteString('/g,');
         pattern := nil;
      end;
   end;
   if pattern <> nil then begin
      codeGen.Dependencies.Add('StrRegExp');
      codeGen.WriteString('.replace(new RegExp(StrRegExp(');
      codeGen.CompileNoWrap(pattern as TTypedExpr);
      codeGen.WriteString('), "g"),');
   end;
   codeGen.CompileNoWrap(e.Args[2] as TTypedExpr);
   codeGen.WriteString(')');
end;

// ------------------
// ------------------ TJSGetTextFuncExpr ------------------
// ------------------

// Create
//
constructor TJSGetTextFuncExpr.Create(codeGen : TdwsCodeGen);
begin
   inherited Create;
   FCodegen:=codeGen;
end;

// CodeGen
//
procedure TJSGetTextFuncExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMagicFuncExpr;
   s : String;
begin
   e := TMagicFuncExpr(expr);

   expr := e.Args[0];
   if (expr is TConstStringExpr) and (FCodeGen.Localizer<>nil) then begin
      FCodeGen.Localizer.LocalizeString(TConstStringExpr(expr).Value, s);
      codeGen.WriteLiteralString(s);
   end else begin
      codeGen.WriteString('(');
      codeGen.CompileValue(expr as TTypedExpr);
      codeGen.WriteString(')');
   end;
end;

// ------------------
// ------------------ TJSFormatExpr ------------------
// ------------------

// FormatIsSimpleEnough
//
function TJSFormatExpr.FormatIsSimpleEnough(splitInfos : TFormatSplitInfos) : Boolean;
var
   i : Integer;
   si : TFormatSplitInfo;
begin
   Result:=False;
   if not splitInfos.IsValid then Exit;
   for i:=0 to splitInfos.Count-1 do begin
      si:=splitInfos[i];
      if si.Details-[fsidIndex, fsidType]<>[] then Exit;
      case si.Typ of
         's', 'f', 'd', 'g' : ;
         #0 : ;
      else
         Exit;
      end;
   end;
   Result:=True;
end;

// CodeGen
//
procedure TJSFormatExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TMagicFuncExpr;
   openArgs : TArrayConstantExpr;
   buf : String;
   splitInfos : TFormatSplitInfos;
   elem : TTypedExpr;
   si : TFormatSplitInfo;
   i : Integer;
begin
   e:=TMagicFuncExpr(expr);

   if e.Args[0] is TConstStringExpr then begin
      e.Args[0].EvalAsString(nil, buf);
      if buf='' then begin
         codeGen.WriteString('""');
         Exit;
      end;
      splitInfos:=TFormatSplitInfos.Create(buf);
      try
         if FormatIsSimpleEnough(splitInfos) then begin
            openArgs:=(e.Args[1] as TArrayConstantExpr);
            codeGen.WriteString('(');
            for i:=0 to splitInfos.Count-1 do begin
               if i>0 then
                  codeGen.WriteString('+');
               si:=splitInfos[i];
               if (fsidType in si.Details) and (si.Index<openArgs.ElementCount) then begin
                  elem:=openArgs.Elements[si.Index];
                  if elem is TConstExpr then begin
                     codeGen.WriteString('(');
                     codeGen.Compile(elem);
                     codeGen.WriteString(')');
                  end else codeGen.Compile(elem);
                  codeGen.WriteString('.toString()');
               end else begin
                  codeGen.WriteLiteralString(si.Str);
               end;
            end;
            codeGen.WriteString(')');
            Exit;
         end;
      finally
         splitInfos.Free;
      end;
   end;

   codeGen.Dependencies.Add('Format');

   inherited CodeGen(codeGen, expr);
end;

// ------------------
// ------------------ TFormatSplitInfos ------------------
// ------------------

// Create
//
constructor TFormatSplitInfos.Create(const fmtString : String);
var
   i, n, p, pn, num, index : Integer;
   info : TFormatSplitInfo;
   scanState : TFormatSplitInfoDetail;
begin
   inherited Create;
   FIsValid:=True;
   if fmtString='' then Exit;
   info:=TFormatSplitInfo.Create;
   n:=Length(fmtString);
   index:=0;
   p:=1;
   i:=p;
   while i<n do begin
      case fmtString[i] of
         '%' : begin
            Inc(i);
            if fmtString[i]<>'%' then begin
               // flush partial string
               if i-1>p then begin
                  info.Str:=Copy(fmtString, p, i-p-1);
                  Add(info);
                  info:=TFormatSplitInfo.Create;
               end;
               // scan a format specifier
               scanState:=fsidIndex;
               repeat
                  // scan a potential number
                  pn:=i;
                  while (i<n) do begin
                     case fmtString[i] of
                        '0'..'9' : ;
                     else
                        break;
                     end;
                     Inc(i);
                  end;
                  if pn<>i then begin
                     // found a number
                     num:=StrToInt(Copy(fmtString, pn, i-pn));
                     case scanState of
                        fsidIndex : begin
                           // initial state, can be an index or a width
                           case fmtString[i] of
                              ':' : begin
                                 // it's an index
                                 Include(info.Details, fsidIndex);
                                 info.Index:=num;
                                 Inc(i);
                                 scanState:=fsidLeftAligned;
                              end;
                           else
                              // it's a width
                              Include(info.Details, fsidWidth);
                              info.Width:=num;
                              if fmtString[i]='.' then begin
                                 // followed by a precision
                                 scanState:=fsidPrecision;
                                 Inc(i);
                              end else begin
                                 // followed by type
                                 scanState:=fsidType;
                              end;
                           end;
                        end;
                        fsidLeftAligned, fsidWidth : begin
                           // it's a width
                           Include(info.Details, fsidWidth);
                           info.Width:=num;
                           if fmtString[i]='.' then begin
                              // followed by a precision
                              scanState:=fsidPrecision;
                              Inc(i);
                           end else begin
                              // followed by type
                              scanState:=fsidType;
                           end;
                        end;
                        fsidPrecision : begin
                           // found precision
                           Include(Info.Details, fsidPrecision);
                           info.Precision:=num;
                           scanState:=fsidType;
                        end
                     else
                        Include(info.Details, fsidError);
                        Break;
                     end;
                  end else begin
                     // we found no number
                     case scanState of
                        fsidIndex, fsidLeftAligned : begin
                           case fmtString[i] of
                              '-' : begin
                                 Include(info.Details, fsidLeftAligned);
                                 Inc(i);
                                 scanState:=fsidWidth;
                              end;
                              '.' : begin
                                 Inc(i);
                                 scanState:=fsidPrecision;
                              end;
                           else
                              scanState:=fsidType;
                           end;
                        end;
                        fsidWidth : begin
                           if fmtString[i]='.' then begin
                              Inc(i);
                              scanState:=fsidPrecision;
                           end else begin
                              scanState:=fsidType;
                           end;
                        end;
                        fsidPrecision :
                           Break;
                     end;
                  end;
               until scanState=fsidType;
            end;
            case fmtString[i] of
               'd', 'e', 'f', 'g', 'm', 'n', 'p', 's', 'u', 'x' : begin
                  Include(info.Details, fsidType);
                  info.Typ:=fmtString[i];
               end;
            else
               Include(info.Details, fsidError);
            end;
            if fsidIndex in info.Details then
               index:=info.Index+1
            else begin
               info.Index:=index;
               Inc(index);
            end;
            FIsValid:=not (fsidError in info.Details);
            Add(info);
            info:=TFormatSplitInfo.Create;
            if not IsValid then Break;
            p:=i+1;
         end;
      end;
      Inc(i);
   end;
   if p<=n then begin
      info.Str:=Copy(fmtString, p, n-p+1);
      Add(info);
   end else info.Free;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TJSMagicFuncExpr.vAliases := TUnicodeStringList.Create;

   TJSMagicFuncExpr.RegisterAlias('Date', '$Date');
   TJSMagicFuncExpr.RegisterAlias('NaN', '$NaN');
   TJSMagicFuncExpr.RegisterAlias('Infinity', '$Infinity');
   TJSMagicFuncExpr.RegisterAlias('DateToWeekNumber', 'WeekNumber');
   TJSMagicFuncExpr.RegisterAlias('DateToYearOfWeek', 'YearOfWeek');

   TJSMagicFuncExpr.vAliases.CaseSensitive := False;
   TJSMagicFuncExpr.vAliases.Sorted := True;

finalization

   FreeAndNil(TJSMagicFuncExpr.vAliases);

end.
