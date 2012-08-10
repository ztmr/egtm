; GT.M call-in API for Erlang NIF library
; XXX: temporary --> convert indirection to Xecutes 
; XXX: (because of speed and also to make TP emulation easier)

init q  ;s $zt="n tmp s e=$ecode s tmp=$p($ecode,"","",2) q:$q $e(tmp,2,$l(tmp)) q" q
set(k,v) d txsubmitcmd("s @k=$g(v)","k,v") q
setp(k,d,p,v) d txsubmitcmd("s $p(@k,p,d)=$g(v)","k,d,p,v") q
get(k) q $g(@$g(k))
getp(k,d,p) q $p($g(@$g(k)),p,d)
order(k,d) q $o(@k,d)
kill(k) d txsubmitcmd("k @k","k") q
zkill(k) d txsubmitcmd("zkill @k","k") q
do(c) d @c q
call(c) n x x "s x=$$"_c q $g(x)
merge(k1,k2) d txsubmitcmd("m @k1=@k2","k1,k2") q
tstart(v) d txstart()  q  ; XXX: TP Arguments?
tcommit d txcommit()  q
trollback d txrollback()  q
lock(k) d txsubmitcmd("l +@k","k") q  ; XXX: Lock timeout?
;lock(k,t) s t=+$g(t)
;  i t>-1 l +@k:t q $T
;  e  l +@k q $T
unlock(k) d txsubmitcmd("l -@k","k") q
data(k) q $d(@k)
xecute(x) x x q

horo() q $h
zver() q $zver
job() q $j
iget(v) q @("$"_v)

; TP emulation internals
txtest(k,d,p,w) d txsubmitcmd("w k,d,p,w,!") q
txsubmitcmd(cmd,pars)
  i $G(%EGTMTP) d
  . n cmdid,i,var s cmdid=$O(%EGTMTP($G(%EGTMTP),""),-1)+1
  . f i=1:1:10 s var=$P(pars,",",i) q:var=""  d
  . . s %EGTMTP(%EGTMTP,cmdid,var)=@var
  . s %EGTMTP(%EGTMTP,cmdid)=cmd_";;;"_pars
  e  x cmd
  q
txstart() s %EGTMTP=$G(%EGTMTP)+1 q
txrollback() d txclear($G(%EGTMTP)) q
txcommit()
  tstart  n i,j,id,cmd,pars s id=$G(%EGTMTP)
  s i="" f  s i=$O(%EGTMTP(id,i)) q:i=""  d
  . s cmd=$P($G(%EGTMTP(id,i)),";;;")
  . s pars=$P($G(%EGTMTP(id,i)),";;;",2) n @pars
  . s j="" f  s j=$O(%EGTMTP(id,i,j)) q:j=""  d
  . . s @j=%EGTMTP(id,i,j)
  . x cmd
  tcommit  d txclear(id) q
txclear(id) k %EGTMTP(id) s %EGTMTP=$G(%EGTMTP)-1 q

; Test labels for `call' and `do'
testIntrinsic(a,b) q +$g(a)+$g(b)           ; to be called by `call'
testExtrinsic(a,b) s ^ZTMR=+$g(a)+$g(b) q   ; to be called by `do'
testBlocking(n)
  n i f i=0:1:n s ^ZTMR(i)="abc"
  q

testPerformance(n)
  k ^%EUnit("perf","m") n t1 s t1=$h
  n i f i=0:1:n d
  . s ^%EUnit("perf","m",i)=$tr($r(123456789),"1234567890","ABCDEFGHIJ")
  f i=0:1:n d
  . n rnd s rnd=$r(123456789)
  . n j s j="" f  s j=$o(^%EUnit("perf","m",j)) q:j=""  d:j=rnd
  . . s rnd=$increment(^%EUnit("perf","m",j,"upd"))
  n t2 s t2=$h
  q (+t2-t1)_","_($p(t2,",",2)-$p(t1,",",2))

testPerfOrder1() ; native order (fast)
  n i,r s (i,r)="" f  s i=$o(^%EUnit("perf","e",i)) q:i=""  d
  . s r=r+$g(^%EUnit("perf","e",i))
  q r
testPerfOrder2() ; using indirection-based order (slow)
  n i,r s (i,r)="" f  s i=$$order^%egtmapi("^%EUnit(""perf"",""e"","""_i_""")",1) q:i=""  d
  . s r=r+$g(^%EUnit("perf","e",i))
  q r
testPerfOrder3() ; using xecute-based order (fast)
  n i,r s (i,r)="" f  s i=$$fastOrder^%egtmapi("^%EUnit(""perf"",""e"","""_i_""")",1) q:i=""  d
  . s r=r+$g(^%EUnit("perf","e",i))
  q r

testPerfPrepare()
  n i f i=0:1:10000 s ^%EUnit("perf","e",i)=$r(1234567890)
  q

fastOrder(name,direction) n x x "s x=$order("_name_",direction)" q x

; vim: fdm=syntax:fdn=3:tw=74:ts=2:syn=mumps
