/* test.f */ 

/* if (true) then (succ 10) else (succ 11); */ 

true;
if false then true else false; 

(\x:Bool->Bool. x false) (\x:Bool.x) ;
(\x:Bool.x) true;
iszero (pred (succ (succ 0)));
