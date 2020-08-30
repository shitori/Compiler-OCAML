Lambda 32
DclArg i
SetVar bar
CstRe  10    # debut For
SetVar i     # i = 10;
GetVar i     # debut cond. For
CstRe  1
GreEqR       # i >= 10
ConJmp 19    # Jump vers fin For
Continue 21 False    # explication 3.2
GetVar bar
StCall
GetVar i
CstRe  3
MultRe
SetArg
Call
Print
Drop         # explication 3.3
Jump 3
SetVar err     # Catch, explication 3.4
GetVar err
Print
GetVar i
CstRe 1
SubsRe
SetVar i
Jump   -23   # Jump vers cond. For
Jump   8
GetVar i
CstRe  0
Equal
ConJmp 2
CstRe 42
Throw        # explication 3.1
GetVar i
Return
Halt