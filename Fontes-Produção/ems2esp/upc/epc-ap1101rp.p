
/* Include i-epc200.i: Defini��o Temp-Table tt-epc */
{include/i-epc200.i1}

def input param p-ind-event as char no-undo.
def input-output param table for tt-epc.

for each tt-epc no-lock
    where tt-epc.cod-event = p-ind-event:

    OUTPUT TO "D:\temp\daniel.lima\EPC_1101RP.txt".
    
    /* Exemplo de uso dos par�metros */
	   disp tt-epc. 
    OUTPUT CLOSE.
end.

