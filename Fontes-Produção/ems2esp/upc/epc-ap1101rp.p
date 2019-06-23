
/* Include i-epc200.i: Defini‡Æo Temp-Table tt-epc */
{include/i-epc200.i1}

def input param p-ind-event as char no-undo.
def input-output param table for tt-epc.

for each tt-epc no-lock
    where tt-epc.cod-event = p-ind-event:

    OUTPUT TO "D:\temp\daniel.lima\EPC_1101RP.txt".
    
    /* Exemplo de uso dos parƒmetros */
	   disp tt-epc. 
    OUTPUT CLOSE.
end.

