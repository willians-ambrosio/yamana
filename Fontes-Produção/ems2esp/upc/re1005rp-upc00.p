
/*-----------------------------------------------------------------------------------
    PROGRAMA : esim0100-u00
    OBJETIVO : Chamar UPCs do programa IM0100
    AUTOR    : 
    DATA     : 11/04/2008
-----------------------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i esim0100-u00.P 2.06.00.000}
{utp/ut-glob.i}
{include/i-epc200.i}

/*************************************************************************************
                                     PARAMETROS
*************************************************************************************/

DEFINE INPUT PARAMETER p-ind-event AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-epc.

/*************************************************************************************
                                    MAIN-BLOCK
*************************************************************************************/

/* UPC Especifica da Yamana */
RUN upc/re1005Arp-upc.p (INPUT p-ind-event,
			 INPUT-OUTPUT TABLE tt-epc).

/* UPC para NF-e Datasul */
RUN upc/re1005rp-upc.p (INPUT p-ind-event,
			INPUT-OUTPUT TABLE tt-epc).

RETURN RETURN-VALUE.



