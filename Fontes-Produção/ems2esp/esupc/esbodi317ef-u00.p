/*****************************************************************************************************************/
/* Programa     : esbodi317ef-u00.p                                                                                */
/* Descri»’o    : EPC do programa bodi317ef                                                                      */
/* Desenvolvedor: Sergio Luiz Neto da Silveira - DSC Praxis                                                      */
/*****************************************************************************************************************/

{include/i-epc200.i bodi317ef-u00}

{cdp/cdcfgdis.i}

DEF INPUT PARAM p-ind-event  AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.

RUN esupc/esbodi317ef-u01.p (INPUT p-ind-event,
                             INPUT-OUTPUT TABLE tt-epc).

IF RETURN-VALUE = "NOK":U THEN 
   RETURN "NOK":U.
                                                
RETURN "OK":U.

