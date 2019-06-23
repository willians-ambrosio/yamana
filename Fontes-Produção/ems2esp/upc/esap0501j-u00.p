
/*-----------------------------------------------------------------------------------
    PROGRAMA : esap0501a-u00
    OBJETIVO : Chamar UPCs do programa ap0501j
    AUTOR    : 
    DATA     : 22/04/2008
-----------------------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i esap0501j-u00.P 2.06.00.000}

{UTP/UT-GLOB.I}

/*************************************************************************************
                                     PARAMETROS
*************************************************************************************/


define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.

/*************************************************************************************
                                    MAIN-BLOCK
*************************************************************************************/

IF i-ep-codigo-usuario <> "201" THEN NEXT.

RUN upc/esap0501j-u01.p(INPUT p-ind-event ,
                         INPUT p-ind-object,
                         INPUT p-wgh-object,
                         INPUT p-wgh-frame ,
                         INPUT p-cod-table ,
                         INPUT p-row-table ).

RETURN RETURN-VALUE.




