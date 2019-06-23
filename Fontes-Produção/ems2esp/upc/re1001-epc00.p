
/*-----------------------------------------------------------------------------------
    PROGRAMA : re1001-epc00
    OBJETIVO : Chamar UPCs do programa IM0100
    AUTOR    : 
    DATA     : 11/04/2008
-----------------------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/
{include/i-prgvrs.i re1001-epc00 2.06.00.001}

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


/* UPC Especifica da Yamana */
/*RUN upc/re1001-upc.p (INPUT p-ind-event,
                      INPUT p-ind-object,
                      INPUT p-wgh-object,
                      INPUT p-wgh-frame,
                      INPUT p-cod-table,
                      INPUT p-row-table).*/


/* UPC para NF-e Datasul */
RUN epc/re1001-epc.p (INPUT p-ind-event,
                      INPUT p-ind-object,
                      INPUT p-wgh-object,
                      INPUT p-wgh-frame,
                      INPUT p-cod-table,
                      INPUT p-row-table).
IF LOOKUP(RETURN-VALUE,",OK") = 0 THEN
    RETURN RETURN-VALUE.

RUN upc/upc-re1001-u01.p (INPUT p-ind-event,
                          INPUT p-ind-object,
                          INPUT p-wgh-object,
                          INPUT p-wgh-frame,
                          INPUT p-cod-table,
                          INPUT p-row-table).
IF LOOKUP(RETURN-VALUE,",OK") = 0 THEN
    RETURN RETURN-VALUE.

RETURN RETURN-VALUE.



