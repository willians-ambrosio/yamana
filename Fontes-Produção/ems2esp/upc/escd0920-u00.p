/*-----------------------------------------------------------------------
    File        : ESCD0920-U00.P
    Purpose     : UPC de chamada para o programa CD0920

    Syntax      :

    Description : 2.06.00.000

    Author(s)   : Rafael Batista
    Created     : 05/2014
    Notes       : Datasul SP
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/*************************************************************************************
                                      INCLUDES   
*************************************************************************************/

{UTP/UT-GLOB.I}

{include/i-prgvrs.i escd0920-u00.P 2.06.00.000}

/* ---> Parametros obrigatorios para UPC. <--- */
DEF INPUT PARAMETER p-ind-event  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-ind-object AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-wgh-object AS HANDLE            NO-UNDO.
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE     NO-UNDO.
DEF INPUT PARAMETER p-cod-table  AS CHAR              NO-UNDO.
DEF INPUT PARAMETER p-row-table  AS ROWID             NO-UNDO.

RUN upc/escd0920-u01.p   (INPUT p-ind-event,
                          INPUT p-ind-object,
                          INPUT p-wgh-object,
                          INPUT p-wgh-frame, 
                          INPUT p-cod-table, 
                          INPUT p-row-table).  

IF RETURN-VALUE = "NOK" THEN  
  RETURN "NOK":U.
                             
RETURN 'OK'.

