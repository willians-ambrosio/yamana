/****************************************************************************************** 
** 	   Programa: rpt_bord_ap_ret_pagto_escr-U01.p
**   	  Autor: Felipe Vieira
** 	 Fornecedor: DKP
**         Data: 14/08/2018
** Change/Chamado: 
**      Objetivo: Criar um bot∆o "imprimir" e pagar linhas de arquivos *descrever depois* 
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: rpt_bord_ap_ret_pagto_escr.p 
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{utp\ut-glob.i}
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID NO-UNDO.


/* MESSAGE "p-ind-evento.....: " p-ind-event         SKIP */
/*         "p-ind-objeto.....: " p-ind-object        SKIP */
/*         "Handle do Objeto.: " p-wgh-object        SKIP */
/*         "Frame............: " p-wgh-frame         SKIP */
/*         "Nome da tabela...: " p-cod-table         SKIP */
/*         "Rowid da tabela..: " STRING(p-row-table) SKIP */
/*     VIEW-AS ALERT-BOX TITLE "bas-espec-docto".         */

/* Inicio define */

DEFINE NEW GLOBAL SHARED VARIABLE h-aux-bt-imprimir AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-esp-bt-imprimir AS WIDGET-HANDLE NO-UNDO.


IF p-ind-event = "ENABLE" THEN DO:

    RUN piBuscaWidget(INPUT "bt_print",
                      INPUT  p-wgh-frame:HANDLE,
                      OUTPUT h-aux-bt-imprimir).



     /* cria o botao para pagar o registro correspondente na tabela ext_espec_docto */
    CREATE BUTTON h-esp-bt-imprimir
           ASSIGN ROW                = h-aux-bt-imprimir:ROW              
                  COLUMN             = h-aux-bt-imprimir:COLUMN           
                  WIDTH              = h-aux-bt-imprimir:WIDTH            
                  HEIGHT             = h-aux-bt-imprimir:HEIGHT           
                  LABEL              = h-aux-bt-imprimir:LABEL 
                  FRAME              = h-aux-bt-imprimir:FRAME            
                  FLAT-BUTTON        = h-aux-bt-imprimir:FLAT-BUTTON 
                  TOOLTIP            = "*" + h-aux-bt-imprimir:TOOLTIP
                  HELP               = h-aux-bt-imprimir:HELP             
                  NAME               = "elimina_esp"             
                  SENSITIVE          = YES       
                  VISIBLE            = YES          
                  CONVERT-3D-COLOR   = h-aux-bt-imprimir:CONVERT-3D-COLOR
                  
                  TRIGGERS:
                    ON "CHOOSE" PERSISTENT RUN prgint\upc\rpt_bord_ap_ret_pagto_escr-U01.p (INPUT "Imprimir":U,
                                                                                                INPUT p-ind-object, 
                                                                                                INPUT p-wgh-object, 
                                                                                                INPUT p-wgh-frame,  
                                                                                                INPUT p-cod-table,  
                                                                                                INPUT p-row-table).


                  END TRIGGERS.


           h-esp-bt-imprimir:LOAD-IMAGE-UP(h-aux-bt-imprimir:IMAGE-UP).
           h-esp-bt-imprimir:LOAD-IMAGE-INSENSITIVE(h-aux-bt-imprimir:IMAGE-INSENSITIVE).
           h-esp-bt-imprimir:MOVE-TO-TOP().
           ASSIGN h-aux-bt-imprimir:TAB-STOP  = NO
                  h-aux-bt-imprimir:SENSITIVE = NO.

END.

IF p-ind-event = "Imprimir" THEN DO:


 DEFINE VARIABLE h-van006 AS HANDLE      NO-UNDO.
 DEFINE NEW GLOBAL SHARED VAR cTipoOrigem AS CHAR NO-UNDO.
 DEFINE VAR cId         AS CHAR NO-UNDO. 
 DEFINE VAR cProc       AS CHAR NO-UNDO.
 DEFINE NEW SHARED VAR r_row_webserver AS ROWID NO-UNDO.
 DEFINE NEW SHARED VAR r_row_param_dir AS ROWID NO-UNDO.
 
 ASSIGN cTipoOrigem = "erp".

 RUN van\van006.p PERSISTENT SET h-van006.

 RUN pi-listamsg IN h-van006.

 APPLY "CHOOSE" TO h-aux-bt-imprimir.

END.

PROCEDURE piBuscaWidget:
    DEF INPUT  PARAM pNome     AS CHAR.
    DEF INPUT  PARAM pFrame    AS WIDGET-HANDLE.
    DEF OUTPUT PARAM pObject   AS WIDGET-HANDLE.

    DEF VAR hFrame             AS WIDGET-HANDLE.
    DEF VAR whObjeto           AS WIDGET-HANDLE.

    ASSIGN hFrame = pFrame:FIRST-CHILD.

    DO WHILE VALID-HANDLE(hFrame):

        IF hFrame:TYPE <> "field-group" THEN
        DO:

            IF hFrame:Type = "frame" THEN
            DO:
                RUN piBuscaWidget(INPUT  pNome,
                                  INPUT  hFrame,
                                  OUTPUT whObjeto).
                IF whObjeto <> ? THEN
                DO:
                    ASSIGN pObject = whObjeto.
                    LEAVE.
                END.
            END.

/*             MESSAGE hFrame:NAME SKIP               */
/*                     hFrame:TOOLTIP                 */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */

            IF hFrame:NAME = pNome THEN
            DO:
                ASSIGN pObject = hFrame.
                LEAVE.
            END.

            ASSIGN hFrame = hFrame:NEXT-SIBLING.
        END.
        ELSE ASSIGN hFrame = hFrame:FIRST-CHILD.
    END.
END PROCEDURE.

