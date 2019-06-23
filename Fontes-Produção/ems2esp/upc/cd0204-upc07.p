/****************************************************************************************** 
** 	       Programa: cd0204-upc07.p
**   	      Autor: Felipe Vieira
**   	 Fornecedor: Grupo DKP
**    	       Data: 12/03/2019 - 20/03/2019
**   Change/Chamado: N/A
**         Objetivo: Determinar se o item ‚ um epi ou um epc e o ca
**                   salvar a data de altera‡Æo caso seja alterado algum dos seguintes campos da tabela item {
**                     'descricao-1', 'item.descricao-2', 'item.tipo-con-est', 'item.lote-economi', 'item.data-obsol', 'item.narrativa'
**                   }
**                   ou o campo 'ca' da tabela ext_item
** 
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
** 
**
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: p-ind-event ,p-ind-object ,p-wgh-object ,p-wgh-frame ,p-cod-table ,p-row-table  
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE INPUT PARAM p-ind-event      AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-ind-object     AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-wgh-object     AS HANDLE           NO-UNDO.
DEFINE INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE    NO-UNDO.
DEFINE INPUT PARAM p-cod-table      AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-row-table      AS ROWID           NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR wh-flag-epi    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-char-ca     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-text-ca     AS WIDGET-HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE codigo-item AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-objeto AS CHARACTER   NO-UNDO.

DEF NEW GLOBAL SHARED TEMP-TABLE tt_aux
    FIELD descricao-1  LIKE item.descricao-1
    FIELD descricao-2  LIKE item.descricao-2
    FIELD tipo-con-est LIKE item.tipo-con-est
    FIELD lote-economi LIKE item.lote-economi
    FIELD ca           LIKE ext_item.ca
    FIELD data-obsol   LIKE item.data-obsol
    FIELD narrativa    LIKE item.narrativa.

DEFINE VARIABLE listaCamposItem    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE listaCamposAuxItem AS CHARACTER   NO-UNDO INITIAL "ca".

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA,"~/"), p-wgh-object:PRIVATE-DATA,"~/").

/* MESSAGE "evento:     " p-ind-event              SKIP */
/*         "objeto:     " p-ind-object             SKIP */
/*         "tabela:     " p-cod-table              SKIP */
/*         "frame:      " p-wgh-frame:NAME         SKIP */
/*         "wgh objeto: " p-wgh-object:FILE-NAME   SKIP */
/*         "Rowid:      " string(p-row-table)      SKIP */
/*         "C-Objeto:   " c-objeto                      */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.               */

IF c-objeto MATCHES "*v36in172.w*" THEN
DO:
    /* pega o registro corrente da tabela item*/
    FIND FIRST ITEM WHERE ROWID(item) = p-row-table NO-ERROR.

    /* cria os campos epi/epc e ca na tela */
    IF   NOT VALID-HANDLE(wh-flag-epi) 
     AND NOT VALID-HANDLE(wh-char-ca)  THEN DO:  

        CREATE TOGGLE-BOX wh-flag-epi
        ASSIGN FRAME     = p-wgh-frame
               ROW       = 7.25
               COL       = 27.8
               VISIBLE   = YES
               SENSITIVE = NO 
               LABEL     = "Item ‚ EPI?".
        
        ON VALUE-CHANGED OF wh-flag-epi PERSISTENT RUN upc/cd0204-upc07.p (INPUT "inserirCA",  
                                                                           input p-ind-object, 
                                                                           input p-wgh-object, 
                                                                           input p-wgh-frame,  
                                                                           input p-cod-table,  
                                                                           input p-row-table).
                                                                              
        CREATE FILL-IN wh-char-ca
        ASSIGN FRAME     =  p-wgh-frame 
               ROW       = 7.25                 
               COL       = 48
               FORMAT    = "x(20)"
               HEIGHT-CHARS = 0.8
               WIDTH-CHARS = 21
               VISIBLE   = YES                  
               SENSITIVE = NO                   
               TOOLTIP   = "Certificado de Autoriza‡Æo".

        CREATE TEXT wh-text-ca
        ASSIGN FRAME   =  p-wgh-frame
               ROW     = 7.25 
               COL     = 45
               HEIGHT-CHARS = 0.8
               WIDTH-CHARS = 3.
        wh-text-ca:SCREEN-VALUE = "CA:".
        
                   
    END.

    /* busca o registro especifico na tabela ext_item alem de armazenar o codigo do item para deletar o registro espefico caso o registro do item seja deletado */
    IF p-ind-event = 'Display'  AND p-ind-object = 'VIEWER'
                                AND VALID-HANDLE(wh-flag-epi) THEN DO:

        ASSIGN wh-flag-epi:CHECKED    = FALSE
               wh-flag-epi:SENSITIVE  = NO.
        
        IF AVAIL item THEN DO:

            codigo-item = item.it-codigo. /*usado para excluir o especifico*/

            FIND FIRST ext_item NO-LOCK
                 WHERE ext_item.it-codigo  =  item.it-codigo NO-ERROR. 

        END.

        IF AVAIL ext_item THEN
            ASSIGN wh-flag-epi:CHECKED     =  ext_item.epi-epc
                   wh-char-ca:SCREEN-VALUE =  ext_item.ca. 
        
    END.
    
    /* habilita os campos especificos para edi‡Æo */
    IF p-ind-event = 'ENABLE' AND p-ind-object  =   'VIEWER'
                              AND VALID-HANDLE(wh-flag-epi) THEN DO:
        ASSIGN wh-flag-epi:SENSITIVE = YES
               wh-char-ca:SENSITIVE  = wh-flag-epi:CHECKED.
        RUN preTabTem.
    END.

    /* cria ou altera o registro na tabela especifica (quanod o item ‚ alterado) */
    IF p-ind-event = 'ASSIGN' AND p-ind-object  =   'VIEWER'
                              AND VALID-HANDLE(wh-flag-epi) THEN DO:
        IF AVAIL item THEN
        DO:
            FIND FIRST ext_item EXCLUSIVE-LOCK
                 WHERE ext_item.it-codigo = item.it-codigo NO-ERROR.

            IF NOT AVAIL ext_item THEN
            DO:               
                CREATE ext_item.
                ASSIGN ext_item.it-codigo =  item.it-codigo.
            END.
            ASSIGN ext_item.epi-epc = wh-flag-epi:CHECKED
                   ext_item.ca      = wh-char-ca:SCREEN-VALUE
                   ext_item.situacao = 1
                   ext_item.dataAlt = TODAY. 
        END.
    END.

    /* desabilita os campos especificos  */
    IF p-ind-event = 'DISABLE' AND p-ind-object = 'VIEWER'
                               AND VALID-HANDLE(wh-flag-epi) THEN
        ASSIGN wh-flag-epi:SENSITIVE = NO
               wh-char-ca:SENSITIVE  = NO.
END.

/* deleta o registro especifico caso o item seja deletado */
IF p-ind-event = 'AFTER-DELETE' AND p-ind-object = 'VIEWER' THEN DO: 

    FIND FIRST ext_item EXCLUSIVE-LOCK
         WHERE ext_item.it-codigo = codigo-item NO-ERROR.
    IF AVAIL ext_item THEN DO:
       DELETE ext_item.
    END.

END.

/*** Verifica campos que foram alterados ***/
IF p-ind-event = "BEFORE-ASSING" AND 
   c-objeto MATCHES "*v36in172.w*"  THEN DO:

    IF AVAIL ext_item AND 
       (wh-char-ca:SCREEN-VALUE <> ext_item.ca OR
       wh-flag-epi:CHECKED <> ext_item.epi-epc)
    THEN ASSIGN ext_item.situacao = 1
                ext_item.dataAlt = NOW.

END.

IF p-ind-event = 'DISPLAY' AND
   p-ind-object = 'VIEWER' AND 
   c-objeto MATCHES "*v36in172.w*" THEN DO: 

    FIND FIRST ITEM WHERE ROWID(item) = p-row-table NO-ERROR.
    FIND FIRST ext_item NO-LOCK
        WHERE ext_item.it-codigo = item.it-codigo NO-ERROR.
    IF AVAIL ext_item
         THEN ASSIGN wh-flag-epi:CHECKED = ext_item.epi-epc
                     wh-char-ca:SCREEN-VALUE = ext_item.ca.
    ELSE ASSIGN wh-flag-epi:CHECKED      = NO
                wh-char-ca:SCREEN-VALUE  = "".

END.


IF p-ind-event = 'AFTER-END-UPDATE' AND p-ind-object = 'VIEWER' AND c-objeto MATCHES "*v36in172.w*" THEN DO: 

    RUN atualizaData.

END.
IF p-ind-event = 'VALIDATE' AND p-ind-object = 'VIEWER' AND c-objeto MATCHES "*v34in172.w*" THEN DO: 

    IF wh-flag-epi:CHECKED AND wh-char-ca:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Item sem Certificado de Autori‡Æo" skip
                "Item de EPI deve ter o campo CA informado!"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN "NOK".
    END.
END.

/* Criados  */
IF p-ind-event = "inserirCA" THEN DO: 

  ASSIGN wh-char-ca:SENSITIVE = wh-flag-epi:CHECKED. 
  IF NOT wh-flag-epi:CHECKED THEN wh-char-ca:SCREEN-VALUE = "". 

END.


PROCEDURE preTabTem:

  EMPTY TEMP-TABLE tt_aux.
  CREATE tt_aux.
  ASSIGN tt_aux.descricao-1  = item.descricao-1   
         tt_aux.descricao-2  = item.descricao-2   
         tt_aux.tipo-con-est = item.tipo-con-est  
         tt_aux.lote-economi = item.lote-economi   
         tt_aux.data-obsol   = item.data-obsol   
         tt_aux.narrativa    = item.narrativa
         tt_aux.ca           = wh-char-ca:SCREEN-VALUE.

END PROCEDURE.       

PROCEDURE atualizaData:

   FIND FIRST tt_aux NO-LOCK NO-ERROR.
   FIND FIRST ext_item EXCLUSIVE-LOCK 
        WHERE ext_item.it-codigo = item.it-codigo NO-ERROR.

   BUFFER-COMPARE ITEM TO tt_aux
       SAVE RESULT IN listaCamposItem.
   BUFFER-COMPARE ext_item TO tt_aux
       SAVE RESULT IN listaCamposAuxItem.
       
   IF NUM-ENTRIES(listaCamposItem) <> 0 OR NUM-ENTRIES(listaCamposAuxItem) <> 0 THEN DO:    
        ASSIGN ext_item.dataAlt = NOW
               ext_item.situacao = 1.
   END.

END PROCEDURE.

RETURN "OK".


