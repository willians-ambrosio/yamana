/****************************************************************************************** 
** 	   Programa: re1001-u01.p
**   	      Autor: Felipe
**   	 Fornecedor: DKP
**    	 Data: 09/2018
** Change/Chamado: REQ03
**    Objetivo: Diversas customiza‡äes tela RE1001.
**
******************************** CONTROLE DE ALTERA€åES *********************************
** 
** Data         Autor   		Fornecedor  Change/Chamado Descri‡Æo da Altera‡Æo
** 30/10/2018   Vando Ribeiro	DKP                        Inserido gatinho pra execu‡Æo do programa
                                                           rep\rep005-p01.p para cria‡Æo de pedidos
                                                           de execu‡Æo RPW.
                                                           
                                                           Controle para habilitar/desabilitar botäes
                                                           
                                                           Mensagem alerta de bloqueio.
****************************** INFORMA€åES ADICIONAIS ************************************
** PAR¶METROS DE ENTRADA: p-ind-event, p-ind-object, p-wgh-object, p-wgh-frame,
                          p-cod-table e p-row-table 
** PAR¶METROS DE SAÖDA: N/A
** CADASTRADO NO FONTE TOTVS: N/A
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

DEFINE INPUT PARAM p-ind-event      AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-ind-object     AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-wgh-object     AS HANDLE           NO-UNDO.
DEFINE INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE    NO-UNDO.
DEFINE INPUT PARAM p-cod-table      AS CHAR             NO-UNDO.
DEFINE INPUT PARAM p-row-table      AS ROWID            NO-UNDO.
    
/* --- ---------------------------------------------- ------------------ */

/* MESSAGE "p-ind-evento.....: " p-ind-event         SKIP */
/*         "p-ind-objeto.....: " p-ind-object        SKIP */
/*         "Handle do Objeto.: " p-wgh-object        SKIP */
/*         "Frame............: " p-wgh-frame         SKIP */
/*         "Nome da tabela...: " p-cod-table         SKIP */
/*         "Rowid da tabela..: " STRING(p-row-table) SKIP */
/*     VIEW-AS ALERT-BOX TITLE "bas-espec-docto".         */

DEF NEW GLOBAL SHARED VAR h-btConf_esp       AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-btDelete_esp     AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-btDelete         AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-btUpdate         AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-inf-adic      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR aux-h-bt-inf-adic  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-nfe-imp       AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-embalagem     AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-importacao    AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-btRecFisico      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-mes              AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fPage1           AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fPage2           AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fPage3           AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fPage4           AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fPage5           AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fPage6           AS HANDLE NO-UNDO.


DEF BUFFER buf-docum-est FOR docum-est.

IF p-ind-event  = "AFTER-INITIALIZE" AND
   p-ind-object = "CONTAINER"        AND
   p-cod-table  = "docum-est"        THEN DO:

   RUN piBuscaWidget(INPUT "btConf_esp",
                     INPUT  p-wgh-frame,
                     OUTPUT h-btConf_esp).

   RUN piBuscaWidget(INPUT "btDelete_esp",
                     INPUT  p-wgh-frame,
                     OUTPUT h-btDelete_esp).

   RUN piBuscaWidget(INPUT "btDelete",
                     INPUT  p-wgh-frame,
                     OUTPUT h-btDelete).

   RUN piBuscaWidget(INPUT "btUpdate",
                     INPUT  p-wgh-frame,
                     OUTPUT h-btUpdate).

  RUN piBuscaWidget(INPUT "bt-inf-adic",
                    INPUT  p-wgh-frame,
                    OUTPUT h-bt-inf-adic).

   RUN piBuscaWidget(INPUT "bt-nfe-imp",
                     INPUT  p-wgh-frame,
                     OUTPUT h-bt-nfe-imp).

   RUN piBuscaWidget(INPUT "bt-embalagem",
                     INPUT  p-wgh-frame,
                     OUTPUT h-bt-embalagem).

   RUN piBuscaWidget(INPUT "bt-importacao",
                     INPUT  p-wgh-frame,
                     OUTPUT h-bt-importacao).

   RUN piBuscaWidget(INPUT "btRecFisico",
                     INPUT  p-wgh-frame,
                     OUTPUT h-btRecFisico).  

   RUN piBuscaWidget(INPUT "fPage1",
                     INPUT  p-wgh-frame,
                     OUTPUT h-fPage1).

   RUN piBuscaWidget(INPUT "fPage2",
                     INPUT  p-wgh-frame,
                     OUTPUT h-fPage2).
   
   RUN piBuscaWidget(INPUT "fPage3",
                     INPUT  p-wgh-frame,
                     OUTPUT h-fPage3).

   RUN piBuscaWidget(INPUT "fPage4",
                     INPUT  p-wgh-frame,
                     OUTPUT h-fPage4).

   RUN piBuscaWidget(INPUT "fPage5",
                     INPUT  p-wgh-frame,
                     OUTPUT h-fPage5).

   RUN piBuscaWidget(INPUT "fPage6",
                     INPUT  p-wgh-frame,
                     OUTPUT h-fPage6).


   IF VALID-HANDLE(h-bt-inf-adic) THEN DO:
        CREATE BUTTON aux-h-bt-inf-adic
            ASSIGN  ROW         = h-bt-inf-adic:ROW  
                    COLUMN      = h-bt-inf-adic:COLUMN
                    WIDTH       = h-bt-inf-adic:WIDTH
                    HEIGHT      = h-bt-inf-adic:HEIGHT
                    LABEL       = h-bt-inf-adic:LABEL
                    FRAME       = h-bt-inf-adic:FRAME
                    FLAT-BUTTON = h-bt-inf-adic:FLAT-BUTTON
                    TOOLTIP     = h-bt-inf-adic:TOOLTIP
                    HELP        = h-bt-inf-adic:HELP
                    NAME        = "b-atualiza"
                    SENSITIVE   = h-bt-inf-adic:SENSITIVE  
                    VISIBLE     = h-bt-inf-adic:VISIBLE
                    CONVERT-3D-COLOR = h-bt-inf-adic:CONVERT-3D-COLOR.

         ON "CHOOSE" OF aux-h-bt-inf-adic PERSISTENT RUN upc/re1001-u01.p(INPUT "RE1001-aux-h-bt-inf-adic",
                                                                       INPUT p-ind-object,
                                                                       INPUT p-wgh-object,
                                                                       INPUT p-wgh-frame,
                                                                       INPUT p-cod-table,
                                                                       INPUT p-row-table).
        aux-h-bt-inf-adic:LOAD-IMAGE-UP(h-bt-inf-adic:IMAGE-UP).
        aux-h-bt-inf-adic:LOAD-IMAGE-INSENSITIVE(h-bt-inf-adic:IMAGE-INSENSITIVE).
        aux-h-bt-inf-adic:MOVE-TO-TOP().
        ASSIGN h-bt-inf-adic:TAB-STOP  = NO
               h-bt-inf-adic:SENSITIVE = NO.


   END. /*IF VALID-HANDLE(h-bt-inf-adic) THEN DO:*/

   CREATE TEXT h-mes 
   ASSIGN NAME         = "mensagem"
          WIDTH        = 25      
          FORMAT       = "X(20)"
          ROW          = 4   
          COLUMN       = 40
          SCREEN-VALUE = "Documento Bloqueado"
          FRAME        = p-wgh-frame
          FGCOLOR      = 4.
      
   RUN Bloqueia.

   RUN rep\rep005-p01.p. /*cria pedidos de execu‡Æo rpw conforme frequencia.*/
END.


IF p-ind-event  = "AFTER-CONTROL-TOOL-BAR" AND
   p-ind-object = "CONTAINER"        AND
   p-cod-table  = "docum-est"        THEN DO:

   RUN Bloqueia.

END. /*IF p-ind-event  = "AFTER-INITIALIZE" AND*/

IF p-ind-event  = "RE1001-aux-h-bt-inf-adic" THEN DO:
   IF VALID-HANDLE(h-btUpdate) THEN
      IF h-btUpdate:SENSITIVE THEN APPLY "CHOOSE" TO h-bt-inf-adic.
END.


PROCEDURE Bloqueia: 

    FIND FIRST buf-docum-est WHERE ROWID(buf-docum-est) = p-row-table.

    IF AVAIL buf-docum-est THEN
    DO:
        FIND LAST esp_pend_aprov 
            WHERE esp_pend_aprov.cod-estabel            = buf-docum-est.cod-estabel   
              AND esp_pend_aprov.serie-docto            = buf-docum-est.serie-docto                
              AND esp_pend_aprov.nro-docto              = buf-docum-est.nro-docto                  
              AND esp_pend_aprov.cod-emitente           = buf-docum-est.cod-emitente               
              AND esp_pend_aprov.nat-operacao           = buf-docum-est.nat-operacao 
              AND esp_pend_aprov.bloq-atualizar NO-LOCK NO-ERROR.
          
        IF AVAIL esp_pend_aprov THEN
        DO:
           
            IF esp_pend_aprov.cod_usuario_aprovador1  = "" AND 
               esp_pend_aprov.cod_usuario_reprovador  = "" THEN
            DO:
              
                IF VALID-HANDLE(h-btConf_esp)     AND 
                   VALID-HANDLE(h-btDelete_esp)   AND 
                   VALID-HANDLE(h-btDelete)       AND 
                   VALID-HANDLE(h-btUpdate)       and 
                   VALID-HANDLE(h-fPage1)         and 
                   VALID-HANDLE(h-fPage2)         and 
                   VALID-HANDLE(h-fPage3)         and 
                   VALID-HANDLE(h-fPage4)         and 
                   VALID-HANDLE(h-fPage5)         and 
                   VALID-HANDLE(h-fPage6)         and 
                   VALID-HANDLE(h-bt-nfe-imp)     and 
                   VALID-HANDLE(h-bt-embalagem)   and 
                   VALID-HANDLE(h-bt-importacao)  AND 
                   VALID-HANDLE(h-btRecFisico)    and
                   VALID-HANDLE(aux-h-bt-inf-adic) THEN
                DO:
                    h-btConf_esp:SENSITIVE       = NO.
                    h-btDelete_esp:SENSITIVE     = NO.
                    h-btDelete:SENSITIVE         = NO.
                    h-btUpdate:SENSITIVE         = NO.
                    h-fPage1:SENSITIVE           = NO.
                    h-fPage2:SENSITIVE           = NO.
                    h-fPage3:SENSITIVE           = NO.
                    h-fPage4:SENSITIVE           = NO.
                    h-fPage5:SENSITIVE           = NO.
                    h-fPage6:SENSITIVE           = NO.
                    /*  aux-h-bt-inf-adic:SENSITIVE  = NO. */
                    /*  h-bt-inf-adic:SENSITIVE      = no. */
                    h-bt-nfe-imp:SENSITIVE       = no.
                    h-bt-embalagem:SENSITIVE     = no.
                    h-bt-importacao:SENSITIVE    = no.
                    h-btRecFisico:SENSITIVE      = no.
                    h-mes:SCREEN-VALUE           = "Documento Bloqueado".
                    h-mes:VISIBLE                = YES.              
    
                    RUN utp/ut-msgs.p (INPUT "show":U, INPUT 27979, 
                                       INPUT "Documento Bloqueado~~Documento bloqueado dependendo de aprova‡Æo para atualiza‡Æo!").
                END.
            END.
            ELSE IF esp_pend_aprov.cod_usuario_reprovador <> "" AND
                    esp_pend_aprov.cod_usuario_aprovador1 = ""  AND
                    VALID-HANDLE(h-mes) THEN
            DO:
                h-mes:SCREEN-VALUE = "Documento Reprovado".
                h-mes:VISIBLE      = YES.
                 
                IF VALID-HANDLE(h-fPage1) AND
                   VALID-HANDLE(h-fPage2) AND
                   VALID-HANDLE(h-fPage3) AND
                   VALID-HANDLE(h-fPage4) AND
                   VALID-HANDLE(h-fPage5) AND
                   VALID-HANDLE(h-fPage6) THEN
                ASSIGN
                      h-fPage1:SENSITIVE = YES
                      h-fPage2:SENSITIVE = yes
                      h-fPage3:SENSITIVE = yes
                      h-fPage4:SENSITIVE = yes
                      h-fPage5:SENSITIVE = yes
                      h-fPage6:SENSITIVE = yes.
            END.
        END.
        ELSE
        DO: 
            IF VALID-HANDLE(h-mes) THEN h-mes:VISIBLE = NO.

            IF VALID-HANDLE(h-fPage1) and
               VALID-HANDLE(h-fPage2) and
               VALID-HANDLE(h-fPage3) and
               VALID-HANDLE(h-fPage4) and
               VALID-HANDLE(h-fPage5) and
               VALID-HANDLE(h-fPage6) THEN
            ASSIGN    
                h-fPage1:SENSITIVE = YES
                h-fPage2:SENSITIVE = YES
                h-fPage3:SENSITIVE = YES
                h-fPage4:SENSITIVE = YES
                h-fPage5:SENSITIVE = YES
                h-fPage6:SENSITIVE = YES.
        END.
    END.
END PROCEDURE.


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
/*                     hFrame:TYPE                    */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */

            IF    hFrame:NAME = pNome THEN
            DO:
                ASSIGN pObject = hFrame.
                LEAVE.
            END.

            ASSIGN hFrame = hFrame:NEXT-SIBLING.
        END.
        ELSE ASSIGN hFrame = hFrame:FIRST-CHILD.
    END.
END PROCEDURE.
