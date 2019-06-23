
/****************************************************************************************** 
** 	   Programa: fnc_movto_operac_financ_pagto_emprest_u00.p
**   	  Autor: Felipe Vieira
** 	 Fornecedor: DKP
**         Data: 30/07/2018
** Change/Chamado: 
**      Objetivo: Criar campo "taxa contratada"
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS:  fnc_movto_operac_financ_pagto_emprest
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

DEFINE NEW GLOBAL SHARED VARIABLE wh-fill-tx-contrat              AS WIDGET-HANDLE NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-text-tx-contrat              AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fill-moeda-conta-corrente    AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fill-pagto AS WIDGET-HANDLE NO-UNDO.


IF p-ind-event = "DISPLAY" THEN DO:
                                                         

    RUN piBuscaWidget(INPUT "v_cod_indic_econ_cta_corren",
                      INPUT  p-wgh-frame:HANDLE,
                      OUTPUT wh-fill-moeda-conta-corrente).

    RUN piBuscaWidget(INPUT "v_dat_pagto",
                      INPUT  p-wgh-frame:HANDLE,
                      OUTPUT wh-fill-pagto).



    IF VALID-HANDLE(wh-fill-moeda-conta-corrente) AND 
       wh-fill-moeda-conta-corrente:SCREEN-VALUE = "REAL"  THEN DO:
    
       CREATE TEXT wh-text-tx-contrat
            assign frame        = p-wgh-frame
                   format       = "x(04)"
                   width        = 4
                   screen-value = "TC:"
                   row          = wh-fill-pagto:row
                   col          = wh-fill-pagto:COL + 15
                   visible      = YES.   
    
         CREATE FILL-IN wh-fill-tx-contrat
            assign frame              = p-wgh-frame
                   DATA-TYPE          = "DECIMAL"
                   format             = ">9.9999":U 
                   width              = 9
                   height             = 0.88
                   row                = wh-fill-pagto:ROW
                   col                = wh-fill-pagto:COL + 18
                   visible            = yes
                   sensitive          = YES.
            wh-fill-tx-contrat:MOVE-AFTER-TAB-ITEM(wh-fill-pagto).

    END.
END.

IF p-ind-event = "VALIDATE" THEN DO:

   IF VALID-HANDLE(wh-fill-tx-contrat) AND
      DECIMAL(wh-fill-tx-contrat:SCREEN-VALUE) = 0 THEN DO:

/*        RUN utp/ut-msgs.p (INPUT "show",                                                            */
/*                           INPUT 17006,                                                             */
/*                           INPUT "Taxa contratada~~O valor da taxa contratada n∆o pode ser zero!"). */

       MESSAGE "O valor da taxa contratada n∆o pode ser zero!"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.


       RETURN "NOK".

   END.



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
/*                     hFrame:TOOLTIP SKIP            */
/*                     hframe:SCREEN-VALUE            */
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

