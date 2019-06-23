
/****************************************************************************************** 
** 	   Programa: BAs_espec_docto-u00.p
**   	  Autor: Felipe 
** 	 Fornecedor: DKP
**         Data: xx/07/2018
** Change/Chamado: 
**      Objetivo: Cria bot∆o para permitir informar se e imposto ou n∆o
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: bas_espec_docto, add_espec_docto, mod_espec_docto, 
** CADASTRADO NA TABELA: N/A
******************************************************************************************/

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


DEF NEW GLOBAL SHARED VAR h-fi-cod-espec          AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-elimina            AS WIDGET-HANDLE NO-UNDO.
/* campos da tela  */
DEF NEW GLOBAL SHARED VAR h-bt-des_espec_docto    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-cod_espec_docto    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-ent                AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-tip_espec_docto    AS WIDGET-HANDLE NO-UNDO.
/* campos da tela  */
/* pega literal */
DEF NEW GLOBAL SHARED VAR h-bt-literal           AS WIDGET-HANDLE EXTENT 3 NO-UNDO.
/* pega literal */
DEF NEW GLOBAL SHARED VAR h-bt-salva              AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-ok                 AS WIDGET-HANDLE NO-UNDO.
DEF new GLOBAL SHARED VAR CHBOX-IMP               AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR CHBOX-FGTS              AS widget-handle no-undo.
DEF NEW GLOBAL SHARED VAR CHBOX-GPS               AS widget-handle no-undo.


if  (p-ind-event = "DISPLAY" 
     OR
     p-ind-event = "ENABLE"
     )
and p-ind-object = "VIEWER"
 then do: 

RUN piBuscaWidget(INPUT "cod_espec_docto",
                  INPUT  p-wgh-frame:HANDLE,
                  OUTPUT h-fi-cod-espec).


RUN piBuscaWidget(INPUT "bt_era1",
                  INPUT  p-wgh-frame:HANDLE,
                  OUTPUT h-bt-elimina).

RUN piBuscaWidget(INPUT "bt_ok",
                  INPUT  p-wgh-frame:HANDLE,
                  OUTPUT h-bt-ok).

RUN piBuscaWidget(INPUT "bt_sav",
                  INPUT  p-wgh-frame:HANDLE,
                  OUTPUT h-bt-salva).

RUN piBuscaWidget(INPUT "des_espec_docto",                  
                  INPUT  p-wgh-frame:HANDLE,                
                  OUTPUT h-bt-des_espec_docto).             
                                                            
RUN piBuscaWidget(INPUT "cod_espec_docto",                  
                  INPUT  p-wgh-frame:HANDLE,                
                  OUTPUT h-bt-cod_espec_docto).             
                                                            
RUN piBuscaWidget(INPUT "bt_ent_29452",                     
                  INPUT  p-wgh-frame:HANDLE,                
                  OUTPUT h-bt-ent).                         
                                                            
RUN piBuscaWidget(INPUT "ind_tip_espec_docto",            
                  INPUT  p-wgh-frame:HANDLE,                
                  OUTPUT h-bt-tip_espec_docto).  

IF NOT VALID-HANDLE(h-bt-tip_espec_docto) THEN 
   RUN piBuscaWidget(INPUT "v_ind_tip_espec_docto",            
                     INPUT  p-wgh-frame:HANDLE,                
                     OUTPUT h-bt-tip_espec_docto).

RUN piBuscaLiteral(INPUT  p-wgh-frame:HANDLE).

RUN prgint/upc/BAS_espec_docto-u01.p (p-ind-event ,
                               p-ind-object,
                               p-wgh-object,
                               p-wgh-frame ,
                               p-cod-table, 
                               p-row-table).


/* MESSAGE h-bt-literal[1]:TYPE SKIP      */
/*         STRING(h-bt-literal[1]) SKIP   */
/*         h-bt-literal[2]:TYPE SKIP      */
/*         STRING(h-bt-literal[2]) SKIP   */
/*         h-bt-literal[3]:TYPE SKIP      */
/*         STRING(h-bt-literal[3])        */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */


END.

if  p-ind-event = "ASSIGN"    
and p-ind-object = "VIEWER"
 then DO: 


        FIND FIRST ext_espec_docto EXCLUSIVE-LOCK
             WHERE ext_espec_docto.cod_espec_docto = h-fi-cod-espec:SCREEN-VALUE NO-ERROR.


        IF NOT AVAIL ext_espec_docto THEN DO:
              CREATE ext_espec_docto.
              ASSIGN ext_espec_docto.cod_espec_docto = h-fi-cod-espec:SCREEN-VALUE
                     ext_espec_docto.LOG_imposto     = IF CHBOX-IMP:SCREEN-VALUE  = "YES" THEN TRUE ELSE FALSE
                     ext_espec_docto.LOG_fgts        = IF CHBOX-FGTS:SCREEN-VALUE = "YES" THEN TRUE ELSE FALSE
                     ext_espec_docto.LOG_GPS         = IF CHBOX-GPS:SCREEN-VALUE  = "YES" THEN TRUE ELSE FALSE.

        END.
        ELSE
              ASSIGN ext_espec_docto.cod_espec_docto = h-fi-cod-espec:SCREEN-VALUE
                            ext_espec_docto.LOG_imposto     = IF CHBOX-IMP:SCREEN-VALUE  = "YES" THEN TRUE ELSE FALSE
                            ext_espec_docto.LOG_fgts        = IF CHBOX-FGTS:SCREEN-VALUE = "YES" THEN TRUE ELSE FALSE
                            ext_espec_docto.LOG_GPS         = IF CHBOX-GPS:SCREEN-VALUE  = "YES" THEN TRUE ELSE FALSE.

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
/*                     hframe:TYPE                    */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */

            IF hFrame:NAME = pNome THEN
            DO:
                ASSIGN pObject = hFrame.
                LEAVE.
            END.

            ASSIGN hFrame = hFrame:NEXT-SIBLING.
        END.
        ELSE
            ASSIGN hFrame = hFrame:FIRST-CHILD.
    END.
END PROCEDURE.

PROCEDURE piBuscaLiteral:
    DEF INPUT  PARAM pFrame    AS WIDGET-HANDLE.

    ASSIGN
    h-bt-literal[1] = ?
    h-bt-literal[2] = ?
    h-bt-literal[3] = ?.

    DEF VAR  pObject   AS WIDGET-HANDLE.
    DEF VAR hFrame     AS WIDGET-HANDLE.
    DEF VAR whObjeto   AS WIDGET-HANDLE.


    ASSIGN hFrame = pFrame:FIRST-CHILD.

    DO WHILE VALID-HANDLE(hFrame):

        IF hFrame:TYPE <> "field-group" THEN
        DO:

            IF hFrame:Type = "frame" THEN
            DO:
                RUN piBuscaLiteral(INPUT  hFrame,
                                  OUTPUT whObjeto).
                IF whObjeto <> ? THEN
                DO:
                    ASSIGN pObject = whObjeto.
                    LEAVE.
                END.
            END.
                                                        
/*             MESSAGE hFrame:NAME SKIP               */
/*                     hFrame:TOOLTIP SKIP            */
/*                     hframe:TYPE                    */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */

            IF hFrame:NAME = ? AND hFrame:TYPE = "LITERAL" THEN
            DO:
/*                                                        */
/*                 MESSAGE hFrame:TYPE SKIP               */
/*                         STRING(hFrame)                 */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                

               IF NOT VALID-HANDLE(h-bt-literal[1]) THEN DO:
                  h-bt-literal[1] = hFrame.
                  ASSIGN hFrame = hFrame:NEXT-SIBLING.
               END.
               ELSE IF NOT VALID-HANDLE(h-bt-literal[2]) THEN DO:
                  h-bt-literal[2] = hFrame.
                  ASSIGN hFrame = hFrame:NEXT-SIBLING.
               END.
               ELSE DO:
                  h-bt-literal[3] = hFrame.
                  ASSIGN hFrame = hFrame:NEXT-SIBLING. 
               END.


            END.

            ASSIGN hFrame = hFrame:NEXT-SIBLING.
        END.
        ELSE
            ASSIGN hFrame = hFrame:FIRST-CHILD.
    END.
END PROCEDURE.
