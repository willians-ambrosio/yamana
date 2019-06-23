
/****************************************************************************************** 
** 	   Programa: add_cta_corren-u01
**   	  Autor: Felipe Vieira
** 	 Fornecedor: DKP
**         Data: 22/08/2018
** Change/Chamado: 
**      Objetivo: Criar campo "imposto" e solicitar o c¢digo do tributo qdo for verdadeiro - Usado apenas qdo o 
                  bordorì n∆o for de DARF
**
******************************** CONTROLE DE ALTERAÄÂES *********************************
** Data         Autor   		Fornecedor  Change/Chamado Descriá∆o da Alteraá∆o
**
**
****************************** INFORMAÄÂES ADICIONAIS ************************************
** PAR∂METROS DE ENTRADA: N/A
** PAR∂METROS DE SA÷DA: N/A
** CADASTRADO NO FONTE TOTVS: add_cta_corren,mod_cta_corren_basico   
** CADASTRADO NA TABELA: N/A
******************************************************************************************/
{utp\ut-glob.i}
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS RECID NO-UNDO.

/*                                                        */
/* MESSAGE "p-ind-evento.....: " p-ind-event         SKIP */
/*         "p-ind-objeto.....: " p-ind-object        SKIP */
/*         "Handle do Objeto.: " p-wgh-object        SKIP */
/*         "Frame............: " p-wgh-frame         SKIP */
/*         "Nome da tabela...: " p-cod-table         SKIP */
/*         "Rowid da tabela..: " STRING(p-row-table) SKIP */
/*     VIEW-AS ALERT-BOX TITLE "bas-espec-docto".         */

DEFINE NEW GLOBAL SHARED VARIABLE aux-contaCorrente      AS WIDGET-HANDLE NO-UNDO.



IF p-ind-event = "ENABLE" THEN DO:

    RUN piBuscaWidget (INPUT "cod_cta_corren",
                       INPUT p-wgh-frame,
                       OUTPUT aux-contaCorrente).

  

IF VALID-HANDLE(aux-contaCorrente)
    THEN DO:
 MESSAGE "OK"
     VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.  

END.

IF p-ind-event = "VALIDATE" THEN DO:



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

