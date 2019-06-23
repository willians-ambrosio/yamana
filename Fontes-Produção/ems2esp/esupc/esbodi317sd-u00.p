/*****************************************************************************************************************/
/* Programa: upc-bodi317va.p                                                                                     */
/* Descri»’o: EPC do programa de valida»’o da NF bodi317va                                                       */
/*            Realiza consist¼ncias para validar valor m­nimo da duplicata                                       */
/* Desenvolvedor: Andr² Rolino - Totvs - andre.rolino@totvs.com.br                                               */
/*****************************************************************************************************************/
/*  Historico
 17/10/2010 - Roger Bruhn - Totvs DRG SP 
    Alterado para imprimir o Transportador de Redespanho nas observa»„es da nota fiscal.
    
*/                                                                                      
       
       
DEFINE NEW GLOBAL SHARED VARIABLE hnr-sequencia AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE tt-wt-it-docto LIKE es-wt-it-docto.

{method/dbotterr.i}  
{include/i-epc200.i1}
{utp/ut-glob.i}

DEF INPUT PARAM p-ind-event AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc. 

IF p-ind-event = "afterGravainfGeraisWtItDocto" THEN DO:
   FIND FIRST tt-epc
        WHERE tt-epc.cod-event     = p-ind-event  AND
              tt-epc.cod-parameter = "table-rowid"
        NO-LOCK NO-ERROR.
   IF AVAILABLE(tt-epc) THEN DO:
      FIND FIRST wt-docto
           WHERE ROWID(wt-docto) = TO-ROWID(tt-epc.val-parameter)
           NO-LOCK NO-ERROR.

      IF AVAILABLE(wt-docto) THEN DO:
         IF VALID-HANDLE(hnr-sequencia) THEN
            FIND FIRST wt-it-docto OF wt-docto
                 WHERE wt-it-docto.nr-sequencia = INTEGER(hnr-sequencia:SCREEN-VALUE)
                 NO-LOCK NO-ERROR.
      END.

      IF AVAILABLE(wt-it-docto) THEN DO:
         FIND FIRST ITEM
              WHERE ITEM.it-codigo = wt-it-docto.it-codigo
              NO-LOCK NO-ERROR.
         IF AVAILABLE(ITEM) AND ITEM.tipo-contr = 4 /* d‚bito direto */ THEN DO:
            FIND FIRST es-conta-cfop
                 WHERE es-conta-cfop.cod-estabel  = wt-docto.cod-estabel and
                       es-conta-cfop.nat-operacao = wt-it-docto.nat-operacao 
                 NO-LOCK NO-ERROR.
            IF AVAILABLE(es-conta-cfop) THEN DO:
               ASSIGN wt-it-docto.ct-cuscon = es-conta-cfop.ct-codigo
                      wt-it-docto.sc-cuscon = es-conta-cfop.sc-codigo.
            END.
         END.
      END.
   END.
END.

IF SEARCH("esupc\esbodi317sd-u01.p") <> ? OR
   SEARCH("esupc\esbodi317sd-u01.r") <> ? THEN
    RUN esupc\esbodi317sd-u01.p (INPUT p-ind-event,
                                 INPUT-OUTPUT TABLE tt-epc).



