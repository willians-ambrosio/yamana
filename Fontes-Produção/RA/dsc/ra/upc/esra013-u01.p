/******************************************************************
**
** Programa: ESRA013-U01
**
** Objetivo: UPC chamadora da ESRA013
**
**    Autor: Willians Ambrosio / Grupo DKP
**
**     Data: JUN/2018
**
******************************************************************/
/* ============================================================= */
{include/i-prgvrs.i ESRA013-U01 12.01.19.000}
/* ============================================================= */
{include/i-epc200.i ESRA013}
/* ============================================================= */
Def Input Param p-ind-event As Char No-undo.
Def Input-output Param Table For tt-epc.
/* ============================================================= */
If (p-ind-event = "BuscaNatOperAutomYamana") Then 
Do:
   For Each tt-epc
      Where tt-epc.cod-event = p-ind-event :

      If tt-epc.cod-param = 'Table-Rowid' Then 
      Do:
         For First nfe-it-nota-fisc-rec                                                            
             Fields(nfe-it-nota-fisc-rec nr-pedcli nr-sequencia it-codigo cod-refer) No-lock     
             Where Rowid(nfe-it-nota-fisc-rec) = To-rowid(tt-epc.val-param):     

             FIND FIRST nfe-nota-fiscal-rec WHERE 
                        nfe-nota-fiscal-rec.chave-acesso-nfe = nfe-it-nota-fisc-rec.chave-acesso-nfe NO-LOCK NO-ERROR.
             IF AVAIL nfe-nota-fiscal-rec                         AND
                TRIM(nfe-it-nota-fisc-rec.item-nat-operacao) = "" THEN 
             DO:
                FIND FIRST emitente WHERE
                           emitente.nome-abrev = nfe-nota-fiscal-rec.nome-abrev NO-LOCK NO-ERROR.
                IF AVAIL emitente THEN
                DO:
                   FIND FIRST estabelec WHERE
                              estabelec.cod-estabel = nfe-nota-fiscal-rec.cod-estabel NO-LOCK NO-ERROR.
                   IF AVAIL estabelec THEN
                   DO:          
                      /* Emitente e estabeleicmento da nota */
                      IF emitente.estado = estabelec.estado THEN
                         ASSIGN nfe-it-nota-fisc-rec.item-nat-operacao = "1999". /*Estadual*/
                      ELSE
                         ASSIGN nfe-it-nota-fisc-rec.item-nat-operacao = "2999". /*Interestadual*/    
                   END.
                END.
             END.
         END.
      End.
   End.   
END.
