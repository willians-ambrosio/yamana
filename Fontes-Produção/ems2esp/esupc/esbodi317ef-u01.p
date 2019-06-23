/*****************************************************************************************************************/
/* Programa     : esbodi317ef-u00.p                                                                                */
/* Descriªío    : EPC do programa bodi317ef                                                                      */
/* Desenvolvedor: Sergio Luiz Neto da Silveira - DSC Praxis                                                      */
/*****************************************************************************************************************/
                                                                                      
       
{method/dbotterr.i}  
{include/i-epc200.i1}

DEF INPUT PARAM p-ind-event AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc. 

def new global shared var v_cod_usuar_corren  as char   no-undo.

DEFINE TEMP-TABLE tt-es-it-nota-fisc-terc LIKE es-it-nota-fisc-terc.

/*************** TRATAMENTO INCLUSÄO DO REDESPACHANTE NAS OBSERVA∞ÜES DA NOAT **************/
IF p-ind-event = "afterCriaItNotaFisc" THEN DO:
   FIND FIRST tt-epc
        WHERE tt-epc.cod-event = p-ind-event
          AND tt-epc.cod-param = "Rowid_WtItDocto_ItNotaFisc" NO-ERROR.
   IF NOT AVAIL tt-epc THEN NEXT.

   FIND FIRST wt-it-docto 
        WHERE ROWID(wt-it-docto) = TO-ROWID(ENTRY(1,tt-epc.val-parameter,","))
        NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(wt-it-docto) THEN 
      NEXT.

   FIND FIRST it-nota-fisc 
        WHERE ROWID(it-nota-fisc) = TO-ROWID(ENTRY(2,tt-epc.val-parameter,","))
        NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(it-nota-fisc) THEN 
      NEXT.

   /* Relaciona as duas tabelas antes de eliminar a wt-it-docto/es-wt-it-docto */
   FIND FIRST es-wt-it-docto OF wt-it-docto
        NO-LOCK NO-ERROR.
   IF AVAILABLE(es-wt-it-docto) THEN DO:
      CREATE es-it-nota-fisc-terc.
      ASSIGN es-it-nota-fisc-terc.cod-estabel       = it-nota-fisc.cod-estabel 
             es-it-nota-fisc-terc.serie             = it-nota-fisc.serie
             es-it-nota-fisc-terc.nr-nota-fisc      = it-nota-fisc.nr-nota-fis
             es-it-nota-fisc-terc.nr-seq-fat        = it-nota-fisc.nr-seq-fat
             es-it-nota-fisc-terc.it-codigo         = it-nota-fisc.it-codigo
             es-it-nota-fisc-terc.cod-usuario       = es-wt-it-docto.cod-usuario
             es-it-nota-fisc-terc.cod-usuario-aprov = es-wt-it-docto.cod-usuario-aprov
             es-it-nota-fisc-terc.usuar-atualizacao = es-wt-it-docto.usuar-atualizacao
             es-it-nota-fisc-terc.data-atual        = es-wt-it-docto.data-atual       
             es-it-nota-fisc-terc.hora-atual        = es-wt-it-docto.hora-atual.       
   END.
END.



/*
IF p-ind-event = "EndEfetivaNota" THEN DO:
   FIND FIRST tt-epc
        WHERE tt-epc.cod-event = p-ind-event
          AND tt-epc.cod-param = "ROWID(nota-fiscal)" NO-ERROR.
   IF NOT AVAIL tt-epc THEN NEXT.

   FIND FIRST nota-fiscal EXCLUSIVE-LOCK
        WHERE ROWID(nota-fiscal) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.
   IF NOT AVAIL nota-fiscal THEN NEXT.

   FIND FIRST natur-oper 
        WHERE natur-oper.nat-operacao = nota-fiscal.nat-operacao 
        NO-LOCK NO-ERROR.
   IF AVAILABLE(natur-oper) AND natur-oper.terceiros THEN DO:
      EMPTY TEMP-TABLE tt-es-it-nota-fisc-terc.

      RUN esp/esft4003.w (INPUT-OUTPUT TABLE tt-es-it-nota-fisc-terc).

      IF CAN-FIND(FIRST tt-es-it-nota-fisc-terc
                  WHERE tt-es-it-nota-fisc-terc.cod-usuario = ""
                  NO-LOCK) THEN DO:
         RUN utp/ut-msgs.p (INPUT "show",
                            INPUT 17006,
                            INPUT "Usu†rio n∆o informado para item da nota~~Usu†rio n∆o informado para item da nota").
         RETURN "NOK":U.
      END.
      ELSE DO:
         FOR EACH tt-es-it-nota-fisc-terc
                  NO-LOCK:
            CREATE es-it-nota-fisc-terc.
            BUFFER-COPY tt-es-it-nota-fisc-terc TO es-it-nota-fisc-terc.

            ASSIGN es-it-nota-fisc-terc.data-atual  = TODAY
                   es-it-nota-fisc-terc.hora-atual  = STRING(TIME,"hh:mm:ss")
                   es-it-nota-fisc-terc.usuar-atual = v_cod_usuar_corren.
         END.
      END.
   END.
END.

RETURN "OK".

*/
