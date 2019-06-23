/*-------------------------------------------------------------------------------
Programa: re9340-epc.p
Objetivo: Epc no programa re9340-epc - Para alterar titulo a Pagar gerado
Autor:    Joao B. C. Bisneto - DSC
Cliente:  Yamana Gold
-------------------------------------------------------------------------------*/
{cdp/cdcfgdis.i}
{include/i-epc200.i1}
/*-------------------------------------------------------------------------------*/
DEF INPUT        PARAM p-ind-event AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM TABLE FOR tt-epc.
/* MESSAGE "====>re9340-epc.p<====="      */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*-------------------------------------------------------------------------------*/
DEF VAR l-complementar   AS LOG           NO-UNDO.
DEF VAR r-docum-est-fil  AS ROWID         NO-UNDO.
DEF VAR r-docum-est-ori  AS ROWID         NO-UNDO.
DEF VAR r-embarque-imp   AS ROWID         NO-UNDO.
DEF VAR c-texto          AS CHAR EXTENT 7 NO-UNDO.
/*-------------------------------------------------------------------------------*/
def buffer bf-tt-epc    for tt-epc.
DEF BUFFER bf-docum-est-rateio    FOR docum-est. 
DEF BUFFER bf-docum-est-principal FOR docum-est. 
/*-------------------------------------------------------------------------------*/
IF  p-ind-event = "AlteraNumTitulo":U THEN 
  DO:
    ASSIGN l-complementar = NO.    
    FOR FIRST tt-epc
      WHERE tt-epc.cod-event     = "AlteraNumTitulo":U
      AND   tt-epc.cod-parameter = "r-docum-est":U :
        /*-------------------------------------------------------------------------------*/
        FIND bf-docum-est-principal /* NF complementar */
          NO-LOCK
          WHERE ROWID(bf-docum-est-principal) = TO-ROWID(tt-epc.val-parameter) 
          NO-ERROR.
        IF AVAIL bf-docum-est-principal THEN
          DO:
            /*
            MESSAGE 
              "DOCUMENTO PRINCIPAL" SKIP
              "serie-docto  " bf-docum-est-principal.serie-docto  skip
              "nro-docto    " bf-docum-est-principal.nro-docto    skip
              "cod-emitente " bf-docum-est-principal.cod-emitente skip
              "nat-operacao " bf-docum-est-principal.nat-operacao skip
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
            */  
            FIND natur-oper
              NO-LOCK
              WHERE natur-oper.nat-operacao = bf-docum-est-principal.nat-operacao
              NO-ERROR.
            IF AVAIL natur-oper THEN
              DO:
                /*
                MESSAGE 
                  "NATUREZA DE OPERAÄ«O DO DOC PRINCIPAL" SKIP
                  "nat-operacao " natur-oper.nat-operacao SKIP
                  "nota-rateio  " natur-oper.nota-rateio  SKIP
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
                */  
                IF 
                  natur-oper.nat-operacao BEGINS "3" AND 
                  natur-oper.nota-rateio  = YES      THEN
                  DO:
                    /*---------------------------------------------------------------------*/
                    FOR FIRST rat-docum
                      NO-LOCK
                      WHERE rat-docum.serie-docto = bf-docum-est-principal.serie-docto
                      AND   rat-docum.nro-docto   = bf-docum-est-principal.nro-docto:
                        FIND bf-docum-est-rateio /* NF original */
                          NO-LOCK
                          WHERE bf-docum-est-rateio.serie-docto  = rat-docum.nf-serie
                          and   bf-docum-est-rateio.nro-docto    = rat-docum.nf-nro
                          and   bf-docum-est-rateio.cod-emitente = rat-docum.nf-emitente
                          and   bf-docum-est-rateio.nat-operacao = rat-docum.nf-nat-oper
                          NO-ERROR.
                        FIND FIRST embarque-imp
                          NO-LOCK
                          WHERE embarque-imp.cod-estabel = bf-docum-est-rateio.cod-estabel
                          AND   embarque-imp.embarque    = SUBSTRING(bf-docum-est-rateio.char-1,1,12)
                          NO-ERROR.
                        /*
                        IF AVAIL embarque-imp THEN
                          MESSAGE "O Embarque Ç " embarque-imp.embarque
                              VIEW-AS ALERT-BOX INFO BUTTONS OK.
                        MESSAGE 
                          "DOCUMENTO DO RATEIO" SKIP
                          "serie-docto  " bf-docum-est-rateio.serie-docto  skip
                          "nro-docto    " bf-docum-est-rateio.nro-docto    skip
                          "cod-emitente " bf-docum-est-rateio.cod-emitente skip
                          "nat-operacao " bf-docum-est-rateio.nat-operacao skip
                          VIEW-AS ALERT-BOX INFO BUTTONS OK.
                        */  
                      END. /* FOR FIRST rat-docum */
                    /*---------------------------------------------------------------------*/
                    IF 
                      AVAIL bf-docum-est-principal AND 
                      AVAIL bf-docum-est-rateio    THEN
                      DO:
                        /*
                        MESSAGE 
                          "NATUREZA DE OPERAÄ«O DO DOC RATEIO" SKIP
                          "nat-operacao " bf-docum-est-rateio.nat-operacao SKIP
                          VIEW-AS ALERT-BOX INFO BUTTONS OK.
                        */  
                        IF bf-docum-est-rateio.nat-operacao BEGINS "3" THEN
                          DO:
                            RUN pi-troca-numero.
                          END.
                      END.
                    /*---------------------------------------------------------------------*/
                  END.
              END. /* IF AVAIL natur-oper THEN */
          END. /* IF AVAIL docum-est THEN */
      END. /* FOR FIRST tt-epc WHERE tt-epc.cod-event = "AlteraNumTitulo":U*/
  END. /* IF  p-ind-event = "AlteraNumTitulo":U THEN  */
PROCEDURE pi-troca-numero:
    /*-----------------------------------------------*/
    DEF VAR c-nro-tit LIKE tit_ap.cod_tit_ap NO-UNDO.  
    /*-----------------------------------------------*/
    FOR EACH tt-epc
      WHERE tt-epc.cod-event     = "AlteraNumTitulo":U
      AND   tt-epc.cod-parameter = "tt-lin-i-ap":U : 
        ASSIGN c-nro-tit = "".
        /*
        MESSAGE 
           "embarque-imp.cod-estabel " embarque-imp.cod-estabel skip
           "docum-est.serie-docto    " bf-docum-est-principal.serie-docto   SKIP 
           "embarque-imp.embarque    " embarque-imp.embarque    skip
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        */    
        FOR EACH tit_ap
          NO-LOCK
          WHERE tit_ap.cod_estab          =      embarque-imp.cod-estabel
            /* AND   tit_ap.cdn_fornecedor     =      docum-est.cod-emitente */
            /* AND   tit_ap.cod_espec_docto = TRIM(STRING(docum-est.esp-docto)) */
            AND   tit_ap.cod_ser_docto      =      bf-docum-est-principal.serie-docto
            AND   tit_ap.cod_tit_ap         BEGINS embarque-imp.embarque + "C"
            AND   tit_ap.cod_parcela        =      "1":
              IF tit_ap.cod_tit_ap > c-nro-tit THEN
                ASSIGN c-nro-tit = tit_ap.cod_tit_ap.
              /*
              MESSAGE "=====> " tit_ap.cod_tit_ap SKIP c-nro-tit
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              */    
          END. /* FOR EACH tit_ap NO-LOCK WHERE tit_ap.cod_estab = embarque-imp.cod-estabel */
        /*
        MESSAGE "calculo do maior existente c-nro-tit " c-nro-tit
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        */    
        IF c-nro-tit = "" THEN
          DO:
            /*
            MESSAGE "VAI SER " embarque-imp.embarque + "C1"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            */    
            ASSIGN c-nro-tit = embarque-imp.embarque + "C1".
          END.
        ELSE
          DO:
            /*
            MESSAGE "O INTEIRO ê " + ENTRY(2,c-nro-tit,"C")
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            */    
            ASSIGN 
              c-nro-tit = ENTRY(2,c-nro-tit,"C")
              c-nro-tit = embarque-imp.embarque + "C" + TRIM(STRING(INT(c-nro-tit) + 1)).
          END.
        CREATE bf-tt-epc.
        ASSIGN 
          bf-tt-epc.cod-event     = "AlteraNumTitulo":U
          bf-tt-epc.cod-parameter = "retorno-tt-lin-i-ap":U.
        /*
        MESSAGE 
            "O NRO. DO TITULO SERµ " c-nro-tit
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        */    
        ASSIGN 
          c-texto[01]          =  ENTRY(1,tt-epc.val-parameter,CHR(24)) /* empresa */
          c-texto[02]          =  ENTRY(2,tt-epc.val-parameter,CHR(24)) /* referencia */
          c-texto[03]          =  ENTRY(3,tt-epc.val-parameter,CHR(24)) /* sequencia */
          c-texto[04]          =  ENTRY(4,tt-epc.val-parameter,CHR(24)) /* seq-import*/
          c-texto[05]          =  /* ENTRY(5,tt-epc.val-parameter,CHR(24)) */ c-nro-tit  /* nro-docto*/
          c-texto[06]          =  ENTRY(6,tt-epc.val-parameter,CHR(24)) /* parcela */
          bf-tt-epc.val-parameter =  
            c-texto[01] + CHR(24) + 
            c-texto[02] + CHR(24) + 
            c-texto[03] + CHR(24) + 
            c-texto[04] + CHR(24) + 
            c-texto[05] + CHR(24) + 
            c-texto[06] + CHR(24).        
      END. /*     FOR EACH tt-epc WHERE tt-epc.cod-event = "AlteraNumTitulo":U*/
    /*-----------------------------------------------*/
  END PROCEDURE. /* PROCEDURE pi-troca-numero: */
/*================================================================================================*/
