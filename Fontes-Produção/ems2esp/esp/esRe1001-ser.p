/*---------------------------------------------------------------------------
Programa: esRe1001-ser.p
Objetivo: Eliminar e Recriar  a duplicata a pagar para rec de serviáos.
Autor   : Joao B. C. Bisneto
-----------------------------------------------------------------------------*/
DEF INPUT  PARAM p-docum-est               AS ROWID                        NO-UNDO.
DEF OUTPUT PARAM p-tipo                    AS CHAR                         NO-UNDO.
DEF OUTPUT PARAM p-embarque                LIKE embarque-imp.embarque      NO-UNDO.
DEF OUTPUT PARAM p-est-embarque            LIKE embarque-imp.cod-estabel   NO-UNDO.
/*---------------------------------------------------------------------------*/
DEF VAR l-complementar   AS LOG           NO-UNDO.
DEF VAR r-docum-est-fil  AS ROWID         NO-UNDO.
DEF VAR r-docum-est-ori  AS ROWID         NO-UNDO.
DEF VAR r-embarque-imp   AS ROWID         NO-UNDO.
/*---------------------------------------------------------------------------*/
DEF BUFFER bf-docum-est  FOR docum-est. 
DEF BUFFER bf2-docum-est FOR docum-est.
/*---------------------------------------------------------------------------*/
/* MESSAGE                                */
/*     "Vou verificar se Ç S ou F"        */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
ASSIGN
  p-tipo         = ""
  p-embarque     = ""
  p-est-embarque = "".
FIND FIRST docum-est
  NO-LOCK
  WHERE ROWID(docum-est) = p-docum-est
  NO-ERROR.
IF NOT AVAIL docum-est THEN
  DO:
    /*
    MESSAGE 
      "Documento de Estoque n∆o localizado!"
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    */  
  END.
ELSE
IF AVAIL docum-est THEN
  DO:
    /*---- Verifica se eh frete --------------------------------------------*/
    IF docum-est.esp-fiscal = "CTE" THEN
      DO:
        /*----------------------------------------------------*/
        FIND natur-oper
          NO-LOCK
          WHERE natur-oper.nat-operacao = docum-est.nat-operacao
          NO-ERROR.
        IF  
          natur-oper.nat-operacao BEGINS "1" OR 
          natur-oper.nat-operacao BEGINS "2" THEN
          DO:
            /*----------------------------------------------------------------*/
            FOR FIRST rat-docum
              NO-LOCK
              WHERE rat-docum.serie-docto = docum-est.serie-docto
              AND   rat-docum.nro-docto   = docum-est.nro-docto:
                FIND bf-docum-est /* NF do rateio */
                  NO-LOCK
                  WHERE bf-docum-est.serie-docto  = rat-docum.nf-serie
                  and   bf-docum-est.nro-docto    = rat-docum.nf-nro
                  and   bf-docum-est.cod-emitente = rat-docum.nf-emitente
                  and   bf-docum-est.nat-operacao = rat-docum.nf-nat-oper
                  NO-ERROR.
                IF AVAIL bf-docum-est THEN
                  DO:
                    IF bf-docum-est.nat-operacao BEGINS "3" THEN
                      DO:
                        /*----------------------------------------------------*/
                        FIND FIRST embarque-imp
                          NO-LOCK
                          WHERE embarque-imp.cod-estabel = bf-docum-est.cod-estabel
                          AND   embarque-imp.embarque    = SUBSTRING(bf-docum-est.char-1,1,12)
                          NO-ERROR.
                        IF AVAIL embarque-imp THEN
                          DO:
                            /*--------------------------------------------------------*/
                            ASSIGN 
                              p-embarque     = embarque-imp.embarque
                              p-est-embarque = embarque-imp.cod-estabel
                              p-tipo         = "F".
                            /*
                            MESSAGE "ê Frete"
                                VIEW-AS ALERT-BOX INFO BUTTONS OK.
                            */    
                            /*--------------------------------------------------------*/
                          END. /* IF AVAIL embarque-imp THEN */
                        /*----------------------------------------------------*/
                      END. /* IF bf-docum-est.nat-operacao BEGINS "3" THEN */
                  END. /* IF AVAIL bf-docum-est THEN */
              END. /* FOR FIRST rat-docum */    
            /*----------------------------------------------------------------*/
          END.
        /*----------------------------------------------------*/
      END.
    /*---- Verifica se eh servico --------------------------------------------*/
    ELSE
    IF docum-est.esp-fiscal = "NFE" THEN
      DO:
        /*----------------------------------------------------------------------------*/
        IF docum-est.cod-observa = 4 THEN 
          DO:
            IF  
              docum-est.nat-operacao BEGINS "1" OR 
              docum-est.nat-operacao BEGINS "2" THEN
              DO:
                /*----------------------------------------------------------------------------*/
                FOR FIRST rat-docum
                  NO-LOCK
                  WHERE rat-docum.serie-docto = docum-est.serie-docto
                  AND   rat-docum.nro-docto   = docum-est.nro-docto:
                    FIND bf-docum-est /* NF do rateio */
                      NO-LOCK
                      WHERE bf-docum-est.serie-docto  = rat-docum.nf-serie
                      and   bf-docum-est.nro-docto    = rat-docum.nf-nro
                      and   bf-docum-est.cod-emitente = rat-docum.nf-emitente
                      and   bf-docum-est.nat-operacao = rat-docum.nf-nat-oper
                      NO-ERROR.
                    IF AVAIL bf-docum-est THEN
                      DO:
                        IF bf-docum-est.nat-operacao BEGINS "3" THEN
                          DO:
                            /*----------------------------------------------------*/
                            FIND FIRST embarque-imp
                              NO-LOCK
                              WHERE embarque-imp.cod-estabel = bf-docum-est.cod-estabel
                              AND   embarque-imp.embarque    = SUBSTRING(bf-docum-est.char-1,1,12)
                              NO-ERROR.
                            IF AVAIL embarque-imp THEN
                              DO:
                                /*--------------------------------------------------------*/
                                ASSIGN 
                                  p-embarque     = embarque-imp.embarque
                                  p-est-embarque = embarque-imp.cod-estabel
                                  p-tipo         = "S".
                                /*
                                MESSAGE "ê Serviáo"
                                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                                */    
                                /*--------------------------------------------------------*/
                              END. /* IF AVAIL embarque-imp THEN */
                            /*----------------------------------------------------*/
                          END. /* IF bf-docum-est.nat-operacao BEGINS "3" THEN */
                      END. /* IF AVAIL bf-docum-est THEN */
                  END. /* FOR FIRST rat-docum */    
                /*----------------------------------------------------------------------------*/
              END. /* IF docum-est.nat-operacao BEGINS "1" OR docum-est.nat-operacao BEGINS "2" THEN*/
          END. /* IF docum-est.cod-observa = 4 THEN */
        /*----------------------------------------------------------------------------*/
      END. /* IF docum-est.esp-fiscal = "NFE" THEN */
    /*----------------------------------------------------------------------------*/
  END. /* IF AVAIL docum-est THEN */
