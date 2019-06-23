/*------------------------------------------------------------------------
    File        : tw-item-doc-est.p
    Description : Trigger para criar duplicata dos impostos na nf de 
                  nacionalizacao
    Author(s)   : Joao B. C. Bisneto
------------------------------------------------------------------------*/
DEF PARAMETER BUFFER b-new-item-doc-est FOR item-doc-est.
DEF PARAMETER BUFFER b-old-item-doc-est FOR item-doc-est. 
/*------------------------------------------------------------------------*/
/* MESSAGE "----tw-item-doc-est.p----"    */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*------------------------------------------------------------------------*/
DEF VAR de-valor-ipi  AS DEC FORMAT ">>>>,>>>,>>9.99" NO-UNDO.
DEF VAR de-valor-icm  AS DEC FORMAT ">>>>,>>>,>>9.99" NO-UNDO.
DEF VAR de-valor-pis  AS DEC FORMAT ">>>>,>>>,>>9.99" NO-UNDO.
DEF VAR de-val-cofins AS DEC FORMAT ">>>>,>>>,>>9.99" NO-UNDO.
DEF VAR l-achou       AS LOG                          NO-UNDO.
def var h-bocx255     as handle                       no-undo.
def var r-chave       as rowid                        no-undo.
/*------------------------------------------------------------------------*/
DEFINE BUFFER bf-item-doc-est      FOR item-doc-est.
DEFINE BUFFER bf-dupli-apagar-cex  FOR dupli-apagar-cex.
DEFINE BUFFER bf2-dupli-apagar-cex FOR dupli-apagar-cex.
/*------------------------------------------------------------------------*/
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
/*------------------------------------------------------------------------*/
DEF TEMP-TABLE tt-dupli-apagar-cex
  LIKE dupli-apagar-cex
  FIELD r-rowid AS ROWID.
{include/boerrtab.i}
{cdp/cdcfgdis.i}
{cdp/cdcfgmat.i}
/*------------------------------------------------------------------------*/
IF /* NEW b-new-item-doc-est AND */ b-new-item-doc-est.valor-icm[1] > 0 THEN
  DO:
    FOR FIRST docum-est
      NO-LOCK
      WHERE docum-est.serie-docto  = b-new-item-doc-est.serie-docto  
      AND   docum-est.nro-docto    = b-new-item-doc-est.nro-docto    
      AND   docum-est.cod-emitente = b-new-item-doc-est.cod-emitente 
      AND   docum-est.nat-operacao = b-new-item-doc-est.nat-operacao:    
        /*---------------------------------------------------------*/
        ASSIGN
          de-valor-ipi  = 0
          de-valor-icm  = 0
          de-valor-pis  = 0
          de-val-cofins = 0
          l-achou       = NO.
        RUN pi-total-impostos(
          OUTPUT de-valor-ipi,
          OUTPUT de-valor-icm,
          OUTPUT de-valor-pis,
          OUTPUT de-val-cofins).
        /*----IPI-----------------------------------------------------------------------*/
        IF de-valor-ipi > 0 THEN
          RUN pi-cria-tt-dupli-apagar-cex(INPUT de-valor-ipi,  INPUT 50).
        /*----PIS-----------------------------------------------------------------------*/
        IF de-valor-icm > 0 THEN
          RUN pi-cria-tt-dupli-apagar-cex(INPUT de-valor-icm,  INPUT 51).
        /*----COFINS--------------------------------------------------------------------*/
        IF de-valor-pis > 0 THEN
          RUN pi-cria-tt-dupli-apagar-cex(INPUT de-valor-pis,  INPUT 52).
        /*----ICMS----------------------------------------------------------------------*/
        IF de-val-cofins > 0 THEN
          RUN pi-cria-tt-dupli-apagar-cex(INPUT de-val-cofins, INPUT 53).
        /*---------------------------------------------------------*/
      END.
  END.
/*---------------------------------------------------------*/
PROCEDURE pi-cria-tt-dupli-apagar-cex:
  
  DEF INPUT PARAM p-vl-a-pagar LIKE tt-dupli-apagar-cex.vl-a-pagar NO-UNDO.
  DEF INPUT PARAM p-parcela    LIKE dupli-apagar-cex.parcela       NO-UNDO.
  FIND FIRST dupli-apagar-cex 
      NO-LOCK
      WHERE dupli-apagar-cex.serie-docto        = b-new-item-doc-est.serie-docto  
      AND dupli-apagar-cex.nro-docto            = b-new-item-doc-est.nro-docto    
      AND dupli-apagar-cex.cod-emitente         = b-new-item-doc-est.cod-emitente 
      AND dupli-apagar-cex.nat-operacao         = b-new-item-doc-est.nat-operacao 
      /* AND dupli-apagar-cex.cod-emitente-desp    = */
      /* AND dupli-apagar-cex.parcela              = "1" */
      AND dupli-apagar-cex.cod-esp              = "DI"
      NO-ERROR.
  IF
    AVAIL dupli-apagar-cex                   AND 
    dupli-apagar-cex.nat-operacao BEGINS "3" THEN 
    DO:
      ASSIGN r-chave = ?.
      /*-------------------------------------------------------*/
      /*       for each tt-dupli-apagar-cex:   */
      /*           delete tt-dupli-apagar-cex. */
      /*         end.                          */
      FIND FIRST bf-dupli-apagar-cex 
        NO-LOCK
        WHERE bf-dupli-apagar-cex.serie-docto        = dupli-apagar-cex.serie-docto      
        AND   bf-dupli-apagar-cex.nro-docto          = dupli-apagar-cex.nro-docto        
        AND   bf-dupli-apagar-cex.cod-emitente       = dupli-apagar-cex.cod-emitente     
        AND   bf-dupli-apagar-cex.nat-operacao       = dupli-apagar-cex.nat-operacao     
        AND   bf-dupli-apagar-cex.cod-emitente-desp  = dupli-apagar-cex.cod-emitente-desp
        AND   bf-dupli-apagar-cex.parcela            = p-parcela
        NO-ERROR.
      IF NOT AVAIL bf-dupli-apagar-cex THEN
        DO:
          create bf2-dupli-apagar-cex.
          BUFFER-COPY dupli-apagar-cex EXCEPT parcela vl-a-pagar cod-esp TO bf2-dupli-apagar-cex.
          ASSIGN
            bf2-dupli-apagar-cex.parcela           = STRING(p-parcela)
            bf2-dupli-apagar-cex.vl-a-pagar        = p-vl-a-pagar
            bf2-dupli-apagar-cex.cod-esp           = "DP". 
          IF p-parcela = "53" THEN
            DO:
              FIND CURRENT dupli-apagar-cex EXCLUSIVE-LOCK NO-ERROR.
              ASSIGN dupli-apagar-cex.cod-esp          = "DP".
              FIND CURRENT dupli-apagar-cex NO-LOCK NO-ERROR.
            END.
        END.
      ELSE
        DO:
          FIND CURRENT bf-dupli-apagar-cex EXCLUSIVE-LOCK.
          ASSIGN bf-dupli-apagar-cex.vl-a-pagar = p-vl-a-pagar.
          FIND CURRENT bf-dupli-apagar-cex NO-LOCK.
        END.
      /*
      IF NOT VALID-HANDLE(h-bocx255) THEN
        run cxbo/bocx255.p persistent set h-bocx255.
      /*-------------------------------------------------------*/
      IF VALID-HANDLE(h-bocx255) THEN
        run validateCreate in h-bocx255(
          input table tt-dupli-apagar-cex,
          output table tt-bo-erro,
          output r-chave).
      find first tt-bo-erro no-error.
      if avail tt-bo-erro then 
        do:
          run utp/ut-msgs.p (
            input "show",
            input tt-bo-erro.cd-erro,
            input tt-bo-erro.mensagem).
          return "adm-error".                         
        end. /* if avail tt-bo-erro then  */
      IF VALID-HANDLE(h-bocx255) THEN
        delete procedure h-bocx255. 
      */  
      /*-------------------------------------------------------*/
    END.
END PROCEDURE. /* pi-cria-tt-dupli-apagar-cex */
PROCEDURE pi-total-impostos:
  DEF OUTPUT PARAM p-valor-ipi  AS DEC FORMAT ">>>>,>>>,>>9.99" NO-UNDO. 
  DEF OUTPUT PARAM p-valor-icm  AS DEC FORMAT ">>>>,>>>,>>9.99" NO-UNDO. 
  DEF OUTPUT PARAM p-valor-pis  AS DEC FORMAT ">>>>,>>>,>>9.99" NO-UNDO. 
  DEF OUTPUT PARAM p-val-cofins AS DEC FORMAT ">>>>,>>>,>>9.99" NO-UNDO. 
  FOR EACH bf-item-doc-est 
    NO-LOCK
    WHERE bf-item-doc-est.serie-docto   = docum-est.serie-docto  
    and   bf-item-doc-est.nro-docto     = docum-est.nro-docto    
    and   bf-item-doc-est.cod-emitente  = docum-est.cod-emitente 
    and   bf-item-doc-est.nat-operacao  = docum-est.nat-operacao:
      ASSIGN            
        p-valor-ipi  = p-valor-ipi  + bf-item-doc-est.valor-ipi[1] 
        p-valor-icm  = p-valor-icm  + bf-item-doc-est.valor-icm[1] 
        p-valor-pis  = p-valor-pis  + bf-item-doc-est.valor-pis    
        p-val-cofins = p-val-cofins + bf-item-doc-est.val-cofins
        l-achou       = YES.   
    END. /* FOR EACH item-doc-est */   
END PROCEDURE.
