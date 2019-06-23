&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ES9999RP 2.00.00.000}  /*** 010008 ***/
/*****************************************************************************
**
**       PROGRAMA: ES9999
**
**       DATA....: ABRIL/2008
**
**       AUTOR...: Datasul Manufatura 
**
**       OBJETIVO: 
**
*****************************************************************************/

{include/i_fnctrad.i}

{cdp/cdcfgmnt.i}     /*include com pre-processadores de miniflexibilizacao*/

/** Defini‡Æo das temp-tables **/
{utp/ut-glob.i}      /*include necessaria para o funcionamento da f-gera-nr-ordem-aut*/
{btb/btb008za.i0}    /*include necessaria para o funcionamento da f-gera-nr-ordem-aut*/
{cdp/cd0666.i}       /* Definicao temp-table de erros */

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)":U
    field modelo           AS char format "x(35)":U
    field cal-rat          as logical
    field desc-rat         as logical
    field periodo-ini      as date format "99/99/9999"
    field periodo-fim      as date format "99/99/9999"
    /*Alterado 15/02/2005 - tech1007 - Criado campo l¢gico para verificar se o RTF foi habilitado*/
    field l-habilitaRtf    as LOG.
    /*Fim alteracao 15/02/2005*/

define temp-table tt-movto-estoq-ori no-undo like movto-estoq
    field r-rowid  as rowid.

define temp-table tt-movto-estoq-aux no-undo like movto-estoq
    field cd-tarefa like ord-taref.cd-tarefa
    field r-rowid   as rowid.

def temp-table tt-raw-digita
   field raw-digita as raw.
/****************************************************************************/
                                               /** Defini‡Æo de Parƒmetros **/   
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
/****************************************************************************/
                    /** Transferˆncia de parƒmetros para temp-table padrÆo **/
create tt-param.
raw-transfer raw-param to tt-param.
/****************************************************************************/
                                                /** Defini‡Æo de Vari veis **/
define variable h-acomp           as handle                       no-undo.
define variable c-n-gerada        as character format "X(11)":U   no-undo.
define variable i-conta-sucesso   as integer  init 0              no-undo.
define variable i-conta-erro      as integer  init 0              no-undo.
define variable i-conta-erro-ap   as integer  init 0              no-undo.
define variable d-valor-mat       like movto-estoq.valor-mat-m[1] no-undo.
define variable d-valor-mat-ant   like movto-estoq.valor-mat-m[1] no-undo.

define variable c-lit-plan-eq     as character                    no-undo.
define variable c-lit-plan-eq-tag as character                    no-undo.
define variable l-erro            AS LOGICAL                      no-undo.
define variable c-liter-sel       as character format "x(09)"     no-undo.
define variable c-liter-par       as character format "x(14)"     no-undo.
define variable c-liter-imp       as character format "x(14)"     no-undo.
define variable c-destino         as character format "x(09)"     no-undo.

define buffer bf-movto-estoq    for movto-estoq.
define buffer bf-tt-movto-estoq for tt-movto-estoq-ori.

{include/i-rpvar.i}
                                                     /** Cabe‡alho e Forms **/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 10.5
         WIDTH              = 34.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* {abp/abapi001.i2} /** Cria‡Æo de erros **/ */

/** Parametriza padräes de cabe‡alho e rodap‚ a serem exibidos **/
run piInicial in this-procedure.

/** Imprime cabe‡alho e abre o output para arquivo **/
{include/i-rpcab.i}    
{include/i-rpout.i &TOFILE=tt-param.arquivo}  /** Abertura do output do programa **/

/** Procedure para inicializar c lculos e impressÆo **/
run piPrincipal in this-procedure.

/** Fecha o output para arquivo **/
{include/i-rpclo.i}

return "OK":U. 
/*--- Fim do Programa ---*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-descalculaRateio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE descalculaRateio Procedure 
PROCEDURE descalculaRateio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE c-conta-contabil AS CHARACTER format "x(17)" NO-UNDO.
    DEFINE VARIABLE c-ct-codigo      AS CHARACTER format "x(8)"  NO-UNDO.
    DEFINE VARIABLE c-sc-codigo      AS CHARACTER format "x(8)"  NO-UNDO.

    for each  bf-movto-estoq
        where bf-movto-estoq.esp-docto  = 27 /** 27 - RDD **/
        and   bf-movto-estoq.dt-trans  >= tt-param.periodo-ini
        and   bf-movto-estoq.dt-trans  <= tt-param.periodo-fim no-lock:

        for first ord-prod
            where ord-prod.nr-ord-produ = bf-movto-estoq.nr-ord-produ no-lock:
        end.
        if not avail ord-prod then next.

        if ord-prod.origem = "MI":U then do:
            for first ord-manut
                where ord-manut.nr-ord-produ = bf-movto-estoq.nr-ord-produ no-lock:
                assign c-ct-codigo      = ord-manut.ct-desp
                       c-sc-codigo      = ord-manut.sc-desp
                       c-conta-contabil = ord-manut.conta-despesa.
            end.
        end.
        if ord-prod.origem = "MV":U then do:
            for first mmv-ord-manut
                where mmv-ord-manut.nr-ord-produ = bf-movto-estoq.nr-ord-produ no-lock:
                assign c-ct-codigo      = mmv-ord-manut.ct-ordem
                       c-sc-codigo      = mmv-ord-manut.cc-ordem
                       c-conta-contabil = mmv-ord-manut.ct-ordem + mmv-ord-manut.cc-ordem.
            end.
        end.

        /** Quando conta da OM diferente da conta do movimento,
            cria copia do registro. **/
        if bf-movto-estoq.conta-contabil <> c-conta-contabil then do:
            create tt-movto-estoq-aux.
            buffer-copy bf-movto-estoq to tt-movto-estoq-aux.
            assign tt-movto-estoq-aux.r-rowid = rowid(bf-movto-estoq).
        end.

        /** Cria copia do registro original **/
        if not can-find(first tt-movto-estoq-ori
                        where tt-movto-estoq-ori.nr-ord-produ   = bf-movto-estoq.nr-ord-produ
                        and   tt-movto-estoq-ori.conta-contabil = c-conta-contabil no-lock) then do:
            /** Se a conta do RDD diferente da conta da OM, faz copia,
                porem altera as contas para as contas da OM **/
            if bf-movto-estoq.conta-contabil <> c-conta-contabil then do:
                create tt-movto-estoq-ori.
                buffer-copy bf-movto-estoq except quantidade valor-mat-m ct-codigo sc-codigo conta-contabil to tt-movto-estoq-ori.
                assign tt-movto-estoq-ori.r-rowid        = ?
                       tt-movto-estoq-ori.valor-mat-m[1] = bf-movto-estoq.valor-mat-m[1]
                       tt-movto-estoq-ori.quantidade     = 1
                       tt-movto-estoq-ori.ct-codigo      = c-ct-codigo
                       tt-movto-estoq-ori.sc-codigo      = c-sc-codigo
                       tt-movto-estoq-ori.conta-contabil = c-conta-contabil.
            end.
            else do:
                /** Se conta do RDD igual a conta da OM,
                    apenas faz copia do registro original **/
                create tt-movto-estoq-ori.
                buffer-copy bf-movto-estoq except quantidade to tt-movto-estoq-ori.
                assign tt-movto-estoq-ori.r-rowid    = rowid(bf-movto-estoq)
                       tt-movto-estoq-ori.quantidade = 1.
            end.            
        end.
        else do:
            /** Atualiza valor da conta original **/
            for first tt-movto-estoq-ori
                where tt-movto-estoq-ori.nr-ord-produ   = bf-movto-estoq.nr-ord-produ
                and   tt-movto-estoq-ori.conta-contabil = c-conta-contabil exclusive-lock:
                /** Guarda Rowid do registro Pai (RDD Original) **/
                if  tt-movto-estoq-ori.r-rowid    = ? and 
                    bf-movto-estoq.conta-contabil = c-conta-contabil then
                    assign tt-movto-estoq-ori.r-rowid = rowid(bf-movto-estoq).

                assign tt-movto-estoq-ori.valor-mat-m[1] = tt-movto-estoq-ori.valor-mat-m[1] + bf-movto-estoq.valor-mat-m[1].
            end.
        end.
    end.

    DO  ON ERROR UNDO, RETURN ERROR
        ON STOP  UNDO, RETURN ERROR: 

        /** Efetivando altera‡Æo RDD originais **/
        for each tt-movto-estoq-ori no-lock:
            for first movto-estoq fields (nr-trans quantidade valor-mat-m)
                where rowid(movto-estoq) = tt-movto-estoq-ori.r-rowid exclusive-lock:
                assign movto-estoq.quantidade     = tt-movto-estoq-ori.quantidade
                       movto-estoq.valor-mat-m[1] = tt-movto-estoq-ori.valor-mat-m[1].
            end.
        end.
        /** Elimina RDDs filhas **/
        for each tt-movto-estoq-aux no-lock:
            for first movto-estoq
                where rowid(movto-estoq) = tt-movto-estoq-aux.r-rowid exclusive-lock:
                delete movto-estoq.
            end.
        end.
    end. /** DO **/

    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-displayDados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayDados Procedure 
PROCEDURE displayDados :
/*------------------------------------------------------------------------------
  Purpose:     displayDados
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
form tt-movto-estoq-aux.nr-ord-produ  
     tt-movto-estoq-aux.cd-tarefa     
     tt-movto-estoq-ori.conta-contabil
     tt-movto-estoq-aux.conta-contabil
     with width 132 no-attr-space no-box down frame f-dados stream-io.

put "Data e Hora da Execu‡Æo do c lculo: " string(today,"99/99/9999") + " - " + string(time,"HH:MM:SS"). 
put skip(1).
put "Periodo: " tt-param.periodo-fim.

for each tt-movto-estoq-ori:
    for each tt-movto-estoq-aux
        where tt-movto-estoq-aux.nr-ord-produ = tt-movto-estoq-ori.nr-ord-produ:
        
        disp tt-movto-estoq-aux.nr-ord-produ
             tt-movto-estoq-aux.cd-tarefa
             tt-movto-estoq-ori.conta-contabil
             tt-movto-estoq-aux.conta-contabil with frame f-dados.
    end.
end.


/*DEFINE VARIABLE i-page AS INTEGER    NO-UNDO. 

PUT SKIP(1).

{utp/ut-liter.i TOTALIZA€ÇO *}
PUT RETURN-VALUE FORMAT "x(15)":U AT 20. 
PUT "-------------------------------------------------------------":U AT 20.
{utp/ut-liter.i OMs_Preditivas_geradas_com_sucesso: *}
PUT RETURN-VALUE FORMAT "x(37)":U AT 30.
PUT i-conta-sucesso AT 70.
{utp/ut-liter.i OMs_Preditivas_nÆo_geradas_por_erro: *}
PUT RETURN-VALUE FORMAT "x(37)":U AT 30.
PUT i-conta-erro AT 70.
{utp/ut-liter.i OMs_com_erro_no_Apoio: *}
PUT RETURN-VALUE FORMAT "x(37)":U AT 30.
PUT i-conta-erro-ap AT 70.

PUT SKIP(1).

{utp/ut-liter.i Gerando_Relat¢rio *}.
run pi-inicializar in h-acomp (input return-value).

FOR EACH tt-relatorio:    

    {utp/ut-liter.i Equipamento *}
    run pi-acompanhar in h-acomp (input RETURN-VALUE + ": ":U + tt-relatorio.cd-equipto).
    
    IF i-page <> page-number THEN DO:
        /*imprime o cabe‡alho principal a cada nova p gina*/
        DISPLAY WITH FRAME fCabPrincipal.
        ASSIGN i-page = PAGE-NUMBER.
    END.
    ELSE
        PUT SKIP(1).

    DISPLAY tt-relatorio.nr-ord-produ 
            tt-relatorio.cd-equipto   
            tt-relatorio.cd-tag       
            tt-relatorio.dt-manut     
            tt-relatorio.dt-prev      
            tt-relatorio.cd-manut     
            tt-relatorio.descricao    
            tt-relatorio.cd-tipo    
            tt-relatorio.plano-orig
            tt-relatorio.cd-parada    
           WITH FRAME fOrdem.
           DOWN WITH FRAME fOrdem.

    IF tt-relatorio.nr-ord-produ <> c-n-gerada THEN DO:
        /*exibe os apoios gerados para a OM Preditiva*/
        IF CAN-FIND (FIRST mip-ord-taref WHERE mip-ord-taref.nr-ord-produ = INT(tt-relatorio.nr-ord-produ)
                                           AND (mip-ord-taref.nr-ord-apoio <> 0 AND
                                                mip-ord-taref.nr-ord-apoio <> ?) NO-LOCK) THEN DO:

            IF i-page <> page-number THEN DO:
                /*imprime o cabe‡alho principal a cada nova p gina*/
                DISPLAY WITH FRAME fCabPrincipal.
                ASSIGN i-page = PAGE-NUMBER.
            END.

            DISPLAY WITH FRAME fCabApoio.

            FOR EACH mip-ord-taref WHERE mip-ord-taref.nr-ord-produ = INT(tt-relatorio.nr-ord-produ)
                                    AND (mip-ord-taref.nr-ord-apoio <> 0 AND
                                         mip-ord-taref.nr-ord-apoio <> ?) NO-LOCK:
                IF mip-ord-taref.idi-acao-apoio = 2 THEN DO:
                    /*apoio ‚ uma SS. Busca a SS correspondente*/
                    FIND FIRST solic-serv WHERE solic-serv.nr-soli-serv = mip-ord-taref.nr-ord-apoio NO-LOCK NO-ERROR.
                    IF AVAIL solic-serv THEN DO:

                        CASE solic-serv.plano-orig:
                            WHEN "man-eq-tag":U THEN DO:
                                FIND FIRST man-eq-tag WHERE man-eq-tag.cd-tag     = solic-serv.cd-tag
                                                        AND man-eq-tag.cd-equipto = solic-serv.cd-equipto
                                                        AND man-eq-tag.cd-manut   = solic-serv.cd-manut NO-LOCK NO-ERROR.
                                ASSIGN c-desc-manut = man-eq-tag.descricao
                                       i-tip-manut  = man-eq-tag.cd-tipo.
                            END.
                            WHEN "man-equip":U THEN DO:
                                FIND FIRST man-equip WHERE man-equip.cd-equipto = solic-serv.cd-equipto
                                                       AND man-equip.cd-manut   = solic-serv.cd-manut NO-LOCK NO-ERROR.
                                ASSIGN c-desc-manut = man-equip.descricao
                                       i-tip-manut  = man-equip.cd-tipo.
                            END.
                            WHEN "manut":U THEN DO:
                                FIND FIRST manut WHERE manut.cd-manut   = solic-serv.cd-manut NO-LOCK NO-ERROR.
                                ASSIGN c-desc-manut = manut.descricao
                                       i-tip-manut  = manut.cd-tipo.
                            END.
                            OTHERWISE
                                ASSIGN c-desc-manut = "":U.
                        END CASE.

                        IF i-page <> page-number THEN DO:
                            /*imprime o cabe‡alho principal a cada nova p gina*/
                            DISPLAY WITH FRAME fCabPrincipal.
                            ASSIGN i-page = PAGE-NUMBER.
                        END.

                        {utp/ut-liter.i SS *}
                        /*display informa‡äes da solic-serv fApoio */
                        DISPLAY RETURN-VALUE              @  c-apoio    
                                solic-serv.nr-soli-serv   @  i-nr-apoio 
                                solic-serv.dt-inicio      @  d-dt-ini   
                                solic-serv.dt-termino     @  d-dt-fim   
                                solic-serv.cd-manut       @  c-cd-manut 
                                c-desc-manut
                                i-tip-manut
                                solic-serv.cd-parada      @  c-cd-parada
                            WITH FRAME fApoio.
                            DOWN WITH FRAME fApoio.
                    END.
                END. /*IF mip-ord-taref.idi-acao-apoio = 2*/
                ELSE
                    IF mip-ord-taref.idi-acao-apoio = 3 THEN DO:
                        FIND FIRST ord-manut WHERE ord-manut.nr-ord-produ = mip-ord-taref.nr-ord-apoio NO-LOCK NO-ERROR.
                        IF AVAIL ord-manut THEN DO:

                            IF i-page <> page-number THEN DO:
                                /*imprime o cabe‡alho principal a cada nova p gina*/
                                DISPLAY WITH FRAME fCabPrincipal.
                                ASSIGN i-page = PAGE-NUMBER.
                            END.
                            
                            /*display informa‡äes da ord-manut (apoio) fApoio*/
                            {utp/ut-liter.i OM *}
                            DISPLAY RETURN-VALUE             @  c-apoio    
                                    ord-manut.nr-ord-produ   @  i-nr-apoio 
                                    ord-manut.dt-manut       @  d-dt-ini   
                                    ord-manut.dt-prev        @  d-dt-fim   
                                    ord-manut.cd-manut       @  c-cd-manut 
                                    ord-manut.des-man-corr   @  c-desc-manut
                                    ord-manut.cd-tipo        @  i-tip-manut
                                    ord-manut.cd-parada      @  c-cd-parada
                                WITH FRAME fApoio.
                                DOWN WITH FRAME fApoio.
                        END.
                    END.

            END. /*FOR EACH mip-ord-taref*/
        END. /*IF CAN-FIND (FIRST mip-ord-taref*/
    END. /*IF tt-relatorio.nr-ord-produ <> c-n-gerada*/

    IF CAN-FIND (FIRST tt-erro-relat WHERE tt-erro-relat.rowttRelat = ROWID(tt-relatorio)) THEN DO:
        /*Exibe os erros gerados*/

        IF i-page <> page-number THEN DO:
            /*imprime o cabe‡alho principal a cada nova p gina*/
            DISPLAY WITH FRAME fCabPrincipal.
            ASSIGN i-page = PAGE-NUMBER.
        END.

        DISPLAY WITH FRAME fCabErro.

        FOR EACH tt-erro-relat WHERE tt-erro-relat.rowttRelat = ROWID(tt-relatorio):
            /*Caso tenha dado erro na geracao da OM Preditiva, e este erro seja dos apoios, 
              ignora, pois os apoios foram desfeitos na transa‡Æo. */
            IF tt-relatorio.nr-ord-produ = c-n-gerada AND TRIM((ENTRY(1,tt-erro-relat.mensagem," ":U))) = trim(c-tar) THEN
                 NEXT.

            IF i-page <> page-number THEN DO:
                /*imprime o cabe‡alho principal a cada nova p gina*/
                DISPLAY WITH FRAME fCabPrincipal.
                ASSIGN i-page = PAGE-NUMBER.
            END.

            /*display erros fErro*/
            DISPLAY tt-erro-relat.cd-erro
                    substring(tt-erro-relat.mensagem,1,106) @ tt-erro-relat.mensagem
               WITH FRAME fErro.
               DOWN WITH FRAME fErro.
        END.
    END. /*IF CAN-FIND (FIRST tt-erro-relat*/
END.
    */
    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-displayParametros) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayParametros Procedure 
PROCEDURE displayParametros :
/*------------------------------------------------------------------------------
  Purpose:     displayParametros
  Parameters:  <none>
  Notes:       Mostra os parƒmetros selecionados
------------------------------------------------------------------------------*/
/** Cria a p gina dos parƒmetros **/
/*page.

IF tt-param.rs-todas = 1 THEN DO:
    {utp/ut-liter.i Efetivar_OMs_marcadas_em_TODAS_as_dimensäes *}
END.
ELSE DO:
    {utp/ut-liter.i Efetivar_OMs_marcadas_APENAS_na_dimensÆo_exibida_atualmente *}
END.
ASSIGN c-todas = RETURN-VALUE.

DISPLAY c-todas
        c-destino
        tt-param.arquivo
        tt-param.usuario  
    WITH FRAME fParam.
*/
    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-efetivaDados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE efetivaDados Procedure 
PROCEDURE efetivaDados :
/*------------------------------------------------------------------------------
  Purpose:     Faz todo o processamento das informa‡äes
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define variable i-conta-ordem like ord-manut.conta-despesa no-undo.

    for each  bf-movto-estoq
        where bf-movto-estoq.esp-docto  = 27 /** 27 - RDD **/
        and   bf-movto-estoq.dt-trans  >= tt-param.periodo-ini
        and   bf-movto-estoq.dt-trans  <= tt-param.periodo-fim no-lock:

        for first ord-prod
            where ord-prod.nr-ord-produ = bf-movto-estoq.nr-ord-produ no-lock:
        end.
        if not avail ord-prod then next.

        if ord-prod.origem = "MI":U then do:
            for first ord-manut
                where ord-manut.nr-ord-produ = bf-movto-estoq.nr-ord-produ no-lock:
                for each  ord-taref
                    where ord-taref.nr-ord-produ = ord-manut.nr-ord-produ no-lock:
                    /** Busca Conta Despesa da Tarefa da OM **/
                    for first mi-tar-ord
                        where mi-tar-ord.nr-ord-produ = ord-taref.nr-ord-produ
                        and   mi-tar-ord.cd-tarefa    = ord-taref.cd-tarefa no-lock:

                        assign i-conta-ordem = mi-tar-ord.ct-despesa + mi-tar-ord.sc-despesa.
                        /** Se a Conta Despesa da Tarefa for IGUAL a da OM, vai
                            para o proxima tarefa, nÆo h  necessidade do rateio **/
                        if i-conta-ordem = ord-manut.conta-despesa then
                            next.
                        else do:
                            /** Inicia o Rateio.
                                Busca todas as Solicita‡äes de Material e Compra realizadas para a Tarefa **/
                            for each  req-ord-produc
                                where req-ord-produc.nr-ord-produ = ord-taref.nr-ord-produ
                                and   req-ord-produc.num-tarefa   = ord-taref.cd-tarefa no-lock:

                                for first it-requisicao
                                    where it-requisicao.nr-requisicao = req-ord-produc.nr-requisicao
                                    and   it-requisicao.sequencia     = req-ord-produc.sequencia
                                    and   it-requisicao.qt-atendida   > 0 no-lock:

                                    for each  movto-mat
                                        where movto-mat.int-1        = it-requisicao.sequencia 
                                        and  (movto-mat.nro-docto    = string(it-requisicao.nr-requisicao,"999,999,999") or 
                                              movto-mat.nro-docto    = string(it-requisicao.nr-requisicao,"999,999"))
                                        and   movto-mat.it-codigo    = it-requisicao.it-codigo 
                                        and   movto-mat.nr-ord-produ = ord-taref.nr-ord-produ
                                        and  (movto-mat.esp-docto    = 30 or
                                              movto-mat.esp-docto    = 28 or
                                              movto-mat.esp-docto    = 5)
                                        and   movto-mat.dt-trans    >= tt-param.periodo-ini
                                        and   movto-mat.dt-trans    <= tt-param.periodo-fim no-lock:

                                        for first movto-estoq
                                            where movto-estoq.nro-docto    = movto-mat.nro-docto
                                            and   movto-estoq.num-sequen   = movto-mat.num-sequen
                                            and   movto-estoq.it-codigo    = it-requisicao.it-codigo 
                                            and   movto-estoq.esp-docto    = movto-mat.esp-docto no-lock:

                                            if  movto-estoq.esp-docto = 30 or
                                                movto-estoq.esp-docto = 5 then
                                                if movto-estoq.serie-docto  <> " " or
                                                   movto-estoq.cod-emitente <> 0   or
                                                   movto-estoq.nat-operacao <> " " then next.

                                            if  movto-estoq.esp-docto = 28 then
                                                if movto-estoq.cod-emitente = 0 then next.

                                            /** Cria copia da RDD original **/
                                            if not can-find(first tt-movto-estoq-ori
                                                            where tt-movto-estoq-ori.nr-ord-produ   = bf-movto-estoq.nr-ord-produ   
                                                            and   tt-movto-estoq-ori.conta-contabil = bf-movto-estoq.conta-contabil no-lock) then do:
                                                create tt-movto-estoq-ori.
                                                buffer-copy bf-movto-estoq to tt-movto-estoq-ori.
                                                assign tt-movto-estoq-ori.r-rowid = rowid(bf-movto-estoq).
                                            end.

                                            /** Cria copia da movto-estoq e acumula valor por Ordem e Conta **/
                                            if not can-find(first tt-movto-estoq-aux
                                                            where tt-movto-estoq-aux.nr-ord-produ   = movto-estoq.nr-ord-produ   
                                                            and   tt-movto-estoq-aux.conta-contabil = i-conta-ordem no-lock) then do: /** i-conta-ordem = Conta da Tarefa **/ 
                                                create tt-movto-estoq-aux.
                                                buffer-copy bf-movto-estoq except quantidade valor-mat-m ct-codigo sc-codigo conta-contabil to tt-movto-estoq-aux.
                                                /** 1 - Entrada, 2 - Saida / Estoque **/
                                                if movto-estoq.tipo-trans = 1 then
                                                    assign tt-movto-estoq-aux.valor-mat-m[1] = movto-estoq.valor-mat-m[1] * -1.
                                                if movto-estoq.tipo-trans = 2 then
                                                    assign tt-movto-estoq-aux.valor-mat-m[1] = movto-estoq.valor-mat-m[1].

                                                assign tt-movto-estoq-aux.quantidade     = (movto-estoq.valor-mat-m[1] / bf-movto-estoq.valor-mat-m[1])
                                                       tt-movto-estoq-aux.ct-codigo      = mi-tar-ord.ct-despesa
                                                       tt-movto-estoq-aux.sc-codigo      = mi-tar-ord.sc-despesa
                                                       tt-movto-estoq-aux.conta-contabil = mi-tar-ord.ct-despesa + mi-tar-ord.sc-despesa
                                                       tt-movto-estoq-aux.cd-tarefa      = req-ord-produc.num-tarefa.
                                            end.
                                            else do:
                                                for first tt-movto-estoq-aux
                                                    where tt-movto-estoq-aux.nr-ord-produ   = movto-estoq.nr-ord-produ
                                                    and   tt-movto-estoq-aux.conta-contabil = i-conta-ordem no-lock:
                                                    /** 1 - Entrada, 2 - Saida / Estoque **/
                                                    if movto-estoq.tipo-trans = 1 then
                                                        assign tt-movto-estoq-aux.valor-mat-m[1] = tt-movto-estoq-aux.valor-mat-m[1] - movto-estoq.valor-mat-m[1].
                                                    if movto-estoq.tipo-trans = 2 then
                                                        assign tt-movto-estoq-aux.valor-mat-m[1] = tt-movto-estoq-aux.valor-mat-m[1] + movto-estoq.valor-mat-m[1].

                                                    assign tt-movto-estoq-aux.quantidade = (tt-movto-estoq-aux.valor-mat-m[1] / bf-movto-estoq.valor-mat-m[1]).
                                                end.
                                            end.
                                        end.
                                    end.
                                end.
                            end.
                        end.
                    end.
                end.
            end.
        end.
        if ord-prod.origem = "MV":U then do:
            for first mmv-ord-manut
                where mmv-ord-manut.nr-ord-produ = bf-movto-estoq.nr-ord-produ no-lock:
                for each  mmv-tar-ord-manut
                    where mmv-tar-ord-manut.nr-ord-produ = mmv-ord-manut.nr-ord-produ no-lock:

                    /** Busca Conta Despesa da Tarefa da OM **/
                    for first mv-tar-ord
                        where mv-tar-ord.nr-ord-produ = mmv-tar-ord-manut.nr-ord-produ
                        and   mv-tar-ord.cd-tarefa    = mmv-tar-ord-manut.num-seq no-lock:

                        assign i-conta-ordem = mv-tar-ord.ct-despesa + mv-tar-ord.sc-despesa.
                        /** Se a Conta Despesa da Tarefa for IGUAL a da OM, vai
                            para o proxima tarefa, nÆo h  necessidade do rateio **/
                        if i-conta-ordem = mmv-ord-manut.ct-ordem + mmv-ord-manut.cc-ordem then
                            next.
                        else do:
                            /** Inicia o Rateio.
                                Busca todas as Solicita‡äes de Material e Compra realizadas para a Tarefa **/
                            for each  req-ord-produc
                                where req-ord-produc.nr-ord-produ = mmv-tar-ord-manut.nr-ord-produ
                                and   req-ord-produc.num-tarefa   = mmv-tar-ord-manut.num-seq no-lock:

                                for first it-requisicao
                                    where it-requisicao.nr-requisicao = req-ord-produc.nr-requisicao
                                    and   it-requisicao.sequencia     = req-ord-produc.sequencia
                                    and   it-requisicao.qt-atendida   > 0 no-lock:

                                    for each  movto-mat
                                        where movto-mat.int-1        = it-requisicao.sequencia 
                                        and  (movto-mat.nro-docto    = string(it-requisicao.nr-requisicao,"999,999,999") or 
                                              movto-mat.nro-docto    = string(it-requisicao.nr-requisicao,"999,999"))
                                        and   movto-mat.it-codigo    = it-requisicao.it-codigo 
                                        and   movto-mat.nr-ord-produ = ord-taref.nr-ord-produ
                                        and  (movto-mat.esp-docto    = 30 or
                                              movto-mat.esp-docto    = 28 or
                                              movto-mat.esp-docto    = 5)
                                        and   movto-mat.dt-trans    >= tt-param.periodo-ini
                                        and   movto-mat.dt-trans    <= tt-param.periodo-fim no-lock:

                                        for first movto-estoq
                                            where movto-estoq.nro-docto    = movto-mat.nro-docto
                                            and   movto-estoq.num-sequen   = movto-mat.num-sequen
                                            and   movto-estoq.it-codigo    = it-requisicao.it-codigo 
                                            and   movto-estoq.esp-docto    = movto-mat.esp-docto no-lock:

                                            if  movto-estoq.esp-docto = 30 or
                                                movto-estoq.esp-docto = 5 then
                                                if movto-estoq.serie-docto  <> " " or
                                                   movto-estoq.cod-emitente <> 0   or
                                                   movto-estoq.nat-operacao <> " " then next.

                                            if  movto-estoq.esp-docto = 28 then
                                                if movto-estoq.cod-emitente = 0 then next.

                                            /** Cria copia da RDD original **/
                                            if not can-find(first tt-movto-estoq-ori
                                                            where tt-movto-estoq-ori.nr-ord-produ   = bf-movto-estoq.nr-ord-produ   
                                                            and   tt-movto-estoq-ori.conta-contabil = bf-movto-estoq.conta-contabil no-lock) then do:
                                                create tt-movto-estoq-ori.
                                                buffer-copy bf-movto-estoq to tt-movto-estoq-ori.
                                                assign tt-movto-estoq-ori.r-rowid = rowid(bf-movto-estoq).
                                            end.

                                            /** Cria copia da movto-estoq e acumula valor por Ordem e Conta **/
                                            if not can-find(first tt-movto-estoq-aux
                                                            where tt-movto-estoq-aux.nr-ord-produ   = movto-estoq.nr-ord-produ   
                                                            and   tt-movto-estoq-aux.conta-contabil = i-conta-ordem no-lock) then do: /** i-conta-ordem = Conta da Tarefa **/
                                                create tt-movto-estoq-aux.
                                                buffer-copy bf-movto-estoq except quantidade valor-mat-m ct-codigo sc-codigo conta-contabil to tt-movto-estoq-aux.
                                                /** 1 - Entrada, 2 - Saida / Estoque **/
                                                if movto-estoq.tipo-trans = 1 then
                                                    assign tt-movto-estoq-aux.valor-mat-m[1] = movto-estoq.valor-mat-m[1] * -1.
                                                if movto-estoq.tipo-trans = 2 then
                                                    assign tt-movto-estoq-aux.valor-mat-m[1] = movto-estoq.valor-mat-m[1].

                                                assign tt-movto-estoq-aux.quantidade     = (movto-estoq.valor-mat-m[1] / bf-movto-estoq.valor-mat-m[1])
                                                       tt-movto-estoq-aux.ct-codigo      = mv-tar-ord.ct-despesa
                                                       tt-movto-estoq-aux.sc-codigo      = mv-tar-ord.sc-despesa
                                                       tt-movto-estoq-aux.conta-contabil = mv-tar-ord.ct-despesa + mv-tar-ord.sc-despesa
                                                       tt-movto-estoq-aux.cd-tarefa      = req-ord-produc.num-tarefa.
                                            end.
                                            else do:
                                                for first tt-movto-estoq-aux
                                                    where tt-movto-estoq-aux.nr-ord-produ   = movto-estoq.nr-ord-produ
                                                    and   tt-movto-estoq-aux.conta-contabil = i-conta-ordem no-lock:
                                                    /** 1 - Entrada, 2 - Saida / Estoque **/
                                                    if movto-estoq.tipo-trans = 1 then
                                                        assign tt-movto-estoq-aux.valor-mat-m[1] = tt-movto-estoq-aux.valor-mat-m[1] - movto-estoq.valor-mat-m[1].
                                                    if movto-estoq.tipo-trans = 2 then
                                                        assign tt-movto-estoq-aux.valor-mat-m[1] = tt-movto-estoq-aux.valor-mat-m[1] + movto-estoq.valor-mat-m[1].

                                                    assign tt-movto-estoq-aux.quantidade = (tt-movto-estoq-aux.valor-mat-m[1] / bf-movto-estoq.valor-mat-m[1]).
                                                end.
                                            end.
                                        end.
                                    end.
                                end.
                            end.
                        end.
                    end.
                end.
            end.
        end.
    end.

    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FASE-2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FASE-2 Procedure 
PROCEDURE FASE-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /** FASE 2 **/

    /** Atualiza Quantidade e Valor dos RDDs Originais **/
    for each tt-movto-estoq-ori no-lock:
        assign d-valor-mat     = 0
               d-valor-mat-ant = 0.
        for each  tt-movto-estoq-aux
            where tt-movto-estoq-aux.nr-ord-produ = tt-movto-estoq-ori.nr-ord-produ no-lock:
            /** Acumula valor gasto em contas diferentes da OM **/
            assign d-valor-mat = d-valor-mat + tt-movto-estoq-aux.valor-mat-m[1].
        end.
        /** Guarda valor total do RDD Original **/
        assign d-valor-mat-ant = tt-movto-estoq-ori.valor-mat-m[1].

        assign tt-movto-estoq-ori.valor-mat-m[1] = tt-movto-estoq-ori.valor-mat-m[1] - d-valor-mat
               tt-movto-estoq-ori.quantidade     = tt-movto-estoq-ori.valor-mat-m[1] / d-valor-mat-ant. 
    end.

    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FASE-3) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FASE-3 Procedure 
PROCEDURE FASE-3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /** FASE 3 **/
    DO  ON ERROR UNDO, RETURN ERROR
        ON STOP  UNDO, RETURN ERROR: 

        /** Atualiza RDDs originais **/
        for each tt-movto-estoq-ori no-lock:
            for first movto-estoq fields (nr-trans valor-mat-m quantidade)
                where rowid(movto-estoq) = tt-movto-estoq-ori.r-rowid exclusive-lock:
                assign movto-estoq.valor-mat-m[1] = tt-movto-estoq-ori.valor-mat-m[1]
                       movto-estoq.quantidade     = tt-movto-estoq-ori.quantidade.
            end.
        end.

        /** Cria Movimentos da Tarefas **/
        for each tt-movto-estoq-aux no-lock:
            create movto-estoq.
            buffer-copy tt-movto-estoq-aux to movto-estoq.
        end.
    end. /** DO **/
    
    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piInicial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piInicial Procedure 
PROCEDURE piInicial :
/*------------------------------------------------------------------------------
  Purpose:     piInicial
  Parameters:  <none>
  Notes:       Define os valores que serÆo mostrados o cabe‡alho e rodap‚
------------------------------------------------------------------------------*/
    assign c-programa = "ES9999"
           c-versao   = "2.00"
           c-revisao  = "000".
           /** Define o destino do arquivo a ser gerado **/
    run utp/ut-liter.p (trim({varinc/var00002.i 04 integer(tt-param.destino)}), "*", "").
    assign c-destino  = return-value.
    
    /** Busca os parƒmetros criados na interface gr fica **/
    find first tt-param exclusive-lock no-error.
    
    /** Busca empresa padrÆo **/
    for first param-global fields(grupo) no-lock:
        assign c-empresa = param-global.grupo.
    end.
    
    /** Guarda valores para imprimir t¡tulos **/
    {utp/ut-liter.i Processo_de_Rateio *}
    assign c-titulo-relat = trim(return-value).
    {utp/ut-liter.i Custos *}
    assign c-sistema = trim(return-value).

    return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piPrincipal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piPrincipal Procedure 
PROCEDURE piPrincipal :
/*------------------------------------------------------------------------------
  Purpose:     piPrincipal
  Parameters:  <none>
  Notes:       Corpo princiapl da aplica‡Æo
------------------------------------------------------------------------------*/
/** Mostra frames de cabe‡alho e rodap‚ padräes da Datasul **/
view frame f-cabec.
view frame f-rodape. 

/** Inicializa programa de acompanhamento padrÆo Datasul **/
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input c-titulo-relat).

/*********************************************************************
   Neste espa‡o deve-se colocar a l¢gica para busca as informa‡äes e 
   tamb‚m para fazer o display, pode-se inclui a chamada de procedures
   ou fazer a l¢gica neste espa‡o.
*********************************************************************/

if tt-param.cal-rat then do:
    /** Faz todo o processamento dos dados **/
    run efetivaDados in this-procedure.

    /** Prepara/Atualiza temp-table dos registros pai **/
    run FASE-2 in this-procedure.

    /** Efetiva a grava‡Æo do banco de dados **/
    run FASE-3 in this-procedure.
end.

if tt-param.desc-rat then do:
    run descalculaRateio in this-procedure.
end.

/** Mostra dados importados **/
run displayDados in this-procedure.

/*********************************************************************
   Fim do espa‡o para l¢gica de c lculo e display das informa‡äes
*********************************************************************/

/** Mostra parƒmetros selecionados **/
run displayParametros in this-procedure.

/** Finaliza programa de acompanhamento padrÆo Datasul **/
run pi-finalizar in h-acomp.
if valid-handle(h-acomp) then
    delete procedure h-acomp.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

