/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESIM0665RP 2.00.00.038}  /*** 010038 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i esim0665rp MIM}
&ENDIF

{include/i_fnctrad.i}
/******************************************************************************
**
**       PROGRAMA:  ESIM0665RP.P
**
**       DATA....:  FEVEREIRO DE 2003
**
**       OBJETIVO:  Custo Real de Importaá∆o
**
**       VERSAO..:  1.00.000 - Ivonei Vock
**
******************************************************************************/
{cdp/cdcfgmat.i} 
   
{imp/esim0665.i} /* Definiá∆o tt-param           **
               ** Definiá∆o tt-detalha-item    **
               ** Definiá∆o tt-embarque-cabec  **
               ** Definiá∆o tt-embarque-corpo  **
               ** Definiá∆o tt-embarque-fatura **
               ** Definiá∆o tt-detalha-despesa **
               ** Definiá∆o tt-embarque-rel    
               ** Definiá∆o tt-detalha-despesa-aux - despesa total do cabeáalho*/
{imp/esim0665.i1} /* Definiá∆o de Forms */

DEFINE TEMP-TABLE tt-notas-fiscais NO-UNDO
    field embarque                          like embarque-imp.embarque      
    field cod-estabel                       like embarque-imp.cod-estabel   
    field nr-nota-fis                       like docum-est.nro-docto
    field dt-emissao                        like docum-est.dt-emissao
    field nr-di                             like embarque-imp.declaracao-import
    field dt-di                             like embarque-imp.data-di
    FIELD r-docum-est                       AS ROWID.

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.
    
create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.
/* MESSAGE "esim0665rp.p"                   */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
def buffer b-docum-est          for docum-est.
def buffer b-docum-est-aux      for docum-est.
def buffer b-embarque-imp       for embarque-imp.
def buffer b-ordens-embarque    for ordens-embarque.
def buffer b-item-doc-est       for item-doc-est.
def buffer b-item-doc-est-aux   for item-doc-est.
def buffer b-item-doc-est-aux2  for item-doc-est.
def buffer b-historico-embarque for historico-embarque.
def buffer bf-tt-detalha-item   for tt-detalha-item.
DEF BUFFER bf-natur-oper        FOR natur-oper.

/*Definiá∆o de Vari†veis*/
def var c-desc-estabel                like estabelec.nome               no-undo.
def var c-desc-emitente               like emitente.nome-abrev          no-undo.
def var d-taxa-moeda                  as   decimal                      no-undo.
def var d-data-conv                   as   date                         no-undo.
def var d-data-emb                    as   date                         no-undo.
def var d-data-nac                    as   date                         no-undo.
def var c-taxa                        as   char                         no-undo.
def var c-embarque                    as   char format "x(20)"          no-undo.
def var i-cod-desp-ii                 like docum-est-cex.cod-desp       no-undo.
def var c-desc-pto-emb                like pto-contr.descricao          no-undo.
def var c-desc-pto-nac                like pto-contr.descricao          no-undo.
def var vl-ii-orcad-det               like cotacao-item-cex.val-desp    no-undo.
def var vl-ii2-orcad-det              like cotacao-item-cex.val-desp    no-undo.
def var vl-ii-reali-det               like item-doc-est-cex.val-desp    no-undo.
def var vl-ii2-reali-det              like item-doc-est-cex.val-desp    no-undo.
def var vl-ipi-orcad-det              like cotacao-item-cex.val-desp    no-undo.
def var vl-icms-orcad-det             like cotacao-item-cex.val-desp    no-undo.
def var vl-pis-orcad-det              like cotacao-item-cex.val-desp    no-undo.
def var vl-cofins-orcad-det           like cotacao-item-cex.val-desp    no-undo.
def var vl-desp-orcad-det             like cotacao-item-cex.val-desp    no-undo.
def var vl-despesa-frete              like item-doc-est-cex.val-desp    no-undo.
def var vl-despesa-embalagem          like item-doc-est-cex.val-desp    no-undo.
def var vl-despesa-seguro             like item-doc-est-cex.val-desp    no-undo.
def var vl-despesa-outras             like item-doc-est-cex.val-desp    no-undo.
def var tot-despesa-orc               like item-doc-est-cex.val-desp    no-undo.
def var tot-despesa-rea               like item-doc-est-cex.val-desp    no-undo.
def var tot-despesa-orc-total         like item-doc-est-cex.val-desp    no-undo.
def var tot-despesa-rea-total         like item-doc-est-cex.val-desp    no-undo.
def var var-preco-unit                like item-doc-est-cex.val-desp    no-undo.
def var var-moeda-conv-orc            like item-doc-est-cex.mo-codigo   no-undo.
def var var-moeda-conv-rea            like item-doc-est-cex.mo-codigo   no-undo.
def var var-preco-item-rea            like item-doc-est-cex.val-desp    no-undo.
def var var-valor-ipi-rea             like item-doc-est-cex.val-desp    no-undo.
def var var-valor-icm-rea             like item-doc-est-cex.val-desp    no-undo.
def var var-valor-pis-rea             like item-doc-est-cex.val-desp    no-undo.
def var var-valor-cofins-rea          like item-doc-est-cex.val-desp    no-undo.
def var var-preco-unit-aux            like item-doc-est-cex.val-desp    no-undo.
def var var-valor-ipi-mattran-rea     like item-doc-est-cex.val-desp    no-undo.
def var var-valor-icm-mattran-rea     like item-doc-est-cex.val-desp    no-undo.
def var var-valor-pis-mattran-rea     like item-doc-est-cex.val-desp    no-undo.
def var var-valor-cofins-mattran-rea  like item-doc-est-cex.val-desp    no-undo.
DEF VAR de-vl-cofins-rea              LIKE cotacao-item-cex.val-desp    NO-UNDO.  
DEF VAR de-vl-pis-rea                 LIKE cotacao-item-cex.val-desp    NO-UNDO.
DEF VAR de-vl-cofins-rea-total        LIKE cotacao-item-cex.val-desp    NO-UNDO.  
DEF VAR de-vl-pis-rea-total           LIKE cotacao-item-cex.val-desp    NO-UNDO.
DEF VAR vl-desp-orcad-aux             LIKE cotacao-item-cex.val-desp    NO-UNDO.
DEF VAR vl-ii2-orcad-det-aux          LIKE cotacao-item-cex.val-desp    NO-UNDO.
DEF VAR vl-ii-orcad-det-aux           LIKE cotacao-item-cex.val-desp    NO-UNDO.
DEF VAR de-preco-unit-sem-ipi         LIKE ordem-compra.preco-unit      NO-UNDO.
DEF VAR de-valor-icm                  LIKE item-doc-est.valor-icm[1]    NO-UNDO.
DEF VAR de-valor-ipi                  LIKE item-doc-est.valor-ipi[1]    NO-UNDO.

DEF VAR  de-nf-valor-ipi              AS DEC FORMAT ">>>>,>>>,>>9.99"   NO-UNDO.
DEF VAR  de-nf-valor-icm              AS DEC FORMAT ">>>>,>>>,>>9.99"   NO-UNDO.
DEF VAR  de-nf-valor-pis              AS DEC FORMAT ">>>>,>>>,>>9.99"   NO-UNDO.
DEF VAR  de-nf-val-cofins             AS DEC FORMAT ">>>>,>>>,>>9.99"   NO-UNDO.


DEF VAR de-val-desp-nfc2              AS   DECIMAL                      NO-UNDO.
DEF VAR de-val-desp-aux-rea           AS   DECIMAL                      NO-UNDO.
DEF VAR de-tot-orc                    AS   DECIMAL                      NO-UNDO.
DEF VAR de-tot-rea                    AS   DECIMAL                      NO-UNDO.

def var no-despesa                    as   logical                      no-undo.
def var l-desemb-parc-modif           as   logical initial no           no-undo.
def var l-cria-embarque-corpo         as   logical initial no           no-undo.
def var h-acomp                       as   handle                       no-undo.
def var i-parcela                     as   integer                      no-undo.
DEF VAR l-nota-mae                  AS   LOGICAL                      NO-UNDO.


run utp/ut-acomp.p persistent set h-acomp.

{include/i-rpvar.i}
{include/i-rpcab.i}

find first tt-param     no-lock no-error.
find first param-global no-lock no-error.

find first ems2cadme.empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error.
if  avail empresa then
    assign c-empresa = empresa.razao-social.
else do:
    run utp/ut-msgs.p (input "show", input 16, input "").
    return error.
end.

{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape.

for each tt-detalha-item:
    delete tt-detalha-item.
end.
for each tt-embarque-cabec:
    delete tt-embarque-cabec.
end.
for each tt-embarque-corpo:
    delete tt-embarque-corpo.
end.
FOR EACH tt-embarque-nota-mae:
    DELETE tt-embarque-nota-mae.
END.

FOR EACH tt-detalha-despesa-aux:
    DELETE tt-detalha-despesa-aux.
END.
 
run pi-inicializar in h-acomp (input "Preparando Dados...").

for each embarque-imp no-lock
   where embarque-imp.cod-estabel >= tt-param.c-cod-estabel-ini
     and embarque-imp.cod-estabel <= tt-param.c-cod-estabel-fim
     and embarque-imp.embarque    >= tt-param.c-embarque-ini
     and embarque-imp.embarque    <= tt-param.c-embarque-fim:

    run pi-acompanhar in h-acomp (input "Embarque: " + string(embarque-imp.embarque)).

    for first ordens-embarque fields( embarque  cod-estabel  numero-ordem
                                      char-2    quantidade )
        where ordens-embarque.embarque    = embarque-imp.embarque
          and ordens-embarque.cod-estabel = embarque-imp.cod-estabel no-lock:
    end.
    if  not avail ordens-embarque then next.
    
    for first ordem-compra fields( numero-ordem  num-pedido cod-emitente)
        where ordem-compra.numero-ordem = ordens-embarque.numero-ordem no-lock:
    end.
    if  not avail ordem-compra then next.
    
    for first b-historico-embarque fields(cod-itiner)
        where b-historico-embarque.cod-estabel = embarque-imp.cod-estabel
          and b-historico-embarque.embarque    = embarque-imp.embarque no-lock:
    end.
    
    if  b-historico-embarque.cod-itiner <= tt-param.i-cod-itiner-ini
    and b-historico-embarque.cod-itiner >= tt-param.i-cod-itiner-fim then next.
    
    for first processo-imp fields( num-pedido   cod-exportador
                                   nr-proc-imp  cod-itiner )
        where processo-imp.num-pedido          =  ordem-compra.num-pedido
          and processo-imp.nr-proc-imp        >= tt-param.c-nr-proc-imp-ini
          and processo-imp.nr-proc-imp        <= tt-param.c-nr-proc-imp-fim
          and processo-imp.cod-exportador     >= tt-param.i-cod-emitente-ini
          and processo-imp.cod-exportador     <= tt-param.i-cod-emitente-fim no-lock:
    end.
    if  not avail processo-imp then next.
    
    /****** DESCRICAO ******/
    for first estabelec fields( cod-estabel  nome )
        where estabelec.cod-estabel = embarque-imp.cod-estabel no-lock:
    end.
    assign c-desc-estabel = if avail estabelec then estabelec.nome else "":U.

    for first emitente fields( cod-emitente  nome-abrev  pais )
        where emitente.cod-emitente = processo-imp.cod-exportador no-lock:
    end.
    assign c-desc-emitente = if avail emitente then emitente.nome-abrev else "":U.
    /****** ********* ******/
    
    /******** MOEDA ********/
    for first moeda fields( mo-codigo descricao )
        where moeda.mo-codigo = tt-param.moeda no-lock:
    end.
    if  not avail moeda then next.
    assign d-data-conv  = tt-param.da-dt-conv
           c-desc-moeda = moeda.descricao.
    if  l-dt-nacional then
        assign d-data-conv = embarque-imp.data-1.
    run cdp/cd0812.p (input  tt-param.moeda, /*moeda origem*/
                      input  0,              /*moeda destino*/
                      input  1,              /*valor origem*/
                      input  d-data-conv,    /*data conversao*/
                      output d-taxa-moeda).  /*valor destino*/
    /******* ******* *******/

    for first itinerario fields( cod-itiner  pto-embarque  pto-desembarque )
        where itinerario.cod-itiner = b-historico-embarque.cod-itiner no-lock:
    end.
    if  not avail itinerario then next.
     
    /* Data e Ponto de Embarque */
    &if "{&bf_mat_versao_ems}" >= "2.05" &then
        for first historico-embarque fields( cod-estabel  embarque  cod-pto-contr
                                             dt-previsao   cod-itiner )
            where historico-embarque.cod-estabel   =  ordens-embarque.cod-estabel
              and historico-embarque.embarque      =  ordens-embarque.embarque
              and historico-embarque.cod-itiner    =  itinerario.cod-itiner
              and historico-embarque.cod-pto-contr =  itinerario.pto-embarque no-lock:
        end.
    &else
        for first historico-embarque fields( cod-estabel  embarque  cod-pto-contr
                                             dt-efetiva  cod-itiner )
            where historico-embarque.cod-estabel   =  ordens-embarque.cod-estabel
              and historico-embarque.embarque      =  ordens-embarque.embarque
              and historico-embarque.cod-itiner    =  itinerario.cod-itiner
              and historico-embarque.cod-pto-contr =  itinerario.pto-embarque no-lock:
        end.
    &endif
    if  not avail historico-embarque then next.
     
    for first pto-contr fields( cod-pto-contr  descricao )
        where pto-contr.cod-pto-contr = itinerario.pto-embarque no-lock:
    end.
    if  not avail pto-contr then next.
    
    &if "{&bf_mat_versao_ems}" >= "2.05" &then
        assign d-data-emb     = historico-embarque.dt-previsao
               c-desc-pto-emb = pto-contr.descricao.
    &else
        assign d-data-emb     = historico-embarque.dt-efetiva
               c-desc-pto-emb = pto-contr.descricao.
    &endif
    /* ****************************** */
    
    /* Data e Ponto de Nacionalizaá∆o */
    &if "{&bf_mat_versao_ems}" >= "2.05" &then
        for first historico-embarque fields( cod-estabel  embarque  cod-pto-contr
                                             dt-previsao  cod-itiner )
            where historico-embarque.cod-estabel    =  ordens-embarque.cod-estabel
              and historico-embarque.embarque       =  ordens-embarque.embarque
              and historico-embarque.cod-itiner     =  itinerario.cod-itiner
              and historico-embarque.cod-pto-contr  =  itinerario.pto-desembarque
              and historico-embarque.dt-previsao   >= tt-param.da-dt-nac-ini
              and historico-embarque.dt-previsao   <= tt-param.da-dt-nac-fim no-lock:
        end.
    &else
        for first historico-embarque fields( cod-estabel  embarque  cod-pto-contr
                                             dt-efetiva   cod-itiner )
            where historico-embarque.cod-estabel    =  ordens-embarque.cod-estabel
              and historico-embarque.embarque       =  ordens-embarque.embarque
              and historico-embarque.cod-itiner     =  itinerario.cod-itiner
              and historico-embarque.cod-pto-contr  =  itinerario.pto-desembarque
              and historico-embarque.dt-efetiva    >= tt-param.da-dt-nac-ini
              and historico-embarque.dt-efetiva    <= tt-param.da-dt-nac-fim no-lock:
        end.
    &endif
    if  not avail historico-embarque then next.
     
    for first pto-contr fields( cod-pto-contr  descricao )
        where pto-contr.cod-pto-contr = itinerario.pto-desembarque no-lock:
    end.
    if  not avail pto-contr then next.

    &if "{&bf_mat_versao_ems}" >= "2.05" &then
        assign d-data-nac     = historico-embarque.dt-previsao
               c-desc-pto-nac = pto-contr.descricao.
    &else
        assign d-data-nac     = historico-embarque.dt-efetiva
               c-desc-pto-nac = pto-contr.descricao.
    &endif
    /* ****************************** */

    
    for first param-imp fields( cod-estabel char-1 
                                &if "{&bf_mat_versao_ems}" >= "2.062" &then
                                    cdn-despes-impto-ipi cdn-despes-impto-icms cdn-despes-impto-pis cdn-despes-impto-cofins
                                &endif)
        where param-imp.cod-estabel = embarque-imp.cod-estabel no-lock:
    end.
    if  not avail param-imp then next.
    assign i-cod-desp-ii   = int(substring(param-imp.char-1,21,20)). 
    
     
    /* => CABEÄALHO *******************************/
    create tt-embarque-cabec.
    assign tt-embarque-cabec.cod-estabel    = embarque-imp.cod-estabel
           tt-embarque-cabec.desc-estab     = if avail estabelec then estabelec.nome else "":U
           tt-embarque-cabec.embarque       = embarque-imp.embarque
           tt-embarque-cabec.cod-emitente   = processo-imp.cod-exportador
           tt-embarque-cabec.desc-emit      = if avail emitente then emitente.nome-abrev else "":U
           tt-embarque-cabec.pais           = if avail emitente then emitente.pais else "":U
           tt-embarque-cabec.cod-incoterm   = embarque-imp.cod-incoterm
           tt-embarque-cabec.cod-via-transp = embarque-imp.cod-via-transp
           tt-embarque-cabec.pto-embarque   = c-desc-pto-emb
           tt-embarque-cabec.data-embarque  = d-data-emb
           tt-embarque-cabec.pto-nacional   = c-desc-pto-nac
           tt-embarque-cabec.data-nacional  = d-data-nac

           &if "{&bf_mat_versao_ems}" >= "2.05" &then
               tt-embarque-cabec.data-chegada   = if   historico-embarque.dt-previsao = ? then historico-embarque.dt-previsao
                                                  else historico-embarque.dt-previsao
           &else
               tt-embarque-cabec.data-chegada   = if   historico-embarque.dt-efetiva = ? then historico-embarque.dt-previsao
                                                  else historico-embarque.dt-efetiva
           &endif
           
           tt-embarque-cabec.data-cotacao   = embarque-imp.data-1
           tt-embarque-cabec.desc-moeda     = c-desc-moeda
           tt-embarque-cabec.mo-taxa        = d-taxa-moeda
           tt-embarque-cabec.data-convercao = d-data-conv.

    /******** NOTA FISCAL DE FATURA ********/
    for each invoice-emb-imp no-lock
        where invoice-emb-imp.cod-estabel = embarque-imp.cod-estabel
          and invoice-emb-imp.embarque    = embarque-imp.embarque:
        create tt-embarque-fatura.
        assign tt-embarque-fatura.embarque      = embarque-imp.embarque
               tt-embarque-fatura.cod-estabel   = embarque-imp.cod-estabel
               tt-embarque-fatura.nr-fatura     = invoice-emb-imp.nr-invoice
               tt-embarque-fatura.parcela       = invoice-emb-imp.parcela
               tt-embarque-fatura.dt-venc-fat   = invoice-emb-imp.dt-vencim.
    end.

    /****** NOTA FISCAL PRINCIPAL ******/
    bloco:
    for EACH docum-est fields( char-1  cod-estabel  serie-docto  nro-docto
                                &if "{&bf_mat_versao_ems}" >= "2.06" &then
                                    idi-nf-simples-remes
                                &else
                                    char-2
                                &endif 
                                cod-emitente  nat-operacao  dt-emissao )
        where docum-est.cod-emitente              = ordem-compra.cod-emitente
          AND trim(substr(docum-est.char-1,1,12)) = embarque-imp.embarque no-lock
        USE-INDEX emitente:

        FIND FIRST natur-oper
            WHERE natur-oper.nat-operacao = docum-est.nat-operacao NO-LOCK NO-ERROR.
        IF NOT AVAIL natur-oper
        OR (AVAIL natur-oper AND natur-oper.nota-rateio) THEN 
            NEXT bloco.

        /*Tratamento realizado para a situaá∆o em que o mesmo embarque possuir nota simples remessa m∆e e nota simples remessa filha. 
        Neste caso a nota m∆e ser† gravada na tt-embarque-nota-mae para ser apresentada em tela.*/
        &if "{&bf_mat_versao_ems}" >= "2.06" &then
            if docum-est.idi-nf-simples-remes = 1 THEN DO:
        &else
            if int(substring(docum-est.char-2,101,1)) = 1 THEN DO:
        &endif 
         
            CREATE tt-embarque-nota-mae.
            ASSIGN tt-embarque-nota-mae.embarque     = embarque-imp.embarque   
                   tt-embarque-nota-mae.cod-estabel  = embarque-imp.cod-estabel
                   tt-embarque-nota-mae.nr-nota-fis  = docum-est.nro-docto             
                   tt-embarque-nota-mae.dt-emissao   = docum-est.dt-emissao            
                   tt-embarque-nota-mae.nr-di        = embarque-imp.declaracao-import  
                   tt-embarque-nota-mae.dt-di        = embarque-imp.data-di
                   tt-embarque-nota-mae.r-docum-est  = ROWID(docum-est).          
            ASSIGN l-nota-mae = YES. 
        END.
        ELSE
            ASSIGN l-nota-mae = NO.

        /****** NOTA PRINCIPAL ******/
        &if "{&bf_mat_versao_ems}" >= "2.06" &then
            if docum-est.idi-nf-simples-remes = 1 
        &else
            if int(substring(docum-est.char-2,101,1)) = 1 
        &endif 
                and tt-param.l-desemb-parc = no then
                assign tt-param.l-desemb-parc = yes
                       l-desemb-parc-modif    = yes.
        IF NOT l-nota-mae THEN
            RUN gera-valores (INPUT docum-est.serie-docto,
                              INPUT docum-est.nro-docto,     
                              INPUT docum-est.cod-emitente,
                              INPUT docum-est.nat-operacao,
                              INPUT embarque-imp.embarque,
                              INPUT docum-est.cod-estabel,
                              INPUT 1). /*Nota Principal*/
        /****************************/

        
        /****** EMBARQUES RELACIONADOS ******/
        IF  tt-param.l-desemb-parc THEN DO:
        &if "{&bf_mat_versao_ems}" >= "2.06" &then
            if docum-est.idi-nf-simples-remes <> 2 then do:
        &else
            if int(substring(docum-est.char-2,101,1)) <> 2 then do:
        &endif
            RUN geraEmbarquesRelacionados (embarque-imp.embarque).
        END.
        END.
        /****** ********* ******/
        /*os pr¢ximos embarques que poder∆o n∆o ser de simples de remessa, ser∆o mostrados corretamente*/
        if l-desemb-parc-modif then
            assign tt-param.l-desemb-parc = no.

        /****** NOTAS COMPLEMENTARES ******/

        IF  tt-param.l-nf-compl THEN DO:
            for each b-docum-est fields( nro-docto  serie-docto  cod-emitente  nat-operacao char-1)
                WHERE b-docum-est.serie-docto  = docum-est.serie-docto
                  AND b-docum-est.nro-docto    = docum-est.nro-docto
                  AND b-docum-est.cod-emitente = docum-est.cod-emitente
                  AND b-docum-est.nat-operacao = docum-est.nat-operacao NO-LOCK:

              for first b-item-doc-est
                  where b-item-doc-est.serie-docto     = b-docum-est.serie-docto
                    and b-item-doc-est.nro-docto       = b-docum-est.nro-docto    
                    and b-item-doc-est.cod-emitente    = b-docum-est.cod-emitente 
                    and b-item-doc-est.nat-operacao    = b-docum-est.nat-operacao 
                    and b-item-doc-est.baixa-ce = yes no-lock:

                    run busca-nf-complementar (input b-item-doc-est.serie-docto,
                                               input b-item-doc-est.nro-docto,
                                               input b-item-doc-est.cod-emitente,
                                               input b-item-doc-est.nat-operacao,
                                               input embarque-imp.embarque,
                                               input embarque-imp.cod-estabel).
                end.
            end.

            &if "{&bf_mat_versao_ems}" >= "2.05" &then
            for each  b-embarque-imp fields( embarque-ori  cod-estabel  embarque 
                                             declaracao-import  data-di)
                where b-embarque-imp.embarque-ori = embarque-imp.embarque no-lock:
            &else
            for each  b-embarque-imp fields( char-1  cod-estabel  embarque 
                                             declaracao-import  data-di)
                where trim(substring(b-embarque-imp.char-1,21,20)) = embarque-imp.embarque no-lock:
            &endif
            
                for first b-docum-est fields( nro-docto  serie-docto  cod-emitente  nat-operacao )
                    where trim(substring(b-docum-est.char-1,1,20)) = b-embarque-imp.embarque no-lock:

                    run busca-nf-complementar (input b-docum-est.serie-docto,
                                               input b-docum-est.nro-docto,
                                               input b-docum-est.cod-emitente,
                                               input b-docum-est.nat-operacao,
                                               input b-embarque-imp.embarque,
                                               input b-embarque-imp.cod-estabel).
                end.
            end.
        END. /*fim tt-param.l-nf-com*/
        /****** ********* ******/
    end. /*docum-est*/      

    ASSIGN de-val-desp-aux-rea = 0.
   
    FOR EACH  tt-embarque-corpo
        WHERE tt-embarque-corpo.embarque    = embarque-imp.embarque
          AND tt-embarque-corpo.cod-estabel = embarque-imp.cod-estabel:

        FOR EACH  tt-detalha-item
            WHERE tt-detalha-item.embarque     = tt-embarque-corpo.embarque
             AND  tt-detalha-item.cod-estabel  = tt-embarque-corpo.cod-estabel
            use-index ch-codigo:
            
            ASSIGN tt-embarque-corpo.vl-ger-mercado-orc           = tt-embarque-corpo.vl-ger-mercado-orc           + tt-detalha-item.vl-mercado-orc-total
                   tt-embarque-corpo.vl-ger-ii-orc                = tt-embarque-corpo.vl-ger-ii-orc                + tt-detalha-item.vl-ii-orc-aux

                   &IF "{&bf_mat_versao_ems}" < "2.062" &THEN
                   tt-embarque-corpo.vl-ger-ipi-orc               = tt-embarque-corpo.vl-ger-ipi-orc               + tt-detalha-item.vl-ipi-orc
                   tt-embarque-corpo.vl-ger-icms-orc              = tt-embarque-corpo.vl-ger-icms-orc              + tt-detalha-item.vl-icms-orc
                   tt-embarque-corpo.vl-ger-pis-orc               = tt-embarque-corpo.vl-ger-pis-orc               + tt-detalha-item.vl-pis-orc
                   tt-embarque-corpo.vl-ger-cofins-orc            = tt-embarque-corpo.vl-ger-cofins-orc            + tt-detalha-item.vl-cofins-orc
                   tt-embarque-corpo.vl-ger-tot-imp-orc           = tt-embarque-corpo.vl-ger-tot-imp-orc         
                                                                    + IF tt-param.i-desp-imposto = 1 THEN  /*Despesa*/   
                                                                       tt-detalha-item.vl-ipi-orc + tt-detalha-item.vl-icms-orc + tt-detalha-item.vl-pis-orc + tt-detalha-item.vl-cofins-orc
                                                                    ELSE /*Imposto*/ 
                                                                       tt-detalha-item.vl-ii-orc-aux + tt-detalha-item.vl-ipi-orc   + tt-detalha-item.vl-icms-orc + tt-detalha-item.vl-pis-orc + tt-detalha-item.vl-cofins-orc
                   &ELSE
                   tt-embarque-corpo.vl-ger-ipi-orc               = tt-embarque-corpo.vl-ger-ipi-orc               
                                                                    + IF  tt-param.i-custo-item = 1 THEN 
                                                                          tt-detalha-item.vl-ipi-orc * tt-detalha-item.quantidade
                                                                      ELSE
                                                                          tt-detalha-item.vl-ipi-orc
                   tt-embarque-corpo.vl-ger-icms-orc              = tt-embarque-corpo.vl-ger-icms-orc              
                                                                    + IF  tt-param.i-custo-item = 1 THEN 
                                                                          tt-detalha-item.vl-icms-orc * tt-detalha-item.quantidade
                                                                      ELSE
                                                                          tt-detalha-item.vl-icms-orc
                   tt-embarque-corpo.vl-ger-pis-orc               = tt-embarque-corpo.vl-ger-pis-orc               
                                                                    + IF  tt-param.i-custo-item = 1 THEN 
                                                                          (tt-detalha-item.vl-pis-orc * tt-detalha-item.quantidade)
                                                                      ELSE
                                                                          tt-detalha-item.vl-pis-orc
                   tt-embarque-corpo.vl-ger-cofins-orc            = tt-embarque-corpo.vl-ger-cofins-orc              
                                                                    + IF  tt-param.i-custo-item = 1 THEN 
                                                                          tt-detalha-item.vl-cofins-orc * tt-detalha-item.quantidade
                                                                      ELSE
                                                                          tt-detalha-item.vl-cofins-orc
                   tt-embarque-corpo.vl-ger-tot-imp-orc           = IF tt-param.i-desp-imposto = 1 THEN  /*Despesa*/ 
                                                                       tt-embarque-corpo.vl-ger-ipi-orc + tt-embarque-corpo.vl-ger-icms-orc + tt-embarque-corpo.vl-ger-pis-orc + tt-embarque-corpo.vl-ger-cofins-orc
                                                                    ELSE /*Imposto*/ 
                                                                       tt-embarque-corpo.vl-ger-ii-orc + tt-embarque-corpo.vl-ger-ipi-orc + tt-embarque-corpo.vl-ger-icms-orc + tt-embarque-corpo.vl-ger-pis-orc + tt-embarque-corpo.vl-ger-cofins-orc 
                   &ENDIF
                   tt-embarque-corpo.vl-ger-ii2-orc               = tt-embarque-corpo.vl-ger-ii2-orc               + tt-detalha-item.vl-ii2-orc-aux
                   tt-embarque-corpo.vl-ger-des-orc               = tt-embarque-corpo.vl-ger-des-orc               + tt-detalha-item.vl-des-orc-total
                                                                                                                                         
                   tt-embarque-corpo.vl-ger-mercado-rea           = tt-embarque-corpo.vl-ger-mercado-rea           + tt-detalha-item.vl-mercado-rea-total
                   tt-embarque-corpo.vl-ger-ii-rea                = tt-embarque-corpo.vl-ger-ii-rea                + tt-detalha-item.vl-ii-rea-total
                   tt-embarque-corpo.vl-ger-ipi-rea               = tt-embarque-corpo.vl-ger-ipi-rea               + tt-detalha-item.vl-ipi-rea-total
                   tt-embarque-corpo.vl-ger-icms-rea              = tt-embarque-corpo.vl-ger-icms-rea              + tt-detalha-item.vl-icms-rea-total            
                   tt-embarque-corpo.vl-ger-pis-rea               = tt-embarque-corpo.vl-ger-pis-rea               + tt-detalha-item.vl-pis-rea-total
                   tt-embarque-corpo.vl-ger-cofins-rea            = tt-embarque-corpo.vl-ger-cofins-rea            + tt-detalha-item.vl-cofins-rea-total                 
                   tt-embarque-corpo.vl-ger-tot-imp-rea           = tt-embarque-corpo.vl-ger-tot-imp-rea           + tt-detalha-item.vl-tot-imp-rea-total         
                   tt-embarque-corpo.vl-ger-ii2-rea               = tt-embarque-corpo.vl-ger-ii2-rea               + tt-detalha-item.vl-ii2-rea-total
                   tt-embarque-corpo.vl-ger-des-rea               = tt-embarque-corpo.vl-ger-des-rea               + tt-detalha-item.vl-des-rea-total
                   tt-embarque-corpo.vl-ger-tot-com-ipi-icms-rea  = tt-embarque-corpo.vl-ger-tot-com-ipi-icms-rea  + tt-detalha-item.vl-tot-com-ipi-icms-rea-total
                   tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-rea  = tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-rea  + tt-detalha-item.vl-tot-sem-ipi-icms-rea-total
                                                                                                                                         
                   tt-embarque-corpo.total-vl-frete               = tt-embarque-corpo.total-vl-frete               + tt-detalha-item.vl-des-frete
                   tt-embarque-corpo.total-vl-embalagem           = tt-embarque-corpo.total-vl-embalagem           + tt-detalha-item.vl-des-embalagem
                   tt-embarque-corpo.total-vl-seguro              = tt-embarque-corpo.total-vl-seguro              + tt-detalha-item.vl-des-seguro
                   tt-embarque-corpo.total-vl-outras              = tt-embarque-corpo.total-vl-outras              + tt-detalha-item.vl-des-outras
                   
                   tt-embarque-corpo.per-mercado                  = (tt-embarque-corpo.vl-ger-mercado-rea * 100)   / tt-embarque-corpo.vl-ger-mercado-rea
                   tt-embarque-corpo.per-imposto                  = (tt-embarque-corpo.vl-ger-tot-imp-rea * 100)   / tt-embarque-corpo.vl-ger-mercado-rea
                   tt-embarque-corpo.per-ii                       = (tt-embarque-corpo.vl-ger-ii-rea      * 100)   / tt-embarque-corpo.vl-ger-mercado-rea.
                
            IF tt-embarque-corpo.vl-ger-mercado-orc = ? THEN
                ASSIGN tt-embarque-corpo.vl-ger-mercado-orc = 0.

            ASSIGN de-tot-rea = 0.
            FOR EACH tt-detalha-despesa-aux
               WHERE tt-detalha-despesa-aux.embarque    = tt-embarque-corpo.embarque
                 AND tt-detalha-despesa-aux.cod-estabel = tt-embarque-corpo.cod-estabel:

                ASSIGN de-tot-rea = de-tot-rea + tt-detalha-despesa-aux.vl-des-rea.

            END.
            
            IF tt-param.i-desp-imposto = 1 THEN
                ASSIGN tt-embarque-corpo.vl-ger-tot-des-rea = de-tot-rea + tt-embarque-corpo.vl-ger-ii2-rea.
            ELSE
                ASSIGN tt-embarque-corpo.vl-ger-tot-des-rea = de-tot-rea.

            IF tt-param.l-desp-imposto THEN DO: /*detalha despesa/impostos*/
                ASSIGN tt-embarque-corpo.per-despesa     = (tt-embarque-corpo.vl-ger-tot-des-rea * 100) /
                                                            tt-embarque-corpo.vl-ger-mercado-rea.
            END.
            ELSE  DO:
                ASSIGN tt-embarque-corpo.per-despesa     = (tt-embarque-corpo.vl-ger-tot-des-rea * 100) /
                                                            tt-embarque-corpo.vl-ger-mercado-rea.
            END.

            /* Convers∆o */
            IF tt-param.moeda <> var-moeda-conv-rea THEN DO:
                IF tt-param.l-desp-imposto THEN DO:
                    ASSIGN tt-embarque-corpo.vl-ger-tot-imp-des-rea = tt-embarque-corpo.vl-ger-tot-des-rea     +
                                                                      tt-embarque-corpo.vl-ger-tot-imp-rea.
                END.
                ELSE DO:
                    ASSIGN tt-embarque-corpo.vl-ger-tot-imp-des-rea = tt-embarque-corpo.vl-ger-tot-des-rea     +
                                                                      tt-embarque-corpo.vl-ger-tot-imp-rea.
                END.
            END.
            ELSE DO:
                IF NOT tt-param.l-desp-imposto THEN DO:
                    ASSIGN tt-embarque-corpo.per-despesa              = (tt-embarque-corpo.vl-ger-tot-des-rea * 100) /
                                                                         tt-embarque-corpo.vl-ger-mercado-rea.
                END.
                ASSIGN tt-embarque-corpo.vl-ger-tot-imp-des-rea       = tt-embarque-corpo.vl-ger-tot-imp-rea
                                                                      + tt-embarque-corpo.vl-ger-tot-des-rea.
            END.

            ASSIGN tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea   = tt-detalha-item.vl-tot-des-rea +
                                                                        tt-detalha-item.vl-tot-imp-rea +
                                                                        tt-detalha-item.vl-mercado-rea
                   tt-detalha-item.vl-tot-sem-ipi-icms-dsp-merc-rea   = tt-detalha-item.vl-tot-des-rea +
                                                                        tt-detalha-item.vl-mercado-rea
                   tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea = tt-embarque-corpo.vl-ger-tot-des-rea +
                                                                        tt-embarque-corpo.vl-ger-tot-imp-rea +
                                                                        tt-embarque-corpo.vl-ger-mercado-rea
                   tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-merc-rea = tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea -
                                                                        tt-embarque-corpo.vl-ger-tot-imp-rea.

            /*PERCENTUAL TT-DETALHA-ITEM - REALIZADO*/
            ASSIGN tt-detalha-item.per-ipi-rea                = (tt-detalha-item.vl-ipi-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-icms-rea               = (tt-detalha-item.vl-icms-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-pis-rea                = (tt-detalha-item.vl-pis-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-cofins-rea             = (tt-detalha-item.vl-cofins-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-mercado-orc            = (tt-detalha-item.vl-mercado-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-mercado-rea            = (tt-detalha-item.vl-mercado-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-ii-orc                 = (tt-detalha-item.vl-ii-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-ii-rea                 = (tt-detalha-item.vl-ii-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-ipi-orc                = (tt-detalha-item.vl-ipi-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-icms-orc               = (tt-detalha-item.vl-icms-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-pis-orc                = (tt-detalha-item.vl-pis-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-cofins-orc             = (tt-detalha-item.vl-cofins-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-imp-orc            = (tt-detalha-item.vl-tot-imp-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-imp-rea            = (tt-detalha-item.vl-tot-imp-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-ii2-orc                = (tt-detalha-item.vl-ii2-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-ii2-rea                = (tt-detalha-item.vl-ii2-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-tot-des-orc            = (tt-detalha-item.vl-tot-des-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-des-rea            = (tt-detalha-item.vl-tot-des-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-tot-imp-des-orc        = (tt-detalha-item.vl-tot-imp-des-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-imp-des-rea        = (tt-detalha-item.vl-tot-imp-des-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-tot-com-ipi-icms-orc   = (tt-detalha-item.vl-tot-com-ipi-icms-orc-total
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-com-ipi-icms-rea   = (tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100
                   tt-detalha-item.per-tot-sem-ipi-icms-orc   = (tt-detalha-item.vl-tot-sem-ipi-icms-orc-total
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-sem-ipi-icms-rea   = (tt-detalha-item.vl-tot-sem-ipi-icms-dsp-merc-rea / 
                                                                 tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100.
            
            IF tt-detalha-item.vl-tot-com-ipi-icms-orc-total = ? THEN
                ASSIGN tt-detalha-item.vl-tot-com-ipi-icms-orc-total = 0.

            IF tt-detalha-item.vl-tot-sem-ipi-icms-orc-total = ? THEN
                ASSIGN tt-detalha-item.vl-tot-sem-ipi-icms-orc-total = 0.

            IF tt-detalha-item.vl-mercado-orc = ? THEN
                ASSIGN tt-detalha-item.vl-mercado-orc = 0.

            IF tt-detalha-item.per-mercado-orc = ? THEN
                ASSIGN tt-detalha-item.per-mercado-orc = 0.

            IF tt-detalha-item.per-mercado-orc = ? THEN
                ASSIGN tt-detalha-item.per-mercado-orc = 0.

            IF tt-detalha-item.per-ii-orc = ? THEN
                ASSIGN tt-detalha-item.per-ii-orc = 0.

            IF tt-detalha-item.per-ipi-orc = ? THEN
                ASSIGN tt-detalha-item.per-ipi-orc = 0.

            IF tt-detalha-item.per-icms-orc = ? THEN
                ASSIGN tt-detalha-item.per-icms-orc = 0.

            IF tt-detalha-item.per-pis-orc = ? THEN
                ASSIGN tt-detalha-item.per-pis-orc = 0.

            IF tt-detalha-item.per-cofins-orc = ? THEN
                ASSIGN tt-detalha-item.per-cofins-orc = 0.

            IF tt-detalha-item.per-tot-imp-orc = ? THEN
                ASSIGN tt-detalha-item.per-tot-imp-orc = 0.

            IF tt-detalha-item.per-ii2-orc = ? THEN
                ASSIGN tt-detalha-item.per-ii2-orc = 0.

            IF tt-detalha-item.per-tot-des-orc = ? THEN
                ASSIGN tt-detalha-item.per-tot-des-orc = 0.

            IF tt-detalha-item.per-tot-imp-des-orc = ? THEN
                ASSIGN tt-detalha-item.per-tot-imp-des-orc = 0.

            IF tt-detalha-item.per-tot-com-ipi-icms-orc = ? THEN
                ASSIGN tt-detalha-item.per-tot-com-ipi-icms-orc = 0.

            IF tt-detalha-item.per-tot-sem-ipi-icms-orc = ? THEN
                ASSIGN tt-detalha-item.per-tot-sem-ipi-icms-orc = 0.

            /*C†lculo dos percentuais das despesas do tt-detalha-item - Oráado e Realizado*/
            FOR EACH  tt-detalha-despesa
                WHERE tt-detalha-despesa.embarque     = embarque-imp.embarque
                  AND tt-detalha-despesa.cod-estabel  = embarque-imp.cod-estabel
                  AND tt-detalha-despesa.numero-ordem = tt-detalha-item.numero-ordem
                  AND tt-detalha-despesa.it-codigo    = tt-detalha-item.it-codigo
                  AND tt-detalha-despesa.sequencia    = tt-detalha-item.sequencia:

                ASSIGN tt-detalha-despesa.per-des-orc = (tt-detalha-despesa.vl-des-orc 
                                                      /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                       tt-detalha-despesa.per-des-rea = (tt-detalha-despesa.vl-des-rea
                                                      /  tt-detalha-item.vl-tot-com-ipi-icms-dsp-merc-rea) * 100.
                IF tt-detalha-despesa.per-des-orc = ? THEN
                    ASSIGN tt-detalha-despesa.per-des-orc = 0.

            END.
            ASSIGN de-tot-orc = 0.
            FOR EACH tt-detalha-despesa-aux
               WHERE tt-detalha-despesa-aux.embarque    = tt-embarque-corpo.embarque
                 AND tt-detalha-despesa-aux.cod-estabel = tt-embarque-corpo.cod-estabel
                 AND tt-detalha-despesa-aux.sequencia   = tt-detalha-despesa-aux.sequencia:
                 
                ASSIGN de-tot-orc = de-tot-orc + tt-detalha-despesa-aux.vl-des-orc.
            END.
        END. /*tt-detalha-item*/

        /*C†lculo dos percentuais das despesas do tt-embarque-corpo - Oráado e Realizado*/
        IF tt-param.i-desp-imposto = 1 THEN /*Despesa*/
            ASSIGN tt-embarque-corpo.vl-ger-tot-des-orc  = tt-embarque-corpo.vl-ger-ii2-orc
                                                         + de-tot-orc.
        ELSE /*Imposto*/
            ASSIGN tt-embarque-corpo.vl-ger-tot-des-orc  = de-tot-orc.

        ASSIGN tt-embarque-corpo.vl-ger-tot-imp-des-orc       = tt-embarque-corpo.vl-ger-tot-imp-orc 
                                                              + tt-embarque-corpo.vl-ger-tot-des-orc
               tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc  = tt-embarque-corpo.vl-ger-mercado-orc              
                                                              + tt-embarque-corpo.vl-ger-tot-imp-orc
                                                              + tt-embarque-corpo.vl-ger-tot-des-orc
               tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-orc  = tt-embarque-corpo.vl-ger-mercado-orc
                                                              + tt-embarque-corpo.vl-ger-tot-des-orc.
        FOR EACH tt-detalha-despesa-aux
           WHERE tt-detalha-despesa-aux.embarque    = embarque-imp.embarque
             AND tt-detalha-despesa-aux.cod-estabel = embarque-imp.cod-estabel:
            IF tt-param.l-desp-imposto THEN DO:
               ASSIGN tt-detalha-despesa-aux.per-des-orc  = (tt-detalha-despesa-aux.vl-des-orc / 
                                                             tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
                      tt-detalha-despesa-aux.per-des-rea  = (tt-detalha-despesa-aux.vl-des-rea /
                                                             tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100.
               IF tt-detalha-despesa-aux.per-des-orc = ? THEN
                   ASSIGN tt-detalha-despesa-aux.per-des-orc = 0.
            END.
        END.
         
        /* PERCENTUAIS */ 
        assign /* -------------- ORÄADO -------------- */
               tt-embarque-corpo.per-ger-mercado-orc          = (tt-embarque-corpo.vl-ger-mercado-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-ii-orc               = (tt-embarque-corpo.vl-ger-ii-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-ipi-orc              = (tt-embarque-corpo.vl-ger-ipi-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-icms-orc             = (tt-embarque-corpo.vl-ger-icms-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-pis-orc              = (tt-embarque-corpo.vl-ger-pis-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-cofins-orc           = (tt-embarque-corpo.vl-ger-cofins-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-tot-imp-orc          = (tt-embarque-corpo.vl-ger-tot-imp-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-ii2-orc              = (tt-embarque-corpo.vl-ger-ii2-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-des-orc              = (tt-embarque-corpo.vl-ger-des-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-tot-des-orc          = (tt-embarque-corpo.vl-ger-tot-des-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-tot-imp-des-orc      = (tt-embarque-corpo.vl-ger-tot-imp-des-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-tot-com-ipi-icms-orc = (tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
               tt-embarque-corpo.per-ger-tot-sem-ipi-icms-orc = (tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-orc
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-orc) * 100
    
               /* -------------- REALIZADO -------------- */
               tt-embarque-corpo.per-ger-mercado-rea          = (tt-embarque-corpo.vl-ger-mercado-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-ii-rea               = (tt-embarque-corpo.vl-ger-ii-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-ipi-rea              = (tt-embarque-corpo.vl-ger-ipi-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-icms-rea             = (tt-embarque-corpo.vl-ger-icms-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-pis-rea              = (tt-embarque-corpo.vl-ger-pis-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-cofins-rea           = (tt-embarque-corpo.vl-ger-cofins-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-tot-imp-rea          = (tt-embarque-corpo.vl-ger-tot-imp-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-ii2-rea              = (tt-embarque-corpo.vl-ger-ii2-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-des-rea              = (tt-embarque-corpo.vl-ger-des-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-tot-des-rea          = (tt-embarque-corpo.vl-ger-tot-des-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-tot-imp-des-rea      = (tt-embarque-corpo.vl-ger-tot-imp-des-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-tot-com-ipi-icms-rea = (tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100
               tt-embarque-corpo.per-ger-tot-sem-ipi-icms-rea = (tt-embarque-corpo.vl-ger-tot-sem-ipi-icms-merc-rea
                                                              /  tt-embarque-corpo.vl-ger-tot-com-ipi-icms-merc-rea) * 100.


               IF tt-embarque-corpo.per-ger-mercado-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-mercado-orc = 0.
                                                              
               IF tt-embarque-corpo.per-ger-ii-orc = ? THEN 
                   ASSIGN tt-embarque-corpo.per-ger-ii-orc = 0.
                                                              
               IF tt-embarque-corpo.per-ger-ipi-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-ipi-orc = 0.
                                                              
               IF tt-embarque-corpo.per-ger-icms-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-icms-orc = 0.
                                                              
               IF tt-embarque-corpo.per-ger-pis-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-pis-orc = 0.
                                                              
               IF tt-embarque-corpo.per-ger-cofins-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-cofins-orc = 0.
                                                              
               IF tt-embarque-corpo.per-ger-tot-imp-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-tot-imp-orc = 0.
                                                              
               IF tt-embarque-corpo.per-ger-ii2-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-ii2-orc = 0.
                                                              
               IF tt-embarque-corpo.per-ger-des-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-des-orc = 0.
                                                              
               IF tt-embarque-corpo.per-ger-tot-des-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-tot-des-orc = 0.
                                                              
               IF tt-embarque-corpo.per-ger-tot-imp-des-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-tot-imp-des-orc = 0.
                                                              
               IF tt-embarque-corpo.per-ger-tot-com-ipi-icms-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-tot-com-ipi-icms-orc = 0.
                                                              
               IF tt-embarque-corpo.per-ger-tot-sem-ipi-icms-orc = ? THEN
                   ASSIGN tt-embarque-corpo.per-ger-tot-sem-ipi-icms-orc = 0.
                                                              
    END.
    
END. /*embarque-imp*/

run pi-acompanhar in h-acomp (input "Imprimindo Dados...").

case tt-param.classifica:
    when 1 then do:
        run imp/esim0665a.p (input table tt-param,
                           input table tt-detalha-item,
                           input table tt-embarque-cabec,
                           input table tt-embarque-corpo,
                           input table tt-embarque-fatura,
                           input table tt-detalha-despesa,
                           input table tt-detalha-despesa-aux,
                           input table tt-embarque-rel,
                           input table tt-nf-comp,
                           INPUT TABLE tt-embarque-nota-mae).
    end.
    when 2 then do:
        run imp/esim0665b.p (input table tt-param,
                           input table tt-detalha-item,
                           input table tt-embarque-cabec,
                           input table tt-embarque-corpo).
    end.
    when 3 then do:
        run imp/esim0665c.p (input table tt-param,
                           input table tt-detalha-item,
                           input table tt-embarque-cabec,
                           input table tt-embarque-corpo).
    end.
    when 4 then do:
        run imp/esim0665d.p (input table tt-param,
                           input table tt-detalha-item,
                           input table tt-embarque-cabec,
                           input table tt-embarque-corpo).
    end.
end case.

case tt-param.destino:
   when 1 then 
       c-destino = "Impressora".
   when 2 then 
       c-destino = "Arquivo".
   when 3 then 
       c-destino = "Terminal".
end.
RUN pi-notas-fiscais.
DEF STREAM s-excel.
DEF VAR c-ender-arq AS CHAR NO-UNDO.
ASSIGN c-ender-arq = SESSION:TEMP-DIRECTORY + STRING(TIME) + STRING(TODAY,"99999999") + ".csv".
OUTPUT STREAM s-excel TO VALUE(c-ender-arq). 
DEF VAR c-titulo AS CHAR NO-UNDO.
DEF VAR l-entrou AS LOG NO-UNDO.
ASSIGN
  c-titulo =
  "Estabelecimento;Desc Estabel;Exportador;Embarque;Incoterm;" + 
  "Via de Transporte;Pais de Origem;Ponto de Embarque;Data Embarque;" +
  "Ponto de Nacionalizacao;Data Nacionalizacao;Data Chegada na Fabrica;" +
  "Data Cotacao Nacionaliz;Moeda-Taxa-Dt Conversao;Taxa Moeda;Data Conversao;" +
  "Nr Nota Fiscal;Dt Emissao;Numero DI;Data DI;" +
  "NF Valor IPI;NF Valor ICM;NF Valor PIS;NF Valor COFINS;" +
  "NF Complementar;Dt Emiss∆o;" + 
  "NF Valor IPI;NF Valor ICM;NF Valor PIS;NF Valor COFINS;"
  .
PUT STREAM s-excel 
  c-titulo FORMAT "X(600)"
  SKIP.
FOR EACH tt-embarque-cabec 
  NO-LOCK:
  FOR EACH tt-notas-fiscais 
    WHERE  tt-notas-fiscais.embarque    = tt-embarque-cabec.embarque
    AND    tt-notas-fiscais.cod-estabel = tt-embarque-cabec.cod-estabel:
    /*--------------------------------------------------------------------*/ 
    RUN pi-imprime-dados.
    /*--------------------------------------------------------------------*/ 
    PUT STREAM s-excel tt-notas-fiscais.nr-nota-fis ";".
    PUT STREAM s-excel tt-notas-fiscais.dt-emissao ";".
    PUT STREAM s-excel tt-notas-fiscais.nr-di ";".
    PUT STREAM s-excel tt-notas-fiscais.dt-di ";".
    /*--------------------------------------------------------------------*/ 
    RUN pi-busca-impostos(
      INPUT  tt-notas-fiscais.r-docum-est,
      /*----------------*/
      OUTPUT de-nf-valor-ipi,
      OUTPUT de-nf-valor-icm,
      OUTPUT de-nf-valor-pis,
      OUTPUT de-nf-val-cofins).
    /*--------------------------------------------------------------------*/ 
    PUT STREAM s-excel de-nf-valor-ipi   ";".    
    PUT STREAM s-excel de-nf-valor-icm   ";".    
    PUT STREAM s-excel de-nf-valor-pis   ";".    
    PUT STREAM s-excel de-nf-val-cofins  ";".  
    PUT STREAM s-excel SKIP.
  END.
  ASSIGN l-entrou = NO.
  FOR EACH tt-nf-comp
    WHERE  tt-nf-comp.embarque    = tt-embarque-cabec.embarque
    AND    tt-nf-comp.cod-estabel = tt-embarque-cabec.cod-estabel:
    ASSIGN l-entrou = YES.
    /*--------------------------------------------------------------------*/
    RUN pi-imprime-dados.
    /*--------------------------------------------------------------------*/
    PUT STREAM s-excel /* tt-notas-fiscais.nr-nota-fis */ ";".
    PUT STREAM s-excel /* tt-notas-fiscais.dt-emissao  */ ";".
    PUT STREAM s-excel /* tt-notas-fiscais.nr-di       */ ";".
    PUT STREAM s-excel /* tt-notas-fiscais.dt-di       */ ";".
    /*--------------------------------------------------------------------*/
    PUT STREAM s-excel /* de-nf-valor-ipi   */ ";".    
    PUT STREAM s-excel /* de-nf-valor-icm   */ ";".    
    PUT STREAM s-excel /* de-nf-valor-pis   */ ";".    
    PUT STREAM s-excel /* de-nf-val-cofins  */ ";".  
    /*--------------------------------------------------------------------*/
    PUT STREAM s-excel tt-nf-comp.nr-nf-comp ";".
    PUT STREAM s-excel tt-nf-comp.dt-emis-comp ";".
    /*--------------------------------------------------------------------*/
    RUN pi-busca-impostos(
      INPUT tt-nf-comp.r-docum-est,
      /*----------------*/
      OUTPUT de-nf-valor-ipi,
      OUTPUT de-nf-valor-icm,
      OUTPUT de-nf-valor-pis,
      OUTPUT de-nf-val-cofins).
    /*--------------------------------------------------------------------*/
    PUT STREAM s-excel de-nf-valor-ipi  ";".   
    PUT STREAM s-excel de-nf-valor-icm  ";".   
    PUT STREAM s-excel de-nf-valor-pis  ";".   
    PUT STREAM s-excel de-nf-val-cofins ";".   
    PUT STREAM s-excel SKIP.
  END.
  IF l-entrou = NO THEN
    PUT STREAM s-excel SKIP.
END.
OUTPUT STREAM s-excel CLOSE.
DOS SILENT START excel.exe VALUE(c-ender-arq).
/* Display do parÉmetros */
disp
    tt-param.c-cod-estabel-ini
    tt-param.c-cod-estabel-fim
    tt-param.c-embarque-ini
    tt-param.c-embarque-fim
    tt-param.c-it-codigo-ini
    tt-param.c-it-codigo-fim
    tt-param.i-ge-codigo-ini
    tt-param.i-ge-codigo-fim
    tt-param.c-fm-codigo-ini
    tt-param.c-fm-codigo-fim
    tt-param.i-cod-emitente-ini
    tt-param.i-cod-emitente-fim
    tt-param.c-nr-proc-imp-ini
    tt-param.c-nr-proc-imp-fim
    tt-param.i-cod-itiner-ini
    tt-param.i-cod-itiner-fim
    tt-param.da-dt-nac-ini
    tt-param.da-dt-nac-fim
    tt-param.desc-classifica
    tt-param.moeda
    c-desc-moeda
    tt-param.da-dt-conv
    tt-param.l-dt-nacional
    tt-param.l-orc-real
    tt-param.l-desp-imposto
    tt-param.l-itens
    tt-param.c-custo-item
    tt-param.c-desp-imposto
    tt-param.l-desemb-parc
    tt-param.l-nf-compl
    c-destino
    tt-param.arquivo
    tt-param.usuario
    with frame f-parametros.

run pi-finalizar in h-acomp.

{include/i-rpclo.i}


PROCEDURE converte-moeda:
    def input-output param p-vl-conversao as decimal no-undo.
    def input parameter p-moeda like moeda.mo-codigo no-undo.

    run cdp/cd0812.p (input  p-moeda,         /*moeda origem*/
                      input  tt-param.moeda,  /*moeda destino*/
                      input  p-vl-conversao,  /*valor origem*/
                      input  d-data-conv,     /*data conversao*/
                      output p-vl-conversao). /*valor destino*/


END PROCEDURE.

PROCEDURE geraEmbarquesRelacionados:

    DEF INPUT PARAM c-embarque-pai LIKE embarque-imp.embarque NO-UNDO.

    &if "{&bf_mat_versao_ems}" >= "2.05" &then
        for each  b-embarque-imp fields( embarque-ori  cod-estabel  embarque 
                                         declaracao-import  data-di)
            where b-embarque-imp.embarque-ori = c-embarque-pai no-lock:
    &else
        for each  b-embarque-imp fields( char-1  cod-estabel  embarque 
                                         declaracao-import  data-di)
            where trim(substring(b-embarque-imp.char-1,21,20)) = c-embarque-pai no-lock:
    &endif
        FOR FIRST ordens-embarque NO-LOCK
            WHERE ordens-embarque.cod-estabel  = b-embarque-imp.cod-estabel
              AND ordens-embarque.embarque     = b-embarque-imp.embarque
            USE-INDEX embarque:
        END.
    
        FOR FIRST ordem-compra NO-LOCK
            WHERE ordem-compra.numero-ordem = ordens-embarque.numero-ordem
            USE-INDEX ordem:
        END.
    
        create tt-embarque-rel.
        assign tt-embarque-rel.embarque      = embarque-imp.embarque
               tt-embarque-rel.cod-estabel   = b-embarque-imp.cod-estabel
               tt-embarque-rel.embarque-rel  = b-embarque-imp.embarque.
        for first b-docum-est fields( char-1  serie-docto  nro-docto cod-estabel
                                      cod-emitente  nat-operacao  dt-emissao )
            where b-docum-est.cod-emitente              = ordem-compra.cod-emitente
              AND trim(substr(b-docum-est.char-1,1,12)) = b-embarque-imp.embarque no-lock
            USE-INDEX emitente:
        end.
        if  avail b-docum-est then do:

            assign tt-embarque-rel.nr-nota-rel = b-docum-est.nro-docto
                   tt-embarque-rel.dt-emis-rel = b-docum-est.dt-emissao
                   tt-embarque-rel.nr-di-rel   = b-embarque-imp.declaracao-import
                   tt-embarque-rel.dt-di-rel   = b-embarque-imp.data-di.

            RUN gera-valores (INPUT b-docum-est.serie-docto,
                              INPUT b-docum-est.nro-docto,     
                              INPUT b-docum-est.cod-emitente,
                              INPUT b-docum-est.nat-operacao,
                              INPUT b-embarque-imp.embarque, /*embarque relacionado*/
                              INPUT b-docum-est.cod-estabel,
                              INPUT 2). /*Nota Relacionada*/
        end.
        RUN geraEmbarquesRelacionados(INPUT b-embarque-imp.embarque).
    end.
END PROCEDURE.

PROCEDURE gera-valores:
    define input parameter p-serie        like docum-est.serie          no-undo.
    define input parameter p-nro-docto    like docum-est.nro-docto      no-undo. 
    define input parameter p-cod-emitente like docum-est.cod-emitente   no-undo. 
    define input parameter p-nat-operacao like docum-est.nat-operacao   no-undo. 
    DEFINE INPUT PARAMETER pc-embarque    LIKE embarque-imp.embarque    NO-UNDO.
    DEFINE INPUT PARAMETER pc-cod-estabel LIKE embarque-imp.cod-estabel NO-UNDO.
    define input parameter p-situacao     as int                        no-undo. 
     
    DEF VAR de-tot-des-orc-aux AS DECIMAL NO-UNDO.
    DEF VAR de-tot-desp        AS DECIMAL NO-UNDO.
    DEF VAR de-tot-desp-aux    AS DECIMAL NO-UNDO. 
    DEF VAR de-val-desp-aux    AS DECIMAL NO-UNDO.
    DEF VAR de-val-desp-nfc    AS DECIMAL NO-UNDO.
    def var de-val-desp        as decimal no-undo.
    DEF VAR de-tot-imp-rea     AS DECIMAL NO-UNDO.
    DEF VAR de-despesa-total   AS DECIMAL NO-UNDO.
    DEF VAR de-residuo         AS DECIMAL NO-UNDO.
    
    /* p-situacao..: 1- Nota Principal    **
    **               2- Nota Relacionada  **
    **               3- Nota Complementar */

    for each  item-doc-est fields( serie-docto   nro-docto      cod-emitente   nat-operacao  sequencia
                                   it-codigo     preco-unit[1]  numero-ordem   quantidade    qt-do-forn
                                   aliquota-ipi  aliquota-icm   valor-ipi[1]   valor-icm[1]  un parcela 
                                   item-doc-est.preco-total[1]
                                   cd-trib-icm cd-trib-ipi seq-comp
                                   &if "{&bf_mat_versao_ems}" >= "2.06" &then
                                       num-ord-import num-parc-import valor-pis val-cofins val-aliq-pis  val-aliq-cofins 
                                   &else
                                       char-2
                                   &endif
                                   )
        where item-doc-est.serie-docto  =  p-serie
          and item-doc-est.nro-docto    =  p-nro-docto
          and item-doc-est.cod-emitente =  p-cod-emitente
          and item-doc-est.nat-operacao =  p-nat-operacao no-lock
        use-index documento
        break by item-doc-est.it-codigo
              by &if "{&bf_mat_versao_ems}" >= "2.06" &then
                     item-doc-est.num-ord-import
                 &ELSE
                     int(substr(item-doc-est.char-2,145,8))
                 &endif
              by item-doc-est.numero-ordem:

        FIND FIRST item-doc-est-cex OF item-doc-est NO-LOCK NO-ERROR.
        IF  AVAIL item-doc-est-cex THEN DO:
                IF  p-situacao = 3 AND
                &if "{&bf_mat_versao_ems}" >= "2.06" &THEN
                    item-doc-est-cex.embarque <> pc-embarque
                &ELSE
                    (trim(substr(item-doc-est-cex.char-1,1,12)) <> pc-embarque)
                &ENDIF
                THEN
                NEXT.
        END.

        if item-doc-est.it-codigo < tt-param.c-it-codigo-ini or
           item-doc-est.it-codigo > tt-param.c-it-codigo-fim then next.

        for first item fields(it-codigo  ge-codigo  fm-codigo desc-item un)
            where item.it-codigo =  item-doc-est.it-codigo
              and item.ge-codigo >= tt-param.i-ge-codigo-ini
              and item.ge-codigo <= tt-param.i-ge-codigo-fim
              and item.fm-codigo >= tt-param.c-fm-codigo-ini
              and item.fm-codigo <= tt-param.c-fm-codigo-fim no-lock:
        end.
        if  not avail item then next.

        /*Notas Complementares*/
        if  p-situacao = 3 then do:
        
            for first  tt-detalha-item
                 where tt-detalha-item.embarque    = embarque-imp.embarque
                 and   tt-detalha-item.cod-estabel = embarque-imp.cod-estabel
                 and   tt-detalha-item.it-codigo   = item-doc-est.it-codigo 
                 AND   tt-detalha-item.sequencia   = item-doc-est.seq-comp no-lock
                use-index ch-codigo:
            end.
            if  not avail tt-detalha-item then next.
            
            for first ordens-embarque fields(numero-ordem)
                where ordens-embarque.embarque    = embarque-imp.embarque
                  and ordens-embarque.cod-estabel = embarque-imp.cod-estabel no-lock: end. 
            if  not avail ordens-embarque then next. 
            
            for first ordem-compra 
                where ordem-compra.numero-ordem = ordens-embarque.numero-ordem no-lock: end. 
            if  not avail ordem-compra then next.

            &if "{&bf_mat_versao_ems}" >= "2.05" &then
                for first cotacao-item fields( numero-ordem  cod-emitente  mo-codigo  aliquota-ii
                                               data-cotacao  cot-aprovada  it-codigo  seq-cotac char-1)
                    where cotacao-item.numero-ordem = ordem-compra.numero-ordem
                      and cotacao-item.cod-emitente = ordem-compra.cod-emitente
                      and cotacao-item.data-cotacao = ordem-compra.data-cotacao
                      and cotacao-item.cot-aprovada = yes no-lock:
                end.
            &else
                for first cotacao-item fields( numero-ordem  cod-emitente  mo-codigo  char-1
                                               data-cotacao  cot-aprovada  it-codigo  seq-cotac )
                    where cotacao-item.numero-ordem = ordem-compra.numero-ordem
                      and cotacao-item.cod-emitente = ordem-compra.cod-emitente
                      and cotacao-item.data-cotacao = ordem-compra.data-cotacao
                      and cotacao-item.cot-aprovada = yes no-lock:
                end.
            &endif
            if  not avail cotacao-item then
                next.
            
            assign tot-despesa-rea = 0
                   de-val-desp     = 0
                   de-tot-desp-aux  = 0
                   de-despesa-total = 0.
            
            for each  item-doc-est-cex fields(val-desp nro-docto sequencia)
                    where item-doc-est-cex.serie-docto  = item-doc-est.serie-docto
                    and   item-doc-est-cex.nro-docto    = item-doc-est.nro-docto
                    and   item-doc-est-cex.cod-emitente = item-doc-est.cod-emitente
                    and   item-doc-est-cex.nat-operacao = item-doc-est.nat-operacao
                    and   item-doc-est-cex.sequencia    = item-doc-est.sequencia
                    &if "{&bf_mat_versao_ems}" <= "2.05" &then
                        and  trim(substr(item-doc-est-cex.char-1,1,12)) = embarque-imp.embarque
                        and  trim(substr(item-doc-est-cex.char-1,13,3)) = pc-cod-estabel no-lock
                    &else
                        and  item-doc-est-cex.embarque    = embarque-imp.embarque
                        and  item-doc-est-cex.cod-estabel = pc-cod-estabel no-lock
                     &endif
                     use-index codigo:

                     assign de-val-desp = de-val-desp + item-doc-est-cex.val-desp.
            end.
            
            for each  item-doc-est-cex fields(mo-codigo cod-desp val-desp nro-docto sequencia)
                where item-doc-est-cex.serie-docto  = item-doc-est.serie-docto   
                and   item-doc-est-cex.nro-docto    = item-doc-est.nro-docto     
                and   item-doc-est-cex.cod-emitente = item-doc-est.cod-emitente 
                and   item-doc-est-cex.nat-operacao = item-doc-est.nat-operacao 
                and   item-doc-est-cex.sequencia    = item-doc-est.sequencia 
                &if "{&bf_mat_versao_ems}" <= '2.05' &then
                    and  trim(substr(item-doc-est-cex.char-1,1,12)) = embarque-imp.embarque
                    and  trim(substr(item-doc-est-cex.char-1,13,3)) = pc-cod-estabel no-lock
                &else
                    and  item-doc-est-cex.embarque    = embarque-imp.embarque
                    and  item-doc-est-cex.cod-estabel = pc-cod-estabel no-lock
                &endif
                use-index codigo BREAK BY item-doc-est-cex.cod-desp:

                /****ORÄADO-INI**/
                for each  cotacao-item-cex fields( numero-ordem  cod-emitente  mapa-cotacao
                                                   cod-incoterm  it-codigo     seq-cotac
                                                   data-cotacao  cod-itiner    cod-desp
                                                   val-desp      mo-codigo)
                    where cotacao-item-cex.numero-ordem = cotacao-item.numero-ordem
                    and   cotacao-item-cex.cod-emitente = cotacao-item.cod-emitente
                    and   cotacao-item-cex.it-codigo    = item-doc-est.it-codigo
                    and   cotacao-item-cex.mapa-cotacao = int(trim(substring(cotacao-item.char-1,1,20)))
                    and   cotacao-item-cex.cod-incoterm = embarque-imp.cod-incoterm
                    and   cotacao-item-cex.cod-itiner   = itinerario.cod-itiner no-lock:
                   
                    for first desp-imp fields (cod-desp tipo descricao)
                         where desp-imp.cod-desp = cotacao-item-cex.cod-desp no-lock: 
                    end.
                    if  not avail desp-imp then next.

                    if  tt-param.i-custo-item = 2 and desp-imp.tipo <> 1 then /*Total e Diferente de Fixo*/
                        assign vl-desp-orcad-det = cotacao-item-cex.val-desp
                                                 * item-doc-est.quantidade.
                    else
                        assign vl-desp-orcad-det = cotacao-item-cex.val-desp.
                        
                    if  tt-param.i-custo-item = 2 and desp-imp.tipo <> 1 then /*Total e Diferente de Fixo*/
                        ASSIGN vl-desp-orcad-aux = cotacao-item-cex.val-desp
                                                 * item-doc-est.quantidade.
                    ELSE
                        ASSIGN vl-desp-orcad-aux = cotacao-item-cex.val-desp.

                    /*Oráado*/
                    IF  desp-imp.cod-desp <> i-cod-desp-ii 
                    &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi 
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms 
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins
                    &ENDIF
                    THEN DO:

                        for first tt-detalha-despesa-aux
                             where tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                               and tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                               and tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp 
                               AND tt-detalha-despesa-aux.sequencia    = item-doc-est.sequencia no-lock
                            use-index ch-codigo:
                        end.
                        if not avail tt-detalha-despesa-aux then do:
                            CREATE tt-detalha-despesa-aux.
                            ASSIGN tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                                   tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                                   tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp
                                   tt-detalha-despesa-aux.desc-despesa = desp-imp.descricao
                                   tt-detalha-despesa-aux.vl-des-orc   = vl-desp-orcad-aux
                                   tt-detalha-despesa-aux.sequencia    = item-doc-est.sequencia.
                            
                            if tt-param.moeda <> var-moeda-conv-orc THEN DO:
                               IF AVAIL tt-detalha-despesa-aux THEN
                                  run converte-moeda (input-output tt-detalha-despesa-aux.vl-des-orc,
                                                      input var-moeda-conv-orc).
                            END.
                            ASSIGN de-tot-desp-aux = de-tot-desp-aux + tt-detalha-despesa-aux.vl-des-orc.
                        END.
                        ELSE DO:
                            ASSIGN de-tot-desp-aux = de-tot-desp-aux + tt-detalha-despesa-aux.vl-des-orc.
                        END.
                    END.

                    /*----- Notas Complementares -----*/
                    IF  desp-imp.cod-desp <> i-cod-desp-ii 
                    &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi  
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms 
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis
                    AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins
                    &ENDIF
                    THEN DO:
                        find FIRST tt-detalha-despesa 
                             WHERE tt-detalha-despesa.embarque    = embarque-imp.embarque 
                               AND tt-detalha-despesa.cod-estabel = embarque-imp.cod-estabel
                               AND tt-detalha-despesa.cod-despesa = desp-imp.cod-desp no-lock no-error.
                        if not avail tt-detalha-despesa then do:
                            create tt-detalha-despesa.
                            assign tt-detalha-despesa.embarque      = embarque-imp.embarque
                                   tt-detalha-despesa.cod-estabel   = embarque-imp.cod-estabel
                                   tt-detalha-despesa.numero-ordem  = cotacao-item-cex.numero-ordem
                                   tt-detalha-despesa.nro-docto     = item-doc-est.nro-docto
                                   tt-detalha-despesa.cod-despesa   = desp-imp.cod-desp
                                   tt-detalha-despesa.desc-despesa  = desp-imp.descricao
                                   tt-detalha-despesa.situacao      = p-situacao
                                   tt-detalha-despesa.vl-des-orc    = vl-desp-orcad-det
                                   tot-despesa-orc                  = tot-despesa-orc
                                                                    + vl-desp-orcad-det
                                   tt-detalha-despesa.it-codigo     = item-doc-est.it-codigo.
        
                            assign var-moeda-conv-orc = cotacao-item-cex.mo-codigo.
        
                            if  tt-param.moeda <> var-moeda-conv-orc THEN 
                                run converte-moeda (input-output tt-detalha-despesa.vl-des-orc,
                                                    input var-moeda-conv-orc).
                        END.
                    END.
                end. /*cotacao-item-cex*/

                /*Convers∆o do Valor Adicional*/
                if  tt-param.i-custo-item = 1 then
                    assign var-preco-unit = de-val-desp / tt-detalha-item.quantidade.
                else 
                    assign var-preco-unit = de-val-desp.


                assign var-preco-unit-aux = de-val-desp. /*item-doc-est.preco-unit[1].*/
                

                if  tt-param.moeda <> item-doc-est-cex.mo-codigo then do:
                    run converte-moeda (input-output var-preco-unit,
                                        input item-doc-est-cex.mo-codigo).
                
                    run converte-moeda (input-output var-preco-unit-aux,
                                        input item-doc-est-cex.mo-codigo).
                end.

                IF item-doc-est.cd-trib-icm <> 1 THEN 
                    ASSIGN var-valor-icm-mattran-rea = item-doc-est.valor-icm[1].

                IF item-doc-est.cd-trib-ipi <> 1 THEN
                    assign var-valor-ipi-mattran-rea = item-doc-est.valor-ipi[1].


                ASSIGN var-valor-pis-mattran-rea    = &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                                                          item-doc-est.valor-pis &ELSE DEC(substring(item-doc-est.char-2,41,14)) &ENDIF
                       var-valor-cofins-mattran-rea = &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                                                          item-doc-est.val-cofins &ELSE DEC(substring(item-doc-est.char-2,103,14)) &ENDIF. 

                if  tt-param.i-custo-item = 1 then /*Unitario*/
                    assign var-valor-ipi-mattran-rea    = var-valor-ipi-mattran-rea    / tt-detalha-item.quantidade
                           var-valor-icm-mattran-rea    = var-valor-icm-mattran-rea    / tt-detalha-item.quantidade
                           var-valor-pis-mattran-rea    = var-valor-pis-mattran-rea    / tt-detalha-item.quantidade
                           var-valor-cofins-mattran-rea = var-valor-cofins-mattran-rea / tt-detalha-item.quantidade.
                
                for first desp-imp
                    where desp-imp.cod-desp = item-doc-est-cex.cod-desp no-lock: end.

                FOR FIRST b-item-doc-est-aux2
                    WHERE b-item-doc-est-aux2.serie-docto  = b-item-doc-est.serie-docto 
                      AND b-item-doc-est-aux2.nro-docto    = b-item-doc-est.nro-docto   
                      AND b-item-doc-est-aux2.cod-emitente = b-item-doc-est.cod-emitente
                      AND b-item-doc-est-aux2.nat-operacao = b-item-doc-est.nat-operacao
                      AND b-item-doc-est-aux2.sequencia    = item-doc-est.seq-comp NO-LOCK:
                END.

                if  avail b-item-doc-est THEN
                    for first b-item-doc-est-aux 
                        where b-item-doc-est-aux.serie-docto  = b-item-doc-est.serie-docto
                        and   b-item-doc-est-aux.nro-docto    = b-item-doc-est.nro-docto   
                        and   b-item-doc-est-aux.cod-emitente = b-item-doc-est.cod-emitente 
                        and   b-item-doc-est-aux.nat-operacao = b-item-doc-est.nat-operacao 
                        and   b-item-doc-est-aux.it-codigo    = item-doc-est.it-codigo no-lock: 
                    end.

                FOR FIRST bf-natur-oper
                     WHERE bf-natur-oper.nat-operacao = item-doc-est.nat-operacao NO-LOCK:
                END.

                find first tt-detalha-despesa
                    where  tt-detalha-despesa.embarque     = embarque-imp.embarque
                    and    tt-detalha-despesa.cod-estabel  = embarque-imp.cod-estabel
                    and    tt-detalha-despesa.numero-ordem = (&if "{&bf_mat_versao_ems}" >= "2.06" &then 
                                                                  if  docum-est.idi-nf-simples-remes = 1 then
                                                                      b-item-doc-est-aux2.num-ord-import 
                                                              &else 
                                                                  if  int(substring(docum-est.char-2,101,1)) = 1 then
                                                                      int(substring(b-item-doc-est.char-2,145,9))
                                                              &endif
                                                                  else if avail b-item-doc-est-aux2 then 
                                                                      b-item-doc-est-aux2.numero-ordem 
                                                                      else 
                                                                          b-item-doc-est.numero-ordem)
                    and    tt-detalha-despesa.nro-docto    = item-doc-est.nro-docto 
                    and    tt-detalha-despesa.cod-despesa  = desp-imp.cod-desp 
                    and    tt-detalha-despesa.it-codigo    = item-doc-est.it-codigo
                    AND    tt-detalha-despesa.sequencia    = b-item-doc-est-aux2.sequencia no-lock no-error.

                if  not avail tt-detalha-despesa then do:
                    create tt-detalha-despesa.
                    assign tt-detalha-despesa.embarque      = embarque-imp.embarque
                           tt-detalha-despesa.cod-estabel   = embarque-imp.cod-estabel
                           tt-detalha-despesa.numero-ordem  = (&if "{&bf_mat_versao_ems}" >= "2.06" &then 
                                                                   if  docum-est.idi-nf-simples-remes = 1 then
                                                                       b-item-doc-est-aux2.num-ord-import 
                                                               &else 
                                                                   if  int(substring(docum-est.char-2,101,1)) = 1 then 
                                                                       int(substring(b-item-doc-est.char-2,145,9))
                                                               &endif
                                                                   else if avail b-item-doc-est-aux2 then
                                                                       b-item-doc-est-aux2.numero-ordem 
                                                                       else
                                                                           b-item-doc-est.numero-ordem)
                           tt-detalha-despesa.nro-docto     = item-doc-est.nro-docto
                           tt-detalha-despesa.cod-despesa   = desp-imp.cod-desp
                           tt-detalha-despesa.desc-despesa  = desp-imp.descricao
                           tt-detalha-despesa.it-codigo     = item-doc-est.it-codigo
                           tt-detalha-despesa.situacao      = p-situacao
                           tot-despesa-orc                  = tot-despesa-orc
                                                            + vl-desp-orcad-det.
                           tt-detalha-despesa.sequencia     = b-item-doc-est-aux2.sequencia.
                
                    IF item-doc-est.cd-trib-icm = 1 AND item-doc-est.valor-icm[1] <> 0 THEN
                        ASSIGN de-valor-icm = (item-doc-est-cex.val-desp * item-doc-est.valor-icm[1]) / item-doc-est.preco-total[1].
                    ELSE
                        ASSIGN de-valor-icm = 0.


                    IF item-doc-est.cd-trib-ipi = 3 AND item-doc-est.valor-ipi[1] <> 0 THEN
                        ASSIGN de-valor-ipi = (item-doc-est-cex.val-desp * item-doc-est.valor-ipi[1]) / item-doc-est.preco-total[1].
                    ELSE
                        ASSIGN de-valor-ipi = 0.

                    IF tt-param.i-custo-item = 1 then DO: /*Unitario*/
                        IF  AVAIL bf-natur-oper
                       &IF '{&bf_mat_versao_ems}' >= '2.08' &THEN
                        AND bf-natur-oper.log-icms-despes-import-nfr
                       &ELSE
                        AND SUBSTR(bf-natur-oper.char-2,146,1) = "1"
                       &ENDIF THEN DO:
                           assign tt-detalha-despesa.vl-des-rea = ((item-doc-est-cex.val-desp + de-valor-ipi) 
                                                                  / tt-detalha-item.quantidade).
                        END.
                        ELSE DO:
                            assign tt-detalha-despesa.vl-des-rea = ((item-doc-est-cex.val-desp - de-valor-icm + de-valor-ipi) 
                                                                    / tt-detalha-item.quantidade).
                        END.

                        IF tt-param.moeda <> var-moeda-conv-rea THEN DO:
                           RUN converte-moeda (INPUT-OUTPUT tt-detalha-despesa.vl-des-rea,
                                               INPUT var-moeda-conv-rea).
                        END.

                        IF  AVAIL bf-natur-oper
                        &IF '{&bf_mat_versao_ems}' >= '2.08' &THEN
                        AND bf-natur-oper.log-icms-despes-import-nfr
                        &ELSE
                        AND SUBSTR(bf-natur-oper.char-2,146,1) = "1"
                        &ENDIF THEN DO:
                            ASSIGN de-val-desp-nfc = item-doc-est-cex.val-desp + de-valor-ipi.
                        END.
                        ELSE DO:
                            ASSIGN de-val-desp-nfc = item-doc-est-cex.val-desp - de-valor-icm + de-valor-ipi.
                        END.
                        IF tt-param.moeda <> 0 THEN
                            RUN converte-moeda (INPUT-OUTPUT de-val-desp-nfc,
                                                INPUT 0).
                        ASSIGN de-tot-desp      = de-tot-desp + tt-detalha-despesa.vl-des-rea
                               de-despesa-total = de-despesa-total + de-val-desp-nfc.
                       
                        IF LAST (item-doc-est-cex.cod-desp) THEN DO:
                            IF (de-despesa-total / tt-detalha-item.quantidade) > de-tot-desp THEN
                                ASSIGN de-residuo = (de-despesa-total / tt-detalha-item.quantidade) - de-tot-desp
                                       tt-detalha-despesa.vl-des-rea = tt-detalha-despesa.vl-des-rea + de-residuo
                                       de-tot-desp = de-tot-desp + de-residuo.
                        END.

                    END.
                    ELSE DO:
                        IF  AVAIL bf-natur-oper
                        &IF '{&bf_mat_versao_ems}' >= '2.08' &THEN
                        AND bf-natur-oper.log-icms-despes-import-nfr
                        &ELSE
                        AND SUBSTR(bf-natur-oper.char-2,146,1) = "1"
                        &ENDIF THEN DO:
                            assign tt-detalha-despesa.vl-des-rea = item-doc-est-cex.val-desp + de-valor-ipi.
                        END.
                        ELSE DO:
                            assign tt-detalha-despesa.vl-des-rea = item-doc-est-cex.val-desp - de-valor-icm + de-valor-ipi.
                        END.
                        if  tt-param.moeda <> var-moeda-conv-rea THEN DO:
                            RUN converte-moeda (INPUT-OUTPUT tt-detalha-despesa.vl-des-rea,
                                                INPUT var-moeda-conv-rea).
                        END.
                        ASSIGN de-tot-desp  = de-tot-desp + tt-detalha-despesa.vl-des-rea.
                    END.
                END. /* tt-detalha-despesa */

                /*DESPESAS DA NF COMPLEMENTAR*/ 
                
                IF item-doc-est.cd-trib-icm = 1 AND item-doc-est.valor-icm[1] <> 0 THEN
                    ASSIGN de-valor-icm = (item-doc-est-cex.val-desp * item-doc-est.valor-icm[1]) / item-doc-est.preco-total[1].
                ELSE
                    ASSIGN de-valor-icm = 0.
                
                
                IF item-doc-est.cd-trib-ipi = 3 AND item-doc-est.valor-ipi[1] <> 0 THEN
                    ASSIGN de-valor-ipi = (item-doc-est-cex.val-desp * item-doc-est.valor-ipi[1]) / item-doc-est.preco-total[1].
                ELSE
                    ASSIGN de-valor-ipi = 0.

                /*reposiciona natureza de operacao na nota complementar*/
                IF  AVAIL bf-natur-oper
                   &IF '{&bf_mat_versao_ems}' >= '2.08' &THEN
                    AND bf-natur-oper.log-icms-despes-import-nfr
                   &ELSE
                    AND SUBSTR(bf-natur-oper.char-2,146,1) = "1"
                   &ENDIF THEN DO:
                   
                       ASSIGN de-val-desp-nfc = item-doc-est-cex.val-desp + de-valor-ipi.
                   END.
                ELSE DO:
                    ASSIGN de-val-desp-nfc = item-doc-est-cex.val-desp - de-valor-icm + de-valor-ipi.
                END.
                
                if  tt-param.moeda <> var-moeda-conv-rea THEN DO:
                    RUN converte-moeda (INPUT-OUTPUT de-val-desp-nfc,
                                        INPUT var-moeda-conv-rea).
                END.

                for first tt-detalha-despesa-aux
                     where tt-detalha-despesa-aux.embarque     = tt-detalha-despesa.embarque
                       and tt-detalha-despesa-aux.cod-estabel  = tt-detalha-despesa.cod-estabel
                       and tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp no-lock
                    use-index ch-codigo:
                end.

                if not avail tt-detalha-despesa-aux then do:
                   CREATE tt-detalha-despesa-aux.
                   ASSIGN tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                          tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                          tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp
                          tt-detalha-despesa-aux.desc-despesa = desp-imp.descricao
                          tt-detalha-despesa-aux.vl-des-rea   = de-val-desp-nfc.
                    
                END.
                ELSE DO:
                    
                    ASSIGN tt-detalha-despesa-aux.vl-des-rea = tt-detalha-despesa-aux.vl-des-rea + de-val-desp-nfc.
                END.
                
                ASSIGN de-val-desp-nfc2 = de-val-desp-nfc2 + de-val-desp-nfc.

                assign tt-detalha-item.vl-ipi-rea     = tt-detalha-item.vl-ipi-rea     + var-valor-ipi-mattran-rea
                       tt-detalha-item.vl-icms-rea    = tt-detalha-item.vl-icms-rea    + var-valor-icm-mattran-rea
                       tt-detalha-item.vl-pis-rea     = tt-detalha-item.vl-pis-rea     + var-valor-pis-mattran-rea
                       tt-detalha-item.vl-cofins-rea  = tt-detalha-item.vl-cofins-rea  + var-valor-cofins-mattran-rea
                       tt-detalha-item.vl-tot-imp-rea = tt-detalha-item.vl-tot-imp-rea + var-valor-ipi-mattran-rea + var-valor-icm-mattran-rea + var-valor-pis-mattran-rea + var-valor-cofins-mattran-rea .

                ASSIGN de-tot-imp-rea = tt-detalha-item.vl-tot-imp-des-rea.

                /*Re-calcular Percentuais*/
                assign tt-detalha-item.per-mercado-rea            = (tt-detalha-item.vl-mercado-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-ii-rea                 = (tt-detalha-item.vl-ii-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-ipi-rea                = (tt-detalha-item.vl-ipi-rea
                                                                  / tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-icms-rea               = (tt-detalha-item.vl-icms-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-pis-rea                = (tt-detalha-item.vl-pis-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-cofins-rea             = (tt-detalha-item.vl-cofins-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-tot-imp-rea            = (tt-detalha-item.vl-tot-imp-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-ii2-rea                = (tt-detalha-item.vl-ii2-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-tot-des-rea            = (tt-detalha-item.vl-tot-des-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-tot-imp-des-rea        = (tt-detalha-item.vl-tot-imp-des-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-tot-com-ipi-icms-rea   = (tt-detalha-item.vl-tot-com-ipi-icms-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                       tt-detalha-item.per-tot-sem-ipi-icms-rea   = (tt-detalha-item.vl-tot-sem-ipi-icms-rea
                                                                  /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100.

            end.  /*item-doc-est-cex*/
            
            IF tt-param.i-custo-item = 1  then do: /*Unitario*/
                assign tt-detalha-item.vl-des-rea                    = tt-detalha-item.vl-des-rea     
                                                                     + de-tot-desp
                       tt-detalha-item.vl-tot-despesa                = tt-detalha-item.vl-tot-despesa 
                                                                     + de-tot-desp
                       tt-detalha-item.vl-tot-des-rea                = tt-detalha-item.vl-tot-des-rea 
                                                                     + de-tot-desp 
                       tt-detalha-item.vl-des-rea-total              = tt-detalha-item.vl-des-rea-total
                                                                     + (de-tot-desp * tt-detalha-item.quantidade)
                       tt-detalha-item.vl-tot-imp-des-rea            = tt-detalha-item.vl-tot-imp-des-rea 
                                                                     + de-tot-desp
                       tt-detalha-item.vl-tot-des-rea-total          = tt-detalha-item.vl-tot-des-rea-total 
                                                                     + (de-tot-desp * tt-detalha-item.quantidade)
                       tt-detalha-item.vl-tot-imp-des-rea-total      = tt-detalha-item.vl-tot-imp-des-rea-total 
                                                                     + (de-tot-desp * tt-detalha-item.quantidade)
                       tt-detalha-item.vl-tot-com-ipi-icms-rea-total = tt-detalha-item.vl-tot-com-ipi-icms-rea-total
                                                                     + (de-tot-desp * tt-detalha-item.quantidade) 
                                                                     - de-tot-desp
                       tt-detalha-item.vl-tot-sem-ipi-icms-rea-total = tt-detalha-item.vl-tot-sem-ipi-icms-rea-total 
                                                                     + (de-tot-desp * tt-detalha-item.quantidade)
                                                                     - de-tot-desp
                       de-tot-desp = 0.
            end.
            else do:
                assign tt-detalha-item.vl-tot-des-rea                = tt-detalha-item.vl-tot-des-rea 
                                                                     + de-tot-desp
                       tt-detalha-item.vl-des-rea-total              = tt-detalha-item.vl-des-rea-total
                                                                     + (de-tot-desp)
                       tt-detalha-item.vl-tot-imp-des-rea            = de-tot-imp-rea 
                                                                     + IF tt-detalha-item.vl-des-rea = 0 THEN de-tot-desp ELSE tt-detalha-item.vl-des-rea
                       tt-detalha-item.vl-tot-des-rea-total          = tt-detalha-item.vl-tot-des-rea-total
                                                                     + (de-tot-desp)
                       tt-detalha-item.vl-tot-imp-des-rea-total      = tt-detalha-item.vl-tot-imp-des-rea-total  
                                                                     + (de-tot-desp)
                       tt-detalha-item.vl-tot-com-ipi-icms-rea-total = tt-detalha-item.vl-tot-com-ipi-icms-rea-total
                       tt-detalha-item.vl-tot-sem-ipi-icms-rea-total = tt-detalha-item.vl-tot-sem-ipi-icms-rea-total
                       tt-detalha-item.vl-des-rea                    = tt-detalha-item.vl-des-rea 
                                                                     + de-tot-desp
                       tt-detalha-item.vl-tot-despesa                = tt-detalha-item.vl-tot-despesa 
                                                                     + de-tot-desp.
                ASSIGN de-tot-imp-rea = 0
                       de-tot-desp    = 0.
                
            end.
        END. /*nota fiscal complementar*/

        /*Nota Principal e Relacionadas*/
        ELSE DO:
            /*aqui oráado*/
            IF  tt-param.l-desemb-parc and p-situacao <> 1 and
                trim(substring(embarque-imp.char-1,21,20)) <> "" then next.

            for first ordem-compra fields(numero-ordem  char-2  num-pedido  cod-emitente
                                           data-cotacao  preco-fornec preco-unit aliquota-ipi)
                where ordem-compra.numero-ordem = (&if "{&bf_mat_versao_ems}" >= "2.06" &then 
                                                     if docum-est.idi-nf-simples-remes = 1 then
                                                         item-doc-est.num-ord-import 
                                                   &else 
                                                     if int(substring(docum-est.char-2,101,1)) = 1 then 
                                                         int(substring(item-doc-est.char-2,145,8)) 
                                                   &endif 
                                                     else item-doc-est.numero-ordem)
                                                  no-lock:
            end.

            if  not avail ordem-compra then next.
            
            if  p-situacao = 1 then do: 
                &if "{&bf_mat_versao_ems}" >= "2.06" &then
                    if docum-est.idi-nf-simples-remes = 1 then do:
                        if first-of(item-doc-est.num-ord-import) then

                &else
                    if int(substring(docum-est.char-2,101,1)) = 1 then do:
                        if first-of(int(substring(item-doc-est.char-2,145,8))) then
                &endif
                        assign l-cria-embarque-corpo = yes.
                end.
                else if first-of(item-doc-est.numero-ordem) then
                    assign l-cria-embarque-corpo = yes.
                
                if l-cria-embarque-corpo then do : /*Nota Principal*/
                    create tt-embarque-corpo.
                    assign tt-embarque-corpo.r-docum-est   = ROWID(docum-est)  
                           tt-embarque-corpo.embarque      = embarque-imp.embarque
                           tt-embarque-corpo.cod-estabel   = embarque-imp.cod-estabel
                           tt-embarque-corpo.desc-estabel  = c-desc-estabel
                           tt-embarque-corpo.cod-emitente  = processo-imp.cod-exportador
                           tt-embarque-corpo.desc-emitente = c-desc-emitente
                           tt-embarque-corpo.nr-nota-fis   = docum-est.nro-docto
                           tt-embarque-corpo.dt-emissao    = docum-est.dt-emissao
                           tt-embarque-corpo.nr-di         = embarque-imp.declaracao-import
                           tt-embarque-corpo.dt-di         = embarque-imp.data-di
                           tt-embarque-corpo.numero-ordem  = (&if "{&bf_mat_versao_ems}" >= "2.06" &then 
                                                                if docum-est.idi-nf-simples-remes = 1 then
                                                                    item-doc-est.num-ord-import
                                                              &else
                                                                if int(substring(docum-est.char-2,101,1)) = 1 then 
                                                                    int(substring(item-doc-est.char-2,145,8))
                                                              &endif
                                                                else
                                                                    ordem-compra.numero-ordem)
                           tt-embarque-corpo.it-codigo     = item-doc-est.it-codigo
                           tt-embarque-corpo.desc-item     = item.desc-item.
                    assign p-situacao = 2.
                end.
            end.
            
            
            assign /*tot-despesa-orc = 0 */
                   tot-despesa-rea = 0.

            /* => DETALHA ITEM *******************************/
            &if "{&bf_mat_versao_ems}" >= "2.05" &then
                for first cotacao-item fields( numero-ordem  cod-emitente  mo-codigo  aliquota-ii char-1
                                               data-cotacao  cot-aprovada  it-codigo  seq-cotac aliquota-ipi)
                    where cotacao-item.numero-ordem = ordem-compra.numero-ordem
                      and cotacao-item.cod-emitente = ordem-compra.cod-emitente
                      and cotacao-item.data-cotacao = ordem-compra.data-cotacao
                      and cotacao-item.cot-aprovada = yes no-lock:
                end.
            &else
                for first cotacao-item fields( numero-ordem  cod-emitente  mo-codigo  char-1
                                               data-cotacao  cot-aprovada  it-codigo  seq-cotac aliquota-ipi)
                    where cotacao-item.numero-ordem = ordem-compra.numero-ordem
                      and cotacao-item.cod-emitente = ordem-compra.cod-emitente
                      and cotacao-item.data-cotacao = ordem-compra.data-cotacao
                      and cotacao-item.cot-aprovada = yes no-lock:
                end.
            &endif
            if  not avail cotacao-item then next.

            ASSIGN i-parcela = &if "{&bf_mat_versao_ems}" >= "2.06" &then 
                                    if docum-est.idi-nf-simples-remes = 1 then
                                        item-doc-est.num-parc-import
                               &else
                                    if int(substring(docum-est.char-2,101,1)) = 1 then 
                                        int(substring(item-doc-est.char-2,154,5)) 
                               &endif
                                    else item-doc-est.parcela.

            FOR FIRST b-ordens-embarque FIELDS(char-2 numero-ordem) WHERE
                      b-ordens-embarque.cod-estabel  = embarque-imp.cod-estabel  AND
                      b-ordens-embarque.embarque     = embarque-imp.embarque     AND
                      b-ordens-embarque.numero-ordem = (&if "{&bf_mat_versao_ems}" >= "2.06" &then 
                                                            if docum-est.idi-nf-simples-remes = 1 then
                                                                item-doc-est.num-ord-import
                                                        &else
                                                            if int(substring(docum-est.char-2,101,1)) = 1 then 
                                                                int(substring(item-doc-est.char-2,145,8)) 
                                                        &endif
                                                            else item-doc-est.numero-ordem)
                    and b-ordens-embarque.parcela    = i-parcela      NO-LOCK:
            END.
            
            ASSIGN de-preco-unit-sem-ipi = (ordem-compra.preco-unit * 100) / (100 + ordem-compra.aliquota-ipi).
            create tt-detalha-item.
            assign tt-detalha-item.embarque                   = embarque-imp.embarque
                   tt-detalha-item.cod-estabel                = embarque-imp.cod-estabel  
                   tt-detalha-item.embarque-orig              = pc-embarque
                   tt-detalha-item.nro-docto                  = item-doc-est.nro-docto
                   tt-detalha-item.sequencia                  = item-doc-est.sequencia
                   tt-detalha-item.it-codigo                  = ITEM.it-codigo
                   tt-detalha-item.descricao                  = ITEM.desc-item
                   tt-detalha-item.numero-ordem               = ordem-compra.numero-ordem
                   tt-detalha-item.parcela                    = i-parcela
                   tt-detalha-item.quantidade                 = item-doc-est.quantidade
                   tt-detalha-item.un                         = ITEM.un
                   tt-detalha-item.vl-mercado-orc             = de-preco-unit-sem-ipi
                   tt-detalha-item.aliquota-ipi               = item-doc-est.aliquota-ipi
                   tt-detalha-item.aliquota-icms              = item-doc-est.aliquota-icm
                   tt-detalha-item.aliquota-pis               = &IF "{&bf_mat_versao_ems}":U >= "2.06":U &THEN item-doc-est.val-aliq-pis
                                                                &ELSE DEC(SUBSTRING(item-doc-est.char-2,022,05)) &endif
                   tt-detalha-item.aliquota-cofins            = &IF "{&bf_mat_versao_ems}":U >= "2.06":U &THEN item-doc-est.val-aliq-cofins
                                                                &ELSE DEC(SUBSTRING(item-doc-est.char-2,084,05)) &endif                                             
                   tt-detalha-item.situacao                   = p-situacao.


            FIND FIRST bf-tt-detalha-item 
                 WHERE bf-tt-detalha-item.it-codigo = tt-detalha-item.it-codigo NO-ERROR.
            IF  AVAIL bf-tt-detalha-item THEN
                ASSIGN bf-tt-detalha-item.i-numero-itens = bf-tt-detalha-item.i-numero-itens + 1
                       bf-tt-detalha-item.tot-quantidade = bf-tt-detalha-item.tot-quantidade + tt-detalha-item.quantidade.

            IF AVAIL b-ordens-embarque THEN
               ASSIGN &if "{&bf_mat_versao_ems}" >= "2.05" &then
                         tt-detalha-item.aliquota-ii      = IF  TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6)) = " " THEN cotacao-item.aliquota-ii
                                                            ELSE DEC(TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6)))
                         tt-detalha-item.aliquota-ii2     = IF  TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6)) = " " THEN cotacao-item.aliquota-ii
                                                            ELSE DEC(TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6))).

                      &else
                          tt-detalha-item.aliquota-ii     = IF  TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6)) = " " THEN DEC(TRIM(SUBSTRING(cotacao-item.char-1,61,20)))
                                                            ELSE DEC(TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6)))
                          tt-detalha-item.aliquota-ii2    = IF  TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6)) = " " THEN DEC(TRIM(SUBSTRING(cotacao-item.char-1,61,20)))
                                                            ELSE DEC(TRIM(SUBSTRING(b-ordens-embarque.char-2,1,6))).

                      &endif
            ELSE
                ASSIGN &if "{&bf_mat_versao_ems}" >= "2.05" &then
                          tt-detalha-item.aliquota-ii     = cotacao-item.aliquota-ii
                          tt-detalha-item.aliquota-ii2    = cotacao-item.aliquota-ii.
                       &else
                           tt-detalha-item.aliquota-ii    = DEC(TRIM(SUBSTRING(cotacao-item.char-1,61,20)))
                           tt-detalha-item.aliquota-ii2   = DEC(TRIM(SUBSTRING(cotacao-item.char-1,61,20))).
                       &endif
            
            if  tt-param.i-custo-item = 2 then /*Total*/
                assign tt-detalha-item.vl-mercado-orc = de-preco-unit-sem-ipi
                                                      * item-doc-est.quantidade.
            ASSIGN de-preco-unit-sem-ipi = 0.
            /*Convers∆o*/
            
            if  tt-param.moeda <> cotacao-item.mo-codigo THEN DO:
                run converte-moeda (input-output tt-detalha-item.vl-mercado-orc,
                                    input cotacao-item.mo-codigo).
                
            END.

            assign vl-ii-orcad-det  = 0
                   vl-ii2-orcad-det = 0
                   no-despesa       = no
                   var-moeda-conv-orc = tt-param.moeda
                   tt-detalha-item.vl-mercado-orc-total = if tt-param.i-custo-item = 2 then tt-detalha-item.vl-mercado-orc
                                                          else (tt-detalha-item.vl-mercado-orc * item-doc-est.quantidade).

            &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
            ASSIGN vl-ipi-orcad-det    = 0
                   vl-icms-orcad-det   = 0
                   vl-pis-orcad-det    = 0
                   vl-cofins-orcad-det = 0.
            &ENDIF
            
             /****ORÄADO-INI*/
            for each  cotacao-item-cex fields( numero-ordem  cod-emitente  mapa-cotacao
                                               cod-incoterm  it-codigo     seq-cotac
                                               data-cotacao  cod-itiner    cod-desp
                                               val-desp      mo-codigo)
                where cotacao-item-cex.numero-ordem = cotacao-item.numero-ordem
                  and cotacao-item-cex.cod-emitente = cotacao-item.cod-emitente
                  and cotacao-item-cex.it-codigo    = cotacao-item.it-codigo
                  and cotacao-item-cex.seq-cotac    = cotacao-item.seq-cotac
                  and cotacao-item-cex.data-cotacao = cotacao-item.data-cotacao
                  and cotacao-item-cex.mapa-cotacao = int(trim(substring(cotacao-item.char-1,1,20)))
                  and cotacao-item-cex.cod-incoterm = embarque-imp.cod-incoterm
                  and cotacao-item-cex.cod-itiner   = itinerario.cod-itiner no-lock
                use-index codigo:

                assign var-moeda-conv-orc = cotacao-item-cex.mo-codigo.

                /* II e II2 - ORCADO */
                if  cotacao-item-cex.cod-desp = i-cod-desp-ii and
                    tt-param.i-desp-imposto = 1 then /*Despesa*/
                    assign vl-ii2-orcad-det = cotacao-item-cex.val-desp.

                if  cotacao-item-cex.cod-desp = i-cod-desp-ii and
                    tt-param.i-desp-imposto = 2 then /*Imposto*/ 
                    assign vl-ii-orcad-det  = cotacao-item-cex.val-desp.

                &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN
                    IF  AVAIL param-imp THEN DO:
                        IF  param-imp.cdn-despes-impto-ipi = cotacao-item-cex.cod-desp THEN DO:
                            ASSIGN vl-ipi-orcad-det = cotacao-item-cex.val-desp
                                   no-despesa       = YES.
                        END.

                        IF  param-imp.cdn-despes-impto-icms = cotacao-item-cex.cod-desp THEN DO:
                            ASSIGN vl-icms-orcad-det = cotacao-item-cex.val-desp
                                   no-despesa        = YES.
                        END.
                        IF  param-imp.cdn-despes-impto-pis = cotacao-item-cex.cod-desp THEN DO:
                            ASSIGN vl-pis-orcad-det = cotacao-item-cex.val-desp
                                   no-despesa        = YES.
                        END.
                        IF  param-imp.cdn-despes-impto-cofins = cotacao-item-cex.cod-desp THEN DO:
                            ASSIGN vl-cofins-orcad-det = cotacao-item-cex.val-desp
                                   no-despesa        = YES.
                        END.
                    END.                                   

                &ELSE
                    /* IPI - ORCADO */
                    for first desp-imp fields( cod-desp  descricao )
                        where desp-imp.cod-desp = cotacao-item-cex.cod-desp
                          and substring(desp-imp.descricao,1,3) = "IPI":U no-lock:
                    end.
                    if  avail desp-imp then
                        assign vl-ipi-orcad-det = cotacao-item-cex.val-desp
                               no-despesa       = YES.
                    else 
                        assign vl-ipi-orcad-det = 0.
                    
                    /* ICMS - ORCADO */
                    for first desp-imp fields( cod-desp  descricao )
                        where desp-imp.cod-desp = cotacao-item-cex.cod-desp
                          and substring(desp-imp.descricao,1,4) = "ICMS":U no-lock:
                    end.
                    if  avail desp-imp then
                        assign vl-icms-orcad-det = cotacao-item-cex.val-desp
                               no-despesa        = YES.
                    else 
                        assign vl-icms-orcad-det = 0.

                    /* PIS - ORCADO */
                    for first desp-imp fields( cod-desp  descricao )
                        where desp-imp.cod-desp = cotacao-item-cex.cod-desp
                          and substring(desp-imp.descricao,1,3) = "PIS":U no-lock:
                    end.
                    if  avail desp-imp THEN
                        assign vl-pis-orcad-det = cotacao-item-cex.val-desp
                               no-despesa        = YES.
                        
                    for first desp-imp fields( cod-desp  descricao )
                        where desp-imp.cod-desp = cotacao-item-cex.cod-desp
                          and substring(desp-imp.descricao,1,6) = "COFINS":U no-lock:
                    end.
                    if  avail desp-imp then
                        assign vl-cofins-orcad-det = cotacao-item-cex.val-desp
                               no-despesa        = YES.
                &ENDIF
                
                /* Despesas - ORCADO */
                for first desp-imp fields ( cod-desp  tipo  descricao )
                    where desp-imp.cod-desp = cotacao-item-cex.cod-desp no-lock:
                end.
                
                if  not avail desp-imp then 
                    next.

                if  tt-param.i-custo-item = 2 and desp-imp.tipo <> 1 then /*Total e Diferente de Fixo*/
                    assign vl-desp-orcad-det = cotacao-item-cex.val-desp
                                             * item-doc-est.quantidade.
                else
                    assign vl-desp-orcad-det = cotacao-item-cex.val-desp.



                if desp-imp.tipo <> 1 then /*Total e Diferente de Fixo*/
                   ASSIGN vl-desp-orcad-aux = cotacao-item-cex.val-desp * 
                                              item-doc-est.quantidade.
                ELSE
                    ASSIGN vl-desp-orcad-aux = cotacao-item-cex.val-desp.

                IF  desp-imp.cod-desp <> i-cod-desp-ii 
                &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi                     
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms                    
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins
                &ENDIF
                AND NOT no-despesa THEN DO:
                    find FIRST tt-detalha-despesa
                         WHERE tt-detalha-despesa.embarque    = embarque-imp.embarque
                           AND tt-detalha-despesa.cod-estabel = embarque-imp.cod-estabel
                           AND tt-detalha-despesa.cod-despesa = desp-imp.cod-desp no-lock no-error.
                    if not avail tt-detalha-despesa then do:
                        create tt-detalha-despesa.
                        assign tt-detalha-despesa.embarque      = embarque-imp.embarque
                               tt-detalha-despesa.cod-estabel   = embarque-imp.cod-estabel
                               tt-detalha-despesa.numero-ordem  = cotacao-item-cex.numero-ordem
                               tt-detalha-despesa.nro-docto     = item-doc-est.nro-docto
                               tt-detalha-despesa.cod-despesa   = desp-imp.cod-desp
                               tt-detalha-despesa.desc-despesa  = desp-imp.descricao
                               tt-detalha-despesa.situacao      = p-situacao
                               tt-detalha-despesa.vl-des-orc    = vl-desp-orcad-det
                               tot-despesa-orc                  = tot-despesa-orc
                                                                + vl-desp-orcad-det
                               tt-detalha-despesa.it-codigo     = cotacao-item.it-codigo
                               tt-detalha-despesa.sequencia     = item-doc-est.sequencia. 
    
                        if  tt-param.moeda <> var-moeda-conv-orc THEN
                            IF AVAIL tt-detalha-despesa THEN
                                run converte-moeda (input-output tt-detalha-despesa.vl-des-orc,
                                                    input var-moeda-conv-orc).
                            IF desp-imp.tipo = 1 AND tt-param.i-custo-item = 1 THEN
                                ASSIGN tt-detalha-despesa.vl-des-orc = tt-detalha-despesa.vl-des-orc / item-doc-est.quantidade.

                        ASSIGN de-tot-des-orc-aux = de-tot-des-orc-aux + tt-detalha-despesa.vl-des-orc.
                    END. 
                    ELSE DO:
                        ASSIGN de-tot-des-orc-aux = de-tot-des-orc-aux + tt-detalha-despesa.vl-des-orc. 
                    END.
                END.
                
                IF  desp-imp.cod-desp <> i-cod-desp-ii 
                &IF "{&bf_mat_versao_ems}" >= "2.062" &THEN
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins
                &ENDIF
                THEN DO:
                    
                    IF tt-param.moeda <> var-moeda-conv-orc THEN DO:
                        RUN converte-moeda (INPUT-OUTPUT vl-desp-orcad-aux,
                                            INPUT var-moeda-conv-orc).
                    END.
                    for first tt-detalha-despesa-aux
                         where tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                           and tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                           and tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp 
                           AND tt-detalha-despesa-aux.sequencia    = item-doc-est.sequencia no-lock /**/
                        use-index ch-codigo:
                    end.
                    if not avail tt-detalha-despesa-aux then do:
                        CREATE tt-detalha-despesa-aux.
                        ASSIGN tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                               tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                               tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp
                               tt-detalha-despesa-aux.desc-despesa = desp-imp.descricao
                               tt-detalha-despesa-aux.vl-des-orc   = vl-desp-orcad-aux
                               tt-detalha-despesa-aux.sequencia    = item-doc-est.sequencia.
                                                
                        /*acumula valor total das despesas oráadas convertidas ou nao*/
                        ASSIGN de-tot-desp-aux = de-tot-desp-aux + tt-detalha-despesa-aux.vl-des-orc.
                    END.
                    ELSE DO: /*Soma as despesas dos Desembarques parciais*/
                        ASSIGN tt-detalha-despesa-aux.vl-des-orc = tt-detalha-despesa-aux.vl-des-orc + vl-desp-orcad-aux.
                        ASSIGN de-tot-desp-aux = de-tot-desp-aux + tt-detalha-despesa-aux.vl-des-orc.
                    END.
                END.
                /*Total do Embarque*/
                if  tt-param.i-custo-item = 1 then do:
                    if  desp-imp.tipo = 1 then
                        assign tt-detalha-item.vl-des-orc-total     = tt-detalha-item.vl-des-orc-total
                                                                    + vl-desp-orcad-det.
                    else do:
                        if  not no-despesa then
                            assign tt-detalha-item.vl-des-orc-total = tt-detalha-item.vl-des-orc-total
                                                                    + (vl-desp-orcad-det * item-doc-est.quantidade).
                    end.
                end.
                else 
                    assign tt-detalha-item.vl-des-orc-total = tot-despesa-orc.

                /***********************/

                assign tt-detalha-item.vl-tot-des-orc-total = tt-detalha-item.vl-des-orc-total.
                
                IF tt-param.i-desp-imposto = 1 THEN
                    ASSIGN vl-ii2-orcad-det-aux = vl-ii2-orcad-det * tt-detalha-item.quantidade.
                ELSE
                    ASSIGN vl-ii-orcad-det-aux  = vl-ii-orcad-det * tt-detalha-item.quantidade.

            end. /*cotacao-item-cex*/

            assign tt-detalha-item.vl-ii-orc                  = IF tt-param.i-custo-item = 2 THEN vl-ii-orcad-det * tt-detalha-item.quantidade
                                                                ELSE vl-ii-orcad-det
                   &IF  "{&bf_mat_versao_ems}" < "2.062" &THEN
                   tt-detalha-item.vl-ipi-orc                 = vl-ipi-orcad-det
                   tt-detalha-item.vl-icms-orc                = vl-icms-orcad-det
                   tt-detalha-item.vl-pis-orc                 = vl-pis-orcad-det
                   tt-detalha-item.vl-cofins-orc              = vl-cofins-orcad-det
                   tt-detalha-item.vl-tot-imp-orc             = tt-detalha-item.vl-ii-orc /*Total Imposto  II + IPI + ICMS*/  
                                                              + vl-ipi-orcad-det
                                                              + vl-icms-orcad-det
                                                              + vl-pis-orcad-det
                                                              + vl-cofins-orcad-det
                   &ELSE
                   tt-detalha-item.vl-ipi-orc                 = IF tt-param.i-custo-item = 2 THEN vl-ipi-orcad-det * tt-detalha-item.quantidade
                                                                ELSE vl-ipi-orcad-det
                   tt-detalha-item.vl-icms-orc                = IF tt-param.i-custo-item = 2 THEN vl-icms-orcad-det * tt-detalha-item.quantidade
                                                                ELSE vl-icms-orcad-det
                   tt-detalha-item.vl-pis-orc                = IF tt-param.i-custo-item = 2 THEN vl-pis-orcad-det * tt-detalha-item.quantidade
                                                                ELSE vl-pis-orcad-det
                   tt-detalha-item.vl-cofins-orc             = IF tt-param.i-custo-item = 2 THEN vl-cofins-orcad-det * tt-detalha-item.quantidade
                                                                ELSE vl-cofins-orcad-det
                   tt-detalha-item.vl-tot-imp-orc             = tt-detalha-item.vl-ii-orc /*Total Imposto  II + IPI + ICMS*/  
                                                                + tt-detalha-item.vl-ipi-orc
                                                                + tt-detalha-item.vl-icms-orc
                                                                + tt-detalha-item.vl-pis-orc
                                                                + tt-detalha-item.vl-cofins-orc
                   &ENDIF
                   tt-detalha-item.vl-ii2-orc                 = IF tt-param.i-custo-item = 2 THEN vl-ii2-orcad-det * tt-detalha-item.quantidade 
                                                                ELSE vl-ii2-orcad-det /*II2*/
                   tt-detalha-item.vl-ii2-orc-aux             = vl-ii2-orcad-det-aux
                   tt-detalha-item.vl-ii-orc-aux              = vl-ii-orcad-det-aux
                   tt-detalha-item.vl-des-orc                 = de-tot-des-orc-aux /*tot-despesa-orc*/ /*Despesas*/
                   
                   tt-detalha-item.vl-tot-com-ipi-icms-orc    = tt-detalha-item.vl-mercado-orc  /*Valor Mercado + (Total Imposto + Total Despesa)*/
                                                              + tt-detalha-item.vl-tot-imp-des-orc
                   tt-detalha-item.vl-tot-sem-ipi-icms-orc    = tt-detalha-item.vl-tot-com-ipi-icms-orc /*Valor Mercado + Imposto + Despesa - IPI - ICMS*/
                                                              - vl-ipi-orcad-det
                                                              - vl-icms-orcad-det
                                                              - vl-pis-orcad-det
                                                              - vl-cofins-orcad-det
                   tt-detalha-item.vl-tot-imp-des-orc-total   = tt-detalha-item.vl-tot-imp-orc
                                                              + tt-detalha-item.vl-tot-des-orc-total.


            /****ORÄADO-FIM*/
            assign vl-ii-reali-det  = 0
                   vl-ii2-reali-det = 0
                   de-val-desp-aux  = 0
                   vl-pis-orcad-det = 0
                   vl-cofins-orcad-det = 0
                   de-despesa-total    = 0. 
            /****REALIZADO-INI*/

            for each  item-doc-est-cex fields( serie-docto  nro-docto  cod-emitente  sequencia
                                               nat-operacao  cod-desp  val-desp  mo-codigo )
                where item-doc-est-cex.serie-docto  = item-doc-est.serie-docto
                  and item-doc-est-cex.nro-docto    = item-doc-est.nro-docto
                  and item-doc-est-cex.cod-emitente = item-doc-est.cod-emitente
                  and item-doc-est-cex.nat-operacao = item-doc-est.nat-operacao
                  and item-doc-est-cex.sequencia    = item-doc-est.sequencia no-lock
                use-index codigo BREAK BY item-doc-est-cex.cod-desp:
           
                /* II e II2 - REALIZADO */
                if  item-doc-est-cex.cod-desp = i-cod-desp-ii and
                    tt-param.i-desp-imposto = 1 then /*Despesa*/
                    assign vl-ii2-reali-det  = item-doc-est-cex.val-desp.
                if  item-doc-est-cex.cod-desp = i-cod-desp-ii and
                    tt-param.i-desp-imposto = 2 then /*Imposto*/
                    assign vl-ii-reali-det = item-doc-est-cex.val-desp.

                for first desp-imp fields( cod-desp  inc-val-frete  inc-val-embal  tipo
                                           inc-val-seguro  inc-val-outras-desp  descricao )
                    where desp-imp.cod-desp = item-doc-est-cex.cod-desp no-lock:
                end.
                if  not avail desp-imp then next.

                &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN
                    IF  param-imp.cdn-despes-impto-ipi    = item-doc-est-cex.cod-desp 
                    OR  param-imp.cdn-despes-impto-icms   = item-doc-est-cex.cod-desp 
                    OR  param-imp.cdn-despes-impto-pis    = item-doc-est-cex.cod-desp 
                    OR  param-imp.cdn-despes-impto-cofins = item-doc-est-cex.cod-desp     THEN
                        NEXT.
                &ELSE
                    IF  SUBSTRING(desp-imp.descricao,1,3) = "IPI"
                    OR  SUBSTRING(desp-imp.descricao,1,4) = "ICMS"
                    OR  SUBSTRING(desp-imp.descricao,1,3) = "PIS"    
                    OR  SUBSTRING(desp-imp.descricao,1,6) = "COFINS" THEN 
                        NEXT.
                &ENDIF
    
                /* Outras Despesas */
                if  desp-imp.inc-val-frete then
                    assign vl-despesa-frete = item-doc-est-cex.val-desp.
                if  desp-imp.inc-val-embal then
                    assign vl-despesa-embalagem = item-doc-est-cex.val-desp.
                if  desp-imp.inc-val-seguro then
                    assign vl-despesa-seguro = item-doc-est-cex.val-desp.
                if  desp-imp.inc-val-outras-desp then
                    assign vl-despesa-outras  = item-doc-est-cex.val-desp.

                if  desp-imp.cod-desp <> i-cod-desp-ii 
                &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi                     
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins 
                &ENDIF
                then do:
                    find first tt-detalha-despesa
                         where tt-detalha-despesa.embarque     = embarque-imp.embarque
                           and tt-detalha-despesa.cod-estabel  = embarque-imp.cod-estabel
                           and tt-detalha-despesa.numero-ordem = ordem-compra.numero-ordem
                           and tt-detalha-despesa.cod-despesa  = desp-imp.cod-desp 
                           AND tt-detalha-despesa.sequencia    = item-doc-est.sequencia no-lock no-error.
                    if not avail tt-detalha-despesa then do:
                        create tt-detalha-despesa.
                        assign tt-detalha-despesa.embarque     = embarque-imp.embarque
                               tt-detalha-despesa.cod-estabel  = embarque-imp.cod-estabel
                               tt-detalha-despesa.numero-ordem = ordem-compra.numero-ordem
                               tt-detalha-despesa.nro-docto    = item-doc-est.nro-docto
                               tt-detalha-despesa.cod-despesa  = desp-imp.cod-desp
                               tt-detalha-despesa.desc-despesa = desp-imp.descricao
                               tt-detalha-despesa.situacao     = p-situacao 
                               tt-detalha-despesa.it-codigo    = item-doc-est.it-codigo
                               tt-detalha-despesa.sequencia    = item-doc-est.sequencia.
                        
                        if  tt-param.i-custo-item = 1 /*Unitario*/ THEN DO:
                            assign tt-detalha-despesa.vl-des-rea = item-doc-est-cex.val-desp / item-doc-est.quantidade
                                   tot-despesa-rea               = tot-despesa-rea + 
                                                                   (item-doc-est-cex.val-desp / item-doc-est.quantidade)
                                   de-despesa-total              = de-despesa-total + item-doc-est-cex.val-desp.
                            
                            IF LAST (item-doc-est-cex.cod-desp) THEN DO:
                               
                                IF (de-despesa-total / tt-detalha-item.quantidade) > tot-despesa-rea THEN
                                    ASSIGN de-residuo = (de-despesa-total / tt-detalha-item.quantidade) - tot-despesa-rea
                                           tt-detalha-despesa.vl-des-rea = tt-detalha-despesa.vl-des-rea + de-residuo
                                           tot-despesa-rea = tot-despesa-rea + de-residuo.
                            END.
    
                        END.
                        ELSE 
                            assign tt-detalha-despesa.sequencia   = item-doc-est.sequencia
                                   tt-detalha-despesa.vl-des-rea  = item-doc-est-cex.val-desp 
                                   tot-despesa-rea                = tot-despesa-rea
                                                                   + item-doc-est-cex.val-desp.

                    end.

                    /*Moeda Converte Realizado*/
                    assign var-moeda-conv-rea = item-doc-est-cex.mo-codigo.
                    if  tt-param.moeda <> var-moeda-conv-rea THEN
                        run converte-moeda (input-output tt-detalha-despesa.vl-des-rea,
                                            input var-moeda-conv-rea).
                end.

                if not avail tt-detalha-despesa then do:
                    find first tt-detalha-despesa
                         where tt-detalha-despesa.embarque     = embarque-imp.embarque
                           and tt-detalha-despesa.cod-estabel  = embarque-imp.cod-estabel
                           and tt-detalha-despesa.numero-ordem = ordem-compra.numero-ordem
                           and tt-detalha-despesa.nro-docto    = item-doc-est.nro-docto
                           and tt-detalha-despesa.cod-despesa  = desp-imp.cod-desp no-lock no-error.
        
                    if  not avail tt-detalha-despesa then do:
                        create tt-detalha-despesa.
                        assign tt-detalha-despesa.embarque     = embarque-imp.embarque
                               tt-detalha-despesa.cod-estabel  = embarque-imp.cod-estabel
                               tt-detalha-despesa.numero-ordem = ordem-compra.numero-ordem
                               tt-detalha-despesa.nro-docto    = item-doc-est.nro-docto
                               tt-detalha-despesa.cod-despesa  = desp-imp.cod-desp
                               tt-detalha-despesa.desc-despesa = desp-imp.descricao
                               tt-detalha-despesa.situacao     = p-situacao.
                               tt-detalha-despesa.sequencia    = item-doc-est.sequencia.
                    end.
                end.
                
                assign var-moeda-conv-rea = item-doc-est-cex.mo-codigo.

                /*Total do Embarque*/
                if  desp-imp.cod-desp <> i-cod-desp-ii 
                &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi                 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins 
                &ENDIF    
                then 
                    assign tt-detalha-item.vl-des-rea-total = tt-detalha-item.vl-des-rea-total
                                                            + item-doc-est-cex.val-desp.

                /*Sem NF complementar - Realizado*/
                IF  desp-imp.cod-desp <> i-cod-desp-ii 
                &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-ipi                     
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-icms 
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-pis
                AND desp-imp.cod-desp <> param-imp.cdn-despes-impto-cofins 
                &ENDIF
                THEN DO:
                    ASSIGN de-val-desp-aux = item-doc-est-cex.val-desp.
                    
                    IF tt-param.moeda <> var-moeda-conv-rea THEN DO:
                        RUN converte-moeda (INPUT-OUTPUT de-val-desp-aux,
                                            INPUT var-moeda-conv-rea).
                    END.

                    for first tt-detalha-despesa-aux
                         where tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                           and tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                           and tt-detalha-despesa-aux.cod-despesa  = item-doc-est-cex.cod-desp no-lock
                        use-index ch-codigo:
                    end.
                    if not avail tt-detalha-despesa-aux then do:
                        
                        CREATE tt-detalha-despesa-aux.
                        ASSIGN tt-detalha-despesa-aux.embarque     = embarque-imp.embarque
                               tt-detalha-despesa-aux.cod-estabel  = embarque-imp.cod-estabel
                               tt-detalha-despesa-aux.cod-despesa  = desp-imp.cod-desp
                               tt-detalha-despesa-aux.desc-despesa = desp-imp.descricao
                               tt-detalha-despesa-aux.vl-des-rea   = de-val-desp-aux
                               tt-detalha-despesa-aux.sequencia    = item-doc-est-cex.sequencia.
                    END.
                    ELSE DO:
                        ASSIGN tt-detalha-despesa-aux.vl-des-rea = tt-detalha-despesa-aux.vl-des-rea + de-val-desp-aux.
                    END.
                    ASSIGN de-val-desp-aux-rea = de-val-desp-aux-rea + de-val-desp-aux. 
                END.
            end.
            
            assign var-preco-item-rea    = item-doc-est.preco-unit[1] * item-doc-est.qt-do-forn
                   var-valor-ipi-rea     = item-doc-est.valor-ipi[1]
                   var-valor-icm-rea     = item-doc-est.valor-icm[1]
                   var-valor-pis-rea     = &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN 
                                               item-doc-est.valor-pis 
                                           &ELSE
                                               DEC(SUBSTRING(item-doc-est.CHAR-2,41,14)) &ENDIF
                   var-valor-cofins-rea  = &IF "{&BF_MAT_VERSAO_EMS}" >= "2.062" &THEN
                                               item-doc-est.val-cofins 
                                           &ELSE
                                               DEC(SUBSTRING(item-doc-est.CHAR-2,103,14)) &ENDIF.

            if  tt-param.i-custo-item = 1 then do: /*Unitario*/


                assign vl-ii2-reali-det     = vl-ii2-reali-det     / item-doc-est.quantidade
                       vl-ii-reali-det      = vl-ii-reali-det      / item-doc-est.quantidade
                       var-valor-ipi-rea    = var-valor-ipi-rea    / item-doc-est.quantidade
                       var-valor-icm-rea    = var-valor-icm-rea    / item-doc-est.quantidade
                       var-valor-pis-rea    = var-valor-pis-rea    / item-doc-est.quantidade
                       var-valor-cofins-rea = var-valor-cofins-rea / item-doc-est.quantidade
                       var-preco-item-rea   = var-preco-item-rea   / item-doc-est.quantidade.
            END.
            
            /*tt-detalha-item REALIZADO - NOTA PRINCIPAL*/
            assign tt-detalha-item.vl-mercado-rea           = var-preco-item-rea /*Valor Merado Realizado*/
                   tt-detalha-item.vl-ii-rea                = vl-ii-reali-det
                   tt-detalha-item.vl-ipi-rea               = var-valor-ipi-rea
                   tt-detalha-item.vl-icms-rea              = var-valor-icm-rea
                   tt-detalha-item.vl-pis-rea               = var-valor-pis-rea
                   tt-detalha-item.vl-cofins-rea            = var-valor-cofins-rea
                   tt-detalha-item.vl-tot-imp-rea           = vl-ii-reali-det /*Total Imposto  II + IPI + ICMS*/
                                                            + var-valor-ipi-rea
                                                            + var-valor-icm-rea
                                                            + var-valor-pis-rea
                                                            + var-valor-cofins-rea
                   tt-detalha-item.vl-ii2-rea               = vl-ii2-reali-det
                   tt-detalha-item.vl-des-rea               = tot-despesa-rea
                   tt-detalha-item.vl-tot-des-rea           = vl-ii2-reali-det
                                                            + tot-despesa-rea
                   tt-detalha-item.vl-tot-imp-des-rea       = tt-detalha-item.vl-tot-imp-rea
                                                            + tt-detalha-item.vl-tot-des-rea    
                   tt-detalha-item.vl-tot-com-ipi-icms-rea  = tt-detalha-item.vl-mercado-rea
                                                            + tt-detalha-item.vl-tot-imp-des-rea
                   tt-detalha-item.vl-tot-sem-ipi-icms-rea  = tt-detalha-item.vl-tot-com-ipi-icms-rea
                                                            - var-valor-ipi-rea
                                                            - var-valor-icm-rea
                                                            - var-valor-pis-rea    
                                                            - var-valor-cofins-rea
                   tt-detalha-item.vl-mercado-rea-total     = if tt-param.i-custo-item = 2 then var-preco-item-rea
                                                              else var-preco-item-rea * item-doc-est.quantidade
                   tt-detalha-item.vl-ii-rea-total          = if tt-param.i-custo-item = 2 then vl-ii-reali-det
                                                              else vl-ii-reali-det * item-doc-est.quantidade
                   tt-detalha-item.vl-ii2-rea-total         = if tt-param.i-custo-item = 2 then vl-ii2-reali-det
                                                              else vl-ii2-reali-det * item-doc-est.quantidade
                   tt-detalha-item.vl-ipi-rea-total         = if tt-param.i-custo-item = 2 then var-valor-ipi-rea
                                                              else var-valor-ipi-rea * item-doc-est.quantidade
                   tt-detalha-item.vl-icms-rea-total        = if tt-param.i-custo-item = 2 then var-valor-icm-rea
                                                              else var-valor-icm-rea * item-doc-est.quantidade
                   tt-detalha-item.vl-pis-rea-total         = if tt-param.i-custo-item = 2 then var-valor-pis-rea
                                                              else var-valor-pis-rea * item-doc-est.quantidade
                   tt-detalha-item.vl-cofins-rea-total      = if tt-param.i-custo-item = 2 then var-valor-cofins-rea
                                                              else var-valor-cofins-rea * item-doc-est.quantidade
                   tt-detalha-item.vl-tot-imp-rea-total     = if tt-param.i-custo-item = 2 then tt-detalha-item.vl-tot-imp-rea
                                                              else tt-detalha-item.vl-tot-imp-rea * item-doc-est.quantidade
                   tt-detalha-item.vl-tot-des-rea-total     = if tt-param.i-custo-item = 1 then tt-detalha-item.vl-des-rea-total + tt-detalha-item.vl-ii2-rea-total
                                                              else tt-detalha-item.vl-tot-des-rea
                   tt-detalha-item.vl-tot-imp-des-rea-total = if tt-param.i-custo-item = 1 then tt-detalha-item.vl-tot-imp-rea-total + tt-detalha-item.vl-tot-des-rea-total
                                                              else tt-detalha-item.vl-tot-imp-des-rea
                   tt-detalha-item.vl-tot-com-ipi-icms-rea-total = tt-detalha-item.vl-mercado-rea-total
                                                                 + tt-detalha-item.vl-tot-imp-des-rea-total 
                   tt-detalha-item.vl-tot-sem-ipi-icms-rea-total = tt-detalha-item.vl-tot-com-ipi-icms-rea-total
                                                                 - tt-detalha-item.vl-ipi-rea-total
                                                                 - tt-detalha-item.vl-icms-rea-total
                                                                 - tt-detalha-item.vl-pis-rea-total
                                                                 - tt-detalha-item.vl-cofins-rea-total.

            /****REALIZADO-FIM*/
            
            /*Convers∆o Oráado*/
            
            IF  tt-param.moeda <> var-moeda-conv-orc THEN DO:
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ipi-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-icms-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-pis-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-cofins-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-imp-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii2-orc,
                                    input var-moeda-conv-orc).
                run converte-moeda (input-output tt-detalha-item.vl-des-orc,
                                    input var-moeda-conv-orc).
                run converte-moeda (input-output tt-detalha-item.vl-des-orc-total,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-des-orc-total,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-imp-des-orc-total,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-com-ipi-icms-orc,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-sem-ipi-icms-orc-total,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii2-orc-aux,
                                    input var-moeda-conv-orc).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii-orc-aux,
                                    input var-moeda-conv-orc).
            end.

            /*C†lculo do valor total do oráado ap¢s valores convertidos*/
            ASSIGN tt-detalha-item.vl-tot-des-orc                = tt-detalha-item.vl-ii2-orc /*Total Despesa II2 + Despesa*/ 
                                                                 + de-tot-des-orc-aux /*tot-despesa-orc*/
                   tt-detalha-item.vl-tot-imp-des-orc            = tt-detalha-item.vl-tot-imp-orc /*Total Imposto + Depesa*/
                                                                 + tt-detalha-item.vl-tot-des-orc.
            ASSIGN tt-detalha-item.vl-tot-com-ipi-icms-orc-total = tt-detalha-item.vl-tot-imp-orc +
                                                                   tt-detalha-item.vl-mercado-orc +
                                                                   tt-detalha-item.vl-tot-des-orc.

            ASSIGN tt-detalha-item.vl-tot-sem-ipi-icms-orc-total = tt-detalha-item.vl-tot-com-ipi-icms-orc-total -
                                                                   tt-detalha-item.vl-tot-imp-orc.

            /* Convers∆o Realizado */
            if  tt-param.moeda <> var-moeda-conv-rea then do:
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-mercado-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-mercado-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ipi-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ipi-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-icms-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-icms-rea-total,
                                    input var-moeda-conv-rea).
                ASSIGN de-vl-pis-rea = tt-detalha-item.vl-pis-rea.
                RUN converte-moeda (INPUT-OUTPUT de-vl-pis-rea,
                                    input var-moeda-conv-rea).
                ASSIGN de-vl-pis-rea-total = tt-detalha-item.vl-pis-rea-total.
                RUN converte-moeda (INPUT-OUTPUT de-vl-pis-rea-total,
                                    input var-moeda-conv-rea).
                ASSIGN de-vl-cofins-rea = tt-detalha-item.vl-cofins-rea.
                RUN converte-moeda (INPUT-OUTPUT de-vl-cofins-rea,
                                    input var-moeda-conv-rea).
                ASSIGN de-vl-cofins-rea-total = tt-detalha-item.vl-cofins-rea-total.
                RUN converte-moeda (INPUT-OUTPUT de-vl-cofins-rea-total,
                                    input var-moeda-conv-rea).
                ASSIGN tt-detalha-item.vl-pis-rea-total    = de-vl-pis-rea-total
                       tt-detalha-item.vl-pis-rea          = de-vl-pis-rea
                       tt-detalha-item.vl-cofins-rea-total = de-vl-cofins-rea-total
                       tt-detalha-item.vl-cofins-rea       = de-vl-cofins-rea.

                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-imp-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-imp-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii2-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-ii2-rea-total,
                                    input var-moeda-conv-rea).
                run converte-moeda (input-output tt-detalha-item.vl-des-rea,
                                    input var-moeda-conv-rea).
                run converte-moeda (input-output tt-detalha-item.vl-des-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-des-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-des-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-imp-des-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-imp-des-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-com-ipi-icms-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-com-ipi-icms-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-sem-ipi-icms-rea,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT tt-detalha-item.vl-tot-sem-ipi-icms-rea-total,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT vl-despesa-frete,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT vl-despesa-embalagem,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT vl-despesa-seguro,
                                    input var-moeda-conv-rea).
                RUN converte-moeda (INPUT-OUTPUT vl-despesa-outras,
                                    input var-moeda-conv-rea).
            END.
            
            /* PERCENTUAL */
            assign tt-detalha-item.per-mercado-orc            = (tt-detalha-item.vl-mercado-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-mercado-rea            = (tt-detalha-item.vl-mercado-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-ii-orc                 = (tt-detalha-item.vl-ii-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-ii-rea                 = (tt-detalha-item.vl-ii-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-ipi-orc                = (tt-detalha-item.vl-ipi-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-icms-orc               = (tt-detalha-item.vl-icms-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-pis-orc                = (tt-detalha-item.vl-pis-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-cofins-orc             = (tt-detalha-item.vl-cofins-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-tot-imp-orc            = (tt-detalha-item.vl-tot-imp-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-imp-rea            = (tt-detalha-item.vl-tot-imp-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-ii2-orc                = (tt-detalha-item.vl-ii2-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-ii2-rea                = (tt-detalha-item.vl-ii2-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-tot-des-orc            = (tt-detalha-item.vl-tot-des-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-des-rea            = (tt-detalha-item.vl-tot-des-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-tot-imp-des-orc        = (tt-detalha-item.vl-tot-imp-des-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-imp-des-rea        = (tt-detalha-item.vl-tot-imp-des-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-tot-com-ipi-icms-orc   = (tt-detalha-item.vl-tot-com-ipi-icms-orc-total
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-com-ipi-icms-rea   = (tt-detalha-item.vl-tot-com-ipi-icms-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100
                   tt-detalha-item.per-tot-sem-ipi-icms-orc   = (tt-detalha-item.vl-tot-sem-ipi-icms-orc
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-orc-total) * 100
                   tt-detalha-item.per-tot-sem-ipi-icms-rea   = (tt-detalha-item.vl-tot-sem-ipi-icms-rea
                                                              /  tt-detalha-item.vl-tot-com-ipi-icms-rea) * 100

            /* DESPESAS */
                   tt-detalha-item.vl-des-frete               = vl-despesa-frete
                   tt-detalha-item.vl-des-embalagem           = vl-despesa-embalagem
                   tt-detalha-item.vl-des-seguro              = vl-despesa-seguro
                   tt-detalha-item.vl-des-outras              = vl-despesa-outras
                   tt-detalha-item.vl-tot-despesa             = vl-despesa-frete
                                                              + vl-despesa-embalagem
                                                              + vl-despesa-seguro
                                                              + vl-despesa-outras.

        END.
    END.

END PROCEDURE.

procedure busca-nf-complementar:
    define input parameter p-serie        like docum-est.serie-docto    no-undo.
    define input parameter p-nro-docto    like docum-est.nro-docto      no-undo. 
    define input parameter p-cod-emitente like docum-est.cod-emitente   no-undo. 
    define input parameter p-nat-operacao like docum-est.nat-operacao   no-undo. 
    define input parameter p-embarque     like embarque-imp.embarque    no-undo. 
    define input parameter p-cod-estabel  like embarque-imp.cod-estabel no-undo. 

    for each rat-docum no-lock
       where rat-docum.nf-serie    = p-serie
         and rat-docum.nf-nro      = p-nro-docto
         and rat-docum.nf-emitente = p-cod-emitente
         and rat-docum.nf-nat-oper = p-nat-operacao
        use-index nf-docto:
        
        for each b-docum-est-aux fields( nro-docto  serie-docto  dt-emissao
                                         cod-emitente  nat-operacao cod-estabel)
            where b-docum-est-aux.serie-docto  = rat-docum.serie-docto
              and b-docum-est-aux.nro-docto    = rat-docum.nro-docto
              and b-docum-est-aux.cod-emitente = rat-docum.cod-emitente
              and b-docum-est-aux.nat-operacao = rat-docum.nat-operacao no-lock
            use-index documento:

            FOR FIRST b-item-doc-est WHERE
                      b-item-doc-est.serie-docto  = rat-docum.nf-serie    AND
                      b-item-doc-est.nro-docto    = rat-docum.nf-nro      AND
                      b-item-doc-est.cod-emitente = rat-docum.nf-emitente AND
                      b-item-doc-est.nat-operacao = rat-docum.nf-nat-oper NO-LOCK:
            END.

            create tt-nf-comp.
            assign tt-nf-comp.embarque      = p-embarque
                   tt-nf-comp.cod-estabel   = p-cod-estabel
                   tt-nf-comp.nr-nf-comp    = b-docum-est-aux.nro-docto
                   tt-nf-comp.dt-emis-comp  = b-docum-est-aux.dt-emissao
                   tt-nf-comp.r-docum-est   = ROWID(b-docum-est-aux).

            RUN gera-valores (INPUT b-docum-est-aux.serie-docto,
                              INPUT b-docum-est-aux.nro-docto,
                              INPUT b-docum-est-aux.cod-emitente,
                              INPUT b-docum-est-aux.nat-operacao,
                              INPUT p-embarque,
                              INPUT b-docum-est-aux.cod-estabel,
                              INPUT 3). /*Nota Complementar*/
        end.
    end.
end procedure.
RETURN "OK":U.
PROCEDURE pi-notas-fiscais:
    FOR EACH tt-notas-fiscais: DELETE tt-notas-fiscais. END.
    FOR EACH tt-embarque-nota-mae:
        CREATE tt-notas-fiscais.
        BUFFER-COPY tt-embarque-nota-mae TO tt-notas-fiscais.
        ASSIGN tt-notas-fiscais.r-docum-est = tt-embarque-nota-mae.r-docum-est.
    END.
    for each tt-embarque-corpo:
        CREATE tt-notas-fiscais.
        ASSIGN
          tt-notas-fiscais.embarque       = tt-embarque-corpo.embarque    
          tt-notas-fiscais.cod-estabel    = tt-embarque-corpo.cod-estabel 
          tt-notas-fiscais.nr-nota-fis    = tt-embarque-corpo.nr-nota-fis 
          tt-notas-fiscais.dt-emissao     = tt-embarque-corpo.dt-emissao  
          tt-notas-fiscais.nr-di          = tt-embarque-corpo.nr-di       
          tt-notas-fiscais.dt-di          = tt-embarque-corpo.dt-di
          tt-notas-fiscais.r-docum-est    = tt-embarque-corpo.r-docum-est.
    END.

END PROCEDURE.
PROCEDURE pi-imprime-dados:
  PUT STREAM  s-excel tt-embarque-cabec.cod-estabel ";".    
  PUT STREAM s-excel tt-embarque-cabec.desc-estab  ";". 
  PUT STREAM s-excel tt-embarque-cabec.cod-emitente ";".
  PUT STREAM s-excel tt-embarque-cabec.embarque ";".
  PUT STREAM s-excel tt-embarque-cabec.cod-incoterm ";".
  PUT STREAM s-excel tt-embarque-cabec.cod-via-transp ";".
  PUT STREAM s-excel tt-embarque-cabec.pais ";".
  PUT STREAM s-excel tt-embarque-cabec.pto-embarque ";".
  PUT STREAM s-excel tt-embarque-cabec.data-embarque ";".
  PUT STREAM s-excel tt-embarque-cabec.pto-nacional ";".
  PUT STREAM s-excel tt-embarque-cabec.data-nacional ";".
  PUT STREAM s-excel tt-embarque-cabec.data-chegada ";".
  PUT STREAM s-excel tt-embarque-cabec.data-cotacao ";".
  PUT STREAM s-excel tt-embarque-cabec.desc-moeda ";".
  PUT STREAM s-excel tt-embarque-cabec.mo-taxa ";". 
  PUT STREAM s-excel tt-embarque-cabec.data-convercao ";".
END PROCEDURE.
PROCEDURE pi-busca-impostos:
  /*----------------------------------------------------------------------*/
  DEF INPUT PARAM p-rowid        AS ROWID NO-UNDO. 
  /*----------------------------------------------------------------------*/
  DEF OUTPUT PARAM p-valor-ipi  AS DEC FORMAT ">>>>,>>>,>>9.99" NO-UNDO.
  DEF OUTPUT PARAM p-valor-icm  AS DEC FORMAT ">>>>,>>>,>>9.99" NO-UNDO.
  DEF OUTPUT PARAM p-valor-pis  AS DEC FORMAT ">>>>,>>>,>>9.99" NO-UNDO.
  DEF OUTPUT PARAM p-val-cofins AS DEC FORMAT ">>>>,>>>,>>9.99" NO-UNDO.
  /*----------------------------------------------------------------------*/
  FOR FIRST docum-est
    /*
    FIELDS(
      serie-docto
      nro-docto
      embarque
      cod-emitente
      nat-operacao)
    */  
    NO-LOCK
    WHERE ROWID(docum-est) = p-rowid:

    ASSIGN
      p-valor-ipi  = 0
      p-valor-icm  = 0
      p-valor-pis  = 0
      p-val-cofins = 0.
    FOR EACH item-doc-est OF docum-est
      NO-LOCK:
      ASSIGN
        p-valor-ipi  = p-valor-ipi  + /* 4444 */  item-doc-est.valor-ipi[1] 
        p-valor-icm  = p-valor-icm  + /* 4444 */  item-doc-est.valor-icm[1] 
        p-valor-pis  = p-valor-pis  + /* 4444 */  item-doc-est.valor-pis    
        p-val-cofins = p-val-cofins + /* 4444 */  item-doc-est.val-cofins   
        .
    END.    
  END.

END PROCEDURE.
