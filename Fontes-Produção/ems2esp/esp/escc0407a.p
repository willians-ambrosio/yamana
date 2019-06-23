/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i escc0407A 2.00.00.023}  /*** 010023 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
{include/i-license-manager.i escc0407a MCC}
&ENDIF
/*****************************************************************************
**
**       Programa: escc0407A.P
**
**       Data....: Mar‡o de 1997
**
**       Autor...: DATASUL S.A.
**
**       Objetivo: Listagem das Entregas Previstas por Data de Entrega
**
**       VersÆo..: 1.00.000 - Sandra Stadelhofer
**
*****************************************************************************/

/* Variaveis e Frames Comuns */
{esp/escc0407.i1}.

{cep/ce1234.i} /* Valida‡Æo de Decimais */

def input param raw-param as raw no-undo.

{include/i-rpvar.i}
{include/i_fnctrad.i}

def new global shared var c-dir-spool-servid-exec as char no-undo.
def new global shared var i-num-ped-exec-rpw as int no-undo.



run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Entregas_Previstas_por_Data_de_Entrega *}
run pi-inicializar in h-acomp (input  Return-value ).

create tt-param.
raw-transfer raw-param to tt-param.

find first param-compra no-lock no-error.
find first param-global no-lock no-error.

find first ems2cadme.empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error.
assign c-empresa    = (if avail empresa then empresa.razao-social else "")
       c-programa = "ESCC0407A"
       c-versao   = "1.00"
       c-revisao  = "000"
       l-imprime  = no.

DEFINE VARIABLE c-linha   AS CHARACTER NO-UNDO.
DEFINE VARIABLE c-arquivo AS CHARACTER NO-UNDO.

DEFINE STREAM st-csv.

{utp/ut-liter.i COMPRAS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Entregas_Previstas_de * r}
assign c-titulo-relat = trim(return-value) + " "
                      + string(tt-param.da-iniper, "99/99/9999").
{utp/ut-liter.i a * r}
assign c-titulo-relat = c-titulo-relat     + " "
                      + trim(return-value) + " "
                      + string(tt-param.da-fimper,"99/99/9999").

{include/i-rpcab.i}

for each prazo-compra use-index data
    where prazo-compra.data-entrega >= tt-param.da-iniper
    and   prazo-compra.data-entrega <= tt-param.da-fimper
    and   prazo-compra.quant-saldo   > 0
    and   prazo-compra.situacao     <> 4
    and   prazo-compra.situacao     <> 6 no-lock:
    
    ASSIGN r-rowid-ordem = ?.
    find first ordem-compra use-index ordem
        where ordem-compra.numero-ordem  = prazo-compra.numero-ordem
        and   ordem-compra.situacao      = 2
        and   ordem-compra.cod-estabel  >= tt-param.c-estabel-i
        and   ordem-compra.cod-estabel  <= tt-param.c-estabel-f
        and   ordem-compra.cod-comprado >= tt-param.c-comprado-i
        and   ordem-compra.cod-comprado <= tt-param.c-comprado-f
        and   ordem-compra.cod-emitente >= tt-param.i-fornec-i
        and   ordem-compra.cod-emitente <= tt-param.i-fornec-f
        and   ordem-compra.it-codigo    >= tt-param.c-item-i
        and   ordem-compra.it-codigo    <= tt-param.c-item-f
        and   ordem-compra.num-pedido   >= tt-param.i-pedido-i
        and   ordem-compra.num-pedido   <= tt-param.i-pedido-f
        and   ordem-compra.nr-processo  >= tt-param.i-nr-processo-i
        and   ordem-compra.nr-processo  <= tt-param.i-nr-processo-f
        AND   ordem-compra.nr-contrato  >= tt-param.i-contrato-i
        AND   ordem-compra.nr-contrato  <= tt-param.i-contrato-f
        and   ordem-compra.num-pedido   <> 0 no-lock no-error.

    IF  avail ordem-compra THEN DO:
       IF ordem-compra.nr-contrato <> 0 THEN DO:
          find first item-contrat
               where item-contrat.nr-contrato   = ordem-compra.nr-contrato
                 and item-contrat.num-seq-item =  ordem-compra.num-seq-item
               no-lock no-error.
          if avail item-contrat then do:
             CASE item-contrat.ind-tipo-control:
                  WHEN 1 THEN IF tt-param.l-medicao     = NO THEN NEXT.
                  WHEN 2 THEN IF tt-param.l-ordem       = NO THEN NEXT.
                  WHEN 3 THEN IF tt-param.l-programacao = NO THEN NEXT.
             END CASE.
          END.
       END.



        find first pedido-compr use-index numero
            where pedido-compr.num-pedido = ordem-compra.num-pedido
            no-lock no-error.
        
            if tt-param.l-ped-emitido then do:
                IF  pedido-compr.situacao <> 1 THEN /* Impresso */
                    NEXT.
            END.
             
            if tt-param.l-ped-aprovado then do:
                /** Aprova‡Æo Eletr“nica **/
                assign l-pendente = no.
                if  param-compra.log-1 
                AND AVAIL pedido-compr
                AND r-rowid-ordem <> ROWID(ordem-compra) then do:
                    if  pedido-compr.emergencial then
                        run cdp/cdapi172 (6, rowid(ordem-compra), output l-pendente).
                    else
                        run cdp/cdapi172 (4, rowid(ordem-compra), output l-pendente).
                    ASSIGN r-rowid-ordem = ROWID(ordem-compra).
                end.
                    if  l-pendente then
                NEXT.
            end.
    END.

    if  avail ordem-compra then do:

        i-conta = i-conta + 1.
        run pi-acompanhar in h-acomp (input ordem-compra.it-codigo).
        find first emitente use-index codigo
            where emitente.cod-emitente  = ordem-compra.cod-emitente
            and   emitente.cod-gr-forn  >= tt-param.i-gr-fornec-i
            and   emitente.cod-gr-forn  <= tt-param.i-gr-fornec-f
            no-lock no-error.
        if  avail emitente then do:
            find first item use-index codigo
                where item.it-codigo = ordem-compra.it-codigo
                no-lock no-error.                

            if  tt-param.i-tipo-moeda = ordem-compra.mo-codigo then
                de-preco-conv = ordem-compra.preco-unit.
            else do:
                run cdp/cd0812.p (input  ordem-compra.mo-codigo,
                                  input  tt-param.i-tipo-moeda,
                                  input  ordem-compra.preco-unit,
                                  input  ordem-compra.data-cotacao,
                                  output de-preco-conv).
                if  de-preco-conv = ? then assign de-preco-conv = 0.
            end.

            find first moeda use-index codigo
                where moeda.mo-codigo = tt-param.i-tipo-moeda
                no-lock no-error.
            assign tt-param.c-descri-moeda = moeda.descricao
            .
            &if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
                DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
                ASSIGN cAuxTraducao001 = {ininc/i01in274.i 04 ordem-compra.natureza}.
                run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                                    INPUT "",
                                    INPUT "").
                ASSIGN c-natureza = RETURN-VALUE.
            &else
                ASSIGN c-natureza = {ininc/i01in274.i 04 ordem-compra.natureza}.
            &endif

            if  avail item then
               assign c-descricao = substring(item.desc-item,1,35).                   

            /* Calculo das qtdades e cria‡Æo da tt-ordem */
            {esp/escc0407.i2}

        end.
    end.
end.

ASSIGN c-arquivo = tt-param.arquivo.

IF i-num-ped-exec-rpw <> 0 Then 
   Assign c-arquivo = c-dir-spool-servid-exec + "~/" + tt-param.arquivo.

ASSIGN c-arquivo = ENTRY(1,c-arquivo,'.') + ".csv".

OUTPUT STREAM st-csv TO VALUE(c-arquivo).


PUT STREAM st-csv
    "Data Entrega;Pedido;Natureza;Grupo;Nome Abrev;Numero Ordem;Parcela;Contrato;Seq;Tp Ctrl;Estabelec;Item;Un;Descri‡Æo;Processo;Ordem Serv;Qt Saldo;Pre‡o Unit;Pre‡o Merc;Vl IPI;Pre‡o Total;Moeda;Atraso;Narrativa;Texo Item;Narrativa Pedido;Narrativa Ordem;" SKIP.

for EACH tt-ordem
    break by tt-ordem.data-entrega :
        
    i-conta = i-conta + 1.

    run pi-acompanhar in h-acomp (input tt-ordem.num-ordem).

    PUT STREAM st-csv 
        tt-ordem.data-entrega                     ";"
        tt-ordem.num-pedido                       ";"
        tt-ordem.natur                            ";"
        tt-ordem.grupo                            ";"
        tt-ordem.nome-abrev                       ";"
        tt-ordem.num-ordem                        ";"
        tt-ordem.parcela                          ";"
        tt-ordem.nr-contrato                      ";"
        tt-ordem.num-seq-item                     ";"
        tt-ordem.tipo                             ";"
        tt-ordem.cod-estabel                      ";"
        tt-ordem.it-codigo                        ";"
        tt-ordem.un                               ";"
        tt-ordem.descr           format "x(36)"   ";"
        tt-ordem.nr-processo                      ";"
        tt-ordem.ordem-servic                     ";"
        tt-ordem.quant-saldo                      ";"
        tt-ordem.preco-unit                       ";"
        tt-ordem.preco-merc                       ";"
        tt-ordem.valor-ipi                        ";"
        tt-ordem.preco-total                      ";"
        tt-ordem.desc-moeda                       ";"
        tt-ordem.atraso                           ";".

/*     if  line-counter > 62 then do: */
/*         page.                      */
/*         view frame f-cab-corpo.    */
/*     end.                           */

    /* Apresentacao das Narrativas e Texto livre do item: */
    assign i-salta-narra = 0.

    /* Narrativa do Item */
    if  tt-param.l-lista-narra-item = yes then do:
       find first narrativa use-index codigo
            where narrativa.it-codigo = tt-ordem.it-codigo 
            no-lock no-error.   
       IF AVAILABLE(narrativa) THEN
          PUT STREAM st-csv replace(replace(narrativa.descricao,CHR(10),""),CHR(13),"") FORMAT "x(2000)".

       PUT STREAM st-csv ";".
       
/*         {esp/escc0407.i3 &display="view frame f-cab-corpo."} */
    end.

    /* Texto Livre do Item */
    if  tt-param.l-perm-texto        = yes
    and tt-param.l-lista-texto-livre = yes then do:

       for each  lin-texto use-index codigo 
           where lin-texto.it-codigo  = tt-ordem.it-codigo
           and   lin-texto.tipo       = tt-param.c-tipo-texto
           and   lin-texto.sequencia >= 0 no-lock
           break by lin-texto.it-codigo by lin-texto.tipo:
       
            c-linha = c-linha + " " + lin-texto.linha.
       END.

       PUT STREAM st-csv replace(replace(c-linha,CHR(10),""),CHR(13),"") FORMAT "x(2000)" ";" .


/*         {esp/escc0407.i4 &display="view frame f-cab-corpo."} */
    end.

    /* Narrativa do Pedido */
    if  tt-param.l-lista-narra-pedido = yes then do:
        PUT STREAM st-csv replace(replace(tt-ordem.comentarios,CHR(10),""),CHR(13),"") FORMAT "x(2000)" ";".
/*         {esp/escc0407.i5 &display="view frame f-cab-corpo."} */
    end.

    /* Narrativa da Ordem */
    if  tt-param.l-lista-narra-ordem = yes then do:
        PUT STREAM st-csv replace(replace(tt-ordem.narrativa,CHR(10),""),CHR(13),"") FORMAT "x(2000)" ";".
/*         {esp/escc0407.i6 &display="view frame f-cab-corpo."} */
    end.


    PUT STREAM st-csv SKIP.

/*     if  i-salta-narra = 1 then put skip(1).                     */
/*     assign l-imprime = yes                                      */
/*            de-tot-data = de-tot-data + tt-ordem.preco-total.    */
/*                                                                 */
/*     if  last-of(tt-ordem.data-entrega) and i-conta > 0 then do: */
/*         put unformatted skip(1)                                 */
/*             c-lb-total                 at 70.                   */
/*         put tt-ordem.data-entrega      at 76 ":" at 89          */
/*             de-tot-data                at 91 skip(1).           */
/*         assign de-tot-geral = de-tot-geral + de-tot-data        */
/*                de-tot-data  = 0                                 */
/*                i-conta      = 0.                                */
/*     end.                                                        */
end.

/* put unformatted c-lb-tot-ger at 70 "........:". */
/* put de-tot-geral at 91 skip.                    */
/*                                                 */
/* assign de-tot-geral = 0.                        */
/* if  l-imprime = no then                         */
/*     view frame f-cab-corpo.                     */

OUTPUT STREAM st-csv CLOSE.

IF i-num-ped-exec-rpw = 0 Then 
   DOS SILENT START excel.exe VALUE(c-arquivo).

run pi-finalizar in h-acomp.

{include/pi-edit.i}

