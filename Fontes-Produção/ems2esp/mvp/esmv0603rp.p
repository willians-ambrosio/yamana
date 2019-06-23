/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i MV0603RP 2.06.00.002}  /*** 010002 ***/
{include/i_fnctrad.i}
/*****************************************************************************
**
**       PROGRAMA: MV0603RP.p
**
**       DATA....: Dezembro de 2003
**
**       AUTOR...: DATASUL S.A.
**
**       OBJETIVO: Reincidˆncias de Servi‡os
**
*****************************************************************************/
                                              /** Defini‡Æo dos Parƒmetros **/
define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field i-seq            as integer
    field c-cod            as character format "x(300)"
    FIELD i-qtd-interv-ini AS INTEGER
    FIELD i-qtd-interv-fim AS INTEGER
    field lParam           as logical.

define shared temp-table tt-digita no-undo
    field sequencia         as   integer
    field cod-dimensao      as   character format "x(300)"
    field cod-oficial       as   character format "X(16)"
    field desc-dimensao     as   character format "x(32)"
    field cod-dimens-pai    as   character format "x(300)"
    field qtd-interv        as   INTEGER   format ">>>,>>>,>>9"     LABEL "Interven‡äes"         
    field qtd-reinc         as   INTEGER   format ">>>,>>>,>>9"     LABEL "Reincidˆncias"        
    field tempo-reparo      as   decimal   format ">,>>>,>>9.99"    LABEL "Tempo Reparo"         
    field per-interv        as   decimal   format ">>>,>>>,>>9.9"   LABEL "Per¡odo Entre Interv" 
    field un                like mab-model.un
    field r-rowid           as   rowid
    field p-image           as   integer
    field seq-tree          as   integer
    index id is primary unique sequencia ascending
                               cod-dimensao ascending.

/* Defini‡Æo e prepara‡Æo dos parƒmetros */
def temp-table tt-raw-digita
   field raw-digita as raw.
   
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

define buffer bfDigita for tt-digita.

create tt-param.
raw-transfer raw-param to tt-param.

find first tt-param.

/***********************************************************************/
                                           /** Defini‡Æo de Vari veis **/
DEFINE VARIABLE h-acomp     AS HANDLE  NO-UNDO.

/***********************************************************************/
                                                /** Cabe‡alho e Forms **/

{mvp/mv0603.i1} /*--- Defini‡Æo dos frames ---*/

{include/i-rpvar.i}
assign c-programa = "MV/00603"
       c-versao   = "2.06"
       c-revisao  = "000"
.
&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
    DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
    ASSIGN cAuxTraducao001 = {varinc/var00002.i 04 integer(tt-param.destino)}.
    run utp/ut-liter.p (INPUT REPLACE(TRIM(cAuxTraducao001)," ","_"),
                        INPUT "",
                        INPUT "").
    ASSIGN c-destino  = RETURN-VALUE.
&else
    ASSIGN c-destino  = {varinc/var00002.i 04 integer(tt-param.destino)}.
&endif

find first param-global no-lock no-error.
if  available param-global then
    assign c-empresa = param-global.grupo.

{utp/ut-liter.i "Reincidˆncia de Servi‡os" *}
assign c-titulo-relat = trim(return-value).

{utp/ut-liter.i MANUTEN€ÇO MEC¶NICA *}
assign c-sistema = trim(return-value).
       
{include/i-rpcab.i}    
{include/i-rpout.i}

view frame f-cabec.
view frame f-rodape. 

/***********************************************************************/
                                               /** In¡cio do Programa **/
run utp/ut-acomp.p persistent set h-acomp.


for first tt-digita
    where tt-digita.sequencia    = tt-param.i-seq
    and   tt-digita.cod-dimensao = tt-param.c-cod no-lock:

    run pi-inicializar in h-acomp(input c-titulo-relat).

    run imprimeVisao (input rowid(tt-digita),
                      input tt-digita.cod-dimens-pai).
    for each  bfDigita NO-LOCK
        where bfDigita.cod-dimens-pai = tt-digita.cod-dimensao 
        AND   bfDigita.qtd-interv >= tt-param.i-qtd-interv-ini
        AND   bfDigita.qtd-interv <= tt-param.i-qtd-interv-fim
        break by bfDigita.cod-dimens-pai
        BY bfDigita.qtd-interv DESCENDING:

        if first-of(bfDigita.cod-dimens-pai) then do:
            case bfDigita.p-image:
                when 3 then do:
                    {utp/ut-liter.i "Grupo Eqpto"}
                end.
                when 4 then do:
                    {utp/ut-liter.i "Modelo Eqpto"}
                end.
                otherwise do:
                    {mvp/mv0603.i2 bfDigita.p-image}
                end.
            end case.
            assign bfDigita.cod-oficial:label in frame f-dados = trim(return-value).
            put skip(2).
        end.
        display bfDigita.cod-oficial
                bfDigita.desc-dimensao
                bfDigita.qtd-interv   
                bfDigita.qtd-reinc    
                bfDigita.tempo-reparo 
                bfDigita.per-interv   
                bfDigita.un           
            with frame f-dados.
        down with frame f-dados.
    end.
end.

/***********************************************************************/
                                             /** P gina de Parƒmetros **/
if tt-param.lParam then do:
    page.
    DISPLAY /*IMPRESSÇO*/
            c-liter-imp          
            c-destino            
            tt-param.arquivo
            tt-param.usuario     
        WITH FRAME f-param-definidos.
end.

/***********************************************************************/
                                          /** Finaliza‡Æo do Programa **/
run pi-finalizar in h-acomp.

{include/i-rpclo.i}     

return "OK". 


procedure imprimeVisao:

    define input parameter rRow    as rowid     no-undo.
    define input parameter cCodPai as character no-undo.

    if can-find(first bfDigita
                where bfDigita.cod-dimensao = cCodPai no-lock) then do:
        for first bfDigita
            where bfDigita.cod-dimensao = cCodPai no-lock:
            run imprimeVisao (input rowid(bfDigita),
                              input bfDigita.cod-dimens-pai).
        end.
    end.
    for first bfDigita
        where rowid(bfDigita) = rRow no-lock:
    end.
    if avail bfDigita then do:
        {mvp/mv0603.i2 bfDigita.p-image}
        assign tt-digita.cod-oficial:label in frame f-visoes = trim(return-value).
    
        display bfDigita.cod-oficial   @ tt-digita.cod-oficial   
                bfDigita.desc-dimensao @ tt-digita.desc-dimensao
            with frame f-visoes.
        down with frame f-visoes.
    end.

end procedure.
/*--- Fim do Programa ---*/
