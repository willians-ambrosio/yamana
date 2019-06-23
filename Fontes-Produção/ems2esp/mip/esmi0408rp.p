/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
def buffer empresa         for ems2cadme.empresa.

{include/i-prgvrs.i ESMI0408RP 2.06.00.044}  /*** 010044 ***/
{include/i_fnctrad.i}
/*****************************************************************************
**
**   Programa: ESMI0408.p
**
**   Data....: 02/2008.
**
**   Autor...: Datasul S/A
**
**   Objetivo: Relatorio das Ordens de Manutencao especifico para emrpesa YAMANA
**
******************************************************************************/
{cdp/cdcfgmnt.i} 
{cdp/cd9911.i}
define temp-table tt-param
    field destino                as integer
    field arquivo                as char    format "x(40)"
    field usuario                as char
    field data-exec              as date
    field hora-exec              as integer
    field classifica             as integer
    field c-cc-ini               as char
    field c-cc-fim               as char
    field i-ordem-ini            as int    format 999999999
    field i-ordem-fim            as int    format 999999999
    field c-equipto-ini          as char   format "x(16)"
    field c-equipto-fim          as char   format "x(16)"
    field c-tag-ini              as char   format "x(16)"
    field c-tag-fim              as char   format "x(16)"
    field c-planeja-ini          as char   format "x(08)"
    field c-planeja-fim          as char   format "x(08)"
    field c-equipe-ini           as char   format "x(08)"
    field c-equipe-fim           as char   format "x(08)"
    field da-data-ini            as date
    field da-data-fim            as date
    field i-prioridade-ini       as int
    field i-prioridade-fim       as int   
    field l-susp                 as log
    field l-nao-inic             as log
    field l-inic                 as log
    field l-term                 as log
    field l-final                as log
    field l-liber                as log
    field l-aloc                 as log
    field l-requi                as log
    field l-separ                as log
    field l-narrativa            as log
    field l-quebra-pag           as log
    field l-item                 as log
    field l-ferramenta           as log
    field l-equip-protecao       as log
    field l-ficha-metodo         as log
    field i-cod-tipo             as INT  /* Valor Default: 999*/
    field desc-classifica        as char format "x(40)"
    field c-estabel-ini          as char format "x(03)"
    field c-estabel-fim          as char format "x(03)"
    field c-familia-ini          as char format "x(08)"
    field c-familia-fim          as char format "x(08)"
    field i-tp-manut-ini         as integer
    field i-tp-manut-fim         as integer
    FIELD c-cod-unid-negoc-ini   AS CHAR FORMAT "x(03)"
    FIELD c-cod-unid-negoc-fim   AS CHAR FORMAT "x(03)".

define temp-table tt-digita
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id is primary unique
        ordem.

DEFINE TEMP-TABLE tt-tag-equipto
    FIELD cd-tag LIKE tag.cd-tag
    INDEX id IS PRIMARY UNIQUE cd-tag.

/* Defini‡Æo e Prepara‡Æo dos Parƒmetros */

def temp-table tt-raw-digita
    field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table     for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

def var h-acomp as handle no-undo.
/*def new global shared var gi-sequencia as integer format ">>9" no-undo.*/


/******************************************************************************/
{include/tt-edit.i} /** Include para impressÆo de narrativa **/

def var c-cc-codigo       like equipto.cc-codigo         no-undo. 
def var c-cd-equipto      like equipto.cd-equipto        no-undo. 
def var c-desc-equipto    like equipto.descricao         no-undo. 
def var c-cd-planejado    like ord-manut.cd-planejado    no-undo. 
def var c-cd-tag          like tag.cd-tag                no-undo. 
def var c-cd-equip-res    like ord-manut.cd-equip-res    no-undo. 
def var l-prim-imp        as logical.
def var i-linha           as integer                     no-undo.
def var i-nr-ord          as int.

def var i-manut           as integer                     no-undo.
def var c-manut           as character format "x(80)"    no-undo.
def var c-tipo-man        as character format "x(9)"     no-undo. 
def var c-est-ord         as character format "x(80)"    no-undo. 
def var c-estado          as character format "x(06)"    no-undo.

def var c-narrativa-ordem as char format "x(18)"  no-undo.
def var c-ordem           as char format "x(05)"  no-undo.
def var c-data-manut      as char format "x(10)"  no-undo.
def var c-cd-manut        as char format "x(10)"  no-undo.
def var c-descricao       as char format "x(09)"  no-undo.
def var c-descricao-1     as char format "x(09)"  no-undo.
def var c-descricao-2     as char format "x(09)"  no-undo.
def var c-est             as char format "x(03)"  no-undo.
def var c-tp-man          as char format "x(08)"  no-undo.
def var c-equipto         as char format "x(11)"  no-undo.
def var c-tag             as char format "x(06)"  no-undo.
def var c-planejado       as char format "x(12)"  no-undo.
def var c-equip-res       as char format "x(11)"  no-undo.
def var c-cod-estab       as char format "x(15)"  no-undo.
def var c-familia         as char format "x(8)"   no-undo.
def var c-resp-equip      as char format "x(11)"  no-undo.
def var c-prioridade      as char format "x(03)"  no-undo.
def var c-prioridade-2    as char format "x(12)"  no-undo.
def var c-traco           as char format "x(84)" no-undo.
def var c-custo           as char format "x(16)"  no-undo.
def var c-cc              as char format "x(10)"  no-undo.
def var c-liter-equipto   as char format "x(25)"  no-undo.
def var c-nome            as char format "x(04)"  no-undo.
def var c-equipe          as char format "x(06)"  no-undo.
DEF VAR c-cod-unid-negoc  AS CHAR FORMAT "x(03)"  NO-UNDO.
DEF VAR c-des-unid-negoc  AS CHAR FORMAT "x(40)"  NO-UNDO.

def var c-par-consid    as char format "x(10)" no-undo.
def var c-ord-susp      as char format "x(17)" no-undo.
def var c-ord-nao-inic  as char format "x(21)" no-undo.
def var c-ord-inic      as char format "x(17)" no-undo.
def var c-ord-term      as char format "x(18)" no-undo.
def var c-ord-final     as char format "x(19)" no-undo.
def var c-ord-liber     as char format "x(17)" no-undo.
def var c-ord-aloc      as char format "x(17)" no-undo.
def var c-ord-requi     as char format "x(21)" no-undo.
def var c-ord-separ     as char format "x(18)" no-undo.
def var c-imp-narrat    as char format "x(19)" no-undo.
def var c-quebra-pag    as char format "x(26)" no-undo.
def var c-selecao       as char format "x(07)" no-undo.
def var c-inicial       as char format "x(07)" no-undo.
def var c-final         as char format "x(05)" no-undo.
def var c-classificac   as char format "x(13)" no-undo.
def var c-por           as char format "x(03)" no-undo.
def var c-liter-impres  as char format "x(09)" no-undo.
def var c-liter-usuario as char format "x(08)" no-undo.
def var c-liter-destin  as char format "x(08)" no-undo.
def var c-destino       as char format "x(08)" no-undo.
def var c-arquivo       as char format "x(40)" no-undo.
def var c-liter-cons-dt as char format "x(15)" no-undo.
def var c-considera     as char format "x(10)" no-undo.
def var c-lista-consid  as char format "x(16)" no-undo.

def var c-susp                      as char format "x(05)"  no-undo.
def var c-nao-inic                  as char format "x(05)"  no-undo.
def var c-inic                      as char format "x(05)"  no-undo.
def var c-term                      as char format "x(05)"  no-undo.
def var c-final-2                   as char format "x(05)"  no-undo.
def var c-liber                     as char format "x(05)"  no-undo.
def var c-aloc                      as char format "x(05)"  no-undo.
def var c-requi                     as char format "x(05)"  no-undo.
def var c-separ                     as char format "x(05)"  no-undo.
def var c-quebra-pag-2              as char format "x(05)"  no-undo.
def var c-consid-parada             as char format "x(07)"  no-undo.
def var c-liter-considera           as char format "x(17)"  no-undo.
def var c-estabel                   as char format "x(05)"  no-undo.
def var c-tipo-manut                as char format "x(17)"  no-undo.
def var c-liter-param               as char format "x(30)"  no-undo.
def var c-param                     as char format "x(03)"  no-undo.
def var c-desc-man                  as char format "x(50)"  no-undo.
def var c-desc-fam                  as char format "x(50)"  no-undo.
def var c-desc-tar                  as char format "x(76)".
def var c-des-man                   as character no-undo format "x(26)".


DEFINE VARIABLE cl-epi              AS CHAR NO-UNDO.
DEFINE VARIABLE cl-equipamento      AS CHAR NO-UNDO.
DEFINE VARIABLE cl-tarefa           AS CHAR NO-UNDO.
DEFINE VARIABLE cl-quantde          AS CHAR NO-UNDO.
DEFINE VARIABLE cl-ferramenta-title AS CHAR NO-UNDO.
DEFINE VARIABLE cl-ferramenta       AS CHAR NO-UNDO.
DEFINE VARIABLE cl-tempo            AS CHAR NO-UNDO.
DEFINE VARIABLE cl-item             AS CHAR NO-UNDO.
DEFINE VARIABLE cl-ficha-met-title  AS CHAR NO-UNDO.
DEFINE VARIABLE cl-ficha-met        AS CHAR NO-UNDO.


{utp/ut-liter.i E_P_I * R } 
ASSIGN cl-epi = TRIM(RETURN-VALUE).
{utp/ut-liter.i EQUIPAMENTO_DE_PROTECAO_INDIVIDUAL * R }
ASSIGN  cl-equipamento = TRIM(RETURN-VALUE).
{utp/ut-liter.i TAREFA * R }
ASSIGN  cl-tarefa = TRIM(RETURN-VALUE).
{utp/ut-liter.i QUANTIDADE * R }
ASSIGN  cl-quantde = TRIM(RETURN-VALUE).
{utp/ut-liter.i F_E_R_R_A_M_E_N_T_A_S * R }
ASSIGN  cl-ferramenta-title = TRIM(RETURN-VALUE).
{utp/ut-liter.i FERRAMENTAS * R }
ASSIGN  cl-ferramenta = TRIM(RETURN-VALUE).    
{utp/ut-liter.i TEMPO * R }
ASSIGN  cl-tempo = TRIM(RETURN-VALUE).    
{utp/ut-liter.i ITEM * R }
ASSIGN  cl-item = TRIM(RETURN-VALUE).    
{utp/ut-liter.i F_I_C_H_A_S__M__T_O_D_O_S * R }
assign  cl-ficha-met-title = trim(return-value).
{utp/ut-liter.i FICHA_DE_MTODO * R }
assign  cl-ficha-met = trim(return-value).
    


/** variaveis Labels **/
DEFINE VARIABLE lb-data-exec   AS CHARACTER                                 NO-UNDO.
DEFINE VARIABLE nr-ord-prod    AS CHARACTER                                 NO-UNDO.
DEFINE VARIABLE cl-data        AS CHARACTER                                 NO-UNDO.
DEFINE VARIABLE c-data         AS DATE                    FORMAT 99/99/9999 NO-UNDO.
DEFINE VARIABLE dt-ini-cedo    AS DATE                    FORMAT 99/99/9999 NO-UNDO.
DEFINE VARIABLE dt-ini-tarde   AS DATE                    FORMAT 99/99/9999 NO-UNDO.
DEFINE VARIABLE tempo-para     AS DECIMAL                                   NO-UNDO.
DEFINE VARIABLE c-num-pag      AS INTEGER                 FORMAT ">>>9"     NO-UNDO.
DEFINE VARIABLE cl-empresa     AS CHARACTER                                 NO-UNDO.

DEFINE VARIABLE gm-codigo      LIKE equipto.gm-codigo                       NO-UNDO.
DEFINE VARIABLE fm-equipto     LIKE equipto.fm-equipto                      NO-UNDO.
DEFINE VARIABLE cc-codigo      LIKE equipto.cc-codigo                       NO-UNDO.
DEFINE VARIABLE cd-equip-res   LIKE ord-manut.cd-equip-res                  NO-UNDO.
DEFINE VARIABLE cod-estabel    LIKE equipto.cod-estabel                     NO-UNDO.
DEFINE VARIABLE cd-planejado   LIKE equipto.cd-planejado                    NO-UNDO.
DEFINE VARIABLE msg-exp        LIKE msg-ord-man.msg-exp                     NO-UNDO.
DEFINE VARIABLE cd-sint-padr   LIKE sint-padrao.descricao    FORMAT "x(29)" NO-UNDO. 
DEFINE VARIABLE cd-causa-padr  LIKE epi.descricao            FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cd-interv-padr LIKE interv-padrao.descricao  FORMAT "x(60)" NO-UNDO.

DEFINE VARIABLE i-nr-ord-produ LIKE ord-manut.nr-ord-produ NO-UNDO.


DEFINE VARIABLE c-narrativa       AS CHARACTER FORMAT "x(130)" NO-UNDO.
DEFINE VARIABLE c-cd-sint-padr    AS CHARACTER FORMAT "x(40)"  NO-UNDO.
DEFINE VARIABLE c-cd-causa-padr   AS CHARACTER FORMAT "x(10)"  NO-UNDO.
DEFINE VARIABLE c-cd-interv-padr  AS CHARACTER FORMAT "x(10)"  NO-UNDO.
DEFINE VARIABLE c-taref-descricao AS CHARACTER FORMAT "x(20)"  NO-UNDO.
define variable razao-social like empresa.razao-social no-undo.

form     
    "+----------------------------------------------------------------------------------+" SKIP
    "|Data:"  AT 1
    c-data    AT 8
    "PµGINA " AT 70
    c-num-pag AT 79
    "|"       AT 84 SKIP
    "|"       AT  1
    "|"       AT 84 SKIP
    "|"       AT  1
    "MI0408"  AT  2
    " - "     AT  8
    "ORDEM DE MANUTEN€ÇO" AT 10
    "|"       AT 84 SKIP
    "|Nr Ordem    : "ord-manut.nr-ord-prod " - " c-des-man
    "|"                                                              AT 84 SKIP
    "|Equipamento : "equipto.cd-equipto " - " "Data: " ord-manut.dt-manut    
    "Tempo parada: "  tempo-para
    "|"                                                              AT 84 SKIP
    "|Descri‡Æo   :"  equipto.descricao
    "|"                                                              AT 84 SKIP
    "|In¡cio mais cedo   :"  dt-ini-cedo "  In¡cio mais tarde   :"  dt-ini-tarde                                  
    "|"                                                              AT 84 SKIP
    "|PrevisÆo de T‚rmino :" ord-manut.dt-prev            
    "|"                                                              AT 84 SKIP
    "|TAG         :" ord-manut.cd-tag "-"  tag.descricao  
    "|"                                                              AT 84 SKIP
    "|Manuten‡Æo  :"  ord-manut.cd-manut " - " ord-manut.des-man-corr
    "|"                                                              AT 84 SKIP
    "+----------------------------------------------------------------------------------+" AT 1 SKIP
    "|Grupo Equipto :" equipto.gm-codigo     
    "Famil¡a :"                                 AT 60 
    equipto.fm-equipto                          AT 70
    "|"                                         AT 84 SKIP
    "|Centro Custo  :" equipto.cc-codigo 
    "Estabelecimento :"                         AT 52
    equipto.cod-estabel
    "|"                                         AT 84 SKIP
    "|"                                         AT  1
    "|"                                         AT 84 SKIP
    "|Equipe :"  ord-manut.cd-equip-res         
    "Planejador :"                              AT 57
    equipto.cd-planejado         
    "|"                                         AT 84 SKIP
    "|"                                         AT 1
    "|"                                         AT 84 SKIP
    "|Interven‡Æo:"                             AT 1
    cd-interv-padr                              AT 15
    "|"                                         AT 84 SKIP
    "|Sintoma    :"                            AT  1
    cd-sint-padr                                AT 15
    "|"                                         AT 84 SKIP
    "|Causa      : "                           AT 1
    cd-causa-padr                               AT 15 
     "|"                                        AT 84 SKIP
    
    "+----------------------------------------------------------------------------------+" AT 1 SKIP
    "+----------------------------- T A R E F A S --------------------------------------+" AT 1 SKIP
    "|"                                         AT  1
     "ACEITE"                                   AT  2
    "|TAREFA "                                  AT  8
    "DESCRI€ÇO DA ATIVIDADE"                    AT 18
    "|"                                         AT 44
    "REPORTE DE HORAS"                          AT 56
    "|"                                         AT 84 SKIP    
    
    WITH width 132 no-box NO-LABELS 64 DOWN STREAM-IO frame f-form-1.

FORM 
    "|"
    reservas.it-codigo  AT 2
    item.descricao-1    AT 20
    reservas.op-codigo  AT 50 FORMAT ">>>>>9"
    reservas.quant-orig AT 56 FORMAT "->,>>9.9999"
    "|" AT 84
    
    /*item.descricao-1    at 15 */
    WITH width 132 no-box 64 DOWN NO-LABEL  STREAM-IO frame f-form-item.

FORM 
    "|"
    ord-epi.cd-epi      AT 2
    epi.descricao       AT 11
    ord-epi.cd-tarefa   AT 50 FORMAT ">>>>>9"
    ord-epi.qtde-epi    AT 56 FORMAT "->,>>9.9999"
    "|" AT 84
    /*item.descricao-1    at 15 */
    WITH width 132 no-box 64 DOWN NO-LABEL  STREAM-IO frame f-form-epi.

FORM 
    "|"
    ord-ferr.cd-tp-ferr AT 2  
    tp-ferr.descricao   AT 10
    ord-ferr.cd-tarefa  AT 50 FORMAT ">>>>>9" 
    ord-ferr.tempo      AT 58 FORMAT "->>9.9999"
    "|" AT 84
    WITH width 132 no-box 64 DOWN NO-LABEL  STREAM-IO frame f-form-ferramentas.

FORM 
    "|"
    ord-fich-met.fi-codigo       AT 2 FORMAT ">>>>9"
    mnt-ficha-metodo.descricao   AT 8
    ord-fich-met.cd-tarefa       AT 71 FORMAT ">>>>>9" 
    "|" AT 84
    WITH width 132 no-box 64 DOWN NO-LABEL  STREAM-IO frame f-form-ficha-metodo.
    
    

FORM
    "+------|-----------------------------------|---------------------------------------+" AT 1  SKIP
    "|      |"ord-taref.cd-tarefa " "c-taref-descricao "  | EXECUTANTE | DATA |INICIAL|FINAL      | " AT 42 SKIP
    "|      |                                   |            |      |       |           |" AT 1 SKIP
    "|      |                                   |            |      |       |           |" AT 1 SKIP
    "|      |                                   |            |      |       |           |" AT 1 SKIP
    WITH width 132 no-box NO-LABELS 64 DOWN STREAM-IO frame f-form-2.

/*------------------------------   Cabe‡alho ---------------------*/

/*         form HEADER                                                          */
/*                                                                              */
/*             with stream-io width 84 no-labels no-box page-top frame f-cabec. */
  /*      
        form header
            fill("-", 84) format "x(84)"
/*             c-empresa c-titulo-relat at 50 */
            
/*             "Periodo:":U i-numper-x at 10 "-" */
/*             da-iniper-x at 15 "a":U da-fimper-x */
            
            with stream-io width 84 no-labels no-box page-top frame f-cabper.
/*     &ELSE */*/
        form HEADER
            
/*             c-empresa c-titulo-relat at 50 */
/*             "P gina:":U at 120 page-number({&STREAM})  at 128 format ">>>>9" skip */
            " "
            with stream-io width 84 no-labels no-box page-bottom frame f-cabec.
/*        
        form header
            fill("-", 84) format "x(84)"
/*             c-empresa c-titulo-relat at 50 */
/*             "P gina:":U at 120 page-number({&STREAM})  at 128 format ">>>>9" skip */
/*             "Periodo:":U i-numper-x at 10 "-" */
/*             da-iniper-x at 15 "a":U da-fimper-x */
/*             fill("-", 74) format "x(72)" today format "99/99/9999"  */
/*             "-" string(time, "HH:MM:SS":U) skip(1)                  */
            with stream-io width 84 no-labels no-box page-top frame f-cabper.
/*     &ENDIF */
/* &ENDIF     */
*/


/* c-rodape = "DATASUL - ":U + c-sistema + " - " + c-prg-obj + " - V:":U + c-prg-vrs. */
/* c-rodape = fill("-", 132 - length(c-rodape)) + c-rodape.                           */

form HEADER
/*     c-rodape format "x(132)" */ " "
    with stream-io width 84 no-labels no-box page-bottom frame f-rodape.

/*------------------------------------------------------------------------*/

{include/i-rpvar.i}

&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
    DEFINE VARIABLE cAuxTraducao001 AS CHARACTER NO-UNDO.
    ASSIGN cAuxTraducao001 = {ininc/i01in434.i 03}.
    RUN utp/ut-list.p (input-output cauxtraducao001).
    ASSIGN  c-manut = cAuxTraducao001.
&else
    ASSIGN c-manut = {ininc/i01in434.i 03}.
&endif
&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
    DEFINE VARIABLE cAuxTraducao002 AS CHARACTER NO-UNDO.
    ASSIGN cAuxTraducao002 = {varinc/var00014.i 03}.
    RUN utp/ut-list.p (input-output cauxtraducao002).
    ASSIGN  c-est-ord = cAuxTraducao002.
&else
    ASSIGN c-est-ord = {varinc/var00014.i 03}.
&endif

find first param-mi no-lock no-error.

find first param-global no-lock no-error.
if  avail param-global then
    assign c-empresa = grupo.

/* {include/i-rpcab.i} */
{include/i-rpout.i}
{include/pi-edit.i}

/*** fo 1204.562 - brasmetal - o cliente nÆo deseja validar o tipo de nivel ***/
def var l-mmi-tipo-nivel as logical no-undo.
IF CAN-FIND (FIRST funcao  WHERE 
                   funcao.cd-funcao = "spp-mmi-tipo-nivel":U AND 
                   funcao.ativo)  THEN 
    ASSIGN l-mmi-tipo-nivel  = YES.

FOR EACH tt-tag-equipto:
    DELETE tt-tag-equipto.
END.

FOR EACH tag 
    WHERE tag.cod-tipo = tt-param.i-cod-tipo
        AND tag.cd-tag     >= tt-param.c-tag-ini
        AND Tag.cd-tag     <= tt-param.c-tag-fim NO-LOCK:

    IF CAN-FIND(FIRST tt-tag-equipto
                WHERE tt-tag-equipto.cd-tag = tag.cd-tag) THEN NEXT.

    IF tag.cod-tipo = 999 THEN DO:
        create tt-tag-equipto.
        assign tt-tag-equipto.cd-tag = tag.cd-tag.
    END.
    ELSE do:
        IF l-mmi-tipo-nivel THEN DO:
            create tt-tag-equipto.
            assign tt-tag-equipto.cd-tag = tag.cd-tag.
        END.

        RUN pi-cria-tt-tag-equipto(INPUT tag.cd-tag).
    END.
END.

IF tt-param.l-quebra-pag = yes THEN DO:
view frame f-cabec.
view frame f-rodape.
END.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Ordens_de_Manuten‡Æo_em_Aberto *}
run pi-inicializar in h-acomp ( input  Return-value ).

assign i-nr-ord = 0.
ASSIGN c-cod-unid-negoc = "".

case tt-param.classifica :
     when 1 then run pi-class-custo.
     when 2 then run pi-class-equipto.
     when 3 then run pi-class-desc.
     when 4 then run pi-class-planej.
     when 5 then run pi-class-tag.
     when 6 then run pi-class-equipe.
end case.

run pi-finalizar in h-acomp.

&if "{&FNC_MULTI_IDIOMA}" = "Yes" &then
DEFINE VARIABLE cAuxTraducao003 AS CHARACTER NO-UNDO.
ASSIGN cAuxTraducao003 = {varinc/var00002.i 04 tt-param.destino}.
run utp/ut-liter.p (input replace(trim(cauxtraducao003)," ","_"),
                    INPUT "",
                    INPUT "").
ASSIGN c-destino = RETURN-VALUE.
&else
ASSIGN c-destino = {varinc/var00002.i 04 tt-param.destino}.
&endif
ASSIGN c-arquivo = tt-param.arquivo.
/* PAGE. */

/************************************  PROCEDURE   ************************************************/
procedure pi-class-custo:
    /*classifica‡Æo do equipamento por centro de custo*/
    assign l-prim-imp     = yes
           c-cc-codigo    = "?"
           i-nr-ord-produ = ?.

   for each ord-manut
        where ord-manut.cd-equipto   >= tt-param.c-equipto-ini
        and   ord-manut.cd-equipto   <= tt-param.c-equipto-fim
        and   ord-manut.nr-ord-produ >= tt-param.i-ordem-ini
        and   ord-manut.nr-ord-produ <= tt-param.i-ordem-fim
        and   ord-manut.cd-equip-res >= tt-param.c-equipe-ini
        and   ord-manut.cd-equip-res <= tt-param.c-equipe-fim
        and   ord-manut.cd-planejado >= tt-param.c-planeja-ini
        and   ord-manut.cd-planejado <= tt-param.c-planeja-fim
        and   ord-manut.cd-tipo      >= tt-param.i-tp-manut-ini
        and   ord-manut.cd-tipo      <= tt-param.i-tp-manut-fim
        and   ord-manut.prioridade   >= tt-param.i-prioridade-ini
        and   ord-manut.prioridade   <= tt-param.i-prioridade-fim
        AND   ord-manut.dt-manut >= tt-param.da-data-ini 
        and   ord-manut.dt-manut <= tt-param.da-data-fim

        /*and
          (if  tt-param.i-cons-parada = 1 then 
               ord-manut.cd-parada    = tt-param.c-cd-parada and
               ord-manut.sequencia    = tt-param.i-sequencia
           else 
              if  tt-param.i-cons-parada = 2 then
                  ord-manut.cd-parada    = "" 
              else 
                  TRUE)
        and
            (if  tt-param.i-tipo-data = 1 then
                 ord-manut.dt-manut >= tt-param.da-data-ini and 
                 ord-manut.dt-manut <= tt-param.da-data-fim
             else 
                 ord-manut.dt-fecham >= tt-param.da-data-ini and 
                 ord-manut.dt-fecham <= tt-param.da-data-fim) */
        &if defined (bf_mnt_ems203) &then
        and ord-manut.cod-estabel    >=     tt-param.c-estabel-ini
        and ord-manut.cod-estabel    <=     tt-param.c-estabel-fim no-lock,
        &else
        ,first ord-prod (FIELDS nr-ord-produ)
              where ord-prod.nr-ord-produ = ord-manut.nr-ord-produ
              and   ord-prod.cod-estabel >= tt-param.c-estabel-ini
              and   ord-prod.cod-estabel <= tt-param.c-estabel-fim no-lock,
       
        &endif 
        
        first tipo-manut
              where tipo-manut.cd-tipo = ord-manut.cd-tipo no-lock,

            first equipto
                  where equipto.cd-equipto = ord-manut.cd-equipto
                  and   equipto.cc-codigo   >= tt-param.c-cc-ini
                  and   equipto.cc-codigo   <= tt-param.c-cc-fim 
                  and   equipto.fm-equipto  >= tt-param.c-familia-ini
                  and   equipto.fm-equipto  <= tt-param.c-familia-fim no-lock
                  break by equipto.cc-codigo 
                        by ord-manut.nr-ord-produ:
       
              /* se a ordem nÆo est  suspensa */
             IF  ord-manut.estado-om NE 3  THEN DO:
                 /* filtra conforme estado da ordem */
                 IF ( tt-param.l-inic     = NO and ord-manut.estado = 6) or
                    ( tt-param.l-nao-inic = NO and ord-manut.estado = 1) OR
                    ( tt-param.l-liber    = NO and ord-manut.estado = 2) or
                    ( tt-param.l-term     = NO and ord-manut.estado = 8) OR
                    ( tt-param.l-final    = NO and ord-manut.estado = 7) OR
                    ( tt-param.l-requi    = NO and ord-manut.estado = 5) OR
                    ( tt-param.l-aloc     = NO and ord-manut.estado = 3) OR
                    ( tt-param.l-separ    = NO and ord-manut.estado = 4) THEN NEXT.
       
             END.
             ELSE 
             /* nÆo considera ordens suspensas */
             IF tt-param.l-susp  = NO THEN NEXT.
             
        {mip/esmi0408.i}
        
        assign i-nr-ord   = i-nr-ord + 1
               c-tipo-man = STRING(tipo-manut.cd-tipo)
               c-estado   = if ord-manut.estado-om = 3 then 'S' else
                               entry(ord-manut.estado,c-est-ord).

        if  equipto.cc-codigo <> c-cc-codigo and tt-param.l-quebra-pag = yes then
            page.

        if  ord-manut.nr-ord-produ <> i-nr-ord-produ AND tt-param.l-quebra-pag = yes then
            PAGE.


        if  ord-manut.nr-ord-produ <> i-nr-ord-produ or  line-counter >= 64
            or  line-counter <= 8 then do:

            /*-------- Busca Sintoma, Causa e IntervensÆo da ordem -------*/
            FOR FIRST causa-padrao
                WHERE causa-padrao.cd-causa-padr = ord-manut.cd-causa-padr NO-LOCK:                
            END.            
            FOR FIRST sint-padrao
                WHERE sint-padrao.cd-sint-padr = ord-manut.cd-sint-padr NO-LOCK:
            END.
            FOR FIRST interv-padrao
                WHERE interv-padrao.cd-interv-padr = ord-manut.cd-interv-padr NO-LOCK:
            END.
            ASSIGN cd-causa-padr  = IF AVAIL causa-padrao  THEN causa-padrao.descricao  ELSE "":U
                   cd-sint-padr   = IF AVAIL sint-padrao   THEN sint-padrao.descricao   ELSE "":U 
                   cd-interv-padr = IF AVAIL interv-padrao THEN interv-padrao.descricao ELSE "":U.

            /********** Mostra todos os dados do painel number 2 ********/
            ASSIGN c-equipto   = ord-manut.cd-equipto
                   c-data      = TODAY  /* ord-manut.dt-manut */
                   c-des-man   = ord-manut.des-man-corr
                   dt-ini-cedo = ord-manut.dt-ini-cedo
                   dt-ini-tarde = ord-manut.dt-ini-tarde 
                   tempo-para   = ord-manut.tempo-para
                   c-num-pag    = PAGE-NUMBER.


            DISPLAY ord-manut.nr-ord-prod
                    ord-manut.des-man-corr
                    c-data
                    c-num-pag
                    ord-manut.cd-tag
                    ord-manut.dt-manut
                    tag.descricao
                    ord-manut.cd-manut
                    equipto.cd-equipto
                    equipto.descricao
                    ord-manut.dt-prev
                    c-des-man
                    dt-ini-cedo
                    dt-ini-tarde
                    tempo-para
                    equipto.fm-equipto
                    equipto.cc-codigo
                    equipto.gm-codigo
                    equipto.cod-estabel  
                    equipto.cd-planejado 
                    ord-manut.cd-equip-res                   
                    cd-sint-padr  
                    cd-interv-padr
                    cd-causa-padr
                with frame f-form-1.
                DOWN WITH FRAME f-form-1.

        /** Busca empresa padrÆo **/
        for first param-global fields(empresa-prin) no-lock:
            for FIRST empresa 
                WHERE empresa.ep-codigo = param-global.empresa-prin no-lock:
                ASSIGN c-empresa = empresa.razao-social
                       cl-empresa = c-empresa. /**Atualiza variavel para display da empresa***/
            end.
        end. 
    
        /*   form 2     */
        FOR EACH  ord-taref
            WHERE ord-taref.nr-ord-produ = ord-manut.nr-ord-produ NO-LOCK:

            ASSIGN c-taref-descricao   = ord-taref.descricao.

            DISPLAY ord-taref.cd-tarefa
                      c-taref-descricao

                  with frame f-form-2.
                DOWN WITH FRAME f-form-2.

        END.

        PUT "+----------------------------------------------------------------------------------+" SKIP.
               
        RUN pi-imprime-item.
        RUN pi-imprime-epi.
        RUN pi-imprime-ferramentas.
        RUN pi-imprime-ficha-metodo.
        
        PUT "+------------------------------- NARRATIVA DA ORDEM -------------------------------+" SKIP.
        
        find msg-ord-man where 
             msg-ord-man.nr-ord-produ = ord-manut.nr-ord-produ no-lock no-error.
        if  avail msg-ord-man then do:
            run pi-print-editor (msg-ord-man.msg-exp, 82).
            for each tt-editor:
                PUT "|" tt-editor.conteudo "  |" SKIP.
            end.
        end.
        
        PUT "+---------------------------------- OBSERVA€âES -----------------------------------+" SKIP.
        PUT "|                                                                                  |" SKIP.
        PUT "|                                                                                  |" SKIP.
        PUT "|                                                                                  |" SKIP.
        PUT "|                                                                                  |" SKIP.
        PUT "+----------------------------------------------------------------------------------+" SKIP.
        PUT "+---------------------------------- RECEBIMENTO -----------------------------------+" SKIP.
        PUT "|                                                                                  |" SKIP.
        PUT "|                         _______________________________                          |" SKIP.
        PUT "|                                   SUPERVISOR                                     |" SKIP.
        PUT "|                                                                                  |" SKIP.        
        PUT "|                         DATA: ___/___/_____                                      |" SKIP.
        PUT "|                                                                                  |" SKIP.    
        PUT "+----------------------------------------------------------------------------------+" SKIP.
        
   END.

{utp/ut-liter.i DATA * R }
assign  cl-data = trim(return-value).   
 
        ASSIGN c-des-man = "".
        &if defined (bf_mnt_ems203) &then
        CASE ord-manut.plano-orig:
            WHEN "man-fam" THEN DO:
                FIND FIRST man-fam 
                     WHERE man-fam.fm-equipto = equipto.fm-equipto AND 
                           man-fam.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-fam THEN
                   ASSIGN c-des-man = man-fam.descricao.
            END.
            WHEN "man-equip" THEN DO:
                FIND FIRST man-equip 
                     WHERE man-equip.cd-equipto = equipto.cd-equipto AND 
                           man-equip.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-equip THEN
                   ASSIGN c-des-man = man-equip.descricao.
            END.
            WHEN "man-tag-fam" THEN DO:
                FIND FIRST man-tag-fam 
                     WHERE man-tag-fam.fm-equipto = equipto.fm-equipto AND
                           man-tag-fam.cd-tag     = ord-manut.cd-tag AND
                           man-tag-fam.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-tag-fam THEN
                   ASSIGN c-des-man = man-tag-fam.descricao.
            END.
            WHEN "man-eq-tag" THEN DO:
                FIND FIRST man-eq-tag
                     WHERE man-eq-tag.cd-equipto = equipto.cd-equipto AND 
                           man-eq-tag.cd-tag     = ord-manut.cd-tag AND
                           man-eq-tag.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-equip THEN
                   ASSIGN c-des-man = man-equip.descricao.
            END.
            OTHERWISE DO:
                FIND FIRST manut
                     WHERE manut.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL manut THEN
                   ASSIGN c-des-man = manut.descricao.
            END.
        END CASE.
        &else
        FIND FIRST manut
             WHERE manut.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
        IF AVAIL manut THEN
           ASSIGN c-des-man = manut.descricao.
        &endif
        
        {mip/esmi0408.i1}
   END.

end procedure.
/*************************************************************************/
procedure pi-class-equipto:
    /*classifica‡Æo por equipamento*/
    assign l-prim-imp   = yes
           c-cd-equipto = "?"
           i-nr-ord-produ = ?.

   for each ord-manut
        where ord-manut.cd-equipto   >= tt-param.c-equipto-ini
        and   ord-manut.cd-equipto   <= tt-param.c-equipto-fim
        and   ord-manut.nr-ord-produ >= tt-param.i-ordem-ini
        and   ord-manut.nr-ord-produ <= tt-param.i-ordem-fim
        and   ord-manut.cd-equip-res >= tt-param.c-equipe-ini
        and   ord-manut.cd-equip-res <= tt-param.c-equipe-fim
        and   ord-manut.cd-planejado >= tt-param.c-planeja-ini
        and   ord-manut.cd-planejado <= tt-param.c-planeja-fim
        and   ord-manut.cd-tipo      >= tt-param.i-tp-manut-ini
        and   ord-manut.cd-tipo      <= tt-param.i-tp-manut-fim
        and   ord-manut.prioridade   >= tt-param.i-prioridade-ini
        and   ord-manut.prioridade   <= tt-param.i-prioridade-fim
        AND   ord-manut.dt-manut     >= tt-param.da-data-ini 
        AND   ord-manut.dt-manut     <= tt-param.da-data-fim
        
        /*and
          (if  tt-param.i-cons-parada = 1 then 
               ord-manut.cd-parada    = tt-param.c-cd-parada and
               ord-manut.sequencia    = tt-param.i-sequencia
           else 
              if  tt-param.i-cons-parada = 2 then
                  ord-manut.cd-parada    = "" 
              else 
                  TRUE)
        and   
            (if  tt-param.i-tipo-data = 1 then
                 ord-manut.dt-manut >= tt-param.da-data-ini and 
                 ord-manut.dt-manut <= tt-param.da-data-fim
             else 
                 ord-manut.dt-fecham >= tt-param.da-data-ini and 
                 ord-manut.dt-fecham <= tt-param.da-data-fim) */
        &if defined (bf_mnt_ems203) &then
        and ord-manut.cod-estabel    >=     tt-param.c-estabel-ini
        and ord-manut.cod-estabel    <=     tt-param.c-estabel-fim no-lock,
        &else
        ,first ord-prod
              where ord-prod.nr-ord-produ = ord-manut.nr-ord-produ
              and   ord-prod.cod-estabel >= tt-param.c-estabel-ini
              and   ord-prod.cod-estabel <= tt-param.c-estabel-fim no-lock,
        &endif 
        first tipo-manut
              where tipo-manut.cd-tipo = ord-manut.cd-tipo no-lock,
        first equipto
              where equipto.cd-equipto = ord-manut.cd-equipto
              and   equipto.cc-codigo   >= tt-param.c-cc-ini
              and   equipto.cc-codigo   <= tt-param.c-cc-fim 
              and   equipto.fm-equipto  >= tt-param.c-familia-ini
              and   equipto.fm-equipto  <= tt-param.c-familia-fim no-lock
              break by equipto.cd-equipto 
              by ord-manut.nr-ord-produ:
               
          /* se a ordem nÆo est  suspensa */
         IF  ord-manut.estado-om NE 3  THEN DO:
             /* filtra conforme estado da ordem */
             IF ( tt-param.l-inic     = NO and ord-manut.estado = 6) or
                ( tt-param.l-nao-inic = NO and ord-manut.estado = 1) OR
                ( tt-param.l-liber    = NO and ord-manut.estado = 2) or
                ( tt-param.l-term     = NO and ord-manut.estado = 8) OR
                ( tt-param.l-final    = NO and ord-manut.estado = 7) OR
                ( tt-param.l-requi    = NO and ord-manut.estado = 5) OR
                ( tt-param.l-aloc     = NO and ord-manut.estado = 3) OR
                ( tt-param.l-separ    = NO and ord-manut.estado = 4) THEN NEXT.

         END.
         ELSE 
             /* nÆo considera ordens suspensas */
             IF tt-param.l-susp  = NO THEN NEXT.

        {mip/esmi0408.i}

        assign i-nr-ord   = i-nr-ord + 1
               c-tipo-man = STRING(tipo-manut.cd-tipo)
               c-estado   = if ord-manut.estado-om = 3 then 'S' else
                               entry(ord-manut.estado,c-est-ord).

        if  equipto.cc-codigo <> c-cc-codigo and tt-param.l-quebra-pag = yes then
            page.

        if  ord-manut.nr-ord-produ <> i-nr-ord-produ AND tt-param.l-quebra-pag = yes then
            PAGE.

        if  ord-manut.nr-ord-produ <> i-nr-ord-produ or  line-counter >= 64
            or  line-counter <= 8 then do:

            
            /*-------- Busca Sintoma, Causa e IntervensÆo da ordem -------*/            
            FOR FIRST causa-padrao
                WHERE causa-padrao.cd-causa-padr = ord-manut.cd-causa-padr NO-LOCK:                
            END.
            FOR FIRST sint-padrao
                WHERE sint-padrao.cd-sint-padr = ord-manut.cd-sint-padr NO-LOCK:
            END.
            FOR FIRST interv-padrao
                WHERE interv-padrao.cd-interv-padr = ord-manut.cd-interv-padr NO-LOCK:
            END.
            ASSIGN cd-causa-padr  = IF AVAIL causa-padrao  THEN causa-padrao.descricao  ELSE "":U
                   cd-sint-padr   = IF AVAIL sint-padrao   THEN sint-padrao.descricao   ELSE "":U 
                   cd-interv-padr = IF AVAIL interv-padrao THEN interv-padrao.descricao ELSE "":U.

            /********** Mostra todos os dados do painel number 2 ********/
            ASSIGN c-equipto   = ord-manut.cd-equipto
                   c-data      = TODAY  /* ord-manut.dt-manut */
                   c-des-man   = ord-manut.des-man-corr
                   dt-ini-cedo = ord-manut.dt-ini-cedo
                   dt-ini-tarde = ord-manut.dt-ini-tarde 
                   tempo-para   = ord-manut.tempo-para
                   c-num-pag    = PAGE-NUMBER.
            
            DISPLAY ord-manut.nr-ord-prod
                    ord-manut.des-man-corr
                    c-data
                    c-num-pag
                    ord-manut.cd-tag
                    ord-manut.dt-manut
                    tag.descricao
                    ord-manut.cd-manut
                    equipto.cd-equipto
                    equipto.descricao
                    ord-manut.dt-prev
                    c-des-man
                    dt-ini-cedo
                    dt-ini-tarde
                    tempo-para
                    equipto.fm-equipto
                    equipto.cc-codigo
                    equipto.gm-codigo
                    equipto.cod-estabel  
                    equipto.cd-planejado 
                    ord-manut.cd-equip-res
                    cd-sint-padr  
                    cd-interv-padr
                    cd-causa-padr
                with frame f-form-1.
                DOWN WITH FRAME f-form-1.

        /** Busca empresa padrÆo **/
        for first param-global fields(empresa-prin) no-lock:
            for FIRST empresa 
                WHERE empresa.ep-codigo = param-global.empresa-prin no-lock:
                ASSIGN c-empresa = empresa.razao-social
                       cl-empresa = c-empresa. /**Atualiza variavel para display da empresa***/
            end.
        end. 
    
        /*   form 2     */
        FOR EACH  ord-taref
            WHERE ord-taref.nr-ord-produ = ord-manut.nr-ord-produ NO-LOCK:

            ASSIGN c-taref-descricao   = ord-taref.descricao.

            DISPLAY ord-taref.cd-tarefa
                      c-taref-descricao

                  with frame f-form-2.
                DOWN WITH FRAME f-form-2.

        END.

     /*-------------------------   FORM 3 ---------------------------------*/
        
        PUT "+----------------------------------------------------------------------------------+" SKIP.
        
        RUN pi-imprime-item.
        RUN pi-imprime-epi.
        RUN pi-imprime-ferramentas.
        RUN pi-imprime-ficha-metodo.
        
        PUT "+------------------------------- NARRATIVA DA ORDEM -------------------------------+" SKIP.
        
        find msg-ord-man where 
             msg-ord-man.nr-ord-produ = ord-manut.nr-ord-produ no-lock no-error.
        if  avail msg-ord-man then do:
            run pi-print-editor (msg-ord-man.msg-exp, 82).
            for each tt-editor:
                PUT "|" tt-editor.conteudo "  |" SKIP.
            end.
        end.

        PUT "+----------------------------------------------------------------------------------+" SKIP.
        PUT "+---------------------------------- RECEBIMENTO -----------------------------------+" SKIP.
        PUT "|                                                                                  |" SKIP.
        PUT "|                         _______________________________                          |" SKIP.
        PUT "|                                   SUPERVISOR                                     |" SKIP.
        PUT "|                                                                                  |" SKIP.        
        PUT "|                         DATA: ___/___/_____                                      |" SKIP.
        PUT "|                                                                                  |" SKIP.    
        PUT "+----------------------------------------------------------------------------------+" SKIP.
        
        END.

    {utp/ut-liter.i DATA * R }
    assign  cl-data = trim(return-value).   
 
        ASSIGN c-des-man = "".
        &if defined (bf_mnt_ems203) &then
        CASE ord-manut.plano-orig:
            WHEN "man-fam" THEN DO:
                FIND FIRST man-fam 
                     WHERE man-fam.fm-equipto = equipto.fm-equipto AND 
                           man-fam.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-fam THEN
                   ASSIGN c-des-man = man-fam.descricao.
            END.
            WHEN "man-equip" THEN DO:
                FIND FIRST man-equip 
                     WHERE man-equip.cd-equipto = equipto.cd-equipto AND 
                           man-equip.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-equip THEN
                   ASSIGN c-des-man = man-equip.descricao.
            END.
            WHEN "man-tag-fam" THEN DO:
                FIND FIRST man-tag-fam 
                     WHERE man-tag-fam.fm-equipto = equipto.fm-equipto AND
                           man-tag-fam.cd-tag     = ord-manut.cd-tag AND
                           man-tag-fam.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-tag-fam THEN
                   ASSIGN c-des-man = man-tag-fam.descricao.
            END.
            WHEN "man-eq-tag" THEN DO:
                FIND FIRST man-eq-tag
                     WHERE man-eq-tag.cd-equipto = equipto.cd-equipto AND 
                           man-eq-tag.cd-tag     = ord-manut.cd-tag AND
                           man-eq-tag.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-equip THEN
                   ASSIGN c-des-man = man-equip.descricao.
            END.
            OTHERWISE DO:
                FIND FIRST manut
                     WHERE manut.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL manut THEN
                   ASSIGN c-des-man = manut.descricao.
            END.
        END CASE.
        &else
        FIND FIRST manut
             WHERE manut.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
        IF AVAIL manut THEN
           ASSIGN c-des-man = manut.descricao.
        &endif
        
        {mip/esmi0408.i1}
   END.

end procedure.
/*************************************************************************/
procedure pi-class-desc:
    /*classifica‡Æo pela descri‡Æo do equipamento*/
    assign l-prim-imp     = yes
           c-desc-equipto = "?"
           i-nr-ord-produ = ?. 

   for each ord-manut
        where ord-manut.cd-equipto   >= tt-param.c-equipto-ini
        and   ord-manut.cd-equipto   <= tt-param.c-equipto-fim
        and   ord-manut.nr-ord-produ >= tt-param.i-ordem-ini
        and   ord-manut.nr-ord-produ <= tt-param.i-ordem-fim
        and   ord-manut.cd-equip-res >= tt-param.c-equipe-ini
        and   ord-manut.cd-equip-res <= tt-param.c-equipe-fim
        and   ord-manut.cd-planejado >= tt-param.c-planeja-ini
        and   ord-manut.cd-planejado <= tt-param.c-planeja-fim
        and   ord-manut.cd-tipo      >= tt-param.i-tp-manut-ini
        and   ord-manut.cd-tipo      <= tt-param.i-tp-manut-fim
        and   ord-manut.prioridade   >= tt-param.i-prioridade-ini
        and   ord-manut.prioridade   <= tt-param.i-prioridade-fim
        AND   ord-manut.dt-manut >= tt-param.da-data-ini 
        and   ord-manut.dt-manut <= tt-param.da-data-fim

        /*and
          (if  tt-param.i-cons-parada = 1 then 
               ord-manut.cd-parada    = tt-param.c-cd-parada and
               ord-manut.sequencia    = tt-param.i-sequencia
           else 
              if  tt-param.i-cons-parada = 2 then
                  ord-manut.cd-parada    = ""
              else 
                  TRUE)
        and   
            (if  tt-param.i-tipo-data = 1 then
                 ord-manut.dt-manut >= tt-param.da-data-ini and 
                 ord-manut.dt-manut <= tt-param.da-data-fim
             else 
                 ord-manut.dt-fecham >= tt-param.da-data-ini and 
                 ord-manut.dt-fecham <= tt-param.da-data-fim) */
        &if defined (bf_mnt_ems203) &then
        and ord-manut.cod-estabel    >=     tt-param.c-estabel-ini
        and ord-manut.cod-estabel    <=     tt-param.c-estabel-fim no-lock,
        &else
        ,first ord-prod 
              where ord-prod.nr-ord-produ = ord-manut.nr-ord-produ
              and   ord-prod.cod-estabel >= tt-param.c-estabel-ini
              and   ord-prod.cod-estabel <= tt-param.c-estabel-fim no-lock,
        &endif 
        first tipo-manut fields (cd-tipo tp-manut)
              where tipo-manut.cd-tipo = ord-manut.cd-tipo no-lock,
        first equipto 
              where equipto.cd-equipto = ord-manut.cd-equipto
              and   equipto.cc-codigo   >= tt-param.c-cc-ini
              and   equipto.cc-codigo   <= tt-param.c-cc-fim 
              and   equipto.fm-equipto  >= tt-param.c-familia-ini
              and   equipto.fm-equipto  <= tt-param.c-familia-fim no-lock
              break by equipto.descricao 
                    by ord-manut.nr-ord-produ:

          /* se a ordem nÆo est  suspensa */
         IF  ord-manut.estado-om NE 3  THEN DO:
             /* filtra conforme estado da ordem */
             IF ( tt-param.l-inic     = NO and ord-manut.estado = 6) or
                ( tt-param.l-nao-inic = NO and ord-manut.estado = 1) OR
                ( tt-param.l-liber    = NO and ord-manut.estado = 2) or
                ( tt-param.l-term     = NO and ord-manut.estado = 8) OR
                ( tt-param.l-final    = NO and ord-manut.estado = 7) OR
                ( tt-param.l-requi    = NO and ord-manut.estado = 5) OR
                ( tt-param.l-aloc     = NO and ord-manut.estado = 3) OR
                ( tt-param.l-separ    = NO and ord-manut.estado = 4) THEN NEXT.

         END.
         ELSE 
             /* nÆo considera ordens suspensas */
             IF tt-param.l-susp  = NO THEN NEXT.

        {mip/esmi0408.i}

        assign i-nr-ord   = i-nr-ord + 1
               c-tipo-man = STRING(tipo-manut.cd-tipo)
               c-estado   = if ord-manut.estado-om = 3 then 'S' else
                               entry(ord-manut.estado,c-est-ord).

        if  equipto.cc-codigo <> c-cc-codigo and tt-param.l-quebra-pag = yes then
            page.

        if  ord-manut.nr-ord-produ <> i-nr-ord-produ AND tt-param.l-quebra-pag = yes then
            PAGE.

        if  ord-manut.nr-ord-produ <> i-nr-ord-produ or  line-counter >= 64
            or  line-counter <= 8 then do:
        
            
            /*-------- Busca Sintoma,Causa e IntervensÆo da ordem -------*/
            FOR FIRST causa-padrao
                WHERE causa-padrao.cd-causa-padr = ord-manut.cd-causa-padr NO-LOCK:                
            END.
            FOR FIRST sint-padrao
                WHERE sint-padrao.cd-sint-padr = ord-manut.cd-sint-padr NO-LOCK:
            END.
            FOR FIRST interv-padrao
                WHERE interv-padrao.cd-interv-padr = ord-manut.cd-interv-padr NO-LOCK:
            END.
            ASSIGN cd-causa-padr  = IF AVAIL causa-padrao  THEN causa-padrao.descricao  ELSE "":U
                   cd-sint-padr   = IF AVAIL sint-padrao   THEN sint-padrao.descricao   ELSE "":U 
                   cd-interv-padr = IF AVAIL interv-padrao THEN interv-padrao.descricao ELSE "":U.

            
            /********** Mostra todos os dados do painel number 2 ********/
            ASSIGN c-equipto   = ord-manut.cd-equipto
                   c-data      = TODAY  /* ord-manut.dt-manut */
                   c-des-man   = ord-manut.des-man-corr
                   dt-ini-cedo = ord-manut.dt-ini-cedo
                   dt-ini-tarde = ord-manut.dt-ini-tarde 
                   tempo-para   = ord-manut.tempo-para
                   c-num-pag    = PAGE-NUMBER.
            
            DISPLAY ord-manut.nr-ord-prod
                    ord-manut.des-man-corr
                    c-data
                    c-num-pag
                    ord-manut.cd-tag
                    ord-manut.dt-manut
                    tag.descricao
                    ord-manut.cd-manut
                    equipto.cd-equipto
                    equipto.descricao
                    ord-manut.dt-prev
                    c-des-man
                    dt-ini-cedo
                    dt-ini-tarde
                    tempo-para
                    equipto.fm-equipto
                    equipto.cc-codigo
                    equipto.gm-codigo
                    equipto.cod-estabel  
                    equipto.cd-planejado 
                    ord-manut.cd-equip-res        
                    cd-sint-padr  
                    cd-interv-padr
                    cd-causa-padr
                with frame f-form-1.
                DOWN WITH FRAME f-form-1.

        /** Busca empresa padrÆo **/
        for first param-global fields(empresa-prin) no-lock:
            for FIRST empresa 
                WHERE empresa.ep-codigo = param-global.empresa-prin no-lock:
                ASSIGN c-empresa = empresa.razao-social
                       cl-empresa = c-empresa. /**Atualiza variavel para display da empresa***/
            end.
        end. 
    
        /*   form 2     */
        FOR EACH  ord-taref
            WHERE ord-taref.nr-ord-produ = ord-manut.nr-ord-produ NO-LOCK:

            ASSIGN c-taref-descricao   = ord-taref.descricao.

            DISPLAY ord-taref.cd-tarefa
                      c-taref-descricao

                  with frame f-form-2.
                DOWN WITH FRAME f-form-2.

        END.

     /*--------------------   FORM 3 ----------------------------------*/ 
        
        PUT "+----------------------------------------------------------------------------------+" SKIP.
        
        RUN pi-imprime-item.
        RUN pi-imprime-epi.
        RUN pi-imprime-ferramentas.
        RUN pi-imprime-ficha-metodo.
        
        PUT "+------------------------------- NARRATIVA DA ORDEM -------------------------------+" SKIP.
        
        find msg-ord-man where 
             msg-ord-man.nr-ord-produ = ord-manut.nr-ord-produ no-lock no-error.
        if  avail msg-ord-man then do:
            run pi-print-editor (msg-ord-man.msg-exp, 82).
            for each tt-editor:
                PUT "|" tt-editor.conteudo "  |" SKIP.
            end.
        end.

        PUT "+----------------------------------------------------------------------------------+" SKIP.
        PUT "+---------------------------------- RECEBIMENTO -----------------------------------+" SKIP.
        PUT "|                                                                                  |" SKIP.
        PUT "|                         _______________________________                          |" SKIP.
        PUT "|                                   SUPERVISOR                                     |" SKIP.
        PUT "|                                                                                  |" SKIP.        
        PUT "|                         DATA: ___/___/_____                                      |" SKIP.
        PUT "|                                                                                  |" SKIP.    
        PUT "+----------------------------------------------------------------------------------+" SKIP.
        
   END.

{utp/ut-liter.i DATA * R }
assign  cl-data = trim(return-value).   
 
        ASSIGN c-des-man = "".
        &if defined (bf_mnt_ems203) &then
        CASE ord-manut.plano-orig:
            WHEN "man-fam" THEN DO:
                FIND FIRST man-fam 
                     WHERE man-fam.fm-equipto = equipto.fm-equipto AND 
                           man-fam.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-fam THEN
                   ASSIGN c-des-man = man-fam.descricao.
            END.
            WHEN "man-equip" THEN DO:
                FIND FIRST man-equip 
                     WHERE man-equip.cd-equipto = equipto.cd-equipto AND 
                           man-equip.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-equip THEN
                   ASSIGN c-des-man = man-equip.descricao.
            END.
            WHEN "man-tag-fam" THEN DO:
                FIND FIRST man-tag-fam 
                     WHERE man-tag-fam.fm-equipto = equipto.fm-equipto AND
                           man-tag-fam.cd-tag     = ord-manut.cd-tag AND
                           man-tag-fam.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-tag-fam THEN
                   ASSIGN c-des-man = man-tag-fam.descricao.
            END.
            WHEN "man-eq-tag" THEN DO:
                FIND FIRST man-eq-tag
                     WHERE man-eq-tag.cd-equipto = equipto.cd-equipto AND 
                           man-eq-tag.cd-tag     = ord-manut.cd-tag AND
                           man-eq-tag.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-equip THEN
                   ASSIGN c-des-man = man-equip.descricao.
            END.
            OTHERWISE DO:
                FIND FIRST manut
                     WHERE manut.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL manut THEN
                   ASSIGN c-des-man = manut.descricao.
            END.
        END CASE.
        &else
        FIND FIRST manut
             WHERE manut.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
        IF AVAIL manut THEN
           ASSIGN c-des-man = manut.descricao.
        &endif
        
        {mip/esmi0408.i1}
   END.

end procedure.
/*************************************************************************/

procedure pi-class-planej:
    /*classifica‡Æo pelo planejador da ordem*/
    assign l-prim-imp     = yes
           c-cd-planejado = "?".
           i-nr-ord-produ = ?. 

   for each ord-manut
        where ord-manut.cd-equipto   >= tt-param.c-equipto-ini
        and   ord-manut.cd-equipto   <= tt-param.c-equipto-fim
        and   ord-manut.nr-ord-produ >= tt-param.i-ordem-ini
        and   ord-manut.nr-ord-produ <= tt-param.i-ordem-fim
        and   ord-manut.cd-equip-res >= tt-param.c-equipe-ini
        and   ord-manut.cd-equip-res <= tt-param.c-equipe-fim
        and   ord-manut.cd-planejado >= tt-param.c-planeja-ini
        and   ord-manut.cd-planejado <= tt-param.c-planeja-fim
        and   ord-manut.cd-tipo      >= tt-param.i-tp-manut-ini
        and   ord-manut.cd-tipo      <= tt-param.i-tp-manut-fim
        and   ord-manut.prioridade   >= tt-param.i-prioridade-ini
        and   ord-manut.prioridade   <= tt-param.i-prioridade-fim
        AND   ord-manut.dt-manut >= tt-param.da-data-ini 
        AND ord-manut.dt-manut <= tt-param.da-data-fim
        
        /*and
          (if  tt-param.i-cons-parada = 1 then 
               ord-manut.cd-parada    = tt-param.c-cd-parada and
               ord-manut.sequencia    = tt-param.i-sequencia
           else 
              if  tt-param.i-cons-parada = 2 then
                  ord-manut.cd-parada    = ""
              else 
                  TRUE)
        and
            (if  tt-param.i-tipo-data = 1 then
                 ord-manut.dt-manut >= tt-param.da-data-ini and 
                 ord-manut.dt-manut <= tt-param.da-data-fim
             else 
                 ord-manut.dt-fecham >= tt-param.da-data-ini and 
                 ord-manut.dt-fecham <= tt-param.da-data-fim) */
        &if defined (bf_mnt_ems203) &then
        and ord-manut.cod-estabel    >=     tt-param.c-estabel-ini
        and ord-manut.cod-estabel    <=     tt-param.c-estabel-fim no-lock,
        &else
        ,first ord-prod 
              where ord-prod.nr-ord-produ = ord-manut.nr-ord-produ
              and   ord-prod.cod-estabel >= tt-param.c-estabel-ini
              and   ord-prod.cod-estabel <= tt-param.c-estabel-fim no-lock,
        &endif 
        first tipo-manut 
              where tipo-manut.cd-tipo = ord-manut.cd-tipo no-lock,
        first equipto 
              where equipto.cd-equipto = ord-manut.cd-equipto
              and   equipto.cc-codigo   >= tt-param.c-cc-ini
              and   equipto.cc-codigo   <= tt-param.c-cc-fim 
              and   equipto.fm-equipto  >= tt-param.c-familia-ini
              and   equipto.fm-equipto  <= tt-param.c-familia-fim no-lock
        break by ord-manut.cd-planejado
              by ord-manut.prioridade
              by ord-manut.cd-equipto
              by ord-manut.nr-ord-produ:


          /* se a ordem nÆo est  suspensa */
         IF  ord-manut.estado-om NE 3  THEN DO:
             /* filtra conforme estado da ordem */
             IF ( tt-param.l-inic     = NO and ord-manut.estado = 6) or
                ( tt-param.l-nao-inic = NO and ord-manut.estado = 1) OR
                ( tt-param.l-liber    = NO and ord-manut.estado = 2) or
                ( tt-param.l-term     = NO and ord-manut.estado = 8) OR
                ( tt-param.l-final    = NO and ord-manut.estado = 7) OR
                ( tt-param.l-requi    = NO and ord-manut.estado = 5) OR
                ( tt-param.l-aloc     = NO and ord-manut.estado = 3) OR
                ( tt-param.l-separ    = NO and ord-manut.estado = 4) THEN NEXT.

         END.
         ELSE 
             /* nÆo considera ordens suspensas */
             IF tt-param.l-susp  = NO THEN NEXT.

        {mip/esmi0408.i}

        assign i-nr-ord   = i-nr-ord + 1
               c-tipo-man = STRING(tipo-manut.cd-tipo)
               c-estado   = if ord-manut.estado-om = 3 then 'S' else
                               entry(ord-manut.estado,c-est-ord).

        if  equipto.cc-codigo <> c-cc-codigo and tt-param.l-quebra-pag = yes then
            page.

        if  ord-manut.nr-ord-produ <> i-nr-ord-produ AND tt-param.l-quebra-pag = yes then
            PAGE.
        
        if  ord-manut.nr-ord-produ <> i-nr-ord-produ or  line-counter >= 64
            or  line-counter <= 8 then do:
                                          

      	    /*-------- Busca Sintoma, Causa e IntervensÆo da ordem -------*/
            FOR FIRST causa-padrao
                WHERE causa-padrao.cd-causa-padr = ord-manut.cd-causa-padr NO-LOCK:                
            END.
            FOR FIRST sint-padrao
                WHERE sint-padrao.cd-sint-padr = ord-manut.cd-sint-padr NO-LOCK:
            END.
            FOR FIRST interv-padrao
                WHERE interv-padrao.cd-interv-padr = ord-manut.cd-interv-padr NO-LOCK:
            END.
            ASSIGN cd-causa-padr  = IF AVAIL causa-padrao  THEN causa-padrao.descricao  ELSE "":U
                   cd-sint-padr   = IF AVAIL sint-padrao   THEN sint-padrao.descricao   ELSE "":U 
                   cd-interv-padr = IF AVAIL interv-padrao THEN interv-padrao.descricao ELSE "":U.


            /********** Mostra todos os dados do painel number 2 ********/
            ASSIGN c-equipto   = ord-manut.cd-equipto
                   c-data      = TODAY  /* ord-manut.dt-manut */
                   c-des-man   = ord-manut.des-man-corr
                   dt-ini-cedo = ord-manut.dt-ini-cedo
                   dt-ini-tarde = ord-manut.dt-ini-tarde 
                   tempo-para   = ord-manut.tempo-para
                   c-num-pag    = PAGE-NUMBER.
            
            DISPLAY ord-manut.nr-ord-prod
                    ord-manut.des-man-corr
                    c-data
                    c-num-pag
                    ord-manut.cd-tag
                    ord-manut.dt-manut
                    tag.descricao
                    ord-manut.cd-manut
                    equipto.cd-equipto
                    equipto.descricao
                    ord-manut.dt-prev
                    c-des-man
                    dt-ini-cedo
                    dt-ini-tarde
                    tempo-para
                    equipto.fm-equipto
                    equipto.cc-codigo
                    equipto.gm-codigo
                    equipto.cod-estabel  
                    equipto.cd-planejado 
                    ord-manut.cd-equip-res
                    cd-sint-padr  
                    cd-interv-padr
                    cd-causa-padr
                with frame f-form-1.
                DOWN WITH FRAME f-form-1.

        /** Busca empresa padrÆo **/
        for first param-global fields(empresa-prin) no-lock:
            for FIRST empresa 
                WHERE empresa.ep-codigo = param-global.empresa-prin no-lock:
                ASSIGN c-empresa = empresa.razao-social
                       cl-empresa = c-empresa. /**Atualiza variavel para display da empresa***/
            end.
        end. 
    
        /*   form 2     */
        FOR EACH  ord-taref
            WHERE ord-taref.nr-ord-produ = ord-manut.nr-ord-produ NO-LOCK:

            ASSIGN c-taref-descricao = ord-taref.descricao.

            DISPLAY ord-taref.cd-tarefa
                      c-taref-descricao

                  with frame f-form-2.
                DOWN WITH FRAME f-form-2.

        END.

     /*--------------------------   FORM 3 --------------------*/
        
        PUT "+----------------------------------------------------------------------------------+" SKIP.
        
        RUN pi-imprime-item.
        RUN pi-imprime-epi.
        RUN pi-imprime-ferramentas.
        RUN pi-imprime-ficha-metodo.
        
        PUT "+------------------------------- NARRATIVA DA ORDEM -------------------------------+" SKIP.
        
        find msg-ord-man where 
             msg-ord-man.nr-ord-produ = ord-manut.nr-ord-produ no-lock no-error.
        if  avail msg-ord-man then do:
            run pi-print-editor (msg-ord-man.msg-exp, 82).
            for each tt-editor:
                PUT "|" tt-editor.conteudo "  |" SKIP.
            end.
        end.

        PUT "+----------------------------------------------------------------------------------+" SKIP.
        PUT "+---------------------------------- RECEBIMENTO -----------------------------------+" SKIP.
        PUT "|                                                                                  |" SKIP.
        PUT "|                         _______________________________                          |" SKIP.
        PUT "|                                   SUPERVISOR                                     |" SKIP.
        PUT "|                                                                                  |" SKIP.        
        PUT "|                         DATA: ___/___/_____                                      |" SKIP.
        PUT "|                                                                                  |" SKIP.    
        PUT "+----------------------------------------------------------------------------------+" SKIP.
        
   END.

{utp/ut-liter.i DATA * R }
assign  cl-data = trim(return-value).   
 
        ASSIGN c-des-man = "".
        &if defined (bf_mnt_ems203) &then
        CASE ord-manut.plano-orig:
            WHEN "man-fam" THEN DO:
                FIND FIRST man-fam 
                     WHERE man-fam.fm-equipto = equipto.fm-equipto AND 
                           man-fam.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-fam THEN
                   ASSIGN c-des-man = man-fam.descricao.
            END.
            WHEN "man-equip" THEN DO:
                FIND FIRST man-equip 
                     WHERE man-equip.cd-equipto = equipto.cd-equipto AND 
                           man-equip.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-equip THEN
                   ASSIGN c-des-man = man-equip.descricao.
            END.
            WHEN "man-tag-fam" THEN DO:
                FIND FIRST man-tag-fam 
                     WHERE man-tag-fam.fm-equipto = equipto.fm-equipto AND
                           man-tag-fam.cd-tag     = ord-manut.cd-tag AND
                           man-tag-fam.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-tag-fam THEN
                   ASSIGN c-des-man = man-tag-fam.descricao.
            END.
            WHEN "man-eq-tag" THEN DO:
                FIND FIRST man-eq-tag
                     WHERE man-eq-tag.cd-equipto = equipto.cd-equipto AND 
                           man-eq-tag.cd-tag     = ord-manut.cd-tag AND
                           man-eq-tag.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-equip THEN
                   ASSIGN c-des-man = man-equip.descricao.
            END.
            OTHERWISE DO:
                FIND FIRST manut
                     WHERE manut.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL manut THEN
                   ASSIGN c-des-man = manut.descricao.
            END.
        END CASE.
        &else
        FIND FIRST manut
             WHERE manut.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
        IF AVAIL manut THEN
           ASSIGN c-des-man = manut.descricao.
        &endif
        
        {mip/esmi0408.i1}
   END.

end procedure.
/*************************************************************************/

procedure pi-class-tag:
    /*classifica‡Æo pelo tag da OM*/
    assign l-prim-imp = yes
           c-cd-tag   = "?"
           i-nr-ord-produ = ?. 

  for  each ord-manut
        where ord-manut.cd-equipto   >= tt-param.c-equipto-ini
        and   ord-manut.cd-equipto   <= tt-param.c-equipto-fim
        and   ord-manut.nr-ord-produ >= tt-param.i-ordem-ini
        and   ord-manut.nr-ord-produ <= tt-param.i-ordem-fim
        and   ord-manut.cd-equip-res >= tt-param.c-equipe-ini
        and   ord-manut.cd-equip-res <= tt-param.c-equipe-fim
        and   ord-manut.cd-planejado >= tt-param.c-planeja-ini
        and   ord-manut.cd-planejado <= tt-param.c-planeja-fim
        and   ord-manut.cd-tipo      >= tt-param.i-tp-manut-ini
        and   ord-manut.cd-tipo      <= tt-param.i-tp-manut-fim
        and   ord-manut.prioridade   >= tt-param.i-prioridade-ini
        and   ord-manut.prioridade   <= tt-param.i-prioridade-fim
        AND   ord-manut.dt-manut >= tt-param.da-data-ini 
        and   ord-manut.dt-manut <= tt-param.da-data-fim

        /*and
          (if  tt-param.i-cons-parada = 1 then 
               ord-manut.cd-parada    = tt-param.c-cd-parada and
               ord-manut.sequencia    = tt-param.i-sequencia
           else 
              if  tt-param.i-cons-parada = 2 then
                  ord-manut.cd-parada    = "" 
              else 
                  TRUE)
        and   
            (if  tt-param.i-tipo-data = 1 then
                 ord-manut.dt-manut >= tt-param.da-data-ini and 
                 ord-manut.dt-manut <= tt-param.da-data-fim
             else 
                 ord-manut.dt-fecham >= tt-param.da-data-ini and 
                 ord-manut.dt-fecham <= tt-param.da-data-fim) */
        &if defined (bf_mnt_ems203) &then
        and ord-manut.cod-estabel    >=     tt-param.c-estabel-ini
        and ord-manut.cod-estabel    <=     tt-param.c-estabel-fim no-lock,
        &else
        first ord-prod 
              where ord-prod.nr-ord-produ = ord-manut.nr-ord-produ
              and   ord-prod.cod-estabel >= tt-param.c-estabel-ini
              and   ord-prod.cod-estabel <= tt-param.c-estabel-fim no-lock,
        &endif
        first tipo-manut fields (cd-tipo tp-manut)
              where tipo-manut.cd-tipo = ord-manut.cd-tipo no-lock,
        first equipto 
              where equipto.cd-equipto = ord-manut.cd-equipto
              and   equipto.cc-codigo   >= tt-param.c-cc-ini
              and   equipto.cc-codigo   <= tt-param.c-cc-fim 
              and   equipto.fm-equipto  >= tt-param.c-familia-ini
              and   equipto.fm-equipto  <= tt-param.c-familia-fim no-lock
        break by ord-manut.cd-tag
              by ord-manut.nr-ord-produ
              by ord-manut.cd-equipto: 
                 
                 
          /* se a ordem nÆo est  suspensa */
         IF  ord-manut.estado-om NE 3  THEN DO:
             /* filtra conforme estado da ordem */
             IF ( tt-param.l-inic     = NO and ord-manut.estado = 6) or
                ( tt-param.l-nao-inic = NO and ord-manut.estado = 1) OR
                ( tt-param.l-liber    = NO and ord-manut.estado = 2) or
                ( tt-param.l-term     = NO and ord-manut.estado = 8) OR
                ( tt-param.l-final    = NO and ord-manut.estado = 7) OR
                ( tt-param.l-requi    = NO and ord-manut.estado = 5) OR
                ( tt-param.l-aloc     = NO and ord-manut.estado = 3) OR
                ( tt-param.l-separ    = NO and ord-manut.estado = 4) THEN NEXT.

         END.
         ELSE 
             /* nÆo considera ordens suspensas */
             IF tt-param.l-susp  = NO THEN NEXT.

        {mip/esmi0408.i}

        assign i-nr-ord   = i-nr-ord + 1
               c-tipo-man = STRING(tipo-manut.cd-tipo)
               c-estado   = if ord-manut.estado-om = 3 then 'S' else
                               entry(ord-manut.estado,c-est-ord).

        if  equipto.cc-codigo <> c-cc-codigo and tt-param.l-quebra-pag = yes then
            page.
        
        if  ord-manut.nr-ord-produ <> i-nr-ord-produ AND tt-param.l-quebra-pag = yes then
            PAGE.

        if  ord-manut.nr-ord-produ <> i-nr-ord-produ or  line-counter >= 64
            or  line-counter <= 8 then do:

            
            /*-------- Busca Sintoma, Causa e IntervensÆo da ordem -------*/
            FOR FIRST causa-padrao
                WHERE causa-padrao.cd-causa-padr = ord-manut.cd-causa-padr NO-LOCK:                
            END.
            FOR FIRST sint-padrao
                WHERE sint-padrao.cd-sint-padr = ord-manut.cd-sint-padr NO-LOCK:
            END.
            FOR FIRST interv-padrao
                WHERE interv-padrao.cd-interv-padr = ord-manut.cd-interv-padr NO-LOCK:
            END.
            ASSIGN cd-causa-padr  = IF AVAIL causa-padrao  THEN causa-padrao.descricao  ELSE "":U
                   cd-sint-padr   = IF AVAIL sint-padrao   THEN sint-padrao.descricao   ELSE "":U 
                   cd-interv-padr = IF AVAIL interv-padrao THEN interv-padrao.descricao ELSE "":U.

            /********** Mostra todos os dados do painel number 2 ********/
            ASSIGN c-equipto   = ord-manut.cd-equipto
                   c-data      = TODAY  /* ord-manut.dt-manut */
                   c-des-man   = ord-manut.des-man-corr
                   dt-ini-cedo = ord-manut.dt-ini-cedo
                   dt-ini-tarde = ord-manut.dt-ini-tarde 
                   tempo-para   = ord-manut.tempo-para
                   c-num-pag    = PAGE-NUMBER.
            
            DISPLAY ord-manut.nr-ord-prod
                    ord-manut.des-man-corr
                    c-data
                    c-num-pag
                    ord-manut.cd-tag
                    ord-manut.dt-manut
                    tag.descricao
                    ord-manut.cd-manut
                    equipto.cd-equipto
                    equipto.descricao
                    ord-manut.dt-prev
                    c-des-man
                    dt-ini-cedo
                    dt-ini-tarde
                    tempo-para
                    equipto.fm-equipto
                    equipto.cc-codigo
                    equipto.gm-codigo
                    equipto.cod-estabel  
                    equipto.cd-planejado 
                    ord-manut.cd-equip-res
                    cd-sint-padr  
                    cd-interv-padr
                    cd-causa-padr
                with frame f-form-1.
                DOWN WITH FRAME f-form-1.

        /** Busca empresa padrÆo **/
        for first param-global fields(empresa-prin) no-lock:
            for FIRST empresa 
                WHERE empresa.ep-codigo = param-global.empresa-prin no-lock:
                ASSIGN c-empresa = empresa.razao-social
                       cl-empresa = c-empresa. /**Atualiza variavel para display da empresa***/
            end.
        end. 
    
        /*   form 2     */
        FOR EACH  ord-taref
            WHERE ord-taref.nr-ord-produ = ord-manut.nr-ord-produ NO-LOCK:

            ASSIGN c-taref-descricao   = ord-taref.descricao.

            DISPLAY ord-taref.cd-tarefa
                      c-taref-descricao

                  with frame f-form-2.
                DOWN WITH FRAME f-form-2.

        END.

     /*---------------------------   FORM 3 --------------------------------------*/


        PUT "+----------------------------------------------------------------------------------+" SKIP.
        
        RUN pi-imprime-item.
        RUN pi-imprime-epi.
        RUN pi-imprime-ferramentas.
        RUN pi-imprime-ficha-metodo.
        
        PUT "+------------------------------- NARRATIVA DA ORDEM -------------------------------+" SKIP.
        
        find msg-ord-man where 
             msg-ord-man.nr-ord-produ = ord-manut.nr-ord-produ no-lock no-error.
        if  avail msg-ord-man then do:
            run pi-print-editor (msg-ord-man.msg-exp, 82).
            for each tt-editor:
                PUT "|" tt-editor.conteudo "  |" SKIP.
            end.
        end.

        PUT "+----------------------------------------------------------------------------------+" SKIP.
        PUT "+---------------------------------- RECEBIMENTO -----------------------------------+" SKIP.
        PUT "|                                                                                  |" SKIP.
        PUT "|                         _______________________________                          |" SKIP.
        PUT "|                                   SUPERVISOR                                     |" SKIP.
        PUT "|                                                                                  |" SKIP.        
        PUT "|                         DATA: ___/___/_____                                      |" SKIP.
        PUT "|                                                                                  |" SKIP.    
        PUT "+----------------------------------------------------------------------------------+" SKIP.
        
   END.

{utp/ut-liter.i DATA * R }
assign  cl-data = trim(return-value).   
 
        ASSIGN c-des-man = "".
        &if defined (bf_mnt_ems203) &then
        CASE ord-manut.plano-orig:
            WHEN "man-fam" THEN DO:
                FIND FIRST man-fam 
                     WHERE man-fam.fm-equipto = equipto.fm-equipto AND 
                           man-fam.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-fam THEN
                   ASSIGN c-des-man = man-fam.descricao.
            END.
            WHEN "man-equip" THEN DO:
                FIND FIRST man-equip 
                     WHERE man-equip.cd-equipto = equipto.cd-equipto AND 
                           man-equip.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-equip THEN
                   ASSIGN c-des-man = man-equip.descricao.
            END.
            WHEN "man-tag-fam" THEN DO:
                FIND FIRST man-tag-fam 
                     WHERE man-tag-fam.fm-equipto = equipto.fm-equipto AND
                           man-tag-fam.cd-tag     = ord-manut.cd-tag AND
                           man-tag-fam.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-tag-fam THEN
                   ASSIGN c-des-man = man-tag-fam.descricao.
            END.
            WHEN "man-eq-tag" THEN DO:
                FIND FIRST man-eq-tag
                     WHERE man-eq-tag.cd-equipto = equipto.cd-equipto AND 
                           man-eq-tag.cd-tag     = ord-manut.cd-tag AND
                           man-eq-tag.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-equip THEN
                   ASSIGN c-des-man = man-equip.descricao.
            END.
            OTHERWISE DO:
                FIND FIRST manut
                     WHERE manut.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL manut THEN
                   ASSIGN c-des-man = manut.descricao.
            END.
        END CASE.
        &else
        FIND FIRST manut
             WHERE manut.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
        IF AVAIL manut THEN
           ASSIGN c-des-man = manut.descricao.
        &endif
        
        {mip/esmi0408.i1}
   END.

end procedure.
/*************************************************************************/

procedure pi-class-equipe:
    /*classifica‡Æo pela equipe da om*/
    assign l-prim-imp     = yes
           c-cd-equip-res = "?"
           i-nr-ord-produ = ?. 

   for  each ord-manut
        where ord-manut.cd-equipto   >= tt-param.c-equipto-ini
        and   ord-manut.cd-equipto   <= tt-param.c-equipto-fim
        and   ord-manut.nr-ord-produ >= tt-param.i-ordem-ini
        and   ord-manut.nr-ord-produ <= tt-param.i-ordem-fim
        and   ord-manut.cd-equip-res >= tt-param.c-equipe-ini
        and   ord-manut.cd-equip-res <= tt-param.c-equipe-fim
        and   ord-manut.cd-planejado >= tt-param.c-planeja-ini
        and   ord-manut.cd-planejado <= tt-param.c-planeja-fim
        and   ord-manut.cd-tipo      >= tt-param.i-tp-manut-ini
        and   ord-manut.cd-tipo      <= tt-param.i-tp-manut-fim
        and   ord-manut.prioridade   >= tt-param.i-prioridade-ini
        and   ord-manut.prioridade   <= tt-param.i-prioridade-fim
        AND   ord-manut.dt-manut >= tt-param.da-data-ini 
        and   ord-manut.dt-manut <= tt-param.da-data-fim
        
        /*and
          (if  tt-param.i-cons-parada = 1 then 
               ord-manut.cd-parada    = tt-param.c-cd-parada and
               ord-manut.sequencia    = tt-param.i-sequencia
           else 
              if  tt-param.i-cons-parada = 2 then
                  ord-manut.cd-parada    = "" 
              else 
                  TRUE)
        and
            (if  tt-param.i-tipo-data = 1 then
                 ord-manut.dt-manut >= tt-param.da-data-ini and 
                 ord-manut.dt-manut <= tt-param.da-data-fim
             else 
                 ord-manut.dt-fecham >= tt-param.da-data-ini and 
                 ord-manut.dt-fecham <= tt-param.da-data-fim) */
        &if defined (bf_mnt_ems203) &then
        and ord-manut.cod-estabel    >=     tt-param.c-estabel-ini
        and ord-manut.cod-estabel    <=     tt-param.c-estabel-fim no-lock,
        &else
        ,first ord-prod 
              where ord-prod.nr-ord-produ = ord-manut.nr-ord-produ
              and   ord-prod.cod-estabel >= tt-param.c-estabel-ini
              and   ord-prod.cod-estabel <= tt-param.c-estabel-fim no-lock,
        &endif
        first tipo-manut
              where tipo-manut.cd-tipo = ord-manut.cd-tipo no-lock,
        first equipto 
              where equipto.cd-equipto = ord-manut.cd-equipto
              and   equipto.cc-codigo   >= tt-param.c-cc-ini
              and   equipto.cc-codigo   <= tt-param.c-cc-fim 
              and   equipto.fm-equipto  >= tt-param.c-familia-ini
              and   equipto.fm-equipto  <= tt-param.c-familia-fim no-lock
        break by ord-manut.cd-equip-res
              by ord-manut.prioridade
              by ord-manut.cd-equipto
              by ord-manut.nr-ord-produ:

          /* se a ordem nÆo est  suspensa */
         IF  ord-manut.estado-om NE 3  THEN DO:
             /* filtra conforme estado da ordem */
             IF ( tt-param.l-inic     = NO and ord-manut.estado = 6) or
                ( tt-param.l-nao-inic = NO and ord-manut.estado = 1) OR
                ( tt-param.l-liber    = NO and ord-manut.estado = 2) or
                ( tt-param.l-term     = NO and ord-manut.estado = 8) OR
                ( tt-param.l-final    = NO and ord-manut.estado = 7) OR
                ( tt-param.l-requi    = NO and ord-manut.estado = 5) OR
                ( tt-param.l-aloc     = NO and ord-manut.estado = 3) OR
                ( tt-param.l-separ    = NO and ord-manut.estado = 4) THEN NEXT.

         END.
         ELSE 
             /* nÆo considera ordens suspensas */
             IF tt-param.l-susp  = NO THEN NEXT.

        {mip/esmi0408.i}

        assign i-nr-ord   = i-nr-ord + 1
               c-tipo-man = STRING(tipo-manut.cd-tipo)
               c-estado   = if ord-manut.estado-om = 3 then 'S' else
                               entry(ord-manut.estado,c-est-ord).

        if  equipto.cc-codigo <> c-cc-codigo and tt-param.l-quebra-pag = yes then 
            page.                                                                 

        if  ord-manut.nr-ord-produ <> i-nr-ord-produ AND tt-param.l-quebra-pag = yes then
            PAGE.

        if  ord-manut.nr-ord-produ <> i-nr-ord-produ or  line-counter >= 64
            or  line-counter <= 8 then do:

            
            /*-------- Busca Sintoma, Causa e IntervensÆo da ordem -------*/
            FOR FIRST causa-padrao
                WHERE causa-padrao.cd-causa-padr = ord-manut.cd-causa-padr NO-LOCK:                
            END.
            FOR FIRST sint-padrao
                WHERE sint-padrao.cd-sint-padr = ord-manut.cd-sint-padr NO-LOCK:
            END.
            FOR FIRST interv-padrao
                WHERE interv-padrao.cd-interv-padr = ord-manut.cd-interv-padr NO-LOCK:
            END.
            ASSIGN cd-causa-padr  = IF AVAIL causa-padrao  THEN causa-padrao.descricao  ELSE "":U
                   cd-sint-padr   = IF AVAIL sint-padrao   THEN sint-padrao.descricao   ELSE "":U 
                   cd-interv-padr = IF AVAIL interv-padrao THEN interv-padrao.descricao ELSE "":U.

            /********** Mostra todos os dados do painel number 2 ********/
            ASSIGN c-equipto   = ord-manut.cd-equipto
                   c-data      = TODAY  /* ord-manut.dt-manut */
                   c-des-man   = ord-manut.des-man-corr
                   dt-ini-cedo = ord-manut.dt-ini-cedo
                   dt-ini-tarde = ord-manut.dt-ini-tarde 
                   tempo-para   = ord-manut.tempo-para
                   c-num-pag    = PAGE-NUMBER.
            
            DISPLAY ord-manut.nr-ord-prod
                    ord-manut.des-man-corr
                    c-data
                    c-num-pag
                    ord-manut.cd-tag
                    ord-manut.dt-manut
                    tag.descricao
                    ord-manut.cd-manut
                    equipto.cd-equipto
                    equipto.descricao
                    ord-manut.dt-prev
                    c-des-man
                    dt-ini-cedo
                    dt-ini-tarde
                    tempo-para
                    equipto.fm-equipto
                    equipto.cc-codigo
                    equipto.gm-codigo
                    equipto.cod-estabel  
                    equipto.cd-planejado 
                    ord-manut.cd-equip-res
                    cd-sint-padr  
                    cd-interv-padr
                    cd-causa-padr
                with frame f-form-1.
                DOWN WITH FRAME f-form-1.

        /** Busca empresa padrÆo **/
        for first param-global fields(empresa-prin) no-lock:
            for FIRST empresa 
                WHERE empresa.ep-codigo = param-global.empresa-prin no-lock:
                ASSIGN c-empresa = empresa.razao-social
                       cl-empresa = c-empresa. /**Atualiza variavel para display da empresa***/
            end.
        end. 
    
        /*   form 2     */
        FOR EACH  ord-taref
            WHERE ord-taref.nr-ord-produ = ord-manut.nr-ord-produ NO-LOCK:

            ASSIGN c-taref-descricao   = ord-taref.descricao.

            DISPLAY ord-taref.cd-tarefa
                      c-taref-descricao

                  with frame f-form-2.
                DOWN WITH FRAME f-form-2.

        END.

     /*------------------------   FORM 3 ---------------------------------*/
        
        PUT "+----------------------------------------------------------------------------------+" SKIP.
        
        RUN pi-imprime-item.
        RUN pi-imprime-epi.
        RUN pi-imprime-ferramentas.
        RUN pi-imprime-ficha-metodo.
        
        PUT "+------------------------------- NARRATIVA DA ORDEM -------------------------------+" SKIP.
        
        find msg-ord-man where 
             msg-ord-man.nr-ord-produ = ord-manut.nr-ord-produ no-lock no-error.
        if  avail msg-ord-man then do:
            run pi-print-editor (msg-ord-man.msg-exp, 82).
            for each tt-editor:
                PUT "|" tt-editor.conteudo "  |" SKIP.
            end.
        end.

        PUT "+----------------------------------------------------------------------------------+" SKIP.
        PUT "+---------------------------------- RECEBIMENTO -----------------------------------+" SKIP.
        PUT "|                                                                                  |" SKIP.
        PUT "|                         _______________________________                          |" SKIP.
        PUT "|                                   SUPERVISOR                                     |" SKIP.
        PUT "|                                                                                  |" SKIP.        
        PUT "|                         DATA: ___/___/_____                                      |" SKIP.
        PUT "|                                                                                  |" SKIP.    
        PUT "+----------------------------------------------------------------------------------+" SKIP.
        
   END.

{utp/ut-liter.i DATA * R }
assign  cl-data = trim(return-value).   
 
        ASSIGN c-des-man = "".
        &if defined (bf_mnt_ems203) &then
        CASE ord-manut.plano-orig:
            WHEN "man-fam" THEN DO:
                FIND FIRST man-fam 
                     WHERE man-fam.fm-equipto = equipto.fm-equipto AND 
                           man-fam.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-fam THEN
                   ASSIGN c-des-man = man-fam.descricao.
            END.
            WHEN "man-equip" THEN DO:
                FIND FIRST man-equip 
                     WHERE man-equip.cd-equipto = equipto.cd-equipto AND 
                           man-equip.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-equip THEN
                   ASSIGN c-des-man = man-equip.descricao.
            END.
            WHEN "man-tag-fam" THEN DO:
                FIND FIRST man-tag-fam 
                     WHERE man-tag-fam.fm-equipto = equipto.fm-equipto AND
                           man-tag-fam.cd-tag     = ord-manut.cd-tag AND
                           man-tag-fam.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-tag-fam THEN
                   ASSIGN c-des-man = man-tag-fam.descricao.
            END.
            WHEN "man-eq-tag" THEN DO:
                FIND FIRST man-eq-tag
                     WHERE man-eq-tag.cd-equipto = equipto.cd-equipto AND 
                           man-eq-tag.cd-tag     = ord-manut.cd-tag AND
                           man-eq-tag.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL man-equip THEN
                   ASSIGN c-des-man = man-equip.descricao.
            END.
            OTHERWISE DO:
                FIND FIRST manut
                     WHERE manut.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
                IF AVAIL manut THEN
                   ASSIGN c-des-man = manut.descricao.
            END.
        END CASE.
        &else
        FIND FIRST manut
             WHERE manut.cd-manut   = ord-manut.cd-manut NO-LOCK NO-ERROR.
        IF AVAIL manut THEN
           ASSIGN c-des-man = manut.descricao.
        &endif
        
        {mip/esmi0408.i1}
   END.

end procedure.
/*************************************************************************/
PROCEDURE pi-cria-tt-tag-equipto:
    def input parameter p-cd-tag LIKE tag.cd-tag no-undo.    

    FOR EACH estr-tag 
       WHERE estr-tag.tag-pai = p-cd-tag NO-LOCK,
       FIRST tag 
       WHERE tag.cd-tag = estr-tag.tag-filho NO-LOCK: 
             
        IF CAN-FIND(FIRST tt-tag-equipto
                    WHERE tt-tag-equipto.cd-tag = tag.cd-tag) THEN NEXT.

        IF tag.cod-tipo = 999 THEN DO:
            create tt-tag-equipto.
            assign tt-tag-equipto.cd-tag = tag.cd-tag.
        END.
        ELSE DO: 
            IF l-mmi-tipo-nivel THEN DO:
                create tt-tag-equipto.
                assign tt-tag-equipto.cd-tag = tag.cd-tag.
            END.

            RUN pi-cria-tt-tag-equipto(INPUT tag.cd-tag).
        END.
    END.

END PROCEDURE.


PROCEDURE pi-imprime-item:
    IF tt-param.l-item = NO OR NOT CAN-FIND(FIRST reservas WHERE reservas.nr-ord-produ = ord-manut.nr-ord-produ) THEN 
        RETURN "NOK":U.
        
    /*PUT "|ITEM                                            TAREFA QUANTIDADE                 |" SKIP
        "|----------------------------------------------- ------ ----------                 |" SKIP.*/
        
    PUT
        "|"
        cl-item     FORMAT "x(11)" AT 2
        cl-tarefa   FORMAT "x(6)" AT 50
        cl-quantde    FORMAT "x(10)" AT 57
        "|" AT 84
        
        SKIP
        "|----------------------------------------------- ------ ----------                 |" SKIP.        
        
    FOR EACH reservas NO-LOCK WHERE
             reservas.nr-ord-produ = ord-manut.nr-ord-produ:
        FIND FIRST item NO-LOCK WHERE item.it-codigo = reservas.it-codigo.
        DISP reservas.it-codigo
             item.descricao-1
             reservas.op-codigo
             reservas.quant-orig
            WITH FRAME f-form-item.
        DOWN WITH FRAME f-form-item.
             
    END.
        
END PROCEDURE.

PROCEDURE pi-imprime-epi:
    IF tt-param.l-equip-protecao = NO OR NOT CAN-FIND(FIRST ord-epi WHERE ord-epi.nr-ord-produ = ord-manut.nr-ord-produ) THEN 
        RETURN "NOK":U.
        
    /*PUT "+-------------------------------------- E P I -------------------------------------+" SKIP
        "|EQUIPAMENTO DE PROTE€ÇO INDIVIDUAL              TAREFA QUANTIDADE                 |" SKIP
        "|-------- -------------------------------------- ------ ----------                 |" SKIP.*/
        
    PUT "+-------------------------------------- "
        cl-epi FORMAT "x(5)"
        " -------------------------------------+" AT 46 SKIP
        "|"
        cl-equipamento  FORMAT "x(34)" AT 2
        cl-tarefa       FORMAT "x(6)" AT 50
        cl-quantde      FORMAT "x(10)" AT 57
        "|" AT 84
        
        SKIP
        "|-------- -------------------------------------- ------ ----------                 |" SKIP.
        
        
    FOR EACH ord-epi NO-LOCK WHERE
             ord-epi.nr-ord-produ = ord-manut.nr-ord-produ:
        FIND FIRST epi NO-LOCK WHERE epi.cd-epi = ord-epi.cd-epi.
        DISP ord-epi.cd-epi
             epi.descricao
             ord-epi.cd-tarefa
             ord-epi.qtde-epi
            WITH FRAME f-form-epi.
        DOWN WITH FRAME f-form-epi.
             
    END.
        
END PROCEDURE.

PROCEDURE pi-imprime-ferramentas:
    IF tt-param.l-ferramenta = NO OR NOT CAN-FIND(FIRST ord-ferr WHERE ord-ferr.nr-ord-produ = ord-manut.nr-ord-produ) THEN 
        RETURN "NOK":U.
        
        
    PUT
        "+------------------------------ "
        cl-ferramenta-title FORMAT "x(21)"
        " -----------------------------+" AT 54 SKIP
        "|"
        cl-ferramenta FORMAT "x(11)" AT 2
        cl-tarefa     FORMAT "x(6)" AT 50
        cl-tempo      FORMAT "x(6)" AT 57
        "|" AT 84
        
        SKIP
        "|------- --------------------------------------- ------ ----------                 |" SKIP.
        
    /*PUT "------------------------------ F E R R A M E N T A S ------------------------------|" SKIP
        "|FERRAMENTAS                                     TAREFA TEMPO                      |" SKIP
        "|------- --------------------------------------- ------ ----------                 |" SKIP.*/
        
    FOR EACH ord-ferr NO-LOCK WHERE
             ord-ferr.nr-ord-produ = ord-manut.nr-ord-produ:
        FIND FIRST tp-ferr NO-LOCK WHERE tp-ferr.cd-tp-ferr = ord-ferr.cd-tp-ferr.
        DISP ord-ferr.cd-tp-ferr
             tp-ferr.descricao
             ord-ferr.cd-tarefa
             ord-ferr.tempo
            WITH FRAME f-form-ferramentas.
        DOWN WITH FRAME f-form-ferramentas.
             
    END.
        
END PROCEDURE.


PROCEDURE pi-imprime-ficha-metodo:
    IF tt-param.l-ficha-metodo = NO OR NOT CAN-FIND(FIRST ord-fich-met WHERE ord-fich-met.nr-ord-produ = ord-manut.nr-ord-produ) THEN 
        RETURN "NOK":U.
        
    DEFINE VARIABLE c-narrativa AS CHAR NO-UNDO.
    
    PUT 
    "--------------------------- "
    cl-ficha-met-title FORMAT "x(27)"
    " ---------------------------|" SKIP.

    
        
    FOR EACH ord-fich-met NO-LOCK WHERE
             ord-fich-met.nr-ord-produ = ord-manut.nr-ord-produ:
        FIND FIRST mnt-ficha-metodo NO-LOCK WHERE mnt-ficha-metodo.fi-codigo = ord-fich-met.fi-codigo NO-ERROR.
        
        PUT "|"
            cl-ficha-met  FORMAT "x(16)"
            cl-tarefa     FORMAT "x(6)" AT 71 
            "|" AT 84
            SKIP
            "|----- -------------------------------------------------------------- ------       |" SKIP.
        
        DISP ord-fich-met.fi-codigo
             mnt-ficha-metodo.descricao
             ord-fich-met.cd-tarefa
            WITH FRAME f-form-ficha-metodo.
        DOWN WITH FRAME f-form-ficha-metodo.
        
        PUT "|                                                                                  |" SKIP.
        
        
        RUN pi-formata-texto(INPUT mnt-ficha-metodo.narrativa, INPUT 79).
             
    END.
END PROCEDURE. 


PROCEDURE pi-formata-texto:
    DEFINE INPUT PARAMETER c-texto      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER i-max-length AS INT NO-UNDO.
    
    DEFINE VARIABLE c-char              AS CHAR NO-UNDO.
     
    DEFINE VARIABLE i-count AS INTEGER NO-UNDO.
    DEFINE VARIABLE i-count-length AS INTEGER NO-UNDO.
    
    ASSIGN i-count-length = 0.
    
    PUT UNFORMATTED "| ".
    
    DO i-count = 1 TO LENGTH(c-texto):
        ASSIGN c-char = SUBSTRING(c-texto, i-count, 1).
        IF i-count-length > i-max-length OR c-char = CHR(10) THEN DO:
            PUT UNFORMATTED FILL(" ", i-max-length - i-count-length + 1) + " |" SKIP.
                
            ASSIGN i-count-length = 0.
            IF i-count < LENGTH(c-texto) THEN 
                PUT UNFORMATTED "| ".
                
            IF c-char <> CHR(13)  THEN DO:
                PUT UNFORMATTED "   ".
                ASSIGN i-count-length = 3.
            END.
                
        END.
        
        IF c-char <> CHR(10) AND c-char <> CHR(13) THEN DO:
            PUT UNFORMATTED c-char.
            ASSIGN i-count-length = i-count-length + 1.
            
        END.
    END.
    
    PUT "|" AT 84 SKIP 
        "|                                                                                  |" SKIP.
    
END.

/* fim de programa */

