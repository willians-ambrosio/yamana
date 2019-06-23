/*****************************************************************************
**       Programa: ESP033RP
**       Data....: 02/05/2011
**       Autor...: Kraft Consulting
**       Objetivo: Relat½rio de Arquivos nos Itens
**       Vers’o..: 2.00.00.000
*******************************************************************************/
define variable c-prog-gerado as character no-undo initial "ESP03327".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
run grapi/gr2013.p (input c-prog-gerado, input "2.00.00.000").

DEF VAR de-qtd-ord AS DEC NO-UNDO.

/****************** Defini‡Æo de Tabelas Tempor rias do Relat¢rio **********************/

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field i-ferra-abre         as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as char
    field v-cod-tipo-grafico-dv203 as character
    field c-cod-estabel-ini     like item.cod-estabel
    field c-cod-estabel-fim     like item.cod-estabel
    field c-fm-codigo-ini       like item.fm-codigo
    field c-fm-codigo-fim       like item.fm-codigo
    field c-it-codigo-ini       like item.it-codigo
    field c-it-codigo-fim       like item.it-codigo
    field c-cod-localiz-ini     like item.cod-localiz
    field c-cod-localiz-fim     like item.cod-localiz
    field c-demanda-ini         like item.demanda 
    field c-demanda-fim         like item.demanda 
    field c-classif-abc-ini     like item.classif-abc
    field c-classif-abc-fim     like item.classif-abc
    field c-criticidade-ini     like item.criticidade
    field c-criticidade-fim     like item.criticidade
    FIELD c-fm-cod-com-ini      LIKE item.fm-cod-com 
    FIELD c-fm-cod-com-fim      LIKE item.fm-cod-com 
    FIELD dt-implantacao-ini    like item.data-implant
    FIELD dt-implantacao-fim    like item.data-implant
    FIELD rd-tipo-descricao     AS INT
    FIELD c-status-item         AS CHAR.

def temp-table tt-raw-digita
    field raw-digita as raw.

/****************** INCLUDE COM VARIæVEIS GLOBAIS *********************/

def new global shared var i-ep-codigo-usuario  like mguni.empresa.ep-codigo no-undo.
def new Global shared var l-implanta           as logical    init no.
def new Global shared var c-seg-usuario        as char format "x(12)" no-undo.
def new global shared var i-num-ped-exec-rpw  as integer no-undo.   
def new global shared var i-pais-impto-usuario as integer format ">>9" no-undo.
def new global shared var l-rpc as logical no-undo.
def new global shared var r-registro-atual as rowid no-undo.
def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var i-num-ped as integer no-undo.         
def new global shared var v_cdn_empres_usuar   like mguni.empresa.ep-codigo        no-undo.
def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.
def new global shared var h_prog_segur_estab     as handle                   no-undo.
def new global shared var v_cod_grp_usuar_lst    as char                     no-undo.
def new global shared var v_num_tip_aces_usuar   as int                      no-undo.
def new global shared var rw-log-exec            as rowid                    no-undo.


def new global shared var c-dir-spool-servid-exec as CHAR no-undo.
/****************** Defini»ao de Par³metros do Relat½rio *********************/ 

def var v-cod-tipo-grafico-dv203-tmp as char no-undo.
def new shared var v-cod-tipo-grafico-dv203 as character format "x(30)" label "Tipo Grÿfico" view-as combo-box list-items "<<Nenhum>>","Colunas Agrupadas","Colunas Agrupadas 3D","Colunas 3D","Barras  Agrupadas","Barras  Agrupadas 3D","Linhas","Linhas  Com Marcadores","Linhas  3D","Pizza","Pizza   Explodida","Pizza   3D","Pizza   Explodida 3D","Colunas Cilindricas Agrupadas","Barras  Cilindricas Agrupadas","Colunas Cilindricas 3D","Colunas CËnicas Agrupadas","Barras  CËnicas Agrupadas","Colunas CËnicas 3D","Colunas Piramidais Agrupadas","Barras  Piramidais Agrupadas","Colunas Piramidais 3D" .

/****************** Defini»ao de Variÿveis de Sele»’o do Relat½rio *********************/ 

def new shared var c-fm-codigo-ini like item.fm-codigo format "x(8)" initial "" no-undo.
def new shared var c-fm-codigo-fim like item.fm-codigo format "x(8)" initial "ZZZZZZZZ" no-undo.

/****************** Defini»ao de Variÿveis p/ Campos Virtuais do Relat½rio *******************/ 

/****************** Defini»ao de Variÿveis Campo Calculado do Relat½rio **********************/ 

/****************** Defini»ao de Variÿveis do Relat½rio N’o Pedidas em Tela ******************/ 

/****************** Defini»ao de Variÿveis de Total do Relat½rio *****************************/ 

/****************** Defini»ao de Variÿveis dos Calculos do Relat½rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/********************* Fun»’o para tratamento (tradu»’o) de strings *************************/

FUNCTION translate RETURNS CHARACTER (str AS CHAR) FORWARD.

/***************** Defini»ao de Variÿveis de Processamento do Relat½rio *********************/

def var h-acomp              as handle no-undo.
def var h-FunctionLibrary    as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.
def var v-cont-registro      as int    no-undo.
def var v-des-retorno        as char   no-undo.
def var v-des-local-layout   as char   no-undo.

create tt-param.
raw-transfer raw-param to tt-param.

def temp-table tt-editor no-undo
    field linha      as integer
    field conteudo   as character format "x(80)"
    index editor-id is primary unique linha.


def var rw-log-exec                            as rowid no-undo.
def var c-erro-rpc as character format "x(60)" initial " " no-undo.
def var c-erro-aux as character format "x(60)" initial " " no-undo.
def var c-ret-temp as char no-undo.
def var h-servid-rpc as handle no-undo.     
define var c-empresa       as character format "x(40)"      no-undo.
define var c-titulo-relat  as character format "x(50)"      no-undo.
define var i-numper-x      as integer   format "ZZ"         no-undo.
define var da-iniper-x     as date      format "99/99/9999" no-undo.
define var da-fimper-x     as date      format "99/99/9999" no-undo.
define var i-page-size-rel as integer                       no-undo.
define var c-programa      as character format "x(08)"      no-undo.
define var c-versao        as character format "x(04)"      no-undo.
define var c-revisao       as character format "999"        no-undo.
define new shared var c-impressora   as character                      no-undo.
define new shared var c-layout       as character                      no-undo.
define new shared var v_num_count     as integer                       no-undo.
define new shared var c-arq-control   as character                     no-undo.
define new shared var c-sistema       as character format "x(25)"      no-undo.
define new shared var c-rodape        as character                     no-undo.
define new shared buffer b_ped_exec_style for ped_exec.
define new shared buffer b_servid_exec_style for servid_exec.
define new shared stream str-rp.

assign c-programa     = "ESP03327"
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Relat½rio de Arquivos nos Itens"
       c-sistema      = "Relat½rio".


find first mguni.empresa no-lock
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail mguni.empresa
then
    assign c-empresa = mguni.empresa.razao-social.
else
    assign c-empresa = "".

if  tt-param.destino = 3
then assign tt-param.arquivo = session:temp-directory + "ESP033C_" + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".
assign tt-param.arquivo = session:temp-directory + "ESP033C_" + REPLACE(STRING(TODAY,'99/99/9999'),'/','') + REPLACE(STRING(TIME,'hh:mm:ss'),':','') + ".csv".

run grapi/gr2013c.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input yes).

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
       v-cod-tipo-grafico-dv203 = tt-param.v-cod-tipo-grafico-dv203
       c-fm-codigo-ini = tt-param.c-fm-codigo-ini
       c-fm-codigo-fim = tt-param.c-fm-codigo-fim
.

def new global shared var v_cod_arq_gerdoc  as char no-undo.

DEFINE VARIABLE dt-saida-movto   AS DATE        NO-UNDO.
DEFINE VARIABLE dt-entrada-movto AS DATE        NO-UNDO.
DEFINE VARIABLE cTipoMovtoEntr   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTipoMovtoSai    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dtAuxMovto       AS DATE        NO-UNDO.
DEFINE VARIABLE iAuxMovto        AS INTEGER     NO-UNDO.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

FOR EACH ITEM NO-LOCK WHERE 
         ITEM.it-codigo    >= tt-param.c-it-codigo-ini    AND
         ITEM.it-codigo    <= tt-param.c-it-codigo-fim    AND
         item.fm-codigo    >= tt-param.c-fm-codigo-ini    and 
         item.fm-codigo    <= tt-param.c-fm-codigo-fim    AND
         ITEM.fm-cod-com   >= tt-param.c-fm-cod-com-ini   AND
         ITEM.fm-cod-com   <= tt-param.c-fm-cod-com-fim   AND
         ITEM.data-implant >= tt-param.dt-implantacao-ini AND 
         ITEM.data-implant <= tt-param.dt-implantacao-fim,
    EACH item-uni-estab NO-LOCK WHERE
         item-uni-estab.it-codigo    = ITEM.it-codigo AND
         item-uni-estab.cod-estabel >= tt-param.c-cod-estabel-ini AND 
         item-uni-estab.cod-estabel <= tt-param.c-cod-estabel-fim.

    IF LOOKUP(STRING(ITEM.cod-obsoleto),tt-param.c-status-item) = 0 THEN NEXT.

    FIND item-estab WHERE 
         item-estab.it-codigo     = item.it-codigo AND
         item-estab.cod-estabel   = item-uni-estab.cod-estabel NO-LOCK NO-ERROR.

    IF item-uni-estab.cod-localiz < c-cod-localiz-ini OR 
       item-uni-estab.cod-localiz > c-cod-localiz-fim THEN NEXT.

    IF item-uni-estab.demanda     < c-demanda-ini     OR 
       item-uni-estab.demanda     > c-demanda-fim     THEN NEXT.

    IF item-uni-estab.classif-abc < c-classif-abc-ini OR
       item-uni-estab.classif-abc > c-classif-abc-fim THEN NEXT.
        
    IF item-uni-estab.criticidade < c-criticidade-ini OR
       item-uni-estab.criticidade > c-criticidade-fim THEN NEXT.

    FIND FIRST mgesp.ext-item-arq no-lock
         where mgesp.ext-item-arq.it-codigo = item.it-codigo NO-ERROR.

    FIND FIRST es-it-depto WHERE 
               es-it-depto.it-codigo = item.it-codigo  NO-LOCK NO-ERROR.
    IF AVAIL   es-it-depto THEN
    FIND FIRST es-depto    WHERE 
               es-depto.codigo = es-it-depto.cod-depto NO-LOCK NO-ERROR.

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
    
   if  v-num-reg-lidos = 1 then do:
       put stream str-rp unformatted 
           chr(34) translate("Familia")                chr(34)  ";"     /*A*/
           chr(34) translate("Item")                   chr(34)  ";"     /*B*/
           chr(34) translate("Descri‡Æo")              chr(34)  ";"     /*C*/
           chr(34) translate("Libera‡Æo")              chr(34)  ";"     /*D*/
           chr(34) translate("Un")                     chr(34)  ";"     /*E*/
           chr(34) translate("C¢digo Complementar")    chr(34)  ";"     /*F*/
           chr(34) translate("Arq PDF/IIMG Item")      chr(34)  ";"     /*G*/
           chr(34) translate("Depto")                  chr(34)  ";"     /*H*/
           chr(34) translate("Descricao")              chr(34)  ";"     /*I*/
           CHR(34) translate("Estabelec")              chr(34)  ";"     /*J*/
           chr(34) translate("Saldo")                  chr(34)  ";"     /*K*/
           chr(34) translate("Pre‡o Medio p/ Estab")   chr(34)  ";"     /*L*/
           chr(34) translate("Vlr Ult Compra")         chr(34)  ";"     /*M*/
           chr(34) translate("Localiza‡Æo")            chr(34)  ";"     /*N*/
           chr(34) translate("Dep PadrÆo")             chr(34)  ";"     /*O*/
           chr(34) translate("Data Ult Saida")         chr(34)  ";"     /*P*/
           chr(34) translate("Tipo Movto Saida")       chr(34)  ";"     /*Q*/
           chr(34) translate("Data Ult Entr")          chr(34)  ";"     /*R*/
           chr(34) translate("Tipo Movto Entr")        chr(34)  ";"     /*S*/
           chr(34) translate("Ponto Enc")              chr(34)  ";"     /*T*/
           chr(34) translate("Est Segur")              chr(34)  ";"     /*U*/
           chr(34) translate("Tipo Estoq Seg")         chr(34)  ";"     /*V*/
           chr(34) translate("Tempo Segur")            chr(34)  ";"     /*W*/
           chr(34) translate("TRFOR")                  chr(34)  ";"     /*X*/
           chr(34) translate("TRFAB")                  chr(34)  ";"     /*Y*/
           chr(34) translate("TRI")                    chr(34)  ";"     /*Z*/
           chr(34) translate("Tipo Controle")          chr(34)  ";"     /*AA*/
           chr(34) translate("Cons Previsto")          chr(34)  ";"     /*AB*/
           chr(34) translate("Tipo Demanda")           chr(34)  ";"     /*AC*/
           chr(34) translate("Situa‡Æo Item no Estab") chr(34)  ";"     /*AD*/
           chr(34) translate("Criticidade")            chr(34)  ";"     /*AE*/
           chr(34) translate("Classif ABC")            chr(34)  ";"     /*AF*/
           chr(34) translate("Saldo de Ordens")        chr(34)  ";"     /*AG*/
           chr(34) translate("Fam. Comercial")         chr(34)          /*AH*/
        skip.
   end.

   FOR EACH saldo-estoq NO-LOCK WHERE
            saldo-estoq.it-codigo   = ITEM.it-codigo AND
            saldo-estoq.cod-estabel = item-uni-estab.cod-estabel:

       ACCUMULATE saldo-estoq.qtidade-atu (TOTAL).
   END.
   /* Begins 15/10/2018 -------------------------------------------------------------- */
   ASSIGN dt-saida-movto   = ?
          dt-entrada-movto = ?
          cTipoMovtoEntr   = ""
          cTipoMovtoSai    = ""
          dtAuxMovto       = 01/01/0001
          iAuxMovto        = 0. 

   FIND LAST movto-estoq WHERE
             movto-estoq.dt-trans      >   dtAuxMovto                    AND
             movto-estoq.esp-docto     =   21                            AND
             movto-estoq.cod-estabel   =   item-uni-estab.cod-estabel    AND
             movto-estoq.it-codigo     =   ITEM.it-codigo                AND 
             movto-estoq.tipo-trans    =   1 /* Entrada */               NO-LOCK NO-ERROR.
   IF AVAIL movto-estoq THEN
   DO:  
      ASSIGN dtAuxMovto = movto-estoq.dt-trans
             iAuxMovto  = movto-estoq.esp-docto. 

      RELEASE movto-estoq.
   END.

   FIND LAST movto-estoq WHERE
             movto-estoq.dt-trans      >   dtAuxMovto                    AND
             movto-estoq.esp-docto     =   15                            AND
             movto-estoq.cod-estabel   =   item-uni-estab.cod-estabel    AND
             movto-estoq.it-codigo     =   ITEM.it-codigo                AND 
             movto-estoq.tipo-trans    =   1 /* Entrada */               NO-LOCK NO-ERROR.
   IF AVAIL movto-estoq THEN
   DO:  
      ASSIGN dtAuxMovto = movto-estoq.dt-trans
             iAuxMovto  = movto-estoq.esp-docto. 

      RELEASE movto-estoq.
   END.

   IF dtAuxMovto = 01/01/0001 THEN
      ASSIGN dt-entrada-movto  = ?
             cTipoMovtoEntr    = "".
   ELSE
      ASSIGN dt-entrada-movto = dtAuxMovto
             cTipoMovtoEntr   = IF iAuxMovto = 21 THEN "NFE" ELSE "INV".

   ASSIGN dtAuxMovto       = 01/01/0001
          iAuxMovto        = 0. 

   FIND LAST movto-estoq WHERE
             movto-estoq.dt-trans      >  dtAuxMovto                     AND
             movto-estoq.esp-docto     =   21                            AND
             movto-estoq.cod-estabel   =   item-uni-estab.cod-estabel    AND
             movto-estoq.it-codigo     =   ITEM.it-codigo                AND 
             movto-estoq.tipo-trans    =   2 /* Sa¡da */                 NO-LOCK NO-ERROR.
   IF AVAIL movto-estoq THEN
      ASSIGN dtAuxMovto = movto-estoq.dt-trans
             iAuxMovto  = movto-estoq.esp-docto.

   FIND LAST movto-estoq WHERE
             movto-estoq.dt-trans      >  dtAuxMovto                     AND
             movto-estoq.esp-docto     =   28  /*REQ*/                   AND
             movto-estoq.cod-estabel   =   item-uni-estab.cod-estabel    AND
             movto-estoq.it-codigo     =   ITEM.it-codigo                NO-LOCK NO-ERROR.
   IF AVAIL movto-estoq THEN
   DO:  
      ASSIGN dtAuxMovto = movto-estoq.dt-trans
             iAuxMovto  = movto-estoq.esp-docto.
      RELEASE movto-estoq.
   END.

   FIND LAST movto-estoq WHERE
             movto-estoq.dt-trans      >  dtAuxMovto                     AND
             movto-estoq.esp-docto     =   30  /*30*/                    AND
             movto-estoq.cod-estabel   =   item-uni-estab.cod-estabel    AND
             movto-estoq.it-codigo     =   ITEM.it-codigo                NO-LOCK NO-ERROR.
   IF AVAIL movto-estoq THEN
   DO:  
      ASSIGN dtAuxMovto = movto-estoq.dt-trans
             iAuxMovto  = movto-estoq.esp-docto.
      RELEASE movto-estoq.
   END.

   FIND LAST movto-estoq WHERE
             movto-estoq.dt-trans      >  dtAuxMovto                     AND
             movto-estoq.esp-docto     =   15                            AND
             movto-estoq.cod-estabel   =   item-uni-estab.cod-estabel    AND
             movto-estoq.it-codigo     =   ITEM.it-codigo                AND 
             movto-estoq.tipo-trans    =   2 /* Sa¡da */                 NO-LOCK NO-ERROR.
   IF AVAIL movto-estoq THEN
   DO:  
      ASSIGN dtAuxMovto = movto-estoq.dt-trans
             iAuxMovto  = movto-estoq.esp-docto.
      RELEASE movto-estoq.
   END.

   IF dtAuxMovto = 01/01/0001 THEN
      ASSIGN dtAuxMovto     = ?
             cTipoMovtoSai = "".
   ELSE
   DO:
       CASE iAuxMovto:
           WHEN 21 THEN ASSIGN cTipoMovtoSai = "NFE".
           WHEN 28 THEN ASSIGN cTipoMovtoSai = "REQ".
           WHEN 30 THEN ASSIGN cTipoMovtoSai = "RM".
           WHEN 15 THEN ASSIGN cTipoMovtoSai = "INV".         
       END CASE.
    
       ASSIGN dt-saida-movto = dtAuxMovto.
   END.

   /* End 15/10/2018 -------------------------------------------------------------- */

   ASSIGN de-qtd-ord = 0.

  for each prazo-compra use-index ch-saldo where
     prazo-compra.it-codigo   = item.it-codigo and
     prazo-compra.quant-saldo > 0 no-lock:

     find first ordem-compra where
        ordem-compra.numero-ordem = prazo-compra.numero-ordem no-lock no-error.
     if  not avail ordem-compra then
         next.

     IF ordem-compra.cod-estabel <> item-uni-estab.cod-estabel THEN NEXT.

     if ordem-compra.dep-almoxar <> "" then do:
         if not can-find (deposito where deposito.cod-depos = ordem-compra.dep-almoxar 
                                     and deposito.cons-saldo)  
         then  next.
     end.
     if prazo-compra.situacao <> 6 and
        prazo-compra.situacao <> 4 then
        assign de-qtd-ord = de-qtd-ord + prazo-compra.quant-saldo.
  end.

  for each ord-prod use-index item no-lock where
     ord-prod.it-codigo = item.it-codigo and
     ord-prod.qt-ordem - ord-prod.qt-produzida > 0 and
     ord-prod.estado <> 8:
     assign de-qtd-ord = de-qtd-ord + ord-prod.qt-ordem - ord-prod.qt-produzida.
  end.
  
   put stream str-rp unformatted 
       chr(34) item.fm-codigo CHR(160) CHR(34) ";"
       chr(34) "@@@@@" item.it-codigo     chr(34)  ";"
       chr(34) IF tt-param.rd-tipo-descricao = 1 THEN item.desc-item ELSE "'" + replace(replace(ITEM.narrativa,";",","),CHR(10)," ")    CHR(160) CHR(34) ";"
       item.data-liberac format "99/99/9999" ";"
       chr(34) item.un           CHR(160) CHR(34) ";"
       chr(34) item.codigo-refer CHR(160) CHR(34) ";"
       chr(34) (IF AVAIL ext-item-arq THEN mgesp.ext-item-arq.image-pdf ELSE "") CHR(160) CHR(34) 
       
       ";"
       chr(34) (IF AVAIL es-it-depto  THEN mgesp.es-it-depto.cod-depto  ELSE 0) CHR(160) CHR(34) ";"
       chr(34) (IF AVAIL es-depto     THEN mgesp.es-depto.descricao     ELSE "") CHR(160) CHR(34) ";"
       chr(34) item-uni-estab.cod-estabel              CHR(160) CHR(34) ";"
       chr(34) ACCUM TOTAL saldo-estoq.qtidade-atu FORMAT '->>>,>>>,>>9.99' CHR(34) ";"
       chr(34) (IF AVAIL item-estab THEN 
               item-estab.val-unit-ggf-m[1] + 
               item-estab.val-unit-mat-m[1] + 
               item-estab.val-unit-mob-m[1] ELSE 0)    CHR(34) ";"
       chr(34) item-uni-estab.preco-ul-ent             CHR(34) ";"
       chr(34) item-uni-estab.cod-localiz              CHR(160) CHR(34) ";"
       chr(34) item-uni-estab.deposito-pad             CHR(160) CHR(34) ";"
/*        chr(34) item-uni-estab.data-ult-sai          format "99/99/9999" CHR(34) ";" */
/*        chr(34) item-uni-estab.data-ult-ent          format "99/99/9999" CHR(34) ";" */
       /* Begins REV001 - InclusÆo de nova coluna */
       chr(34) dt-saida-movto     format "99/99/9999"  CHR(34) ";"
       chr(34) cTipoMovtoSai                           CHR(34) ";"   
       chr(34) dt-entrada-movto   format "99/99/9999"  CHR(34) ";"
       chr(34) cTipoMovtoEntr                          CHR(34) ";"
       /* End REV001 - InclusÆo nova coluna */
       chr(34) item-uni-estab.ponto-encomenda          CHR(34) ";"
       chr(34) item-uni-estab.quant-segur              CHR(34) ";"
       chr(34) IF item-uni-estab.tipo-est-seg <> 0 THEN {ininc/i05in122.i 4 item-uni-estab.tipo-est-seg} ELSE "" CHR(34) ";"
       chr(34) item-uni-estab.tempo-segur              CHR(34) ";"
       chr(34) item-uni-estab.res-for-comp             CHR(160) CHR(34) ";"
       chr(34) item-uni-estab.ressup-fabri             CHR(160) CHR(34) ";"
       chr(34) item-uni-estab.res-int-comp             CHR(160) CHR(34) ";"
       chr(34) IF item.tipo-contr             <> 0 THEN {ininc/i09in122.i 4 item.tipo-contr}             ELSE "" CHR(34) ";"
       chr(34) item-uni-estab.consumo-prev             CHR(34) ";"
       chr(34) IF item-uni-estab.demanda      <> 0 THEN {ininc/i02in122.i 4 item-uni-estab.demanda}      ELSE "" FORMAT 'x(12)'       CHR(34) ";"
       chr(34) IF item-uni-estab.cod-obsoleto <> 0 THEN {ininc/i17in172.i 4 item-uni-estab.cod-obsoleto} ELSE "" FORMAT 'x(12)'  CHR(34) ";"
       chr(34) IF item-uni-estab.criticidade  <> 0 THEN {ininc/i06in095.i 4 item-uni-estab.criticidade}  ELSE "" FORMAT 'x(12)'  CHR(34) ";"
       chr(34) IF item-uni-estab.classif-abc  <> 0 THEN {ininc/i03in172.i 4 item-uni-estab.classif-abc}  ELSE "" FORMAT 'x(12)'  CHR(34) ";"
       chr(34) de-qtd-ord                              CHR(34) ";"
       CHR(34) ITEM.fm-cod-com                         CHR(34) ";"

       skip.
    
end.



output stream str-rp close.

if opsys <> "win32"
then
return translate("Este programa deve ser executado em sistema operacional Windows").

if v-num-reg-lidos <> 0 
then do:
    if  session:set-wait-state("general") then.
    run pi-cria-tabela-dinamica-excel.
end.
else do:
    if   i-num-ped-exec-rpw = 0 then 
         message translate("NÆo foram gerados dados suficientes para a cria‡Æo do Cen rio dinƒmico.") view-as alert-box information. 
    else return translate("NÆo foram gerados dados suficientes para a cria‡Æo do Cen rio dinƒmico.").
end.

Procedure pi-cria-tabela-dinamica-excel:

    DEF VAR v-ch-Excel         AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-Workbook      AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-PivotCache    AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-PivotTable    AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-Publish       AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-1             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-2             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-3             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-4             AS COM-HANDLE NO-UNDO.
    DEF VAR v-ch-5             AS COM-HANDLE NO-UNDO.
    DEF VAR v-titulo           as char format 'X(60)'.
    DEF VAR i-cont             as int.
    DEF VAR l-error            as log.

    assign v-titulo = "Relat¢rio de Arquivos nos Itens".

    assign v-num-reg-lidos = v-num-reg-lidos + 1. /* Incrementa em 1 para linha de labels*/

    IF NOT VALID-HANDLE(v-ch-Excel)
    then do:
        CREATE "Excel.Application " v-ch-Excel CONNECT NO-ERROR.
        IF ERROR-STATUS:ERROR THEN CREATE "Excel.Application" v-ch-Excel.
    end.

    v-ch-Excel:DisplayAlerts = TRUE.
    v-ch-Excel:VISIBLE = FALSE.

/*     CREATE "Excel.Application"  v-ch-Excel. /* Cria a Planilha */ */
/*     ASSIGN v-ch-Workbook =  v-ch-Excel:Workbooks:ADD("").         */
/*      v-ch-Excel:VISIBLE = FALSE.                                  */
/*                                                                   */
/*                                                                   */
/*         v-ch-Excel:DisplayAlerts = TRUE.                          */
/*                                                                   */



    if i-num-ped-exec-rpw = 0
        then v-ch-Excel:Workbooks:Open(replace(tt-param.arquivo,"/","\")).
        else v-ch-Excel:Workbooks:Open(replace(c-dir-spool-servid-exec,"/","\") + "\" + tt-param.arquivo).
    v-ch-Excel:Sheets(1):NAME = translate("Dados").
    v-ch-Workbook = v-ch-Excel:ActiveWorkbook.

    v-ch-Excel:COLUMNS("K:L"):SELECT.
    v-ch-Excel:Selection:NumberFormat = "###.###.##0,00".

    v-ch-Excel:COLUMNS("B:B"):SELECT.
    v-ch-Excel:Selection:NumberFormat = "@".

    v-ch-Excel:Selection:Replace("@@@@@","'").

    v-ch-Excel:COLUMNS("M:M"):SELECT.
    v-ch-Excel:Selection:NumberFormat = "###.###.##0,0000".
/*     v-ch-Excel:COLUMNS("R:S"):SELECT. */
    v-ch-Excel:COLUMNS("T:U"):SELECT.
    v-ch-Excel:Selection:NumberFormat = "###.###.##0,0000".
/*     v-ch-Excel:COLUMNS("Z:Z"):SELECT. */
    v-ch-Excel:COLUMNS("AB:AB"):SELECT.
    v-ch-Excel:Selection:NumberFormat = "###.###.##0,0000".
/*     v-ch-Excel:COLUMNS("AE:AE"):SELECT. */
    v-ch-Excel:COLUMNS("AG:AG"):SELECT.
    v-ch-Excel:Selection:NumberFormat = "###.###.##0,00".
    v-ch-Excel:COLUMNS("A:AH"):EntireColumn:AUTOFIT().


    v-ch-Excel:COLUMNS("C:C"):SELECT.
    v-ch-Excel:Selection:ColumnWidth = 90.
    v-ch-Excel:Selection:VerticalAlignment = -4160.

                                                                   /*A1:G*/
    v-ch-Excel:ActiveSheet:PivotTableWizard(1,translate("Dados") + "!A1:AH" + string(v-num-reg-lidos),"", v-titulo).


    v-ch-Workbook:ActiveSheet:Cells(1, 1):SELECT.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):ColumnGrand = FALSE.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):RowGrand = FALSE.

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Familia")):ORIENTATION = 3 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss­vel inserir o campo Familia na tabela din³mica, pois o mesmo provocou um estouro no limite de pÿginas da tabela din³mica do Excel." skip
                  "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo Familia selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Familia")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Familia")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Familia")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Familia")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Familia")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Familia")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss­vel inserir o campo Item na tabela din³mica, pois o mesmo provocou um estouro no limite de linhas da tabela din³mica do Excel." skip
                  "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo Item selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Item")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera‡Æo")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss­vel inserir o campo Libera‡Æo na tabela din³mica, pois o mesmo provocou um estouro no limite de linhas da tabela din³mica do Excel." skip
                  "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo Libera‡Æo selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera‡Æo")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera‡Æo")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera‡Æo")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera‡Æo")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera‡Æo")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Libera‡Æo")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss­vel inserir o campo Un na tabela din³mica, pois o mesmo provocou um estouro no limite de linhas da tabela din³mica do Excel." skip
                  "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo Un selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Un")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C¢digo Complementar")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss­vel inserir o campo C¢digo Complementar na tabela din³mica, pois o mesmo provocou um estouro no limite de linhas da tabela din³mica do Excel." skip
                  "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo C¢digo Complementar selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C¢digo Complementar")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C¢digo Complementar")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C¢digo Complementar")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C¢digo Complementar")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C¢digo Complementar")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("C¢digo Complementar")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):ORIENTATION = 1 no-error.

    ASSIGN l-error = no.
    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
          ASSIGN l-error = YES.
          MESSAGE "NÆo foi poss­vel inserir o campo Arq PDF/IIMG Item na tabela din³mica, pois o mesmo provocou um estouro no limite de linhas da tabela din³mica do Excel." skip
                  "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo Arq PDF/IIMG Item selecionado para exibi‡Æo." view-as alert-box information.
        END.
      END.
    END.

    IF NOT l-error THEN DO: 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):Subtotals(01) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):Subtotals(02) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):Subtotals(03) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):Subtotals(04) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):Subtotals(05) = no.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Arq PDF/IIMG Item")):Subtotals(06) = no.

    END. 

    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Descri‡Æo")):ORIENTATION = 4 no-error.

    IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      DO i-cont = 1 TO ERROR-STATUS:NUM-MESSAGES: 
        IF ERROR-STATUS:GET-NUMBER(i-cont) = 5890 AND INDEX(ERROR-STATUS:GET-MESSAGE(i-cont),'ORIENTATION') <> 0 THEN DO: 
           MESSAGE "NÆo foi poss­vel inserir o campo Descri‡Æo na tabela din³mica, pois o mesmo provocou um estouro no limite de dados da tabela din³mica do Excel." skip
                   "Pelo motivo descrito acima a tabela din³mica serÿ gerada sem o campo Descri‡Æo selecionado para exibi‡Æo." view-as alert-box information.
         END.
      END.
    END.


    IF index(v-ch-Excel:Version, "9.") = 0 THEN /* Tem Office >= XP */ 
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields("Contar de "   + translate("Descri‡Æo") + ""):NAME = translate("Conta Descri‡Æo") no-error. 
ELSE /* Tem Office 2000 */
    /*v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields("Contagem de " + translate("Descri‡Æo") + ""):NAME = translate("Conta Descri‡Æo") no-error. */
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields("Count of " + translate("Descri‡Æo") + ""):NAME = translate("Conta Descri‡Æo") no-error. 
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields("Count of "    + translate("Descri‡Æo") + ""):NAME = translate("Conta Descri‡Æo") no-error.
    v-ch-Workbook:ActiveSheet:PivotTables(v-titulo):PivotFields(translate("Conta Descri‡Æo")):FUNCTION = 1 no-error.

    v-ch-Workbook:ActiveSheet:NAME = translate("Cen rio").
      
    if index(v-ch-Excel:Version, "8.") = 0 and /* Tem Office 2000 */
       integer(v-cod-tipo-grafico-dv203) <> 0 /* Nenhum */
    then do:
        v-ch-Workbook:Charts:ADD().
        v-ch-Workbook:ActiveChart:ChartType = integer(v-cod-tipo-grafico-dv203).
        v-ch-Workbook:ActiveSheet:NAME = translate("Grÿfico").
    end.

    if tt-param.i-ferra-abre = 2 /* Microsoft Internet Explorer */
    then do:
        if i-num-ped-exec-rpw <> 0
        then do:
            if v-ch-Workbook:ActiveSheet:NAME = translate("Cen rio")
            then v-ch-Publish = v-ch-Workbook:PublishObjects:ADD(6,replace(c-dir-spool-servid-exec,"/","\") + "\" + ENTRY(1,tt-param.arquivo,".") + ".html", v-ch-Workbook:ActiveSheet:NAME, v-titulo,2) no-error.
            else v-ch-Publish = v-ch-Workbook:PublishObjects:ADD(5,replace(c-dir-spool-servid-exec,"/","\") + "\" + ENTRY(1,tt-param.arquivo,".") + ".html", v-ch-Workbook:ActiveSheet:NAME,"":U,3) no-error.
            v-ch-Publish:PUBLISH(TRUE).
        end.
        else do:
            if v-ch-Workbook:ActiveSheet:NAME = translate("Cen rio")
            then v-ch-Publish = v-ch-Workbook:PublishObjects:ADD(6,ENTRY(1,tt-param.arquivo,".") + ".html", v-ch-Workbook:ActiveSheet:NAME, v-titulo ,2) no-error.
            else v-ch-Publish = v-ch-Workbook:PublishObjects:ADD(5,ENTRY(1,tt-param.arquivo,".") + ".html", v-ch-Workbook:ActiveSheet:NAME,"":U,3) no-error.
            v-ch-Publish:PUBLISH(TRUE).
            if  search(ENTRY(1,tt-param.arquivo,".") + ".html") <> ?
            then do:
                file-info:file-name = search(ENTRY(1,tt-param.arquivo,".") + ".html").
                run pi-gera-marcadagua-html(input file-info:file-name).
                if  tt-param.destino = 3
                then
                    run OpenDocument (input file-info:full-pathname) no-error.
                else message translate("Arquivos gerados com sucesso.") view-as alert-box information.
            end.
        end.
    end.
    if session:set-wait-state("") then.

/*     if tt-param.i-ferra-abre = 1 /* Microsoft Excel */                                                                                                             */
/*     then do:                                                                                                                                                       */
/*         if i-num-ped-exec-rpw = 0                                                                                                                                  */
/*         then do:                                                                                                                                                   */
/*             v-ch-excel:ActiveWorkbook:SaveAs(replace(tt-param.arquivo,".xls", ".csv"),1,"","",no,no,no) no-error.                                                  */
/*             v-ch-excel:quit().                                                                                                                                     */
/*             if  tt-param.destino = 3                                                                                                                               */
/*             then do:                                                                                                                                               */
/*                 file-info:file-name = search(replace(tt-param.arquivo,".xls", ".csv")).                                                                            */
/*                 run OpenDocument (input file-info:full-pathname) no-error.                                                                                         */
/*             end.                                                                                                                                                   */
/*             else message translate("Arquivos gerados com sucesso.") view-as alert-box information.                                                                 */
/*         end.                                                                                                                                                       */
/*         else                                                                                                                                                       */
/*             v-ch-excel:ActiveWorkbook:SaveAs(replace(c-dir-spool-servid-exec,"/","\") + "\" , replace(tt-param.arquivo,".xls", ".csv"),1,"","",no,no,no) no-error. */
/*     end.                                                                                                                                                           */
/*                                                                                                                                                                    */
/*     ASSIGN v_cod_arq_gerdoc = ( IF c-dir-spool-servid-exec <> ""                                                                                                   */
/*                                 THEN (REPLACE(c-dir-spool-servid-exec,"/","\") + "\" + replace(tt-param.arquivo,".csv":U,".xls":U))                                */
/*                                 ELSE replace(tt-param.arquivo,".csv",".xls":U)).                                                                                   */


    if tt-param.i-ferra-abre = 1 /* Microsoft Excel */
    then do:
        if i-num-ped-exec-rpw = 0
        then do:
            
            if  tt-param.destino = 3
            then do:
                v-ch-Excel:VISIBLE = TRUE.
            end.
            else DO:
                ASSIGN tt-param.arquivo = replace(tt-param.arquivo,".csv", ".xls").

               

                v-ch-excel:ActiveWorkbook:SaveAs(tt-param.arquivo,1,"","",no,no,no) no-error.

                v-ch-excel:quit().

/*                 v-ch-excel:ActiveWorkbook:SaveAs(replace(tt-param.arquivo,".xls", ".xlsx"),1,"","",no,no,no) no-error. */
/*                                                                                                                        */
/*                  v-ch-Excel:VISIBLE = FALSE.                                                                           */
/*                                                                                                                        */
/*                                                                                                                        */
/*                 v-ch-excel:quit().                                                                                     */

                message translate("Arquivos gerados com sucesso => " + tt-param.arquivo ) view-as alert-box information.
            END.
        end.
        else
            v-ch-excel:ActiveWorkbook:SaveAs(replace(c-dir-spool-servid-exec,"/","\") + "\" , replace(tt-param.arquivo,".xls", ".csv"),1,"","",no,no,no) no-error.
    end.

    ASSIGN v_cod_arq_gerdoc = ( IF c-dir-spool-servid-exec <> ""
                                THEN (REPLACE(c-dir-spool-servid-exec,"/","\") + "\" + replace(tt-param.arquivo,".csv":U,".xls":U))
                                ELSE replace(tt-param.arquivo,".csv",".xls":U)).


    


    RELEASE OBJECT v-ch-1 no-error.
    RELEASE OBJECT v-ch-2 no-error.
    RELEASE OBJECT v-ch-3 no-error.
    RELEASE OBJECT v-ch-4 no-error.
    RELEASE OBJECT v-ch-5 no-error.
    RELEASE OBJECT v-ch-Publish  no-error.
    RELEASE OBJECT v-ch-Workbook no-error.
    if tt-param.i-ferra-abre = 2 or /* Microsoft Internet Explorer */
       i-num-ped-exec-rpw   <> 0
    then v-ch-excel:quit().
    RELEASE OBJECT v-ch-Excel no-error.

End procedure.

def stream str-origem.
def stream str-destino.
Procedure pi-gera-marcadagua-html:

    def input param p-cod-arq-origem as char.

   
    def var v-mpt  as memptr no-undo.
    def var i-cont as integer no-undo.
    def var v-chr  as char no-undo.
    def var v-tag  as char no-undo.
    def var l-tag  as logical no-undo.
    def var v-cod-linha-process as char no-undo.

    FILE-INFO:FILE-NAME = p-cod-arq-origem.
    SET-SIZE(v-mpt) = FILE-INFO:FILE-SIZE.
    input stream str-origem from value(p-cod-arq-origem) binary no-convert.
    import stream str-origem v-mpt.
    INPUT  STREAM str-origem  CLOSE.

    output stream str-destino to value(session:temp-directory + "cenario.tmp") CONVERT TARGET 'iso8859-1'.

    DO  i-cont = 1 TO FILE-INFO:FILE-SIZE:

        assign v-chr = chr(get-byte(v-mpt,i-cont)).

        IF v-chr = "<":U THEN DO: assign l-Tag = YES. END.

        IF l-Tag THEN assign v-Tag = v-Tag + v-chr.
        ELSE put stream str-destino unformatted v-chr.

        IF v-Tag = "<body>":U THEN DO:
            assign l-Tag = NO. put stream str-destino unformatted v-Tag.
          put stream str-destino unformatted "<a name=" chr(34) "dv300inicio" chr(34) "></a>" skip "<a href=" chr(34) "#dv300" chr(34) "class=nav><p class=MsoNormal align=right style='text-align:right'><font size=" chr(34) 1 chr(34) "face=" chr(34) "Arial" chr(34) "></font></p></a>" skip.
        end.

        IF v-Tag = "</body>":U THEN DO:

            put stream str-destino unformatted v-Tag.
            assign l-Tag = NO 
                   v-Tag = "".
        end.

        IF v-Tag = "<param":U THEN DO:
            assign l-Tag = NO.
            put stream str-destino unformatted v-Tag.
            assign v-Tag = "".
        end.

        IF v-chr = ">":U THEN DO:
            IF l-Tag = YES THEN DO: put stream str-destino unformatted v-Tag. ASSIGN l-tag = NO. END.
            put stream str-destino unformatted SKIP.
            ASSIGN v-Tag = "".
        end.

    end.

    OUTPUT STREAM str-destino CLOSE.

    os-copy value(session:temp-directory + "cenario.tmp") value(p-cod-arq-origem).
    os-delete value(session:temp-directory + "cenario.tmp").

End Procedure.

FUNCTION translate RETURNS CHARACTER (str AS CHAR):
    RETURN str.
END FUNCTION.

    output stream str-rp close.

Procedure OpenDocument:
    def input param c-doc as char no-undo.
    def var c-exec        as char no-undo.
    def var h-Inst        as int  no-undo.
    def var c-arq         as char no-undo.

    run ConverteparaNomeDos (input-output c-doc).

    assign c-exec = fill("x",255).

    run FindExecutableA (input c-doc,
                         input "",
                         input-output c-exec,
                         output h-inst).

    if  h-inst >= 0 and h-inst <= 32
    then do:
        message translate("NÆo foi encontrada associa‡Æo do arquivo (.htm)") skip translate("com nenhum software de visualiza‡Æo. Deseja associar agora?") view-as alert-box question buttons yes-no title translate("Associar arquivo") UPDATE l-associar AS LOGICAL.
        if  l-associar = yes
        then
            run ShellExecuteA (input 0,
                               input "open",
                               input "rundll32.exe",
                               input "shell32.dll,OpenAs_RunDLL "+ c-doc,
                               input "",
                               input 1,
                               output h-inst).

        else
            return.
    end.

    assign c-arq = "'" + string(c-exec) + " " + string(c-doc).
    assign c-arq = replace(c-arq,"'","").

    run WinExec (input c-arq,
                 input 1,
                 output h-inst).

    if  h-inst < 0 or
        h-inst > 32
    then
        return "OK".
    else
    return "NOK".

END PROCEDURE.

PROCEDURE ConverteparaNomeDos:
    def input-output param c-Nome as char no-undo.
    def var iLen   as int  init 255 no-undo.
    def var pShort as memptr.

    repeat:
        set-size(pShort) = iLen.
        run GetShortPathNameA (c-Nome,
                               get-pointer-value(pShort),
                               get-size(pShort),
                               output iLen).
        if get-size(pShort) >= iLen then leave.
        set-size(pShort) = 0.
    end.
    c-Nome = get-string(pShort,1).
END PROCEDURE.

PROCEDURE FindExecutableA EXTERNAL "shell32.dll":
    define input parameter lpFile as char.
    define input parameter lpDirectory as char.
    define input-output parameter lpResult as char.
    define return parameter hInstance as long.
END PROCEDURE.

PROCEDURE ShellExecuteA EXTERNAL "shell32.dll":
    define input parameter hwnd as long.
    define input parameter lpOperation as char.
    define input parameter lpFile as char.
    define input parameter lpParameters as char.
    define input parameter lpDirectory as char.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.
END PROCEDURE.

PROCEDURE GetShortPathNameA EXTERNAL "KERNEL32":
    DEF INPUT  PARAM lpszLongPath  AS CHAR NO-UNDO.
    DEF INPUT  PARAM lpszShortPath AS LONG NO-UNDO.
    DEF INPUT  PARAM cchBuffer     AS LONG NO-UNDO.
    DEF RETURN PARAM lenBuffer     AS LONG NO-UNDO.
END PROCEDURE.

PROCEDURE WinExec EXTERNAL "KERNEL32":
    define input parameter lpszCmdLine as char.
    define input parameter fuCmdShow as LONG.
    define return parameter nTask as LONG.
END PROCEDURE.

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

return 'OK'.

/* fim do programa */

