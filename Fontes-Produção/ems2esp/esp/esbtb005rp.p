/*****************************************************************************
**       Programa: esbtb51.p
**       Data....: 22/02/11
**       Autor...: DATASUL S.A.
**       Objetivo: Exporta Conta Contabil
**       Vers∆o..: 1.00.000 - adm
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

define variable c-prog-gerado as character no-undo initial "ESBTB51".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
run grapi/gr2013.p (input c-prog-gerado, input "2.06.00.000").

/****************** DefiniÁ„o de Tabelas Tempor·rias do RelatÛrio **********************/

define temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param
    field destino              as integer
    field arquivo              as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field v_num_tip_aces_usuar as integer
    field ep-codigo            as char
.

def buffer ccusto  for ems5.ccusto.
def buffer empresa for ems5.empresa.

def temp-table tt-conta-contab no-undo
    field cod_empresa    like ccusto.cod_empresa
    field cod_cta_ctbl   like cta_ctbl.cod_cta_ctbl
    field desc-conta     like cta_ctbl.des_tit_ctbl
    field cod_unid_negoc as char format "x(02)"
    field cod_ccusto     like ccusto.cod_ccusto
    field desc-ccusto    like ccusto.des_tit_ctbl
    field id             as char format "x(19)"
    index ix as primary unique id.


/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

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
/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 

/****************** Definiáao de Vari†veis p/ Campos Virtuais do Relat¢rio *******************/ 

/****************** Definiáao de Vari†veis Campo Calculado do Relat¢rio **********************/ 

/****************** Definiáao de Vari†veis do Relat¢rio N∆o Pedidas em Tela ******************/ 

/****************** Definiáao de Vari†veis de Total do Relat¢rio *****************************/ 

/****************** Definiáao de Vari†veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

/***************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/

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

/****************** Definiáao de Forms do Relat¢rio 132 Colunas ***************************************/ 

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
def var c-empresa       as character format "x(40)"      no-undo.
def var c-titulo-relat  as character format "x(50)"      no-undo.
def var i-numper-x      as integer   format "ZZ"         no-undo.
def var da-iniper-x     as date      format "99/99/9999" no-undo.
def var da-fimper-x     as date      format "99/99/9999" no-undo.
def var i-page-size-rel as integer                       no-undo.
def var c-programa      as character format "x(08)"      no-undo.
def var c-versao        as character format "x(04)"      no-undo.
def var c-revisao       as character format "999"        no-undo.
def new shared var c-impressora   as character                      no-undo.
def new shared var c-layout       as character                      no-undo.
def new shared var v_num_count     as integer                       no-undo.
def new shared var c-arq-control   as character                     no-undo.
def new shared var c-sistema       as character format "x(25)"      no-undo.
def new shared var c-rodape        as character                     no-undo.
def new shared buffer b_ped_exec_style for ped_exec.
def new shared buffer b_servid_exec_style for servid_exec.

def new shared stream str-rp.

assign c-programa     = "esbtb51"
       c-versao       = "2.06"
       c-revisao      = ".00.000"
       c-titulo-relat = "Exporta Conta Contabil"
       c-sistema      = "".

find first mguni.empresa no-lock
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
assign c-empresa = if  avail mguni.empresa then mguni.empresa.razao-social else "".

if  tt-param.formato = 1 then do:
form header
    fill("-", 80) format "x(80)" skip
    c-empresa format "x(24)" c-titulo-relat at 30 format "x(30)"
    "Folha:" at 70 page-number(str-rp) at 76 format ">>>>9" skip
    fill("-", 60) format "x(58)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 80 no-labels no-box page-top frame f-cabec-80.
        
form header
    fill("-", 80) format "x(80)" skip
    c-empresa format "x(24)" c-titulo-relat at 30 format "x(30)"
    "Folha:" at 70 page-number(str-rp) at 76 format ">>>>9" skip
    "Per°odo:" i-numper-x at 08 "-"
    da-iniper-x at 14 "to" da-fimper-x
    fill("-", 22) format "x(20)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 80 no-labels no-box page-top frame f-cabper-80.
run grapi/gr2005.p.
form header
    c-rodape format "x(80)"
    with stream-io width 80 no-labels no-box page-bottom frame f-rodape-80.
end. /* tt-param.formato = 1 */ 

if  tt-param.formato = 2 then do:
form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    fill("-", 112) format "x(110)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabec.
form header
    fill("-", 132) format "x(132)" skip
    c-empresa c-titulo-relat at 50
    "Folha:" at 122 page-number(str-rp) at 128 format ">>>>9" skip
    "Periodo:" i-numper-x at 08 "-"
    da-iniper-x at 14 "to" da-fimper-x
    fill("-", 74) format "x(72)" today format "99/99/9999"
    "-" string(time, "HH:MM:SS") skip(1)
    with stream-io width 132 no-labels no-box page-top frame f-cabper.
run grapi/gr2004.p.
form header
    c-rodape format "x(132)"
    with stream-io width 132 no-labels no-box page-bottom frame f-rodape.
end. /* tt-param.formato = 2 */

run grapi/gr2013c.p (input tt-param.destino,
                    input tt-param.arquivo,
                    input tt-param.usuario,
                    input no).

assign i-ep-codigo-usuario = tt-param.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
.

def var l-imprime as logical no-undo.

assign l-imprime = no.
if  tt-param.destino = 1 then
    assign v-cod-destino-impres = "Impressora".
else
    if  tt-param.destino = 2 then
        assign v-cod-destino-impres = "Arquivo".
    else
        assign v-cod-destino-impres = "Terminal".

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

/* gr9020a.p */

    /*Limpa tabela es_conta_ctbl no SQL*/
/*
FIND FIRST es_conta_ctbl NO-LOCK NO-ERROR. 
    IF AVAIL es_conta_ctbl  THEN  DO:  
        RUN esp/esidelete.i 'es_conta_ctbl'.
        RUN pi-executa.
    END.
    ELSE DO:
        RUN pi-executa.
    END.
*/

RUN pi-executa.

    /***  C‡DIGO PARA SA÷DA EM 80 COLUNAS ***/

    if  tt-param.formato = 1 then do:
        view stream str-rp frame f-cabec-80.
        view stream str-rp frame f-rodape-80.
        assign l-imprime = yes.
    end.

    /***  C‡DIGO PARA SA÷DA EM 132 COLUNAS ***/

    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
        assign l-imprime = yes.
    end.

if  l-imprime = no then do:
    if  tt-param.formato = 1 then do:
        view stream str-rp frame f-cabec-80.
        view stream str-rp frame f-rodape-80.
    end.

    if  tt-param.formato = 2 then do:
        view stream str-rp frame f-cabec.
        view stream str-rp frame f-rodape.
    end.
    disp stream str-rp " " with stream-io frame f-nulo.
end.

if  tt-param.destino <> 1 then
    page stream str-rp.
else do:
    if   tt-param.parametro then
         page stream str-rp.
end.

if  tt-param.parametro then do:
   disp stream str-rp "CLASSIFICAÄ«O" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   Exporta Conta Contabil"
        with stream-io side-labels overlay row 040 frame f-imp-cla.

   put stream str-rp unformatted skip(1) "IMPRESS«O" skip(1).

   put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
   put stream str-rp unformatted skip "    " "Execuá∆o: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
   put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
   put stream str-rp unformatted skip "    " "Usu†rio : " tt-param.usuario.
end.
    output stream str-rp close.

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

return 'OK'.

/* fim do programa */

procedure pi-executa :
    for each cta_ctbl no-lock use-index ctactbl_id
       where cta_ctbl.cod_plano_cta_ctbl = "CONTSOC"
/*          and cta_ctbl.cod_cta_ctbl       = <tabela>.ct-codigo */
         and cta_ctbl.dat_inic_valid    <= today
         and cta_ctbl.dat_fim_valid     >= today:
        for each empresa no-lock use-index empresa_id
           where empresa.cod_empresa <> "CAN",
           first estabelecimento no-lock use-index stblcmnt_empresa
           where estabelecimento.cod_empresa = empresa.cod_empresa:
            find first criter_distrib_cta_ctbl no-lock use-index crtrdsta_id
                 where criter_distrib_cta_ctbl.cod_plano_cta_ctbl = cta_ctbl.cod_plano_cta_ctbl
                   and criter_distrib_cta_ctbl.cod_cta_ctbl       = cta_ctbl.cod_cta_ctbl
                   and criter_distrib_cta_ctbl.cod_estab          = estabelecimento.cod_estab
                   and criter_distrib_cta_ctbl.dat_inic_valid    <= today
                   and criter_distrib_cta_ctbl.dat_fim_valid     >= today no-error.
            if  not avail criter_distrib_cta_ctbl or
                criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "N∆o Utiliza" then do:
                run pi-cria-tt(input "000000",
                               input "").
            end.
            else do:
                find first plano_ccusto no-lock use-index plnccst_id
                     where plano_ccusto.cod_empresa     = empresa.cod_empresa
                       and plano_ccusto.dat_inic_valid <= today
                       and plano_ccusto.dat_fim_valid  >= today no-error.

                if  criter_distrib_cta_ctbl.ind_criter_distrib_ccusto = "Utiliza Todos" then
                    for each ccusto no-lock use-index ccusto_id
                       where ccusto.cod_empresa      = plano_ccusto.cod_empresa
                         and ccusto.cod_plano_ccusto = plano_ccusto.cod_plano_ccusto
/*                          and ccusto.cod_ccusto       = */
                         and ccusto.dat_inic_valid  <= today
                         and ccusto.dat_fim_valid   >= today:
                        run pi-cria-tt(input ccusto.cod_ccusto,
                                       input ccusto.des_tit_ctbl).
                    end.
                else do:
                    for each item_lista_ccusto no-lock use-index itmlstcc_id
                       where item_lista_ccusto.cod_estab               = criter_distrib_cta_ctbl.cod_estab
                         and item_lista_ccusto.cod_mapa_distrib_ccusto = criter_distrib_cta_ctbl.cod_mapa_distrib_ccusto
                         and item_lista_ccusto.cod_empresa             = criter_distrib_cta_ctbl.cod_empresa
                         and item_lista_ccusto.cod_plano_ccusto        = plano_ccusto.cod_plano_ccusto,
                        each ccusto no-lock use-index ccusto_id
                       where ccusto.cod_empresa      = item_lista_ccusto.cod_empresa
                         and ccusto.cod_plano_ccusto = item_lista_ccusto.cod_plano_ccusto
                         and ccusto.cod_ccusto       = item_lista_ccusto.cod_ccusto
                         and ccusto.dat_inic_valid  <= today
                         and ccusto.dat_fim_valid   >= today:
                        run pi-cria-tt(input ccusto.cod_ccusto,
                                       input ccusto.des_tit_ctbl).
                    end.
                end.
            end.
        end.
    end.

    for each tt-conta-contab:
        create es_conta_ctbl no-error.
        assign es_conta_ctbl.ep_codigo    = tt-conta-contab.cod_empresa
               es_conta_ctbl.cod_cta_ctbl = tt-conta-contab.cod_cta_ctbl
               es_conta_ctbl.sub_cta_ctbl = "00" + tt-conta-contab.cod_ccusto
               es_conta_ctbl.des_tit_ctbl = tt-conta-contab.desc-conta + " - " + tt-conta-contab.desc-ccusto
               es_conta_ctbl.id           = tt-conta-contab.id.

        assign v-num-reg-lidos = v-num-reg-lidos + 1.
        run pi-acompanhar in h-acomp(input string(v-num-reg-lidos)).
    end.        
end procedure.

procedure pi-cria-tt :
    def input param p-ccusto     as char no-undo.
    def input param p-desc-custo as char no-undo.

    def var c-id as char no-undo.

    assign c-id = empresa.cod_empresa + cta_ctbl.cod_cta_ctbl + "00" + p-ccusto.
    find first tt-conta-contab no-lock
         where tt-conta-contab.id = c-id no-error.
    if  not avail tt-conta-contab then do:
        create tt-conta-contab.
        assign tt-conta-contab.cod_empresa    = empresa.cod_empresa
               tt-conta-contab.cod_cta_ctbl   = cta_ctbl.cod_cta_ctbl
               tt-conta-contab.desc-conta     = cta_ctbl.des_tit_ctbl
               tt-conta-contab.cod_unid_negoc = "00"
               tt-conta-contab.cod_ccusto     = p-ccusto
               tt-conta-contab.desc-ccusto    = p-desc-custo
               tt-conta-contab.id             = c-id.
    end.
end procedure.

procedure pi-print-editor :
    def input param c-editor    as char    no-undo.
    def input param i-len       as integer no-undo.

    def var i-linha  as integer no-undo.
    def var i-aux    as integer no-undo.
    def var c-aux    as char    no-undo.
    def var c-ret    as char    no-undo.

    for each tt-editor:
        delete tt-editor.
    end.

    assign c-ret = chr(255) + chr(255).

    do  while c-editor <> "":
        if  c-editor <> "" then do:
            assign i-aux = index(c-editor, chr(10)).
            if  i-aux > i-len or (i-aux = 0 and length(c-editor) > i-len) then
                assign i-aux = r-index(c-editor, " ", i-len + 1).
            if  i-aux = 0 then
                assign c-aux = substr(c-editor, 1, i-len)
                       c-editor = substr(c-editor, i-len + 1).
            else
                assign c-aux = substr(c-editor, 1, i-aux - 1)
                       c-editor = substr(c-editor, i-aux + 1).
            if  i-len = 0 then
                assign entry(1, c-ret, chr(255)) = c-aux.
            else do:
                assign i-linha = i-linha + 1.
                create tt-editor.
                assign tt-editor.linha    = i-linha
                       tt-editor.conteudo = c-aux.
            end.
        end.
        if  i-len = 0 then
            return c-ret.
    end.
    return c-ret.
end procedure.
