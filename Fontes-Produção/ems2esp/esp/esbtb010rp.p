/*****************************************************************************
**       Programa: esbtb010rp.p
**       Data....: 24/02/11
**       Autor...: Daniel P. de Lima
**       Objetivo: Exporta Movimento AP
**       Vers∆o..: 1.00.000
*******************************************************************************/

def var c-prog-gerado as character no-undo initial "ESBTB010RP".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.
def new global shared var c-prg-vrs as char no-undo.
def new global shared var c-prg-obj as char no-undo.
run grapi/gr2013.p (input c-prog-gerado, input "2.06.00.000").

DEF VAR i-data AS DATE.
DEF VAR i-data-pag AS DATE.
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
    field l-cargaini as logical
    field d-data-corte as date
.

def buffer bf-tit_ap       for tit_ap.
def buffer bf-movto_tit_ap for movto_tit_ap.

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

def new shared var l-cargaini as logical format "yes/no" label "Caraga Inicial ?" view-as toggle-box.
def new shared var d-data-corte as date format "99/99/9999" label "Data Referencia".

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
def var i-transacao          as int    no-undo.
def var i-cod_cond_pag       as int    no-undo.

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

assign c-programa     = "ESBTB010RP"
       c-versao       = "2.06"
       c-revisao      = ".00.000"
       c-titulo-relat = "Exporta Movimento AP"
       c-sistema      = "".

find first mguni.empresa no-lock
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail mguni.empresa
then
    assign c-empresa = mguni.empresa.razao-social.
else
    assign c-empresa = "".

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
       l-cargaini = tt-param.l-cargaini
       d-data-corte = tt-param.d-data-corte
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

/*Limpa tabela es_mov_ap no SQL*/

FUNCTION fc-transacao RETURNS INT (input p_trans AS CHARACTER):
    case p_trans :
        when "AVCR" /* Acerto Valor a Credito               */ then return 05. /* AVA - Acerto de Valor */
        when "AVDB" /* Acerto Valor a Debito                */ then return 05. /* AVA - Acerto de Valor */
        when "AVMA" /* Acerto Valor a Maior                 */ then return 05. /* AVA - Acerto de Valor */
        when "AVMN" /* Acerto Valor a Menor                 */ then return 05. /* AVA - Acerto de Valor */
        when "ADEM" /* Alteracao Data Emissao               */ then return 00.
        when "ADVN" /* Alteracao Data Vencimento            */ then return 00.
        when "ALNC" /* Alteracao nao Contabil               */ then return 00.
        when "BXSB" /* Baixa por Substituicao               */ then return 00.
        when "BXTE" /* Baixa por Transf Estab               */ then return 00.
        when "BXA"  /* Baixa                                */ then return 02. /* BND - Baixa de documentos */
        when "CPCC" /* Compra Cartao de Credito             */ then return 00.
        when "CVAL" /* Correcao de Valor                    */ then return 17. /* VC+ - Variacao cambial a maior */
        when "CVLP" /* Correcao Valor no Pagto              */ then return 00.
        when "EVCR" /* Estorno Acerto Val Credito           */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EVDB" /* Estorno Acerto Val Debito            */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EVMA" /* Estorno Acerto Val Maior             */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EVMN" /* Estorno Acerto Val Menor             */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EBXS" /* Estorno Baixa por Subst              */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EBTE" /* Estorno Bxa Transf Estab             */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "ECVP" /* Estorno Correcao Val Pagto           */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "ECVL" /* Estorno Correcao Valor               */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EBXA" /* Estorno de Titulo "Estorno de Baixa" */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "EPEF" /* Estorno Pagto Extra Fornecedor       */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "ESND" /* Estorno Subst Nota Dupl              */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "ETRE" /* Estorno Transf Estab                 */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "ETRU" /* Estorno Transf Unid Negoc            */ then return 10. /* CAN - Escritural - Cancelar titulo */
        when "IMCR" /* Implantacao a Credito                */ then return 16. /* NDC - Nota de debito/credito */
        when "IMDB" /* Implantacao a Debito                 */ then return 16. /* NDC - Nota de debito/credito */
        when "IMPL" /* Implantacao                          */ then return 01. /* IMD - Implantacao de documentos */
        when "PGEC" /* Pagto Encontro Contas                */ then return 00.
        when "PECR" /* Pagto Extra Fornecedor CR            */ then return 04. /* PEC - Pagto extra-fornecedor (credito) */
        when "PGEF" /* Pagto Extra Fornecedor               */ then return 03. /* PEF - Pagto extra-fornecedor (debito) */
        when "PFCC" /* PEF Cartao de Credito                */ then return 00.
        when "SBND" /* Subst Nota por Duplicata             */ then return 00. /* SND - Substituicao de Notas por Duplicatas */
        when "TRES" /* Transf Estabelecimento               */ then return 00.
        when "TRUN" /* Transf Unidade Negocio               */ then return 00.
        when "ESTT" /*                                      */ then return 00.
/*
        when "" /*  */ then return 11. /* SUS - Escritural - Sustar cancelamento titulo */
        when "" /*  */ then return 12. /* CSP - Escritural - Cancelar /Sustar Protesto */
        when "" /*  */ then return 13. /* DSV - Escritural - Alterar data de vencimento */
        when "" /*  */ then return 14. /* DSD - Escritural - Alterar data de desconto */
        when "" /*  */ then return 15. /* ALT - Escritural - Alterar outros dados */
        when "" /*  */ then return 18. /* VC- - Variacao cambial a menor */
        when "" /*  */ then return 19. /* IVA - Utilizado na Argentina para iva-liberado contabilizacao */
*/
    end case.
END FUNCTION.

FIND FIRST es_mov_ap NO-LOCK NO-ERROR. 
    IF AVAIL es_mov_ap  THEN  DO:  
        RUN esp/esidelete.i 'es_mov_ap'.
    END.

IF l-cargaini = YES THEN DO:
    ASSIGN i-data = d-data-corte.
    RUN pi-executa-inicial.
END.
ELSE DO:
    ASSIGN i-data = TODAY - 90.
    RUN pi-executa-diario-a. /* TITULOS EM ABERTO */
    RUN pi-executa-diario-b. /* TITULOS PAGOS */
END.

/********************************/
PROCEDURE pi-executa-inicial :
    for each estabelecimento no-lock,
        each movto_tit_ap no-lock use-index mvtttp_estab_dat_trans
       where movto_tit_ap.cod_estab         = estabelecimento.cod_estab
         and movto_tit_ap.dat_transacao     > i-data
         and movto_tit_ap.log_movto_estordo = no,
       first tit_ap no-lock
       where tit_ap.cod_estab          = movto_tit_ap.cod_estab
         and tit_ap.num_id_tit_ap      = movto_tit_ap.num_id_tit_ap
         and tit_ap.log_tit_ap_estordo = no:
        {esp/esbtb010.i}
    END.
END PROCEDURE.

PROCEDURE pi-executa-diario-a :
    /*** TITULOS EM ABERTO ***/
    FOR EACH tit_ap NO-LOCK
       WHERE tit_ap.log_sdo_tit_ap
         and tit_ap.log_tit_ap_estordo = no,
        EACH movto_tit_ap OF tit_ap NO-LOCK
       where movto_tit_ap.log_movto_estordo = no:
        {esp/esbtb010.i}
    END.        
END PROCEDURE.

PROCEDURE pi-executa-diario-b:
    /*** TITULOS PAGOS NO PERIODO ***/
    FOR EACH movto_tit_ap NO-LOCK
       WHERE movto_tit_ap.dat_transacao >= i-data 
         AND not movto_tit_ap.ind_trans_ap_abrev begins "IMPL"
         and movto_tit_ap.log_movto_estordo = no,
        EACH tit_ap OF movto_tit_ap NO-LOCK
       WHERE tit_ap.dat_liquidac_tit_ap >= i-data
         AND tit_ap.dat_liquidac_tit_ap <> 12/31/9999
         and tit_ap.log_tit_ap_estordo   = no
         and tit_ap.cod_espec_docto     <> " ":
        {esp/esbtb010.i}
    END.
END PROCEDURE.


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

    if   tt-param.parametro = yes then

         page stream str-rp.

end.

if  tt-param.parametro then do:

    disp stream str-rp "PAR∂METROS" skip(01) with stream-io frame f-imp-par.
    form 
     l-cargaini colon 18 label "Caraga Inicial ?" format "yes/no"
     with stream-io overlay side-labels row 018 frame f-imp-par.



   disp stream str-rp
     l-cargaini
     with frame f-imp-par.

   disp stream str-rp "CLASSIFICAÄ«O" skip(01) with stream-io frame f-imp-cla.
   disp stream str-rp "   Exporta Movimento AP"
        with stream-io side-labels overlay row 040 frame f-imp-cla.

   put stream str-rp unformatted skip(1) "IMPRESS«O" skip(1).

   put stream str-rp unformatted skip "    " "Destino : " v-cod-destino-impres " - " tt-param.arquivo format "x(40)".
   put stream str-rp unformatted skip "    " "Execuá∆o: " if i-num-ped-exec-rpw = 0 then "On-Line" else "Batch".
   put stream str-rp unformatted skip "    " "Formato : " if tt-param.formato = 1 then "80 colunas" else "132 colunas".
   put stream str-rp unformatted skip "    " "Usu†rio : " tt-param.usuario.

end.

    output stream str-rp close.

procedure pi-print-editor:

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

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

return 'OK'.

/* fim do programa */
