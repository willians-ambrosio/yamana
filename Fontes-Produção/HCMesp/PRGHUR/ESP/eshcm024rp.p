&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Listar os dados referente a Hora Extra do Funcionario

    Syntax      : Ler e exportar para o excel alguns campos do HCM

    Description : Buscar todos os dados das tabelas de Funcionario, Cargos,
                  Unidade Lotacao, Centro Custo, Eventos e Movimentos.
                  Exportar para um arquivo formato CSV.
    
    Author(s)   : Nilton Oliveira - DSC Consultoria
    Created     : 07/jan/2008
    Notes       : 
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
{include/buffers_RH.i}

{include/i-prgvrs.i ESHCM024RP 2.09.00.000}

/*   ----- DEFINICAO DAS TEMP-TABLE -----   */
define temp-table tt-param no-undo
    field destino               as integer
    field arquivo               as char format "x(35)"
    field usuario               as char format "x(12)"
    field data-exec             as date
    field hora-exec             as integer
    field cdn_empresa_ini       as char
    field cdn_empresa_fim       as char
    field cdn_estab_ini         as char
    field cdn_estab_fim         as char
    field cdn_funcionario_ini   as integer
    field cdn_funcionario_fim   as integer
    field dat_admis_func_ini    as date
    field dat_admis_func_fim    as date
    field dat_desligto_func_ini as date
    field dat_desligto_func_fim as date
    field cod_unid_lotac_ini    as char
    field cod_unid_lotac_fim    as char
    field cod_rh_ccusto_ini     as char
    field cod_rh_ccusto_fim     as char
    field periodo_ini           as char
    field periodo_fim           as char
    field tipo_relat            as int
    field arquivo-csv           as char.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita
   field raw-digita      as raw.

/*---- DEINICAO DE PARAMETROS DO PROGRAMA -----*/
define input param raw-param as raw no-undo.
define input param table for tt-raw-digita.

/*----- DEFINICAO DE VARIAVEIS LOCAIS -----*/
define variable h-acomp         as handle   no-undo. 

define variable i-cont          as integer     no-undo.
define variable i-ano-ini       as integer     no-undo.
define variable i-ano-fim       as integer     no-undo.
define variable i-mes-ini       as integer     no-undo.
define variable i-mes-fim       as integer     no-undo.

define variable i-aux-1         as integer     no-undo.
define variable i-aux-2         as integer     no-undo.
define variable i-aux-3         as integer     no-undo.

define variable da-hist         as date        no-undo.

define variable de-total-func   as dec         no-undo extent 2.
define variable de-vl-sal-ant   as dec         no-undo.

/*----- PREPARACAO DOS PARAMETROS -----*/
create tt-param.
raw-transfer raw-param to tt-param.

for each tt-raw-digita.
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.

/*   ----- INCLUDES PARA O RELATORIO -----   */
{utp/ut-glob.i}
{include/i-rpvar.i}

/*   ----- DEFINICAO DE BUFFER -----   */

/*   ----- FUNCAO RETIRA ACENTO -----   */
{include/i-freeac.i}

/*   ----- DEFINICAO DO EXCEL  -----   */
define variable i-linha                 as integer      no-undo.

define variable chExcelApplication      as com-handle   no-undo.
define variable chWorkbook              as com-handle   no-undo.
define variable chWorksheet             as com-handle   no-undo.
define variable chChart                 as com-handle   no-undo.
define variable chWorksheetRange        as com-handle   no-undo.
define variable iCount                  as integer      no-undo.
define variable iIndex                  as integer      no-undo.
define variable iTotalNumberOfOrders    as integer      no-undo.
define variable iMonth                  as integer      no-undo.
define variable dAnnualQuota            as decimal      no-undo.
define variable dTotalSalesAmount       as decimal      no-undo.
define variable iColumn                 as integer      no-undo initial 3.
define variable cColumn                 as character    no-undo.
define variable cRange                  as character    no-undo.

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
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 6.13
         WIDTH              = 39.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

find first empresa where empresa.ep-codigo = i-ep-codigo-usuario no-lock no-error.

assign c-empresa      = empresa.nome
       c-programa     = "ESHCM024RP"
       c-versao       = "2.09"
       c-revisao      = ".00.000"
       c-titulo-relat = "Exportacao de Dados - Hora Extra"
       c-sistema      = "Especifico HCM"
       c-rodape       = "DATASUL - " + c-sistema + " - " + c-programa + " - V:" + c-versao + c-revisao
       c-rodape       = fill("-", 210 - length(c-rodape)) + c-rodape.

/*----- DIRECIONAMENTO DO RELATORIO -----*/
{include/i-rpcab.i}
{include/i-rpout.i}

/*---- DEFINICAO DE FRAMES -----*/
view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp ("Aguarde, Processando...").

if tt-param.arquivo-csv = '' then
    run pi-exporta-excel.
else
    run pi-exporta-csv.

run pi-finalizar in h-acomp.

{include/i-rpclo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-exporta-csv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-exporta-csv Procedure 
PROCEDURE pi-exporta-csv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

assign i-ano-ini    = int(substr(tt-param.periodo_ini,3,4))
       i-ano-fim    = int(substr(tt-param.periodo_fim,3,4))
       i-mes-ini    = int(substr(tt-param.periodo_ini,1,2))
       i-mes-fim    = int(substr(tt-param.periodo_fim,1,2)).

/* busca evento sintetico 1000 - valor fixo conf. solic. Eliana Martins */
find first event_sint_fp
     where event_sint_fp.cod_event_sint = "1000"
           no-lock no-error.

output to value(tt-param.arquivo-csv).

if tt-param.tipo_relat = 1 then
    put "Emp;Est;Matricula;Nome Completo;CPF;Cargo;Salario;Cod.Unidade Lotacao;Descricao Lotacao;Cod.Centro Custo;Descricao CCusto;Cod.Evento;Desc.Evento;Qtde.Hrs;Valor;Mes;Ano;" skip.
else
    put "Emp;Est;Matricula;Nome Completo;CPF;Cargo;Salario;Cod.Unidade Lotacao;Descricao Lotacao;Cod.Centro Custo;Descricao CCusto;Qtde Total Hrs;Valor Total R$;" skip.

for each funcionario use-index fncnr_py08508
   where funcionario.cdn_empresa        >= tt-param.cdn_empresa_ini
     and funcionario.cdn_empresa        <= tt-param.cdn_empresa_fim
     and funcionario.cod_rh_ccusto      >= tt-param.cod_rh_ccusto_ini
     and funcionario.cod_rh_ccusto      <= tt-param.cod_rh_ccusto_fim
     and funcionario.cdn_estab          >= tt-param.cdn_estab_ini
     and funcionario.cdn_estab          <= tt-param.cdn_estab_fim
     and funcionario.cdn_funcionario    >= tt-param.cdn_funcionario_ini
     and funcionario.cdn_funcionario    <= tt-param.cdn_funcionario_fim
         no-lock:

    /* busca dados para cpf */
    find first rh_pessoa_fisic
         where rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic
               no-lock no-error.
    if not avail rh_pessoa_fisic then next.

    /* busca dados para cargo */
    find first cargo_basic
         where cargo_basic.cdn_cargo_basic = funcionario.cdn_cargo_basic
               no-lock no-error.
    if not avail cargo_basic then next.

    /* busca dados da unid_lotac */
    find first unid_lotac
         where unid_lotac.cod_unid_lotac = funcionario.cod_unid_lotac
               no-lock no-error.
    if not avail unid_lotac then next.

    /* busca dados do ccusto */
    find first rh_ccusto
         where rh_ccusto.cdn_empresa   = funcionario.cdn_empresa
           and rh_ccusto.cod_rh_ccusto = funcionario.cod_rh_ccusto
               no-lock no-error.
    if not avail rh_ccusto then next.

    /* valida admissao */
    if funcionario.dat_admis_func < tt-param.dat_admis_func_ini or
       funcionario.dat_admis_func > tt-param.dat_admis_func_fim then next.

    /* valida desligamento */
    if funcionario.dat_desligto_func < tt-param.dat_desligto_func_ini or
       funcionario.dat_desligto_func > tt-param.dat_desligto_func_fim then next.
    
    /* valida unidade de lotacao */
    if funcionario.cod_unid_lotac < tt-param.cod_unid_lotac_ini or
       funcionario.cod_unid_lotac > tt-param.cod_unid_lotac_fim then next.

    assign de-total-func[1] = 0
           de-total-func[2] = 0
           de-vl-sal-ant    = 0.

    run pi-acompanhar in h-acomp (input 'Funcionario: ' + funcionario.nom_abrev_pessoa_fisic).

    /* busca os eventos com base no sintetico 1000 - hora extra */
    for each movto_calcul_func use-index mvtclclf_py12401
       where movto_calcul_func.cdn_empresa          = funcionario.cdn_empresa
         and movto_calcul_func.cdn_estab            = funcionario.cdn_estab
         and movto_calcul_func.cdn_funcionario      = funcionario.cdn_funcionario
         and movto_calcul_func.idi_tip_fp           = 1 /*normal*/
             no-lock:
        
        assign i-aux-1  = int(string(movto_calcul_func.num_ano_refer_fp,"9999") + string(movto_calcul_func.num_mes_refer_fp,"99"))
               i-aux-2  = int(string(i-ano-ini,"9999") + string(i-mes-ini,"99"))
               i-aux-3  = int(string(i-ano-fim,"9999") + string(i-mes-fim,"99")).

        if i-aux-1 < i-aux-2
        or i-aux-1 > i-aux-3 then next.

        /* inicio alteracao - mineiro - nol - 14/abr/09 */
        if movto_calcul_func.num_mes_refer_fp = 12 then
            assign da-hist = date("31/" + string(movto_calcul_func.num_mes_refer_fp) + "/" + string(movto_calcul_func.num_ano_refer_fp)).
        else
            assign da-hist = date("01/" + string(movto_calcul_func.num_mes_refer_fp + 1) + "/" + string(movto_calcul_func.num_ano_refer_fp)) - 1.

        /* verifica se o salario no periodo */
        for last histor_sal_func of funcionario
           where histor_sal_func.dat_livre_1 <= da-hist
                   no-lock.
        end.

        /* inicio alteracao - mineiro - nol - 02/abr/09 */
        if avail histor_sal_func and histor_sal_func.val_salario_mensal >= de-vl-sal-ant then
            assign de-vl-sal-ant = histor_sal_func.val_salario_mensal.
        /* fim da alteracao - mineiro - nol - 02/abr/09 */
        
        if de-vl-sal-ant = 0 then
            assign de-vl-sal-ant = funcionario.val_salario_atual.

        do i-cont = 1 to 30:
            if movto_calcul_func.cdn_event_fp[i-cont] = "000" then next.

            find first event_fp
                 where event_fp.cdn_event_fp = movto_calcul_func.cdn_event_fp[i-cont]
                       no-lock no-error.
            if not avail event_fp then next.

            if not can-find(first estrut_efp of event_sint_fp
                            where estrut_efp.cdn_efp_det = movto_calcul_func.cdn_event_fp[i-cont]) then next.
            
            if movto_calcul_func.num_mes_refer_fp = 12 then
                assign da-hist = date("31/" + string(movto_calcul_func.num_mes_refer_fp) + "/" + string(movto_calcul_func.num_ano_refer_fp)).
            else
                assign da-hist = date("01/" + string(movto_calcul_func.num_mes_refer_fp + 1) + "/" + string(movto_calcul_func.num_ano_refer_fp)) - 1.

            /* verifica se o salario no periodo */
            for last histor_sal_func of funcionario
               where histor_sal_func.dat_livre_1 <= da-hist
                       no-lock.
            end.

            /* inicio alteracao - mineiro - nol - 02/abr/09 */
            if avail histor_sal_func and histor_sal_func.val_salario_mensal >= de-vl-sal-ant then
                assign de-vl-sal-ant = histor_sal_func.val_salario_mensal.
            /* fim da alteracao - mineiro - nol - 02/abr/09 */

            /* imprime dados */
            if tt-param.tipo_relat = 1 then do:
                export delimiter ';'
                    funcionario.cdn_empresa
                    funcionario.cdn_estab 
                    funcionario.cdn_funcionario
                    fn-free-accent(funcionario.nom_pessoa_fisic)
                    "'" + rh_pessoa_fisic.cod_id_feder
                    fn-free-accent(cargo_basic.des_cargo_basic)
                    de-vl-sal-ant /*if avail histor_sal_func then histor_sal_func.val_salario_mensal else funcionario.val_salario_atual*/
                    funcionario.cod_unid_lotac
                    fn-free-accent(unid_lotac.des_unid_lotac)
                    funcionario.cod_rh_ccusto
                    fn-free-accent(rh_ccusto.des_rh_ccusto)
                    movto_calcul_func.cdn_event_fp[i-cont]
                    fn-free-accent(event_fp.des_event_fp)
                    movto_calcul_func.qtd_unid_event_fp[i-cont]
                    movto_calcul_func.val_calcul_efp[i-cont]
                    movto_calcul_func.num_mes_refer_fp
                    movto_calcul_func.num_ano_refer_fp.

            end.
            else
                assign de-total-func[1] = de-total-func[1] + movto_calcul_func.qtd_unid_event_fp[i-cont]
                       de-total-func[2] = de-total-func[2] + movto_calcul_func.val_calcul_efp[i-cont].
        end.
        /* fim da alteracao - mineiro - nol - 14/abr/09 */
    end.

    if tt-param.tipo_relat = 2 and de-total-func[1] > 0 then do:
        export delimiter ';'
            funcionario.cdn_empresa
            funcionario.cdn_estab 
            funcionario.cdn_funcionario
            fn-free-accent(funcionario.nom_pessoa_fisic)
            "'" + rh_pessoa_fisic.cod_id_feder
            fn-free-accent(cargo_basic.des_cargo_basic)
            funcionario.val_salario_atual
            funcionario.cod_unid_lotac
            fn-free-accent(unid_lotac.des_unid_lotac)
            funcionario.cod_rh_ccusto
            fn-free-accent(rh_ccusto.des_rh_ccusto)
            de-total-func[1]
            de-total-func[2].

    end.
end.

output close.

put skip(2) 'Arquivo gerado em ' + tt-param.arquivo-csv format 'x(132)'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pi-exporta-excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-exporta-excel Procedure 
PROCEDURE pi-exporta-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* abre a aplicacao do excel */
create "Excel.Application" chExcelApplication.
chworkbook  = chexcelapplication:workbooks:add.
chworksheet = chexcelapplication:sheets:item(1).
chExcelApplication:Visible = false.

/* imprime cabecalho */
chWorkSheet:Range("a1"):Value       = "Relatorio de Horas Extras -   " + string(today).
chWorkSheet:Range("a1"):Font:Size   = 12.
chWorkSheet:Range("a1"):Font:Bold   = true.
chWorkSheet:Range("a2"):Value = "Emp".
chWorkSheet:Range("b2"):Value = "Est".
chWorkSheet:Range("c2"):Value = "Matricula".
chWorkSheet:Range("d2"):Value = "Nome Completo".
chWorkSheet:Range("e2"):Value = "CPF".
chWorkSheet:Range("f2"):Value = "Cargo".
chWorkSheet:Range("g2"):Value = "Salario".
chWorkSheet:Range("h2"):Value = "Cod.Unidade Lotacao".
chWorkSheet:Range("i2"):Value = "Descricao Lotacao".
chWorkSheet:Range("j2"):Value = "Cod.Centro Custo".
chWorkSheet:Range("k2"):Value = "Descricao CCusto".

/* redimenciona colunas */
chWorkSheet:Columns("a"):ColumnWidth = 05.
chWorkSheet:Columns("b"):ColumnWidth = 05.
chWorkSheet:Columns("c"):ColumnWidth = 11.
chWorkSheet:Columns("d"):ColumnWidth = 40.
chWorkSheet:Columns("e"):ColumnWidth = 15.
chWorkSheet:Columns("f"):ColumnWidth = 37.
chWorkSheet:Columns("g"):ColumnWidth = 15.
chWorkSheet:Columns("h"):ColumnWidth = 21.
chWorkSheet:Columns("i"):ColumnWidth = 40.
chWorkSheet:Columns("j"):ColumnWidth = 21.
chWorkSheet:Columns("k"):ColumnWidth = 40.

if tt-param.tipo_relat = 1 then do:
    /* imprime cabecalho */
    chWorkSheet:Range("l2"):Value = "Cod.Evento".
    chWorkSheet:Range("m2"):Value = "Desc.Evento".
    chWorkSheet:Range("n2"):Value = "Qtde.Hrs".
    chWorkSheet:Range("o2"):Value = "Valor".
    chWorkSheet:Range("p2"):Value = "Mes".
    chWorkSheet:Range("q2"):Value = "Ano".

    /* redimenciona colunas */
    chWorkSheet:Columns("l"):ColumnWidth = 21.
    chWorkSheet:Columns("m"):ColumnWidth = 40.
    chWorkSheet:Columns("n"):ColumnWidth = 10.
    chWorkSheet:Columns("o"):ColumnWidth = 15.
    chWorkSheet:Columns("p"):ColumnWidth = 5.
    chWorkSheet:Columns("q"):ColumnWidth = 5.
end.
else do:
    /* imprime cabecalho */
    chWorkSheet:Range("l2"):Value = "Qtde Total Hrs".
    chWorkSheet:Range("m2"):Value = "Valor Total R$".

    /* redimenciona colunas */
    chWorkSheet:Columns("l"):ColumnWidth = 20.
    chWorkSheet:Columns("m"):ColumnWidth = 20.
end.

chWorkSheet:Range("a2:q2"):Font:Bold = true.

assign i-linha      = 3
       i-ano-ini    = int(substr(tt-param.periodo_ini,3,4))
       i-ano-fim    = int(substr(tt-param.periodo_fim,3,4))
       i-mes-ini    = int(substr(tt-param.periodo_ini,1,2))
       i-mes-fim    = int(substr(tt-param.periodo_fim,1,2)).

/* busca evento sintetico 1000 - valor fixo conf. solic. Eliana Martins */
find first event_sint_fp
     where event_sint_fp.cod_event_sint = "1000"
           no-lock no-error.

for each funcionario use-index fncnr_py08508
   where funcionario.cdn_empresa        >= tt-param.cdn_empresa_ini
     and funcionario.cdn_empresa        <= tt-param.cdn_empresa_fim
     and funcionario.cod_rh_ccusto      >= tt-param.cod_rh_ccusto_ini
     and funcionario.cod_rh_ccusto      <= tt-param.cod_rh_ccusto_fim
     and funcionario.cdn_estab          >= tt-param.cdn_estab_ini
     and funcionario.cdn_estab          <= tt-param.cdn_estab_fim
     and funcionario.cdn_funcionario    >= tt-param.cdn_funcionario_ini
     and funcionario.cdn_funcionario    <= tt-param.cdn_funcionario_fim
         no-lock:

    /* busca dados para cpf */
    find first rh_pessoa_fisic
         where rh_pessoa_fisic.num_pessoa_fisic = funcionario.num_pessoa_fisic
               no-lock no-error.
    if not avail rh_pessoa_fisic then next.

    /* busca dados para cargo */
    find first cargo_basic
         where cargo_basic.cdn_cargo_basic = funcionario.cdn_cargo_basic
               no-lock no-error.
    if not avail cargo_basic then next.

    /* busca dados da unid_lotac */
    find first unid_lotac
         where unid_lotac.cod_unid_lotac = funcionario.cod_unid_lotac
               no-lock no-error.
    if not avail unid_lotac then next.

    /* busca dados do ccusto */
    find first rh_ccusto
         where rh_ccusto.cdn_empresa   = funcionario.cdn_empresa
           and rh_ccusto.cod_rh_ccusto = funcionario.cod_rh_ccusto
               no-lock no-error.
    if not avail rh_ccusto then next.

    /* valida admissao */
    if funcionario.dat_admis_func < tt-param.dat_admis_func_ini or
       funcionario.dat_admis_func > tt-param.dat_admis_func_fim then next.

    /* valida desligamento */
    if funcionario.dat_desligto_func < tt-param.dat_desligto_func_ini or
       funcionario.dat_desligto_func > tt-param.dat_desligto_func_fim then next.
    
    /* valida unidade de lotacao */
    if funcionario.cod_unid_lotac < tt-param.cod_unid_lotac_ini or
       funcionario.cod_unid_lotac > tt-param.cod_unid_lotac_fim then next.

    assign de-total-func[1] = 0
           de-total-func[2] = 0
           de-vl-sal-ant    = 0.

    run pi-acompanhar in h-acomp (input 'Funcionario: ' + funcionario.nom_abrev_pessoa_fisic).

    /* busca os eventos com base no sintetico 1000 - hora extra */
    for each movto_calcul_func use-index mvtclclf_py12401
       where movto_calcul_func.cdn_empresa          = funcionario.cdn_empresa
         and movto_calcul_func.cdn_estab            = funcionario.cdn_estab
         and movto_calcul_func.cdn_funcionario      = funcionario.cdn_funcionario
         and movto_calcul_func.idi_tip_fp           = 1 /*normal*/
             no-lock
             by movto_calcul_func.num_mes_refer_fp
             by movto_calcul_func.num_ano_refer_fp:

        assign i-aux-1  = int(string(movto_calcul_func.num_ano_refer_fp,"9999") + string(movto_calcul_func.num_mes_refer_fp,"99"))
               i-aux-2  = int(string(i-ano-ini,"9999") + string(i-mes-ini,"99"))
               i-aux-3  = int(string(i-ano-fim,"9999") + string(i-mes-fim,"99")).

        if i-aux-1 < i-aux-2
        or i-aux-1 > i-aux-3 then next.

        /* inicio alteracao - mineiro - nol - 14/abr/09 */
        if movto_calcul_func.num_mes_refer_fp = 12 then
            assign da-hist = date("31/" + string(movto_calcul_func.num_mes_refer_fp) + "/" + string(movto_calcul_func.num_ano_refer_fp)).
        else
            assign da-hist = date("01/" + string(movto_calcul_func.num_mes_refer_fp + 1) + "/" + string(movto_calcul_func.num_ano_refer_fp)) - 1.

        /* verifica se o salario no periodo */
        for last histor_sal_func of funcionario
           where histor_sal_func.dat_livre_1 <= da-hist
                   no-lock.
        end.

        /* inicio alteracao - mineiro - nol - 02/abr/09 */
        if avail histor_sal_func and histor_sal_func.val_salario_mensal >= de-vl-sal-ant then
            assign de-vl-sal-ant = histor_sal_func.val_salario_mensal.
        /* fim da alteracao - mineiro - nol - 02/abr/09 */

        if de-vl-sal-ant = 0 then
            assign de-vl-sal-ant = funcionario.val_salario_atual.

        do i-cont = 1 to 30:
            if movto_calcul_func.cdn_event_fp[i-cont] = "000" then next.

            find first event_fp
                 where event_fp.cdn_event_fp = movto_calcul_func.cdn_event_fp[i-cont]
                       no-lock no-error.
            if not avail event_fp then next.

            if not can-find(first estrut_efp of event_sint_fp
                            where estrut_efp.cdn_efp_det = movto_calcul_func.cdn_event_fp[i-cont]) then next.

            /* imprime dados */
            if tt-param.tipo_relat = 1 then do:
                assign chExcelApplication:range( "a" + string(i-linha) ):value = funcionario.cdn_empresa
                       chExcelApplication:range( "b" + string(i-linha) ):value = funcionario.cdn_estab 
                       chExcelApplication:range( "c" + string(i-linha) ):value = funcionario.cdn_funcionario
                       chExcelApplication:range( "d" + string(i-linha) ):value = fn-free-accent(funcionario.nom_pessoa_fisic)
                       chExcelApplication:range( "e" + string(i-linha) ):value = "'" + rh_pessoa_fisic.cod_id_feder
                       chExcelApplication:range( "f" + string(i-linha) ):value = fn-free-accent(cargo_basic.des_cargo_basic)
                       chExcelApplication:range( "g" + string(i-linha) ):value = de-vl-sal-ant /*if avail histor_sal_func then histor_sal_func.val_salario_mensal else funcionario.val_salario_atual*/
                       chExcelApplication:range( "h" + string(i-linha) ):value = funcionario.cod_unid_lotac
                       chExcelApplication:range( "i" + string(i-linha) ):value = fn-free-accent(unid_lotac.des_unid_lotac)
                       chExcelApplication:range( "j" + string(i-linha) ):value = funcionario.cod_rh_ccusto
                       chExcelApplication:range( "k" + string(i-linha) ):value = fn-free-accent(rh_ccusto.des_rh_ccusto)
                       chExcelApplication:range( "l" + string(i-linha) ):value = movto_calcul_func.cdn_event_fp[i-cont]
                       chExcelApplication:range( "m" + string(i-linha) ):value = fn-free-accent(event_fp.des_event_fp)
                       chExcelApplication:range( "n" + string(i-linha) ):value = movto_calcul_func.qtd_unid_event_fp[i-cont]
                       chExcelApplication:range( "o" + string(i-linha) ):value = movto_calcul_func.val_calcul_efp[i-cont]
                       chExcelApplication:range( "p" + string(i-linha) ):value = movto_calcul_func.num_mes_refer_fp
                       chExcelApplication:range( "q" + string(i-linha) ):value = movto_calcul_func.num_ano_refer_fp.

                assign i-linha = i-linha + 1.
            end.
            else
                assign de-total-func[1] = de-total-func[1] + movto_calcul_func.qtd_unid_event_fp[i-cont]
                       de-total-func[2] = de-total-func[2] + movto_calcul_func.val_calcul_efp[i-cont].
        end.
        /* fim da alteracao - mineiro - nol - 14/abr/09 */
    end.

    if tt-param.tipo_relat = 2 and de-total-func[1] > 0 then do:
        assign chExcelApplication:range( "a" + string(i-linha) ):value = funcionario.cdn_empresa
               chExcelApplication:range( "b" + string(i-linha) ):value = funcionario.cdn_estab 
               chExcelApplication:range( "c" + string(i-linha) ):value = funcionario.cdn_funcionario
               chExcelApplication:range( "d" + string(i-linha) ):value = fn-free-accent(funcionario.nom_pessoa_fisic)
               chExcelApplication:range( "e" + string(i-linha) ):value = "'" + rh_pessoa_fisic.cod_id_feder
               chExcelApplication:range( "f" + string(i-linha) ):value = fn-free-accent(cargo_basic.des_cargo_basic)
               chExcelApplication:range( "g" + string(i-linha) ):value = funcionario.val_salario_atual
               chExcelApplication:range( "h" + string(i-linha) ):value = funcionario.cod_unid_lotac
               chExcelApplication:range( "i" + string(i-linha) ):value = fn-free-accent(unid_lotac.des_unid_lotac)
               chExcelApplication:range( "j" + string(i-linha) ):value = funcionario.cod_rh_ccusto
               chExcelApplication:range( "k" + string(i-linha) ):value = fn-free-accent(rh_ccusto.des_rh_ccusto)
               chExcelApplication:range( "l" + string(i-linha) ):value = de-total-func[1]
               chExcelApplication:range( "m" + string(i-linha) ):value = de-total-func[2].

        assign i-linha = i-linha + 1.
    end.
end.

chExcelApplication:Visible = true.

/* release com-handles */
release object chExcelApplication.      
release object chWorkbook.
release object chWorksheet. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

