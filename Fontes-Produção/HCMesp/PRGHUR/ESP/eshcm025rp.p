&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Listar os dados referente a Avaliacao por Competencia

    Syntax      : Ler e exportar para o excel alguns campos do HCM

    Description : Buscar todos os dados das tabelas referente a avaliacao
                  por competencia.
    
    Author(s)   : Nilton Oliveira - DSC Consultoria
    Created     : 09/jan/2008
    Notes       : 
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
{include/buffers_RH.i}

{include/i-prgvrs.i ESHCM025RP 2.09.00.000}

/*   ----- DEFINICAO DAS TEMP-TABLE -----   */
define temp-table tt-param no-undo
    field destino                   as integer
    field arquivo                   as char format "x(35)"
    field usuario                   as char format "x(12)"
    field data-exec                 as date
    field hora-exec                 as integer
    field cdn_empresa_ini           as char
    field cdn_empresa_fim           as char
    field cod_rh_ccusto_ini         as char
    field cod_rh_ccusto_fim         as char
    field cod_unid_lotac_ini        as char
    field cod_unid_lotac_fim        as char
    field periodo_ini               as char
    field periodo_fim               as char
    field cdn_avpes_padr_ini        as integer
    field cdn_avpes_padr_fim        as integer
    field cdn_estab_avaliado_ini    as char
    field cdn_estab_avaliado_fim    as char
    field cdn_func_avaliado_ini     as integer
    field cdn_func_avaliado_fim     as integer
    field cdn_estab_avaliador_ini   as char 
    field cdn_estab_avaliador_fim   as char
    field cdn_func_avaliador_ini    as integer
    field cdn_func_avaliador_fim    as integer.

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    field exemplo          as character format "x(30)"
    index id ordem.

def temp-table tt-raw-digita
   field raw-digita      as raw.

define temp-table tt_aval_competencia no-undo
    field cdn_func_respos_avpes     like funcionario.nom_abrev_pessoa_fisic
    field cdn_funcionario           like funcionario.cdn_funcionario
    field nom_funcionario           like funcionario.nom_abrev_pessoa_fisic
    field cod_rh_ccusto             like funcionario.cod_rh_ccusto
    field des_rh_ccusto             like rh_ccusto.des_rh_ccusto
    field cdn_avpes_padr            like avpes_emitid.cdn_avpes_padr
    field des_meta_indual           like avpes_meta_indual.des_meta_indual
    field des_parecer_respos_avpes  like avpes_emitid.des_parecer_respos_avpes
    field dat_previs_conclus_meta   like avpes_meta_indual.dat_previs_conclus_meta
    field des_restdo                like avpes_meta_indual.des_restdo
    field des_parecer               like avpes_meta_indual.des_restdo
    field dt_conclusao              like avpes_meta_indual.dat_conclus
    field dt_in_avaliac             like avpes_emitid.dat_refer_respos_avpes.


/*---- DEINICAO DE PARAMETROS DO PROGRAMA -----*/
define input param raw-param as raw no-undo.
define input param table for tt-raw-digita.

/*----- DEFINICAO DE VARIAVEIS LOCAIS -----*/
define variable h-acomp         as handle   no-undo. 

define variable i-cont          as integer     no-undo.
define variable c-ano-ini       as char        no-undo.
define variable c-ano-fim       as char        no-undo.
define variable c-mes-ini       as char        no-undo.
define variable c-mes-fim       as char        no-undo.

define variable da-periodo-ini  as date        no-undo.
define variable da-periodo-fim  as date        no-undo.

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
define buffer bf_funcionario for funcionario.

/*   ----- DEFINICAO DO EXCEL  -----   */
define variable i-linha                 as integer      no-undo.
define variable i-linha-avaliador       as integer      no-undo.
define variable i-linha-avaliado        as integer      no-undo.
define variable i-linha-ccusto          as integer      no-undo.

define variable excelapp                as com-handle   no-undo.
define variable chworkbooks             as com-handle   no-undo.
define variable chWorksheet             as com-handle   no-undo.

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
       c-programa     = "ESHCM025RP"
       c-versao       = "2.09"
       c-revisao      = ".00.000"
       c-titulo-relat = "Exportacao de Dados - Avaliacao por Competencia"
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

run pi-executar.

run pi-finalizar in h-acomp.

{include/i-rpclo.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pi-executar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-executar Procedure 
PROCEDURE pi-executar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
assign i-linha          = 1
       c-ano-ini        = substr(tt-param.periodo_ini,3,4)
       c-ano-fim        = substr(tt-param.periodo_fim,3,4)
       c-mes-ini        = substr(tt-param.periodo_ini,1,2)
       c-mes-fim        = string(int(substr(tt-param.periodo_fim,1,2)) + 1).

if c-mes-fim = "13" then
    assign c-mes-fim = '1'
           c-ano-fim = string(int(c-ano-fim) + 1).

    assign da-periodo-ini   = date('01/' + c-mes-ini + '/' + c-ano-ini)
           da-periodo-fim   = date('01/' + c-mes-fim + '/' + c-ano-fim) - 1.


empty temp-table tt_aval_competencia.

for each avpes_meta_indual
   where avpes_meta_indual.cdn_empresa      >= tt-param.cdn_empresa_ini
     and avpes_meta_indual.cdn_empresa      <= tt-param.cdn_empresa_fim
     and avpes_meta_indual.cdn_estab        >= tt-param.cdn_estab_avaliado_ini
     and avpes_meta_indual.cdn_estab        <= tt-param.cdn_estab_avaliado_fim
     and avpes_meta_indual.cdn_funcionario  >= tt-param.cdn_func_avaliado_ini
     and avpes_meta_indual.cdn_funcionario  <= tt-param.cdn_func_avaliado_fim
         no-lock:

    run pi-acompanhar in h-acomp (input 'Avaliaá∆o do Funcion†rio: ' + string(avpes_meta_indual.cdn_funcionario)).

    find first rh_pessoa_fisic
         where rh_pessoa_fisic.num_pessoa_fisic = avpes_meta_indual.num_pessoa_fisic
               no-lock no-error.
    if not avail rh_pessoa_fisic then next.

    find first funcionario
         where funcionario.cdn_empresa     = avpes_meta_indual.cdn_empresa
           and funcionario.cdn_estab       = avpes_meta_indual.cdn_estab
           and funcionario.cdn_funcionario = avpes_meta_indual.cdn_funcionario
               no-lock no-error.
    if not avail funcionario then next.

    find first rh_ccusto
         where rh_ccusto.cdn_empresa    = funcionario.cdn_empresa
           and rh_ccusto.cod_rh_ccusto  = funcionario.cod_rh_ccusto
               no-lock no-error.
    if not avail rh_ccusto then next.

    find first cargo_basic
         where cargo_basic.cdn_cargo_basic = funcionario.cdn_cargo_basic
               no-lock no-error.
    if not avail cargo_basic then next.

    /* validacao dos filtros */
    if rh_ccusto.cod_rh_ccusto < tt-param.cod_rh_ccusto_ini or
       rh_ccusto.cod_rh_ccusto > tt-param.cod_rh_ccusto_fim  then next.

    if funcionario.cod_unid_lotac < tt-param.cod_unid_lotac_ini or
       funcionario.cod_unid_lotac > tt-param.cod_unid_lotac_fim then next.

    for each avpes_emitid
       where avpes_emitid.cdn_empresa                  = avpes_meta_indual.cdn_empresa    
         and avpes_emitid.cdn_estab                    = avpes_meta_indual.cdn_estab      
         and avpes_emitid.cdn_funcionario              = avpes_meta_indual.cdn_funcionario
         and avpes_emitid.cdn_avpes_padr              >= tt-param.cdn_avpes_padr_ini
         and avpes_emitid.cdn_avpes_padr              <= tt-param.cdn_avpes_padr_fim
         and avpes_emitid.cdn_estab_func_respos_avpes >= tt-param.cdn_estab_avaliador_ini
         and avpes_emitid.cdn_estab_func_respos_avpes <= tt-param.cdn_estab_avaliador_fim
         and avpes_emitid.cdn_func_respos_avpes       >= tt-param.cdn_func_avaliador_ini
         and avpes_emitid.cdn_func_respos_avpes       <= tt-param.cdn_func_avaliador_fim
             no-lock:
        
        if avpes_emitid.dat_inicial_period_avpes < da-periodo-ini or
           avpes_emitid.dat_final_period_avpes   > da-periodo-fim then next.
        
        find first avpes_item
             where avpes_item.cdn_item_avpes = avpes_emitid.cdn_avpes_padr
                   no-lock no-error.
        if not avail avpes_item then next.

        find first bf_funcionario
             where bf_funcionario.cdn_empresa     = avpes_meta_indual.cdn_empresa
               and bf_funcionario.cdn_estab       = avpes_emitid.cdn_estab_func_respos_avpes
               and bf_funcionario.cdn_funcionario = avpes_emitid.cdn_func_respos_avpes
                   no-lock no-error.
        if not avail bf_funcionario then next.

        create tt_aval_competencia.
        assign tt_aval_competencia.cdn_func_respos_avpes    = bf_funcionario.nom_pessoa_fisic            /*avaliador*/
               tt_aval_competencia.nom_funcionario          = funcionario.nom_pessoa_fisic               /*avaliado*/
               tt_aval_competencia.cdn_funcionario          = funcionario.cdn_funcionario
               tt_aval_competencia.cod_rh_ccusto            = funcionario.cod_rh_ccusto                  /* cod_rh_ccusto */
               tt_aval_competencia.des_rh_ccusto            = rh_ccusto.des_rh_ccusto                    /* des_rh_ccusto */
               tt_aval_competencia.cdn_avpes_padr           = avpes_emitid.cdn_avpes_padr                /*cod.aval*/
               tt_aval_competencia.des_meta_indual          = avpes_meta_indual.des_meta_indual          /*competencias*/
               tt_aval_competencia.des_parecer_respos_avpes = avpes_emitid.des_parecer_respos_avpes      /*acoes*/
               tt_aval_competencia.dat_previs_conclus_meta  = avpes_meta_indual.dat_previs_conclus_meta  /*Previs∆o Conclus∆o*/
               tt_aval_competencia.des_restdo               = avpes_meta_indual.des_restdo               /*status*/
               tt_aval_competencia.des_parecer              = ""                                         /*parecer do cdn_func_respos_avpes*/
               tt_aval_competencia.dt_conclusao             = avpes_meta_indual.dat_conclus
               tt_aval_competencia.dt_in_avaliac            = avpes_emitid.dat_refer_respos_avpes.
    end.
end.

if can-find(first tt_aval_competencia) then do:
    
    assign i-cont = 0.
    for each tt_aval_competencia:
        assign i-cont = i-cont + 1.
    end.

    /* abre a aplicacao do excel */
    create "Excel.Application" excelAPP. 
    excelAPP:visible = false.
    chworkbooks = excelAPP:workbooks:add.

    /* imprime cabecalho */
    excelAPP:Range("a1"):value = "Avaliador".
    excelAPP:Range("b1"):value = "Cod. Avaliado".
    excelAPP:Range("c1"):value = "Avaliado".
    excelAPP:Range("d1"):value = "CCusto".
    excelAPP:Range("e1"):value = "Descriá∆o CCusto".
    excelAPP:Range("f1"):value = "Cod.Avaliacao".
    excelAPP:Range("g1"):value = "In°cio avaliaá∆o".
    excelAPP:Range("h1"):value = "Competencia".
    excelAPP:Range("i1"):value = "Acoes".
    excelAPP:Range("j1"):value = "Conclus∆o".
    excelAPP:Range("k1"):value = "Previs∆o Conclus∆o".
    excelAPP:Range("l1"):value = "Status".
    excelAPP:Range("m1"):value = "Parecer do Avaliador".
    excelAPP:Range("a1:m1"):Interior:ColorIndex = 15.
    excelAPP:Range("a1:m1"):Font:Bold = true.

    for each tt_aval_competencia
             no-lock
             break by tt_aval_competencia.cdn_func_respos_avpes
                   by tt_aval_competencia.nom_funcionario
                   by tt_aval_competencia.cod_rh_ccusto:

        run pi-acompanhar in h-acomp (input 'Importando para o Excel - Linha ' + string(i-linha) + ' de ' + string(i-cont)).

        assign i-linha  = i-linha + 1.

        if first-of(tt_aval_competencia.cdn_func_respos_avpes) then
            assign i-linha-avaliador = i-linha.

        if first-of(tt_aval_competencia.nom_funcionario) then
            assign i-linha-avaliado = i-linha.

        if first-of(tt_aval_competencia.cod_rh_ccusto) then
            assign i-linha-ccusto = i-linha.

        assign 
               excelAPP:Range("f" + string(i-linha)):value = tt_aval_competencia.cdn_avpes_padr
               excelAPP:Range("g" + string(i-linha)):value = tt_aval_competencia.dt_in_avaliac
               excelAPP:Range("h" + string(i-linha)):value = tt_aval_competencia.des_meta_indual
               excelAPP:Range("i" + string(i-linha)):value = tt_aval_competencia.des_parecer_respos_avpes
               excelAPP:Range("j" + string(i-linha)):value = tt_aval_competencia.dt_conclusao
               excelAPP:Range("k" + string(i-linha)):value = tt_aval_competencia.dat_previs_conclus_meta
               excelAPP:Range("l" + string(i-linha)):value = tt_aval_competencia.des_restdo
               excelAPP:Range("m" + string(i-linha)):value = tt_aval_competencia.des_parecer.

        if last-of(tt_aval_competencia.cod_rh_ccusto) then
            assign excelAPP:Range("d" + string(i-linha-ccusto) + ":d" + STRING(i-linha)):mergecells = 1
                   excelAPP:Range("d" + string(i-linha-ccusto)):VALUE = tt_aval_competencia.cod_rh_ccusto
                   excelAPP:Range("e" + string(i-linha-ccusto) + ":e" + STRING(i-linha)):mergecells = 1
                   excelAPP:Range("e" + string(i-linha-ccusto)):VALUE = tt_aval_competencia.des_rh_ccusto.

        if last-of(tt_aval_competencia.nom_funcionario) then do:
            assign excelAPP:Range("c" + string(i-linha-avaliado) + ":c" + string(i-linha)):mergecells = 1
                   excelAPP:Range("c" + string(i-linha-avaliado)):value = tt_aval_competencia.nom_funcionario.

            assign excelAPP:Range("b" + string(i-linha-avaliado) + ":b" + string(i-linha)):mergecells = 1
                   excelAPP:Range("b" + string(i-linha-avaliado)):value = tt_aval_competencia.cdn_funcionario.
        end.

        if last-of(tt_aval_competencia.cdn_func_respos_avpes) then
            assign excelAPP:Range("a" + string(i-linha-avaliador) + ":a" + STRING(i-linha)):mergecells = 1
                   excelAPP:Range("a" + string(i-linha-avaliador)):VALUE = tt_aval_competencia.cdn_func_respos_avpes.
    end.

    excelAPP:Range("A:D"):VerticalAlignment = 2.   /* Alinha o campo */

    excelAPP:Range("A:L"):select.
    excelAPP:Selection:EntireColumn:AutoFit. /* Redimenciona automaticamente */
    excelAPP:Selection:EntireRow:AutoFit. /* Redimenciona automaticamente */

    excelAPP:Range("A1"):select.

    excelAPP:ActiveSheet:PageSetup:PrintTitleRows = "$1:$1".

    excelAPP:visible = true.

    /* release com-handles */
    release object excelAPP. 

end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

