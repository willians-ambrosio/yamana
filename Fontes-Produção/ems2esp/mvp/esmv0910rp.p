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
{include/i-prgvrs.i ESMV0910RP 2.06.00.0000}
/*****************************************************************************
**
**       PROGRAMA: ESMV0910RP.p
**
**       DATA....: 22/05/2008
**
**       AUTOR...: KSM40622 - Gustavo Eduardo Tamanini
**
*****************************************************************************/
                                             /** Defini‡Æo das temp-tables **/

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)":U
    field modelo           as char format "x(35)":U
    field periodo-ini      as date format "99/99/9999":U
    field periodo-fim      as date format "99/99/9999":U
    field ordem-ini        like ord-manut.nr-ord-produ
    field ordem-fim        like ord-manut.nr-ord-produ
    field tecnico-ini      like tecn-mi.cd-tecnico
    field tecnico-fim      like tecn-mi.cd-tecnico
    field emp-ini          as char format "x(3)" 
    field emp-fim          as char format "x(3)" 
    field eqpto-ini        like ord-manut.cd-equipto
    field eqpto-fim        like ord-manut.cd-equipto
    field req-ini          like req-ord-produc.nr-requisicao
    field req-fim          like req-ord-produc.nr-requisicao
    field lFrotas          as logical
    field lMI              as logical
    field lAberta          as logical
    field lFechada         as logical
    field lPendente        as logical
    field lCom             as logical
    field lAprov           as logical
    field lNaoAprov        as logical
    field lReq             as logical
    field lSolic           as logical.

/****************************************************************************/
                                               /** Defini‡Æo de Parƒmetros **/   
def temp-table tt-digita no-undo
    field linha         as integer
    field conteudo      as character format "x(80)"
    field i-ep-codigo   like mab-eqpto.ep-codigo
    field c-cd-equipto  like mab-eqpto.cod-eqpto
    field c-descricao   like mab-model.des-model
    field l-sucesso     as log initial yes.

def temp-table tt-raw-digita
   field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
/****************************************************************************/
                    /** Transferˆncia de parƒmetros para temp-table padrÆo **/
create tt-param.
raw-transfer raw-param to tt-param.
/** Este c¢digo deve existir somente se relat¢rio contiver digita‡Æo **/
for each tt-raw-digita no-lock:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.
end.
/****************************************************************************/
                                                /** Defini‡Æo de Vari veis **/
DEFINE VARIABLE c-destino AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE h-acomp   AS HANDLE                    NO-UNDO.
{include/i-rpvar.i}
/** componentes Excel **/
DEFINE VARIABLE ch-Excel    AS COM-HANDLE                         NO-UNDO.
DEFINE VARIABLE ch-Planilha AS COM-HANDLE                         NO-UNDO.
DEFINE VARIABLE ch-Arquivo  AS COM-HANDLE                         NO-UNDO.
DEFINE VARIABLE c-alfabeto  AS CHAR INIT "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z" NO-UNDO.
DEFINE VARIABLE i-alf       AS INTEGER                            NO-UNDO.
DEFINE VARIABLE i-alf2      AS INTEGER                            NO-UNDO.
DEFINE VARIABLE cSituacao   AS CHARACTER                          NO-UNDO.
DEFINE VARIABLE cEstado     AS CHARACTER                          NO-UNDO.
DEFINE VARIABLE cNome       AS CHARACTER format "x(50)"           NO-UNDO.
DEFINE VARIABLE cTecnico    AS CHARACTER format "x(07)"           NO-UNDO.
/****************************************************************************/
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
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 8.75
         WIDTH              = 33.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/** Busca os parƒmetros criados na interface gr fica **/
find first tt-param.

/** Procedure para inicializar c lculos e impressÆo **/
run piPrincipal in this-procedure.

return "OK":U. 
/*--- Fim do Programa ---*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-piAbertura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piAbertura Procedure 
PROCEDURE piAbertura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cPath AS CHAR NO-UNDO.
    
    session:set-wait-state ("general"). 
    /** Aloca o excel na mem¢ria **/
    create "Excel.Application"  ch-excel no-error.
    if error-status:error then do:
        create "Excel.Application" ch-excel.
    end.

    assign ch-Arquivo  = ch-Excel:WorkBooks:add().
    assign ch-Planilha = ch-Arquivo:Sheets:Item(1).
    ch-Planilha:Activate().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piDados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piDados Procedure 
PROCEDURE piDados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    define variable c-tecnico            as character                no-undo.
    define variable lCabTec              as logical initial no       no-undo.
    define variable i-col                as integer                  no-undo.
    define variable i-lin                as integer                  no-undo.
    define variable l-imprimi-tecnico    as logical                  no-undo.

    assign i-lin         = 5
           i-col         = 1
           lCabTec       = no
           c-tecnico     = "":U.

    repeat i-alf = 1 to 26:
        repeat i-alf2 = 1 to 26:
            assign c-alfabeto = c-alfabeto + "," + entry(i-alf,c-alfabeto,",") + entry(i-alf2,c-alfabeto,",").
        end.
    end.

    /** Titulo **/
    assign ch-Planilha:cells(2,3):value = fill("-":U, 85).
    ch-Planilha:Range(entry(3,c-alfabeto,",") + "2" + ":" + entry(12,c-alfabeto,",") + "2"):select.
    ch-Excel:selection:MergeCells = True.
    ch-excel:Selection:Font:Bold  = True.
    {utp/ut-liter.i "YAMANA GOLD______________________________________Requisi‡Æo/Solicita‡Æo_de_Compra_por_T‚cnico"}
    assign ch-Planilha:cells(3,3):value = return-value.
    ch-Planilha:Range(entry(3,c-alfabeto,",") + "3" + ":" + entry(12,c-alfabeto,",") + "3"):select.
    ch-Excel:selection:MergeCells = True.
    ch-excel:Selection:Font:Bold  = True.
    assign ch-Planilha:cells(4,3):value = fill("-":U, 85).
    ch-Planilha:Range(entry(3,c-alfabeto,",") + "4" + ":" + entry(12,c-alfabeto,",") + "4"):select.
    ch-Excel:selection:MergeCells = True.
    ch-excel:Selection:Font:Bold  = True.

    for each mmi-tec-req-prod 
        where mmi-tec-req-prod.nr-requisicao >= tt-param.req-ini
        and   mmi-tec-req-prod.nr-requisicao <= tt-param.req-fim
        and   mmi-tec-req-prod.cd-tecnico    >= tt-param.tecnico-ini
        and   mmi-tec-req-prod.cd-tecnico    <= tt-param.tecnico-fim no-lock
        break by mmi-tec-req-prod.cd-tecnico
              by mmi-tec-req-prod.nr-requisicao:

         if  first-of (mmi-tec-req-prod.cd-tecnico) then do:
             if c-tecnico <> mmi-tec-req-prod.cd-tecnico then
                 assign c-tecnico         = mmi-tec-req-prod.cd-tecnico
                        l-imprimi-tecnico = yes.
         end.

        for each req-ord-produc
            where req-ord-produc.nr-requisicao = mmi-tec-req-prod.nr-requisicao
            and   req-ord-produc.sequencia     = mmi-tec-req-prod.sequencia 
            and   req-ord-produc.dat-trans    >= tt-param.periodo-ini 
            and   req-ord-produc.dat-trans    <= tt-param.periodo-fim no-lock:

            /** Parametros Tipo Ordem **/
            if not(tt-param.lFrotas) and req-ord-produc.origem    = "MV":U then next.
            if not(tt-param.lMI)     and req-ord-produc.origem    = "MI":U then next.
            /** Parametros Tipo Requisicao **/
            if not(tt-param.lReq)    and req-ord-produc.tp-requis = 1      then next.
            if not(tt-param.lSolic)  and req-ord-produc.tp-requis = 2      then next.

            for first it-requisicao
                where it-requisicao.nr-requisicao = req-ord-produc.nr-requisicao
                and   it-requisicao.sequencia     = req-ord-produc.sequencia no-lock:

                /** Validacoes: 
                 ** Situacao da Requisicao/Solicitacao **/
                if not(tt-param.lAberta)   and it-requisicao.situacao = 1 then next.
                if not(tt-param.lFechada)  and it-requisicao.situacao = 2 then next.
                if not(tt-param.lPendente) and it-requisicao.situacao = 3 then next.
                if not(tt-param.lCom)      and it-requisicao.situacao = 4 then next.
                /** Estado da Requisicao/Solicitacao **/
                if not(tt-param.lAprov)    and it-requisicao.estado = 1 then next.
                if not(tt-param.lNaoAprov) and it-requisicao.estado = 2 then next.

                for first item
                    where item.it-codigo = it-requisicao.it-codigo no-lock:
                end.
                IF NOT AVAIL ITEM THEN NEXT.

                if req-ord-produc.origem = "MV":U then do:
                    assign cTecnico = mmi-tec-req-prod.cd-tecnico.
                    for first mmv-ord-manut
                        where mmv-ord-manut.nr-ord-produ = req-ord-produc.nr-ord-produ 
                        and   mmv-ord-manut.ep-codigo   >= emp-ini  
                        and   mmv-ord-manut.ep-codigo   <= emp-fim  
                        and   mmv-ord-manut.cod-eqpto   >= eqpto-ini 
                        and   mmv-ord-manut.cod-eqpto   <= eqpto-fim  no-lock:
                    end.
                    IF NOT AVAIL mmv-ord-manut THEN NEXT.
                    for first mmv-func-ofici
                        where mmv-func-ofici.cod-matr = mmi-tec-req-prod.cd-tecnico no-lock:
                        assign cNome = mmv-func-ofici.nom-func.
                    end.
                end.
                if req-ord-produc.origem = "MI":U then do:
                    assign cTecnico = string(mmi-tec-req-prod.cd-tecnico,"99999-9").
                    for first ord-manut
                         where ord-manut.nr-ord-produ = req-ord-produc.nr-ord-produ 
                         and   ord-manut.cd-equipto  >= eqpto-ini
                         and   ord-manut.cd-equipto  <= eqpto-fim no-lock:
                    end.
                    IF NOT AVAIL ord-manut THEN NEXT.
                    for first tecn-mi
                        where tecn-mi.cd-tecnico = mmi-tec-req-prod.cd-tecnico no-lock:
                        assign cNome = tecn-mi.nome-compl.
                    end.
                end.
                /** Descricao Situacao/Estado **/
                assign cSituacao = {ininc/i01in385.i 4 it-requisicao.situacao}
                       cEstado   = {ininc/i02in385.i 4 it-requisicao.estado}.

                if l-imprimi-tecnico then do:
                    if lCabTec then
                        assign i-lin = i-lin + 1.

                    assign i-lin = i-lin + 1.
                    /** Formata campo Tecnico **/
                    ch-Planilha:Range(entry(i-col + 1,c-alfabeto,",") + string(i-lin)):select.
                    ch-excel:Selection:NumberFormat = "@":U.

                    /** Informacoes do T‚cnico **/
                    {utp/ut-liter.i "T‚cnico:"}
                    assign ch-Planilha:cells(i-lin,i-col):value     = return-value
                           ch-Planilha:cells(i-lin,i-col + 1):value = cTecnico
                           ch-Planilha:cells(i-lin,i-col + 2):value = cNome.

                    /** Deixa linha do tecnico em Negrito **/
                    ch-Planilha:Range(entry(i-col,c-alfabeto,",") + string(i-lin) + ":" + entry(i-col + 2,c-alfabeto,",") + string(i-lin)):select.
                    ch-excel:Selection:Font:Bold = True.

                    assign lCabTec = yes.
                 end.
                 assign l-imprimi-tecnico = no.

                 if  first-of (mmi-tec-req-prod.nr-requisicao) then do:
                     /** Informacoes da Requisicao **/
                     assign i-lin = i-lin + 2.
                     {utp/ut-liter.i "Requisi‡Æo:"}
                     assign ch-Planilha:cells(i-lin,i-col):value     = return-value
                            ch-Planilha:cells(i-lin,i-col + 1):value = req-ord-produc.nr-requisicao
                            ch-Planilha:cells(i-lin,i-col + 2):value = cSituacao
                            ch-Planilha:cells(i-lin,i-col + 3):value = cEstado
                            ch-Planilha:cells(i-lin,i-col + 4):value = req-ord-produc.dat-trans.

                     /** Cabecalho da Ordem **/
                     assign i-lin = i-lin + 1.
                     {utp/ut-liter.i "Ordem Manuten‡Æo"}
                     assign ch-Planilha:cells(i-lin,i-col):value     = return-value.
                     {utp/ut-liter.i "Item"}
                     assign ch-Planilha:cells(i-lin,i-col + 1):value = return-value.
                     {utp/ut-liter.i "Descri‡Æo"}
                     assign ch-Planilha:cells(i-lin,i-col + 2):value = return-value.
                     {utp/ut-liter.i "Qt_Requisitada"}
                     assign ch-Planilha:cells(i-lin,i-col + 3):value = return-value.
                     {utp/ut-liter.i "Requisi‡Æo"}
                     assign ch-Planilha:cells(i-lin,i-col + 4):value = return-value.
                     {utp/ut-liter.i "Seqˆncia"}
                     assign ch-Planilha:cells(i-lin,i-col + 5):value = return-value.
                     {utp/ut-liter.i "Data_Entrega"}
                     assign ch-Planilha:cells(i-lin,i-col + 6):value = return-value.
                     {utp/ut-liter.i "Equipamento"}
                     assign ch-Planilha:cells(i-lin,i-col + 7):value = return-value.
                     {utp/ut-liter.i "Tipo Requisi‡Æo"}
                     assign ch-Planilha:cells(i-lin,i-col + 8):value = return-value.
                     {utp/ut-liter.i "Narrativa"}
                     assign ch-Planilha:cells(i-lin,i-col + 9):value = return-value.
                 end.

                 assign i-lin = i-lin + 1.                 
                 /** Formata campo Item **/
                 ch-Planilha:Range(entry(i-col + 1,c-alfabeto,",") + string(i-lin)):select.
                 ch-excel:Selection:NumberFormat = "@":U.
                 /** Formata campo Qtd Requisitada **/
                 ch-Planilha:Range(entry(i-col + 3,c-alfabeto,",") + string(i-lin)):select.
                 ch-excel:Selection:NumberFormat = "###.###.##0,000":U.

                 /** Informacoes da Ordem **/
                 assign ch-Planilha:cells(i-lin,i-col):value     = string(req-ord-produc.nr-ord-produ) + "-" + req-ord-produc.origem
                        ch-Planilha:cells(i-lin,i-col + 1):value = it-requisicao.it-codigo
                        ch-Planilha:cells(i-lin,i-col + 2):value = Item.desc-item
                        ch-Planilha:cells(i-lin,i-col + 3):value = it-requisicao.qt-requisitada
                        ch-Planilha:cells(i-lin,i-col + 4):value = req-ord-produc.nr-requisicao
                        ch-Planilha:cells(i-lin,i-col + 5):value = req-ord-produc.sequencia
                        ch-Planilha:cells(i-lin,i-col + 6):value = it-requisicao.dt-entrega
                        ch-Planilha:cells(i-lin,i-col + 7):value = if req-ord-produc.origem = "MI":U then ord-manut.cd-equipto else mmv-ord-manut.cod-eqpto 
                        ch-Planilha:cells(i-lin,i-col + 8):value = {ininc/i03in385.i 4 req-ord-produc.tp-requis}.
                        ch-Planilha:cells(i-lin,i-col + 9):value = replace(it-requisicao.narrativa,chr(10),"").
            end.
        end.
    end.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-piFechamento) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piFechamento Procedure 
PROCEDURE piFechamento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE c-range AS CHARACTER  NO-UNDO.

    /** Range da planilha inteira **/
    if c-range = "" then
        assign c-range = entry(1,c-alfabeto,",") + ":" + entry(256,c-alfabeto,",").
    
    /** Formata Font **/
    ch-planilha:Range(c-range):select.
    ch-Excel:Selection:Font:Name = "Verdana":U.
    ch-Excel:Selection:Font:size = 10.

    /** Acerta tamanho das colunas **/
    ch-planilha:columns(c-range):EntireColumn:AutoFit no-error.

    /* abre o excel com o arquivo */
    ch-Excel:visible = true.
    
    /*** LIbera da men¢ria e fecha a aplica‡Æo do excell ***/
    if valid-handle(ch-planilha) then do:
        release object ch-planilha no-error.
    end.
    
    if valid-handle(ch-arquivo) then do:
        release object ch-arquivo  no-error.
    end.
    
    /*** LIbera da men¢ria e fecha a aplica‡Æo do excell ***/
    if valid-handle(ch-excel) then do:
        release object ch-Excel  no-error.
    end.
    session:set-wait-state ("").
    
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
/** Inicializa programa de acompanhamento padrÆo Datasul **/
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i "Requisi‡Æo/Solicita‡Æo de Compra por T‚cnico" *}
run pi-inicializar in h-acomp(input return-value).

/*********************************************************************
   Neste espa‡o deve-se colocar a l¢gica para busca as informa‡äes e 
   tamb‚m para fazer o display, pode-se inclui a chamada de procedures
   ou fazer a l¢gica neste espa‡o.
*********************************************************************/
run piAbertura in this-procedure.

run piDados in this-procedure.

run piFechamento in this-procedure.

/*********************************************************************
   Fim do espa‡o para l¢gica de c lculo e display das informa‡äes
*********************************************************************/

/** Finaliza programa de acompanhamento padrÆo Datasul **/
run pi-finalizar in h-acomp.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

