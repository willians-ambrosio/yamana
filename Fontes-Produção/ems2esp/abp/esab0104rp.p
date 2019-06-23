/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESAB0104RP 12.1.13.003} /*** 010002 ***/

def buffer empresa for mguni.empresa.
def buffer moeda   for mguni.moeda.

DEFINE VARIABLE deValorItem AS DECIMAL     NO-UNDO.
DEF    VAR      r-rowid     AS ROWID       NO-UNDO.

{cdp/cd9911.i}

/*****************************************************************************
**
**       PROGRAMA: ESAB0104RP.p
**
**       DATA....: 30/05/2012
**
**       OBJETIVO: RP do programa ESAB0104
**
*****************************************************************************/
/** Defini‡Æo das temp-tables **/
{abp/ab9004.i}

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)":U
    field usuario          as char format "x(12)":U
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)":U
    field modelo           AS char format "x(35)":U
    FIELD todos            AS INTEGER
    FIELD arq-destino      AS CHARACTER FORMAT "x(100)":U
    FIELD data-ini         LIKE mab-abastec-lubrific.dat-movto
    FIELD data-fim         LIKE mab-abastec-lubrific.dat-movto
    FIELD empresa-ini      LIKE mab-abastec-lubrific.ep-codigo
    FIELD empresa-fim      LIKE mab-abastec-lubrific.ep-codigo
    FIELD eqpto-ini        LIKE mab-abastec-lubrific.cod-eqpto
    FIELD eqpto-fim        LIKE mab-abastec-lubrific.cod-eqpto
    FIELD placa-ini        LIKE mab-eqpto.cod-placa
    FIELD placa-fim        LIKE mab-eqpto.cod-placa
    FIELD lAtivos          AS LOGICAL
    FIELD lInativos        AS LOGICAL
    FIELD lProprios        AS LOGICAL
    FIELD lTerceiros       AS LOGICAL
    FIELD lErros           AS LOGICAL
    FIELD lHorimetro       AS LOGICAL.

DEFINE TEMP-TABLE RowErrorsAux NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER
    FIELD cPrograma        AS CHARACTER FORMAT "x(8)"
    FIELD iEmpresa         LIKE mab-eqpto.ep-codigo
    FIELD cEquipamento     LIKE mab-eqpto.cod-eqpto
    FIELD dtData           LIKE mab-abastec-lubrific.dat-movto
    FIELD cHora            LIKE mab-abastec-lubrific.hra-inicial
    FIELD cPosto           LIKE mab-abastec-lubrific.cod-posto
    FIELD cBomba           LIKE mab-item-abastec.cod-bomba
    FIELD cMotoris         LIKE mab-motoris.cod-matr
    FIELD iNumDocto        LIKE mab-abastec-lubrific.num-docto
    FIELD Qtde-Combustivel AS   DECIMAL DECIMALS 2 
    FIELD Qtde-CombustivelA AS   DECIMAL DECIMALS 2
    index seq ErrorSequence.

/* _UIB-CODE-BLOCK-END */

DEFINE TEMP-TABLE tt-dados LIKE es_import_abastec
    FIELDS r-rowid AS ROWID
    FIELDS observacao AS CHARACTER FORMAT "x(100)".

DEFINE VARIABLE deContador AS DECIMAL NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 3.92
         WIDTH              = 33.43.
/* END WINDOW DEFINITION */
                                                                        */


 





/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */



 



/*:T--------------------------------------------------------------------------
    Library    : dbott.i
    Purpose    : Include que cont‚m defini‡Æo da temptable RowObject

    Parameters :

    Notes      :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  **************************** */

DEFINE TEMP-TABLE ttAbastecLubrific NO-UNDO LIKE mab-abastec-lubrific
    FIELD r-Rowid AS ROWID.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: DBOTempTable
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW DBOTempTable ASSIGN
         HEIGHT             = 3.63
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */


 





/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */



   /* Temp-table mab-abastec-lubrific */



/*:T--------------------------------------------------------------------------
    Library    : dbott.i
    Purpose    : Include que cont‚m defini‡Æo da temptable RowObject

    Parameters :

    Notes      :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.               */
/*------------------------------------------------------------------------*/

/* ***************************  Definitions  **************************** */

DEFINE TEMP-TABLE ttItemAbastec NO-UNDO LIKE mab-item-abastec
    FIELD r-Rowid AS ROWID.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: DBOTempTable
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW DBOTempTable ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */


 





/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */



       /* Temp-table mab-item-abastec     */

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.

def temp-table tt-raw-digita
   field raw-digita as raw.
/**********************************************************************/
                                               /** Defini‡Æo de Parƒmetros **/   
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.
/**********************************************************************/
                    /** Transferˆncia de parƒmetros para temp-table padrÆo **/
create tt-param.
raw-transfer raw-param to tt-param.
/**********************************************************************/
                                                /** Defini‡Æo de Vari veis **/
define new global shared variable c-seg-usuario as CHARACTER format "x(12)" NO-UNDO.


DEFINE VARIABLE c-observacao AS CHARACTER NO-UNDO.

DEFINE BUFFER bf_es_import_abastec FOR es_import_abastec.
DEFINE BUFFER bf-mab-item-abastec  FOR mab-item-abastec.

define stream   s-ESAB0104.
{include/i-rpvar.i}
/****************************************************************************/
define variable h-acomp             as handle     no-undo.
DEFINE VARIABLE c-liter-par         AS CHARACTER FORMAT "x(14)"                  NO-UNDO.
DEFINE VARIABLE c-liter-sel         AS CHARACTER FORMAT "x(14)"                  NO-UNDO.
DEFINE VARIABLE c-liter-log         AS CHARACTER FORMAT "x(14)"                  NO-UNDO.
DEFINE VARIABLE c-destino           AS CHARACTER FORMAT "x(09)"                  NO-UNDO.
DEFINE VARIABLE c-imprime           AS CHARACTER FORMAT "x(10)"                  NO-UNDO.
DEFINE VARIABLE c-execucao          AS CHARACTER FORMAT "x(10)"                  NO-UNDO.
DEFINE VARIABLE c-liter-un          AS CHARACTER FORMAT "x(16)"                  NO-UNDO.
DEFINE VARIABLE cFaixa              AS CHARACTER FORMAT "x(06)" INITIAL "|<  >|" NO-UNDO.
define variable hDBOAbastecLubrific as handle     no-undo.
define variable hDBOItemAbastec     as handle     no-undo.
define variable hDBOPosto           as handle     no-undo.


DEFINE VARIABLE iEmpMatr          AS CHARACTER    NO-UNDO.

DEFINE VARIABLE cEstabMatr        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMotorista        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTrajeto          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iDocto            AS INTEGER    NO-UNDO.
DEFINE VARIABLE iNextDocto        AS INTEGER    NO-UNDO.
define VARIABLE dValDataHoraInver as decimal   format "999999999999" no-undo.
DEFINE VARIABLE v-hora            AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE v-hora2           AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE cAtivid           AS CHARACTER                       NO-UNDO.
DEFINE VARIABLE cReturnAux        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iPrimeiroDocto    LIKE mab-abastec-lubrific.num-docto NO-UNDO.
DEFINE VARIABLE cTpMater          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lLogAbastec       AS LOGICAL    NO-UNDO.
DEFINE VARIABLE deTanque          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iErro             AS INTEGER    NO-UNDO.
DEFINE VARIABLE cCTCodigo         like  mab-eqpto.ct-codigo  NO-UNDO.
DEFINE VARIABLE cCCcodigo         like mab-eqpto.cc-codigo  NO-UNDO.

DEFINE VARIABLE pContaLub  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pccustolub AS CHARACTER   NO-UNDO.

DEFINE VARIABLE pEstabHist        AS CHARACTER   NO-UNDO.

DEF    VAR      l-abastec-repetido AS LOG        NO-UNDO.

define buffer bfItem for item.

FORM HEADER
    "Programa Empresa Equipamento      Data       Hora     Posto    Bomba        Motorista  Docto EMS    Qtde Combust    Qtde Agrup Erro" SKIP
    "-------- ------- ---------------- ---------- -------- -------- ------------ ---------- ----------- ------------- ------------- ------------------------------------------------------------" SKIP
    WITH FRAME f-erro2 WIDTH 215 PAGE-TOP STREAM-IO NO-LABELS.

/* _UIB-CODE-BLOCK-END */





/* ********************  Preprocessor Definitions  ******************** */




/* _UIB-PREPROCESSOR-BLOCK-END */




/* *********************** Procedure Settings ************************ */


/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
 */


/* *************************  Create Window  ************************** */


/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 6.25
         WIDTH              = 38.
/* END WINDOW DEFINITION */
                                                                        */


 





/* ***************************  Main Block  *************************** */
{abp/abapi001.i2} /** Cria‡Æo de erros **/
{abp/ab9000.i}    /** ConversÆo de horas **/
{abp/ab9001.i}    /** Busca valores alternativos **/
/** Parametriza padräes de cabe‡alho e rodap‚ a serem exibidos **/
run piInicial in this-procedure.

/** Imprime cabe‡alho e abre o output para arquivo **/
{include/i-rpcab.i}    
{include/i-rpout.i &TOFILE=tt-param.arquivo}  /** Abertura do output do programa **/

/** Procedure para inicializar c lculos e impressÆo **/
run piPrincipal in this-procedure.

/** Fecha o output para arquivo **/
{include/i-rpclo.i}

return "OK":U. 
/*--- Fim do Programa ---*/

/* _UIB-CODE-BLOCK-END */



/* **********************  Internal Procedures  *********************** */




PROCEDURE displayParametros :
/*------------------------------------------------------------------------------
  Purpose:     displayParametros
  Parameters:  <none>
  Notes:       Mostra os parƒmetros selecionados
------------------------------------------------------------------------------*/
/** Cria a p gina dos parƒmetros **/
page.

{utp/ut-liter.i "SELE€ÇO" "*" "R"}
assign c-liter-sel = return-value.

{utp/ut-liter.i "PAR¶METROS" "*" "R"}
assign c-liter-par = return-value.

{utp/ut-liter.i "IMPRESSÇO" "*" "R"}
assign c-liter-log = return-value.

form    /*Selecao*/
        skip(1)
        c-liter-sel       AT 09       NO-LABEL
        skip(1)
        tt-param.data-ini    COLON 30 "|<  >|" AT 50
        tt-param.data-fim    NO-LABEL
        tt-param.empresa-ini COLON 30 "|<  >|" AT 50
        tt-param.empresa-fim NO-LABEL
        tt-param.eqpto-ini   COLON 30 "|<  >|" AT 50
        tt-param.eqpto-fim   NO-LABEL
        tt-param.placa-ini   COLON 30 "|<  >|" AT 50
        tt-param.placa-fim   NO-LABEL
        /*Parametro*/
        skip(1)
        c-liter-par     AT 09       NO-LABEL
        skip(1)
        tt-param.lAtivos    COLON 30
        tt-param.lInativos  COLON 30
        tt-param.lProprios  COLON 30
        tt-param.lTerceiros COLON 30
        tt-param.lErros     COLON 30
        /*Impressao*/
        skip(1)   
        c-liter-log     AT 09       no-label
        skip(1)
        c-destino             colon 30 " - " tt-param.arquivo NO-LABEL
        tt-param.usuario      colon 30
    with width 132 side-labels frame f-param-definidos stream-io.

{utp/ut-liter.i "Data" "*" "l"}
assign tt-param.data-ini:label in frame f-param-definidos = return-value.

{utp/ut-liter.i "Empresa" "*" "l"}
assign tt-param.empresa-ini:label in frame f-param-definidos = return-value.

{utp/ut-liter.i "Equipamento" "*" "l"}
assign tt-param.eqpto-ini:label in frame f-param-definidos = return-value.

{utp/ut-liter.i "Placa" "*" "l"}
assign tt-param.placa-ini:label in frame f-param-definidos = return-value.

{utp/ut-liter.i "Ativos" "*" "l"}
assign tt-param.lAtivos:label in frame f-param-definidos = return-value.

{utp/ut-liter.i "Inativos" "*" "l"}
assign tt-param.lInativos:label in frame f-param-definidos = return-value.

{utp/ut-liter.i "Pr¢prios" "*" "l"}
assign tt-param.lProprios:label in frame f-param-definidos = return-value.

{utp/ut-liter.i "Terceiros" "*" "l"}
assign tt-param.lTerceiros:label in frame f-param-definidos = return-value.

{utp/ut-liter.i "Erros" "*" "l"}
assign tt-param.lErros:label in frame f-param-definidos = return-value.

DISPLAY /*Selecao*/
        c-liter-sel
        tt-param.data-ini
        tt-param.data-fim
        tt-param.empresa-ini
        tt-param.empresa-fim
        tt-param.eqpto-ini
        tt-param.eqpto-fim
        tt-param.placa-ini
        tt-param.placa-fim
        c-liter-par
        tt-param.lAtivos
        tt-param.lInativos
        tt-param.lProprios
        tt-param.lTerceiros
        tt-param.lErros
        c-liter-log
        c-destino 
        tt-param.arquivo
        tt-param.usuario   
    WITH FRAME f-param-definidos.

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE finalizaHandle :
/*------------------------------------------------------------------------------
  Purpose:     finalizaHandle
  Parameters:  <none>
  Notes:       Elimina da mem¢ria o handle ds BOS
------------------------------------------------------------------------------*/
    if valid-handle(hDBOAbastecLubrific) THEN   
        run Destroy in hDBOAbastecLubrific.

    if valid-handle(hDBOItemAbastec) THEN   
        run Destroy in hDBOItemAbastec.

    return "ok":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE inicializaHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*:T--- BO da tabela mab-abastec-lubrific ---*/
     IF NOT VALID-HANDLE(hDBOAbastecLubrific) OR
        hDBOAbastecLubrific:TYPE <> "PROCEDURE":U OR
        hDBOAbastecLubrific:FILE-NAME <> "frbo/bofr055.p":U THEN DO:
        run frbo/bofr055.p persistent set hDBOAbastecLubrific.
     END.
     RUN openQueryStatic IN hDBOAbastecLubrific (INPUT "Main":U) NO-ERROR.
     
 /*:T--- BO da tabela item do abastecimento ---*/
     IF NOT VALID-HANDLE(hDBOItemAbastec) OR
        hDBOItemAbastec:TYPE <> "PROCEDURE":U OR
        hDBOItemAbastec:FILE-NAME <> "frbo/bofr057.p":U THEN DO:
        run frbo/bofr057.p persistent set hDBOItemAbastec.
     END.
     RUN openQueryStatic IN hDBOItemAbastec (INPUT "Main":U) NO-ERROR.

     /*:T--- BO da tabela item da lubrifica‡Æo ---*/
     IF NOT VALID-HANDLE(hDBOPosto) OR
        hDBOPosto:TYPE <> "PROCEDURE":U OR
        hDBOPosto:FILE-NAME <> "frbo/bofr021.p":U THEN DO:
        run frbo/bofr021.p persistent set hDBOPosto.
     END.
     RUN openQueryStatic IN hDBOPosto (INPUT "Main":U) NO-ERROR.

     
     /** Verifica se Consiste Ficha **/
     for first mab-param
         where mab-param.cdn-param = 11 no-lock:
         /** 1 = On-Line / 2 = Batch **/
         if mab-param.des-valor = "1":U then do:
             /*--- Seta a BO para executar a API ABAPI001 (Consistˆncia das Fichas) ---*/
             run setaExecutaAPI in hDBOItemAbastec  (input yes).
         END.
         else do:
             run setaExecutaAPI in hDBOItemAbastec  (input no).
         END.
     END.

 
return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE piCriaAbastecLubrific :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /** Deve limpara a temp-table para evitar erros**/
    empty temp-table ttAbastecLubrific.

   /** Cria temp-table **/
    create ttAbastecLubrific.

    /** Busca Atividade e Conta Cont bil do Hist¢rico de Atividade Equipamento **/
    run buscaAtividade in hDBOAbastecLubrific (input  mab-eqpto.ep-codigo,
                                               input  mab-eqpto.cod-eqpto,
                                               input  date(es_import_abastec.dataMovimento),
                                               output cAtivid,
                                               output cCTCodigo,
                                               output cCCcodigo,
                                               output ttAbastecLubrific.cod-unid-negoc,
                                               OUTPUT pContaLub,
                                               OUTPUT pccustolub,
                                               OUTPUT pEstabHist).

    IF tt-param.lHorimetro = YES THEN
       ASSIGN deContador = dec(es_import_abastec.contador) / 10.
    ELSE 
       ASSIGN deContador = dec(es_import_abastec.contador).

    assign ttAbastecLubrific.cc-codigo             = cCCcodigo
           ttAbastecLubrific.cod-ativid            = cAtivid
           ttAbastecLubrific.cod-eqpto             = mab-eqpto.cod-eqpto
           ttAbastecLubrific.ep-codigo-matr        = iEmpMatr
           ttAbastecLubrific.cod-estabel-matr      = cEstabMatr
           ttAbastecLubrific.cod-matr              = cMotorista
           ttAbastecLubrific.cod-posto             = mab-posto.cod-posto
           ttAbastecLubrific.dat-movto             = date(es_import_abastec.dataMovimento)
           ttAbastecLubrific.ep-codigo             = mab-eqpto.ep-codigo 
           ttAbastecLubrific.hra-inicial           = v-hora
           ttAbastecLubrific.hra-final             = v-hora
           ttAbastecLubrific.nr-nota-fis           = ""
           ttAbastecLubrific.num-docto             = iDocto
           ttAbastecLubrific.ct-codigo             = cCTCodigo
           ttAbastecLubrific.val-dat-hora-invrtda  = dValDataHoraInver
           ttAbastecLubrific.val-hodom-horim       = deContador
           ttAbastecLubrific.cod-emitente          = mab-posto.cod-emitente
           ttAbastecLubrific.cod-usuar             = c-seg-usuario
           ttAbastecLubrific.serie                 = ""
           ttAbastecLubrific.cod-rota              = cTrajeto
           ttAbastecLubrific.cod-usuar-alter       = c-seg-usuario
           ttAbastecLubrific.dat-origin            = ?
           ttAbastecLubrific.dsl-obs               = ""
           ttAbastecLubrific.val-hodom-horim-sec   = 0
           ttAbastecLubrific.idi-consist           = 1.

     /*--- Limpa temp-table RowErrors no DBO ---*/
      RUN emptyRowErrors IN hDBOAbastecLubrific.
      /*--- Transfere temp-table tt-requisicao para o DBO ---*/
      RUN setRecord IN hDBOAbastecLubrific (INPUT TABLE ttAbastecLubrific).
      /*--- Altera o registro corrente do DBO ---*/
      RUN createRecord IN hDBOAbastecLubrific.
      /*--- Atualiza vari vel cReturnAux com o conte£do do RETURN-VALUE ---*/
      ASSIGN cReturnAux = RETURN-VALUE.
      
      /*--- Verifica ocorrˆncia de erros durante a grava‡Æo ---*/
      IF cReturnAux = "NOK":U THEN DO:
            /*--- Retorna temp-table RowErrors do DBO ---*/
            RUN getRowErrors IN hDBOAbastecLubrific (OUTPUT TABLE RowErrors).

         /** Elimina todos os erros internos da BO (Navega‡Æo) **/
         for each  RowErrors
             where RowErrors.ErrorSubType = "INTERNAL":U exclusive-lock:
             delete RowErrors.
         end.

        assign iErro = 0.
            /** Busca £ltimo erro criado na BO **/
            for last RowErrorsAux no-lock:
                assign iErro = RowErrorsAux.ErrorSequence.
            end.

            /** Percorre os erros da BO **/
            for each RowErrors no-lock:
                /** Cria erros na BO **/

                create RowErrorsAux.
                buffer-copy RowErrors except RowErrors.ErrorSequence to RowErrorsAux
                    /** Coloca sequˆncia **/
                    assign RowErrorsAux.ErrorSequence = iErro + 1
                           RowErrorsAux.cPrograma     = "ESAB0104":U
                           RowErrorsAux.iEmpresa      = mab-eqpto.ep-codigo
                           RowErrorsAux.cEquipamento  = mab-eqpto.cod-eqpto
                           RowErrorsAux.dtData        = date(es_import_abastec.dataMovimento)
                           RowErrorsAux.cHora         = v-hora
                           RowErrorsAux.cPosto        = mab-posto.cod-posto
                           RowErrorsAux.cBomba        = es_import_abastec.codBomba
                           RowErrorsAux.cMotoris      = cMotorista
                           RowErrorsAux.iNumDocto     = 0
/*                            RowErrorsAux.ErrorDescription = string(RowErrorsAux.ErrorNumber) + "-" + RowErrorsAux.ErrorDescription */
                           .
                assign iErro = RowErrorsAux.ErrorSequence.

                if  RowErrorsAux.ErrorNumber = 17516 THEN DO:

                    RUN pi-verifica-mab-item-abastec.

                    IF  l-abastec-repetido = YES THEN
                        assign es_import_abastec.tptransac = 1 /* Importado */
                               es_import_abastec.obs       = RowErrorsAux.ErrorDescription.
                    ELSE
                    DO:

                        DELETE RowErrorsAux.

                    END.

                END. /* if  RowErrorsAux.ErrorNumber */

            end. /* for each RowErrors */

      end.

END PROCEDURE.

PROCEDURE piCriaItemAbastec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE de-quantidadeCombustivel LIKE es_import_abastec.quantidadeCombustivel NO-UNDO.
    DEFINE VARIABLE l-agrupado               AS   LOGICAL NO-UNDO.
    


    run buscaTipoMaterial in hDBOItemAbastec (input  mab-eqpto.ep-codigo,
                                              input  mab-eqpto.cod-eqpto,
                                              input  mab-tip-mater-item.it-codigo,
                                              input  date(es_import_abastec.dataMovimento),
                                              output cTpMater,
                                              output lLogAbastec,
                                              output deTanque).

    /** Deve limpara a temp-table para evitar erros**/
    empty temp-table ttItemAbastec.
    
    ASSIGN de-quantidadeCombustivel = es_import_abastec.quantidadeCombustivel
           l-agrupado               = NO.

    FOR EACH bf_es_import_abastec
             WHERE bf_es_import_abastec.obs = STRING(es_import_abastec.id)
             NO-LOCK:
       ASSIGN de-quantidadeCombustivel = de-quantidadeCombustivel + bf_es_import_abastec.quantidadeCombustivel. 
       
       ASSIGN l-agrupado = YES.
    END.         
    

    create ttItemAbastec.
    assign ttItemAbastec.cod-eqpto            = mab-eqpto.cod-eqpto
           ttItemAbastec.cod-posto            = mab-posto.cod-posto 
           ttItemAbastec.cod-tip-mater        = cTpMater 
           ttItemAbastec.ep-codigo            = mab-eqpto.ep-codigo 
           ttItemAbastec.it-codigo            = mab-tip-mater-item.it-codigo
           ttItemAbastec.log-abastec-prim     = YES
           ttItemAbastec.num-docto            = iDocto
           ttItemAbastec.val-dat-hora-invrtda = dValDataHoraInver
           ttItemAbastec.val-mater-1          = DECIMAL (de-quantidadeCombustivel /*es_import_abastec.quantidadeCombustivel*/ * deValorItem)
           ttItemAbastec.val-mater-2          = 0
           ttItemAbastec.val-mater-3          = 0
           ttItemAbastec.val-quant            = de-quantidadeCombustivel /*es_import_abastec.quantidadeCombustivel*/
           ttItemAbastec.cod-refer            = ""
           ttItemAbastec.cod-localiz          = mab-posto.cod-posto
           ttItemAbastec.lote                 = ""
           ttItemAbastec.nr-trans             = 0.

    for first bfItem fields(tipo-con-est loc-unica cod-localiz)
        where bfItem.it-codigo = ttItemAbastec.it-codigo no-lock:
        assign ttItemAbastec.cod-localiz = bfItem.cod-localiz.
    END.

    /* Valida Tipo do Posto para colocar (ou nÆo) a bomba */
    /* Pr¢prio = Informa a bomba */
    IF mab-posto.idi-posto = 1 THEN DO:
        ASSIGN ttItemAbastec.cod-bomba = es_import_abastec.codBomba.
    END.

    assign cReturnAux = "":U.
    /*--- Limpa temp-table RowErrors no DBO ---*/
    RUN emptyRowErrors IN hDBOItemAbastec.
    /*--- Transfere temp-table tt-requisicao para o DBO ---*/
    RUN setRecord IN hDBOItemAbastec (INPUT TABLE ttItemAbastec).
    /*--- Altera o registro corrente do DBO ---*/
    RUN createRecord IN hDBOItemAbastec.
    /*--- Atualiza vari vel cReturnAux com o conte£do do RETURN-VALUE ---*/
    ASSIGN cReturnAux = RETURN-VALUE.

  /*--- Verifica ocorrˆncia de erros durante a grava‡Æo ---*/
    IF cReturnAux = "NOK":U THEN DO:
        /*--- Retorna temp-table RowErrors do DBO ---*/
        RUN getRowErrors IN hDBOItemAbastec (OUTPUT TABLE RowErrors).

        /** Elimina todos os erros internos da BO (Navega‡Æo) **/
        for each  RowErrors
           where RowErrors.ErrorSubType = "INTERNAL":U exclusive-lock:
            delete RowErrors.
        end.

        assign iErro = 0.
        /** Busca £ltimo erro criado na BO **/
        for last RowErrorsAux no-lock:
            assign iErro = RowErrorsAux.ErrorSequence.
        end.
        /** Percorre os erros da BO **/
        for each RowErrors no-lock:
            /** Cria erros na BO **/

            create RowErrorsAux.
            buffer-copy RowErrors except RowErrors.ErrorSequence  to RowErrorsAux
                /** Coloca sequˆncia **/
                assign RowErrorsAux.ErrorSequence = iErro + 1
                       RowErrorsAux.cPrograma     = "ESAB0104":U
                       RowErrorsAux.iEmpresa      = mab-eqpto.ep-codigo
                       RowErrorsAux.cEquipamento  = mab-eqpto.cod-eqpto
                       RowErrorsAux.dtData        = date(es_import_abastec.dataMovimento)
                       RowErrorsAux.cHora         = v-hora
                       RowErrorsAux.cPosto        = mab-posto.cod-posto
                       RowErrorsAux.cBomba        = es_import_abastec.codBomba
                       RowErrorsAux.cMotoris      = cMotorista
                       RowErrorsAux.iNumDocto     = 0.
            assign iErro = RowErrorsAux.ErrorSequence.

            ASSIGN es_import_abastec.tptransac = 2 /* Erro */
                   es_import_abastec.obs       = RowErrorsAux.ErrorDescription.
        end.
    END.
    /* Caso nÆo tenha encontrado erros */
    else do:
        assign iErro = 0.
        /** Busca £ltimo erro criado na BO **/
        for last RowErrorsAux no-lock:
            assign iErro = RowErrorsAux.ErrorSequence.
        end.
        /** Cria Mensagens **/
        create RowErrorsAux.
        assign RowErrorsAux.ErrorSequence     = iErro + 1
               RowErrorsAux.ErrorNumber       = 0
               RowErrorsAux.ErrorDescription  = "Importado com Sucesso"
               RowErrorsAux.ErrorParameters   = ""
               RowErrorsAux.ErrorType         = ""
               RowErrorsAux.ErrorHelp         = ""
               RowErrorsAux.ErrorSubType      = ""
               RowErrorsAux.cPrograma         = "ESAB0104":U
               RowErrorsAux.iEmpresa          = mab-eqpto.ep-codigo
               RowErrorsAux.cEquipamento      = mab-eqpto.cod-eqpto
               RowErrorsAux.dtData            = date(es_import_abastec.dataMovimento)
               RowErrorsAux.cHora             = v-hora
               RowErrorsAux.cPosto            = mab-posto.cod-posto
               RowErrorsAux.cBomba            = es_import_abastec.codBomba
               RowErrorsAux.Qtde-Combustivel  = es_import_abastec.quantidadeCombustivel
               RowErrorsAux.Qtde-CombustivelA = IF de-quantidadeCombustivel = es_import_abastec.quantidadeCombustivel THEN 0 ELSE de-quantidadeCombustivel
               RowErrorsAux.cMotoris          = cMotorista
               RowErrorsAux.iNumDocto         = iDocto.
        assign iErro = RowErrorsAux.ErrorSequence.

        assign es_import_abastec.tptransac = 1 /* Importado */
               es_import_abastec.obs       = "Importado com Sucesso " + IF l-agrupado = YES THEN "(Agrupado)" ELSE "".
               
        IF l-agrupado THEN DO:       
           FIND FIRST es-agrup-import-abastec
                WHERE es-agrup-import-abastec.id = es_import_abastec.id
                NO-ERROR.       
           IF NOT AVAILABLE(es-agrup-import-abastec) THEN DO:
              CREATE es-agrup-import-abastec.
              ASSIGN es-agrup-import-abastec.id = es_import_abastec.id.  
           END.     
           
           ASSIGN es-agrup-import-abastec.qt-abastec = es_import_abastec.quantidadeCombustivel
                  es-agrup-import-abastec.dt-abastec = date(es_import_abastec.dataMovimento)
                  es-agrup-import-abastec.hr-abastec = v-hora
                  es-agrup-import-abastec.num-docto  = iDocto.
        END.              
 
        find first param-estoq no-lock no-error.
        if  param-estoq.mensal-ate > date(es_import_abastec.dataMovimento) then
            assign es_import_abastec.obs         = "Importado com Sucesso " + IF l-agrupado = YES THEN "(Agrupado)" ELSE "" + " => Nao gerar movimento combustivel"
                   RowErrorsAux.ErrorDescription = es_import_abastec.obs.

        ASSIGN RowErrorsAux.ErrorDescription = es_import_abastec.obs. 
        
        
        FOR EACH bf_es_import_abastec
                 WHERE bf_es_import_abastec.obs  = STRING(es_import_abastec.id)
                 EXCLUSIVE-LOCK:
           assign iErro = 0.
           /** Busca £ltimo erro criado na BO **/
           for last RowErrorsAux no-lock:
               assign iErro = RowErrorsAux.ErrorSequence.
           end.
           
           ASSIGN v-hora = SUBSTRING(bf_es_import_abastec.horaMovimento,1,2) + SUBSTRING(bf_es_import_abastec.horaMovimento,4,2) + SUBSTRING(bf_es_import_abastec.horaMovimento,7,2).

           /** Cria Mensagens **/
           create RowErrorsAux.
           assign RowErrorsAux.ErrorSequence     = iErro + 1
                  RowErrorsAux.ErrorNumber       = 0
                  RowErrorsAux.ErrorDescription  = "Importado com Sucesso"
                  RowErrorsAux.ErrorParameters   = ""
                  RowErrorsAux.ErrorType         = ""
                  RowErrorsAux.ErrorHelp         = ""
                  RowErrorsAux.ErrorSubType      = ""
                  RowErrorsAux.cPrograma         = "ESAB0104":U
                  RowErrorsAux.iEmpresa          = mab-eqpto.ep-codigo
                  RowErrorsAux.cEquipamento      = mab-eqpto.cod-eqpto
                  RowErrorsAux.dtData            = date(bf_es_import_abastec.dataMovimento)
                  RowErrorsAux.cHora             = v-hora
                  RowErrorsAux.cPosto            = mab-posto.cod-posto
                  RowErrorsAux.cBomba            = bf_es_import_abastec.codBomba
                  RowErrorsAux.Qtde-Combustivel  = bf_es_import_abastec.quantidadeCombustivel
                  RowErrorsAux.Qtde-CombustivelA = 0
                  RowErrorsAux.cMotoris          = cMotorista
                  RowErrorsAux.iNumDocto         = iDocto.
           assign iErro = RowErrorsAux.ErrorSequence.

           assign bf_es_import_abastec.tptransac = 1 /* Importado */
                  bf_es_import_abastec.obs       = "Importado com Sucesso (Agrupado no documento: " + string(iDocto) + ")" .
                  
           FIND FIRST es-agrup-import-abastec
                WHERE es-agrup-import-abastec.id = bf_es_import_abastec.id
                NO-ERROR.       
           IF NOT AVAILABLE(es-agrup-import-abastec) THEN DO:
              CREATE es-agrup-import-abastec.
              ASSIGN es-agrup-import-abastec.id = bf_es_import_abastec.id.  
           END.     
           
           ASSIGN es-agrup-import-abastec.qt-abastec = bf_es_import_abastec.quantidadeCombustivel
                  es-agrup-import-abastec.dt-abastec = date(bf_es_import_abastec.dataMovimento)
                  es-agrup-import-abastec.hr-abastec = v-hora
                  es-agrup-import-abastec.num-docto  = iDocto.
           
           ASSIGN RowErrorsAux.ErrorDescription = bf_es_import_abastec.obs. 
        END.         
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

PROCEDURE piInicial :
/*------------------------------------------------------------------------------
  Purpose:     piInicial
  Parameters:  <none>
  Notes:       Define os valores que serÆo mostrados o cabe‡alho e rodap‚
------------------------------------------------------------------------------*/
assign c-programa = "ESAB0104"
       c-versao   = "2.00"
       c-revisao  = "001"
       /** Define o destino do arquivo a ser gerado **/
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

/** Busca os parƒmetros criados na interface gr fica **/
find first tt-param no-lock no-error.

/** Busca empresa padrÆo **/
    FIND FIRST param-global NO-LOCK NO-ERROR.
    IF  AVAIL param-global THEN DO:
        FOR FIRST empresa
            WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK:
            assign c-empresa = empresa.razao-social.
        END.
        IF NOT AVAIL empresa THEN DO:
            {utp/ut-liter.i "Empresa dos Parƒmetros Globais NÆo Encontrada"}
            ASSIGN c-empresa = RETURN-VALUE.
        END.
    END.
    ELSE DO:
        RETURN "NOK":U.
    END.
    
/** Guarda valores para imprimir t¡tulos **/
{utp/ut-liter.i "Integra‡Æo Abastecimentos" "*"}
assign c-titulo-relat = trim(return-value).
{utp/ut-liter.i "Abastecimento e Lubrifica‡Æo" "*"}
assign c-sistema = trim(return-value).

return "OK":U.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */







PROCEDURE piIntegraAbastecimentos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCont  AS INTEGER NO-UNDO.
DEFINE VARIABLE lErros AS LOGICAL NO-UNDO.

/** Busca Motorista PadrÆo **/
FOR FIRST mab-param
    where mab-param.cdn-param = 37 NO-LOCK:
    IF NUM-ENTRIES(mab-param.des-valor,",") > 0 THEN
        assign iEmpMatr   = entry(1, mab-param.des-valor, ",")
               cEstabMatr = entry(2, mab-param.des-valor, ",")
               cMotorista = entry(3, mab-param.des-valor, ",").
END.

/** Busca Trajeto PadrÆo **/
FOR FIRST mab-param
    where mab-param.cdn-param = 38 NO-LOCK:
    assign cTrajeto   =  mab-param.des-valor.
END.

/* Busca Equipamento Conforme Sele‡Æo */
ASSIGN iErro = 0.


EMPTY TEMP-TABLE tt-dados.

DEFINE VARIABLE d-quantidadeCom LIKE es_import_abastec.quantidadeCom NO-UNDO.

/* Agrupa */
blk_eqpto:
FOR EACH  es_import_abastec
    WHERE es_import_abastec.ep_codigo           >= STRING(tt-param.empresa-ini)
    AND   es_import_abastec.ep_codigo           <= STRING(tt-param.empresa-fim)
    AND   es_import_abastec.cod_eqpto           >= tt-param.eqpto-ini
    AND   es_import_abastec.cod_eqpto           <= tt-param.eqpto-fim
    AND   date(es_import_abastec.dataMovimento) >= tt-param.data-ini
    AND   date(es_import_abastec.dataMovimento) <= tt-param.data-fim
    AND  (es_import_abastec.tptransac = 0 /* nÆo integrado */
     or   es_import_abastec.tptransac = 2 /* Erro */) EXCLUSIVE-LOCK:


    ASSIGN d-quantidadeCom = 0.

    /* Agrupando abastecimentos pelo horario */
    FOR EACH bf_es_import_abastec
             WHERE bf_es_import_abastec.dataMovimento   = es_import_abastec.dataMovimento   AND
                   SUBSTRING(bf_es_import_abastec.horaMovimento,1,5)   = SUBSTRING(es_import_abastec.horaMovimento,1,5)   AND
                   bf_es_import_abastec.contador        = es_import_abastec.contador        AND
                   bf_es_import_abastec.posto           = es_import_abastec.posto           AND
                   bf_es_import_abastec.codBomba        = es_import_abastec.codBomba        AND
                   bf_es_import_abastec.codMotorista    = es_import_abastec.codMotorista    AND
                   bf_es_import_abastec.tipoCombustiv   = es_import_abastec.tipoCombustiv   AND
                   bf_es_import_abastec.cod_eqpto       = es_import_abastec.cod_eqpto       AND
                   bf_es_import_abastec.ep_codigo       = es_import_abastec.ep_codigo       AND
                   bf_es_import_abastec.tptransac       = es_import_abastec.tptransac       AND
                   bf_es_import_abastec.id              <> es_import_abastec.id             AND
                   (bf_es_import_abastec.tptransac = 0 /* nÆo integrado */ OR bf_es_import_abastec.tptransac = 2 /* Erro */)
             EXCLUSIVE-LOCK:

       ASSIGN d-quantidadeCom = d-quantidadeCom + bf_es_import_abastec.quantidadeCom.
       
       
       ASSIGN bf_es_import_abastec.tptransac = 1 /* Importado */
              bf_es_import_abastec.obs       = STRING(es_import_abastec.id).
    END.          
END.

/* Busca Abastecimentos no GTFrotas */
blk_eqpto:
FOR EACH  es_import_abastec
    WHERE es_import_abastec.ep_codigo           >= STRING(tt-param.empresa-ini)
    AND   es_import_abastec.ep_codigo           <= STRING(tt-param.empresa-fim)
    AND   es_import_abastec.cod_eqpto           >= tt-param.eqpto-ini
    AND   es_import_abastec.cod_eqpto           <= tt-param.eqpto-fim
    AND   date(es_import_abastec.dataMovimento) >= tt-param.data-ini
    AND   date(es_import_abastec.dataMovimento) <= tt-param.data-fim
    AND  (es_import_abastec.tptransac = 0 /* nÆo integrado */
     or   es_import_abastec.tptransac = 2 /* Erro */)
     by es_import_abastec.cod_eqpto
     BY date(es_import_abastec.dataMovimento)
     BY es_import_abastec.horaMovimento:


    /* Busca o Motorista do Registro */
    FOR FIRST mab-motoris
        WHERE mab-motoris.cod-matr = es_import_abastec.codMotorista NO-LOCK:
        ASSIGN iEmpMatr   = mab-motoris.ep-codigo
               cEstabMatr = mab-motoris.cod-estabel
               cMotorista = mab-motoris.cod-matr.
    END.

    /* Busca Posto */
    FIND FIRST mab-posto WHERE mab-posto.cod-posto = es_import_abastec.posto NO-LOCK NO-ERROR.
    if  not avail mab-posto then do:
        /* nÆo encontrou equipamento */
        /** Cria Mensagens **/
        create RowErrorsAux.
        ASSIGN RowErrorsAux.ErrorSequence     = iErro + 1
               RowErrorsAux.ErrorNumber       = 0
               RowErrorsAux.ErrorDescription  = "Posto nÆo cadastrado"
               RowErrorsAux.ErrorParameters   = ""
               RowErrorsAux.ErrorType         = ""
               RowErrorsAux.ErrorHelp         = ""
               RowErrorsAux.ErrorSubType      = ""
               RowErrorsAux.cPrograma         = "ESAB0104":U
               RowErrorsAux.iEmpresa          = es_import_abastec.ep_codigo
               RowErrorsAux.cEquipamento      = es_import_abastec.cod_eqpto
               RowErrorsAux.dtData            = date(es_import_abastec.dataMovimento)
               RowErrorsAux.cHora             = v-hora
               RowErrorsAux.cPosto            = es_import_abastec.posto
               RowErrorsAux.cBomba            = es_import_abastec.codBomba
               RowErrorsAux.Qtde-Combustivel  = es_import_abastec.quantidadeCombustivel
               RowErrorsAux.Qtde-CombustivelA = 0
               RowErrorsAux.cMotoris          = cMotorista.
        assign iErro = RowErrorsAux.ErrorSequence.

        ASSIGN es_import_abastec.tptransac = 2 /* Erro */
               es_import_abastec.obs       = "ERRO - Equipamento nÆo cadastrado.".

        NEXT blk_eqpto.
    end.

    /*- Inicia Grava‡Æo da MAB-ABASTEC-LUBRIFIC -*/
    ASSIGN v-hora = SUBSTRING(es_import_abastec.horaMovimento,1,2) + SUBSTRING(es_import_abastec.horaMovimento,4,2) + SUBSTRING(es_import_abastec.horaMovimento,7,2).

    FIND FIRST mab-eqpto NO-LOCK
        WHERE mab-eqpto.ep-codigo = es_import_abastec.ep_codigo
        AND   mab-eqpto.cod-eqpto = es_import_abastec.cod_eqpto NO-ERROR.
    IF AVAILABLE mab-eqpto THEN DO:
        /* Valida Equipamentos Ativos/Inativos */
        IF mab-eqpto.dat-situacao  = ? AND NOT(tt-param.lAtivos)   THEN NEXT blk_eqpto.
        IF mab-eqpto.dat-situacao <> ? AND NOT(tt-param.lInativos) THEN NEXT blk_eqpto.
        /* Valida Equipamentos Pr¢prios/Terceiros */
        IF mab-eqpto.idi-tip-propriet = 1 AND NOT(tt-param.lProprios)  THEN NEXT blk_eqpto.
        IF mab-eqpto.idi-tip-propriet = 2 AND NOT(tt-param.lTerceiros) THEN NEXT blk_eqpto.
    END.
    ELSE DO:
        /* nÆo encontrou equipamento */
        /** Cria Mensagens **/
        create RowErrorsAux.
        ASSIGN RowErrorsAux.ErrorSequence     = iErro + 1
               RowErrorsAux.ErrorNumber       = 0
               RowErrorsAux.ErrorDescription  = "Equipamento nÆo cadastrado"
               RowErrorsAux.ErrorParameters   = ""
               RowErrorsAux.ErrorType         = ""
               RowErrorsAux.ErrorHelp         = ""
               RowErrorsAux.ErrorSubType      = ""
               RowErrorsAux.cPrograma         = "ESAB0104":U
               RowErrorsAux.iEmpresa          = es_import_abastec.ep_codigo
               RowErrorsAux.cEquipamento      = es_import_abastec.cod_eqpto
               RowErrorsAux.dtData            = date(es_import_abastec.dataMovimento)
               RowErrorsAux.cHora             = v-hora
               RowErrorsAux.cPosto            = es_import_abastec.posto
               RowErrorsAux.cBomba            = es_import_abastec.codBomba
               RowErrorsAux.Qtde-Combustivel  = es_import_abastec.quantidadeCombustivel
               RowErrorsAux.Qtde-CombustivelA = 0
               RowErrorsAux.cMotoris          = cMotorista.
        assign iErro = RowErrorsAux.ErrorSequence.

        ASSIGN es_import_abastec.tptransac = 2 /* Erro */
               es_import_abastec.obs       = "ERRO - Equipamento nÆo cadastrado.".

        NEXT blk_eqpto.
    END.

    /** Busca numera‡Æo do documento de Abastecimento/Lubrifica‡Æo**/
    run buscaDocumento in hDBOAbastecLubrific (output iDocto).

    /* INICIO GRAVA€ÇO DO DOCUMENTO */
    ASSIGN lErros = NO.

    run converteParaHoraInvertida (input string(date(es_import_abastec.dataMovimento),"99/99/9999"), 
                                   input v-hora,
                                   output dValDataHoraInver).

    run piCriaAbastecLubrific.

    ASSIGN lErros = NO.
    IF CAN-FIND(FIRST  RowErrorsAux
                WHERE (RowErrorsAux.ErrorSubType = "ERROR":U
                OR     RowErrorsAux.ErrorSubType = "":U)
                AND    RowErrorsAux.ErrorNumber  > 0
                AND    RowErrorsAux.iEmpresa     = mab-eqpto.ep-codigo
                AND    RowErrorsAux.cEquipamento = mab-eqpto.cod-eqpto
                AND    RowErrorsAux.dtData       = date(es_import_abastec.dataMovimento)
                and    RowErrorsAux.cHora        = v-hora) THEN
        ASSIGN lErros = YES.
    /*- FIM: Grava‡Æo da MAB-ABASTEC-LUBRIFIC -*/

    
    if  es_import_abastec.tptransac = 0 or
        es_import_abastec.tptransac = 2 then do:
        if lErros THEN undo blk_eqpto, NEXT blk_eqpto.
    end.
    else do:
        if lErros then next blk_eqpto.
    end.
    

    /*- Inicia Grava‡Æo da MAB-ITEM-ABASTEC -*/

    /* Busca Tipo de Material */
    FIND LAST mab-histor-combust WHERE mab-histor-combust.ep-codigo = mab-eqpto.ep-codigo
                                 AND   mab-histor-combust.cod-eqpto = mab-eqpto.cod-eqpto NO-LOCK NO-ERROR.

    FIND FIRST mab-tip-mater-item WHERE mab-tip-mater-item.cod-tip-mater = mab-histor-combust.cod-tip-mater NO-LOCK NO-ERROR.

    /* bno - 22/11/2013 */
    ASSIGN deValorItem = 0.
    for first item-estab no-lock
        where item-estab.it-codigo   = mab-tip-mater-item.it-codigo
        and   item-estab.cod-estabel = cEstabMatr:
        ASSIGN deValorItem = item-estab.val-unit-mat-m[1]
                           + item-estab.val-unit-mob-m[1]
                           + item-estab.val-unit-ggf-m[1].
    end.
    IF deValorItem = 0 THEN DO:
        for first item-uni-estab no-lock
            where item-uni-estab.it-codigo   = mab-tip-mater-item.it-codigo
            and   item-uni-estab.cod-estabel = cEstabMatr:
            assign deValorItem = item-uni-estab.preco-ul-ent.
        end.
    END.

    run piCriaItemAbastec.

    ASSIGN lErros = NO.
    IF CAN-FIND(FIRST  RowErrorsAux
                WHERE (RowErrorsAux.ErrorSubType = "ERROR":U
                OR     RowErrorsAux.ErrorSubType = "":U)
                AND    RowErrorsAux.ErrorNumber  > 0
                AND    RowErrorsAux.iEmpresa     = mab-eqpto.ep-codigo
                AND    RowErrorsAux.cEquipamento = mab-eqpto.cod-eqpto
                AND    RowErrorsAux.dtData       = date(es_import_abastec.dataMovimento)
                and    RowErrorsAux.cHora        = v-hora) THEN
        ASSIGN lErros = YES.
    /*- FIM: Grava‡Æo da MAB-ITEM-ABASTEC -*/

    if lErros THEN undo blk_eqpto, NEXT blk_eqpto.
END.

return "OK":U.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */

PROCEDURE piPrincipal :
/*------------------------------------------------------------------------------
  Purpose:     piPrincipal
  Parameters:  <none>
  Notes:       Corpo principal da aplica‡Æo
------------------------------------------------------------------------------*/
/** Mostra frames de cabe‡alho e rodap‚ padräes da Datasul **/
view frame f-cabec.
view frame f-rodape. 
/*********************************************************************
   In¡cio do espa‡o para l¢gica de c lculo e display das informa‡äes
*********************************************************************/

/** Inicializa programa de acompanhamento padrÆo Datasul **/
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input c-titulo-relat).

run inicializaHandle in this-procedure. /**Inicializa as BOïs**/

/** Busca numera‡Æo do documento de Abastecimento/Lubrifica‡Æo**/
run buscaDocumento in hDBOAbastecLubrific (output iDocto).

ASSIGN iPrimeiroDocto = iDocto.

RUN piIntegraAbastecimentos IN THIS-PROCEDURE.

    IF tt-param.lErros THEN DO:
        VIEW frame f-erro2.
        for each RowErrorsAux :
            if  RowErrorsAux.ErrorNumber <> 0 then
                assign RowErrorsAux.iNumDocto = 0.

            PUT RowErrorsAux.cPrograma         AT 001
                RowErrorsAux.iEmpresa          AT 010
                RowErrorsAux.cEquipamento      AT 018
                RowErrorsAux.dtData            AT 035
                RowErrorsAux.cHora             AT 046
                RowErrorsAux.cPosto            AT 055
                RowErrorsAux.cBomba            AT 064
                RowErrorsAux.cMotoris          AT 077
                RowErrorsAux.iNumDocto         AT 088
                RowErrorsAux.Qtde-Combustivel  AT 103 
                RowErrorsAux.Qtde-CombustivelA AT 117
                RowErrorsAux.ErrorNumber       AT 128 FORMAT ">>>>>9"
                RowErrorsAux.ErrorDescription FORMAT "x(60)" AT 135 SKIP.
        END.
    END.

run finalizaHandle in this-procedure. /** Elimina da men¢ria **/

/******************************************************************
   Fim do espa‡o para l¢gica de c lculo e display das informa‡äes
******************************************************************/

/** Mostra parƒmetros selecionados **/
run displayParametros in this-procedure.
/** Finaliza programa de acompanhamento padrÆo Datasul **/
run pi-finalizar in h-acomp.

return "OK":U.

END PROCEDURE.

/*-----------------------------------------------------------------*/

PROCEDURE pi-verifica-mab-item-abastec. 

   ASSIGN l-abastec-repetido = YES.

   ASSIGN v-hora2 = substr(es_import_abastec.horaMovimento,1,2) 
                  + substr(es_import_abastec.horaMovimento,4,2)
                  + "00".

   FOR FIRST mab-abastec-lubrific 
       where mab-abastec-lubrific.ep-codigo   = es_import_abastec.ep_codigo
       and   mab-abastec-lubrific.cod-eqpto   = es_import_abastec.cod_eqpto
       and   mab-abastec-lubrific.dat-movto   = date(es_import_abastec.dataMovimento)
       and   mab-abastec-lubrific.hra-inicial = v-hora2 
       no-lock:

       ASSIGN l-abastec-repetido = NO.

       FIND FIRST bf-mab-item-abastec 
            WHERE bf-mab-item-abastec.num-docto = mab-abastec-lubrific.num-docto
            AND   bf-mab-item-abastec.cod-posto = es_import_abastec.posto    
            AND   bf-mab-item-abastec.cod-bomba = es_import_abastec.codBomba 
         /* AND   mab-item-abastec.it-codigo = pit-codigo */
            NO-LOCK NO-ERROR.

        /*--- Verifica se registro foi encontrado, em caso de erro serÿ retornada flag "NOK":U ---*/
        IF  AVAILABLE bf-mab-item-abastec THEN 
            ASSIGN l-abastec-repetido = YES.
        ELSE
            ASSIGN idocto = mab-abastec-lubrific.num-docto.

   END. /* FOR FIRST mab-abastec-lubrific */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */




