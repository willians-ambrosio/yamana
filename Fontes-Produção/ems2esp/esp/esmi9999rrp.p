/********************************************************************************
*******************************************************************************/
{include/i-prgvrs.i ESMI9999R 12.00.00.001} 
/*****************************************************************************
**                        ALTERA€åES
******************************************************************************
** DATA......:15/08/2018
** AUTOR.....: WILLIANS AMBROSIO - GRUPO DKP
** DESCRI€ÇO.: INCLUSÇO DE COLUNAS SEGUNDO CHG0032597 (REV001)
*******************************************************************************/

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field nr-ord-ini       as INTEGER
    field nr-ord-fim       as INTEGER
    FIELD equipamento-ini  AS CHARACTER
    FIELD equipamento-fim  AS CHARACTER
    FIELD dt-abertura-ini  AS DATE
    FIELD dt-abertura-fim  AS DATE
    FIELD dt-exec-ini      AS DATE
    FIELD dt-exec-fim      AS DATE
    FIELD status-ini       AS CHARACTER
    FIELD status-fim       AS CHARACTER
    FIELD tag-ini          AS CHARACTER
    FIELD tag-fim          AS CHARACTER
    /* Begins REV001 - inclusÆo equipe responsavel */
    FIELD cd-equip-res-ini AS CHARACTER
    FIELD cd-equip-res-fin AS CHARACTER.
    /* end REV001 */

/*
DEFINE TEMP-TABLE tt-raw-digita
    FIELD raw-digita as raw.

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita. */

DEFINE INPUT PARAMETER TABLE FOR tt-param.
/* ============================================================================================== */
/* Fun»’o para limpeza de caracteres especiais                                                    */
/* ============================================================================================== */
{include/i-freeac.i}

FUNCTION RetiraSimbolos RETURNS CHAR
   ( INPUT-OUTPUT p_Texto AS CHAR):

   DEF VAR v_Chars AS CHAR INIT "›õÝä«—&*<>@".           
   /* Lista de caracteres a retirar */
   DEF VAR i         AS INT.

   DO i = 1 TO LENGTH(v_Chars):
      ASSIGN p_Texto = REPLACE(p_Texto,SUBSTR(v_Chars,i,1),'').
   END.

   RETURN p_Texto.   /* Retorna a nova string */
END.

/* ============================================================================================== */
DEFINE VARIABLE h-acomp            AS HANDLE                 NO-UNDO.
DEFINE VARIABLE chExcel            AS COM-HANDLE             NO-UNDO.
DEFINE VARIABLE iLinha             AS INTEGER                NO-UNDO.
DEFINE VARIABLE iCol               AS INTEGER                NO-UNDO.
DEFINE VARIABLE iLinhaAux          AS INTEGER                NO-UNDO.
DEFINE VARIABLE cStringSCarcEspec  AS CHARACTER              NO-UNDO.
/* ============================================================================================== */
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp( INPUT "Imprimindo Ordens...").

/* CREATE tt-param.
RAW-TRANSFER raw-param to tt-param. */

FIND FIRST tt-param NO-LOCK NO-ERROR.
RUN piIniciaExcel.

ASSIGN iLinha = 1.

FOR EACH ord-manut
    WHERE ord-manut.nr-ord-produ  >= tt-param.nr-ord-ini     
      AND ord-manut.nr-ord-produ  <= tt-param.nr-ord-fim     
      AND ord-manut.cd-equipto    >= tt-param.equipamento-ini
      AND ord-manut.cd-equipto    <= tt-param.equipamento-fim
      AND ord-manut.dt-ini-orig   >= tt-param.dt-abertura-ini
      AND ord-manut.dt-ini-orig   <= tt-param.dt-abertura-fim
      AND ord-manut.dt-prev-manut >= tt-param.dt-exec-ini    
      AND ord-manut.dt-prev-manut <= tt-param.dt-exec-fim    
      AND ord-manut.cd-tag        >= tt-param.tag-ini  
      AND ord-manut.cd-tag        <= tt-param.tag-fim 
      AND ord-manut.cd-equip-res  >= tt-param.cd-equip-res-ini 
      AND ord-manut.cd-equip-res  <= tt-param.cd-equip-res-fin 
      NO-LOCK:                

    FIND FIRST es-ord-manut-ext WHERE 
               es-ord-manut-ext.nr-ord-produ = ord-manut.nr-ord-produ NO-LOCK NO-ERROR.
    IF AVAIL es-ord-manut-ext THEN
    DO:
        IF es-ord-manut-ext.id-status < tt-param.status-ini OR
           es-ord-manut-ext.id-status > tt-param.status-fim THEN
           NEXT.

        FIND es-ord-manut-status WHERE es-ord-manut-status.id = es-ord-manut-ext.id-status NO-LOCK NO-ERROR.
    END.

    FIND msg-ord-man WHERE msg-ord-man.nr-ord-produ = ord-manut.nr-ord-produ NO-LOCK NO-ERROR.

    FIND equipto WHERE equipto.cd-equipto = ord-manut.cd-equipto NO-LOCK NO-ERROR.
                                                      
    RUN pi-acompanhar IN h-acomp(INPUT ord-manut.nr-ord-produ).

    ASSIGN iLinha = iLinha + 1.

    ASSIGN cStringSCarcEspec = ord-manut.des-man-corr
           cStringSCarcEspec = IF SUBSTRING(cStringSCarcEspec,1,1) = "@" THEN SUBSTRING(cStringSCarcEspec,2,LENGTH(cStringSCarcEspec)) ELSE cStringSCarcEspec
           chExcel:Range("A" + STRING(iLinha)):VALUE = ord-manut.nr-ord-produ  
           chExcel:Range("A" + STRING(iLinha)):NumberFormat = "###.###.###.##0" 
           chExcel:Range("B" + STRING(iLinha)):VALUE = cStringSCarcEspec
           chExcel:Range("B" + STRING(iLinha)):NumberFormat = "@"
           chExcel:Range("C" + STRING(iLinha)):VALUE = ord-manut.cd-equipto + ( IF AVAIL equipto THEN " - " + equipto.descricao ELSE "" )
           chExcel:Range("C" + STRING(iLinha)):NumberFormat = "@"
           chExcel:Range("D" + STRING(iLinha)):VALUE = ord-manut.cd-tag
           chExcel:Range("D" + STRING(iLinha)):NumberFormat = "@"
           chExcel:Range("E" + STRING(iLinha)):VALUE = ord-manut.dt-ini-orig
           chExcel:Range("E" + STRING(iLinha)):NumberFormat = "dd/mm/aaaa"            
           chExcel:Range("F" + STRING(iLinha)):VALUE = ord-manut.dt-prev-manut
           chExcel:Range("F" + STRING(iLinha)):NumberFormat = "dd/mm/aaaa"
           chExcel:Range("M" + STRING(iLinha)):VALUE = ord-manut.cd-manut
           chExcel:Range("M" + STRING(iLinha)):NumberFormat = "@"
           chExcel:Range("N" + STRING(iLinha)):VALUE = {ininc/i01in271.i 4 ord-manut.estado}
           chExcel:Range("N" + STRING(iLinha)):NumberFormat = "@"
           chExcel:Range("O" + STRING(iLinha)):VALUE = IF AVAIL es-ord-manut-status THEN es-ord-manut-status.descricao ELSE ""
           chExcel:Range("O" + STRING(iLinha)):NumberFormat = "@"
           chExcel:Range("P" + STRING(iLinha)):VALUE = ord-manut.cd-planejado
           chExcel:Range("P" + STRING(iLinha)):NumberFormat = "@"
           chExcel:Range("Q" + STRING(iLinha)):VALUE = ord-manut.cd-equip-res                     
           chExcel:Range("Q" + STRING(iLinha)):NumberFormat = "@".

    IF AVAIL es-ord-manut-ext THEN
    DO:
       FIND usuar_mestre WHERE usuar_mestre.cod_usuario = es-ord-manut-ext.usuario-alt NO-LOCK NO-ERROR.
       IF AVAIL usuar_mestre THEN
          ASSIGN chExcel:Range("R" + STRING(iLinha)):VALUE = usuar_mestre.cod_usuario + " - " + usuar_mestre.nom_usuario
                 chExcel:Range("R" + STRING(iLinha)):NumberFormat = "@". 
    END.

    ASSIGN cStringSCarcEspec = msg-ord-man.msg-exp
           cStringSCarcEspec = IF SUBSTRING(cStringSCarcEspec,1,1) = "@" THEN SUBSTRING(cStringSCarcEspec,2,LENGTH(cStringSCarcEspec)) ELSE cStringSCarcEspec.

    ASSIGN chExcel:Range("S" + STRING(iLinha)):VALUE = cStringSCarcEspec
           chExcel:Range("S" + STRING(iLinha)):NumberFormat = "@"
           iLinhaAux = iLinha.

    FOR EACH ord-esp WHERE
             ord-esp.nr-ord-produ = ord-manut.nr-ord-produ NO-LOCK:  

        ASSIGN chExcel:Range("G" + STRING(iLinha)):VALUE = ord-esp.cd-tarefa
               chExcel:Range("G" + STRING(iLinha)):NumberFormat = "###.##0"           
               chExcel:Range("H" + STRING(iLinha)):VALUE = ord-esp.tp-especial
               chExcel:Range("H" + STRING(iLinha)):NumberFormat = "@".

        FIND FIRST mi-espec WHERE 
                   mi-espec.tp-especial = ord-esp.tp-especial NO-LOCK NO-ERROR.
        IF AVAIL mi-espec THEN
           ASSIGN chExcel:Range("I" + STRING(iLinha)):VALUE = mi-espec.descricao
                  chExcel:Range("I" + STRING(iLinha)):NumberFormat = "@".         
               
        ASSIGN chExcel:Range("J" + STRING(iLinha)):VALUE = ord-esp.nr-homens 
               chExcel:Range("J" + STRING(iLinha)):NumberFormat = "###.##0"
               chExcel:Range("K" + STRING(iLinha)):VALUE = ord-esp.tempo  
               chExcel:Range("K" + STRING(iLinha)):NumberFormat = "###.###.##0,0000".

        FIND FIRST ord-taref WHERE 
                   ord-taref.nr-ord-produ = ord-esp.nr-ord-produ AND
                   ord-taref.cd-tarefa    = ord-esp.cd-tarefa    NO-LOCK NO-ERROR.
        IF AVAIL ord-taref THEN
           ASSIGN chExcel:Range("L" + STRING(iLinha)):VALUE = ord-taref.tempo-real           
                  chExcel:Range("L" + STRING(iLinha)):NumberFormat = "###.###.##0,0000".

        /* GRAVA O REGISTRO E Jµ REPOSICIONA NA PRàXIMA LINHA */
        ASSIGN iLinha = iLinha + 1.
    END.

    /* VOLTA UMA LINHA PARA NÇO PULAR LINHA */
    IF iLinhaAux < iLinha THEN
       ASSIGN iLinha = iLinha - 1.

    IF AVAIL es-ord-manut-ext THEN
       RELEASE es-ord-manut-status.
END.


RUN pi-inicializar IN h-acomp( INPUT "Finalizando Excel, Aguarde...").
RUN piFinalizaExcel.

RUN pi-finalizar IN h-acomp.

RETURN 'OK'.


/* ============================================================================================== */
/* ===================== Inicia Excel ========================== */
PROCEDURE piIniciaExcel:

    /* ======= Cria um novo objeto Excel ====== */
    CREATE "Excel.Application" chExcel NO-ERROR.

    IF  chExcel = ? THEN 
    DO:
        RUN pi-finalizar IN h-acomp.
        RUN utp/ut-msgs.p (INPUT "show",
                           INPUT  15825,
                           INPUT "Aplicativo MS EXCEL nCo instalado !~~" + 
                                 "O Microsoft Excel nCo estÙ instalado ou configurado adequadamente, e por este motivo o relat?rio nCo pode ser gerado.").
        RETURN 'NOK'.
    END.
    
    /* ================================= */
    chExcel:VISIBLE        = NO.
    chExcel:ScreenUpdating = NO.

    chExcel:WorkBooks:ADD( SEARCH("esp/esmi9999r.xltx") ) NO-ERROR.
    
    chExcel:worksheets:ITEM(1):SELECT     NO-ERROR.
        
END PROCEDURE.


/* ============================= Finaliza Excel ==================== */
PROCEDURE piFinalizaExcel:

    chExcel:Range("A:S"):EntireColumn:AutoFit.

    chExcel:ScreenUpdating = YES.
    chExcel:VISIBLE = YES.
    
    /* chWorkSheet:SAVE().  */
    
    /* RELEASE OBJECT chWorksheet              NO-ERROR. */
    RELEASE OBJECT chExcel       NO-ERROR. 
    /* RELEASE OBJECT chWorkbook               NO-ERROR. */

END PROCEDURE.
