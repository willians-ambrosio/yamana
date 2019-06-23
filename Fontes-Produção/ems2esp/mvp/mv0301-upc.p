/***************************************************************************************
** Programa: mv0301-upc.p
** Objetivo:
** Autor...: Ricardo Costa Sutil - Manufatura    - Datasul Manufatura
             Leonardo Correia Santos de Oliveira - Datasul Manufatura
             Gustavo Eduardo Tamanini            - Datasul Manufatura
** Data.........: 02/2008
** ?
** Data.........: 05/2008 
** 2.00.00.001..: 06/2009 - Gustavo Eduardo Tamanini - FO 2006.829
****************************************************************************************/
{include/i-prgvrs.i MV0301-UPC 2.00.00.001}
{cdp/cd0669.i}
{esp/esapi002.i}

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTE      NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.
/********************************* WIDGET-HANDLE **************************************/
DEFINE NEW GLOBAL SHARED VARIABLE h-mv0301-upc           AS HANDLE         NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-btTecnico-mv0301    AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-combo-box-mv0301    AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fill-in-data-mv0301 AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fill-in-hora-mv0301 AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-win-mv0301          AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-prog-mv0301         AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE tx-label-data-mv0301   AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE tx-label-status-mv0301 AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-nr-ord-produ-mv0301 AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fPage2              AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fPage1              AS WIDGET-HANDLE  NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-objeto               AS HANDLE         NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-brSon2-mv0301       AS WIDGET-HANDLE  NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-tx-mv0301-ordem-inv AS WIDGET-HANDLE  NO-UNDO. 
DEFINE NEW GLOBAL SHARED VARIABLE wh-mv0301-ordem-inv    AS WIDGET-HANDLE  NO-UNDO. 

/** Var Globais existentes no programa MV0301, utilizadas para atualizar as Contas **/
DEFINE NEW GLOBAL SHARED VARIABLE c-glob-conta-contabil AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE c-glob-Ccusto         AS CHARACTER NO-UNDO.

/*************************** Vari veis para Tradu‡Æo **********************************/
DEFINE VARIABLE labelStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE labelData   AS CHARACTER NO-UNDO.
/************************************ Tradu‡Æo ****************************************/
{utp/ut-liter.i Status: * R} 
ASSIGN labelStatus = TRIM(RETURN-VALUE).
{utp/ut-liter.i Data_Situa‡Æo: * R}
ASSIGN labelData = TRIM(RETURN-VALUE).

ASSIGN h-objeto = p-wgh-object.

DEFINE VARIABLE h-fiConta  AS HANDLE NO-UNDO.
DEFINE VARIABLE h-fiCcusto AS HANDLE NO-UNDO.

RUN findWidget (INPUT "fPage1",   INPUT "FRAME",   INPUT p-wgh-frame, OUTPUT wh-fPage1).
RUN findWidget (INPUT "fiConta",  INPUT "FILL-IN", INPUT wh-fPage1,   OUTPUT h-fiConta).
RUN findWidget (INPUT "fiCcusto", INPUT "FILL-IN", INPUT wh-fPage1,   OUTPUT h-fiCcusto).
   
if p-ind-event = "leave-estabel":U and
   valid-handle(h-fiConta) and
   valid-handle(h-fiCcusto) then
   assign c-glob-conta-contabil = h-fiConta:screen-value
          c-glob-Ccusto         = h-fiCcusto:screen-value.

/** Ao iniciar a tela, eventos para campos espec¡ficos **/ 
IF p-ind-event = "AFTER-INITIALIZE":U THEN DO: 

    RUN mvp/mv0301-upc.p PERSISTENT SET h-mv0301-upc (INPUT "",
                                                      INPUT "",
                                                      INPUT p-wgh-object,
                                                      INPUT p-wgh-frame,
                                                      INPUT "",
                                                      INPUT p-row-table).

    RUN findWidget (INPUT "nr-ord-produ", 
                    INPUT "FILL-IN", 
                    INPUT p-wgh-frame, 
                    OUTPUT wh-nr-ord-produ-mv0301).

    RUN findWidget (INPUT "fPage2", 
                    INPUT "FRAME", 
                    INPUT p-wgh-frame, 
                    OUTPUT wh-fPage2).

    /** Busca a handle do Browse **/
    RUN findWidget (INPUT "brSon2", 
                    INPUT "BROWSE", 
                    INPUT wh-fPage2, 
                    OUTPUT wh-brSon2-mv0301).
    
    ASSIGN wh-win-mv0301  = p-wgh-frame:window.
           wh-prog-mv0301 = p-wgh-object:handle.
    
    /** Label para o Status **/
    CREATE TEXT tx-label-status-mv0301
    ASSIGN FRAME        = p-wgh-frame
           FORMAT       = "x(40)"
           WIDTH        = 15
           ROW          = 5.95
           COL          = 64.5
           SCREEN-VALUE = labelStatus 
           VISIBLE      = YES
           FONT         = 1.

    /** Combo-Box status da OM**/
    CREATE COMBO-BOX wh-combo-box-mv0301
    ASSIGN FRAME              = p-wgh-frame
           SIDE-LABEL-HANDLE  = tx-label-status-mv0301:HANDLE
           WIDTH              = 20.57
           ROW                = 5.83
           COL                = 69.5
           FONT               = 1
           VISIBLE            = YES
           SENSITIVE          = NO
           FORMAT             = "x(40)"
           INNER-LINES        = 5
           &IF "{&aplica_facelift}" = "YES" &THEN
                {include/i_fcldin.i wh-fill}
           &ENDIF
           TRIGGERS:
           END TRIGGERS.
    
    /** Para tradu‡Æo dos valores apresentado na Combo-box **/
     ASSIGN wh-combo-box-mv0301:LIST-ITEMS = {ydminc/i00ydm001.i 03}. 
    
    /** Label para o Data **/
    CREATE TEXT tx-label-data-mv0301
        ASSIGN FRAME        = p-wgh-frame
               FORMAT       = "x(40)"
               WIDTH        = 15
               ROW          = 4.95
               COL          = 63
               SCREEN-VALUE = labelData 
               VISIBLE      = YES
               FONT         = 1.

    /** Fill-in Data apresentado em tela **/
    CREATE FILL-IN wh-fill-in-data-mv0301
        ASSIGN FRAME     = p-wgh-frame
               SIDE-LABEL-HANDLE  = tx-label-data-mv0301:HANDLE 
               WIDTH              = 10
               HEIGHT             = 0.88
               ROW                = 4.83
               COL                = 73.50
               FONT               = 1
               VISIBLE            = YES
               SENSITIVE          = NO
               &IF "{&aplica_facelift}" = "YES" &THEN
		            {include/i_fcldin.i wh-fill}
	           &ENDIF
               TRIGGERS:
               END TRIGGERS.

    /** Fill-in para apresentar o valor hora **/
    CREATE FILL-IN wh-fill-in-hora-mv0301
        ASSIGN FRAME     = p-wgh-frame
               WIDTH     = 6
               HEIGHT    = 0.88
               ROW       = 4.83
               COL       = 84.15
               FONT      = 1
               FORMAT    = "99:99"
               VISIBLE   = YES
               SENSITIVE = NO
               &IF "{&aplica_facelift}" = "YES" &THEN
		            {include/i_fcldin.i wh-fill}
	           &ENDIF
               TRIGGERS:
               END TRIGGERS. 


    /** Botao T‚cnico **/
    CREATE BUTTON wh-btTecnico-mv0301
    ASSIGN FRAME     = wh-fPage2
           LABEL     = "&T‚cnico":U
           WIDTH     = 10.00
           HEIGHT    = 1.00
           ROW       = 11.25
           COL       = 74
           FONT      = 1
           VISIBLE   = yes
           SENSITIVE = YES
    TRIGGERS:   
        on choose persistent run mvp/mv0301-upca.p (INPUT wh-nr-ord-produ-mv0301,
                                                    INPUT wh-brSon2-mv0301).
    END TRIGGERS. 

    CREATE TEXT wh-tx-mv0301-ordem-inv
        ASSIGN FRAME        = wh-fPage1
               FORMAT       = "x(20)"
               WIDTH        = 10.30
               SCREEN-VALUE = "Ordem Invest:"
               ROW          = 7.40
               COL          = 58.00
               VISIBLE      = YES
               FONT         = 1.

    CREATE FILL-IN wh-mv0301-ordem-inv
        ASSIGN FRAME             = wh-fPage1
               SIDE-LABEL-HANDLE = wh-tx-mv0301-ordem-inv:handle
               DATA-TYPE         = "integer"
               FORMAT            = ">>>>>,>>>"
               NAME              = "Ordem Invest"
               WIDTH             = 16.57
               HEIGHT            = 0.88
               ROW               = 7.25
               COL               = 67.50
               LABEL             = "Ordem Invest: "
               VISIBLE           = YES
               SENSITIVE         = NO
               FONT              = 1
        TRIGGERS:
            ON 'F5':U                    PERSISTENT RUN pi-pesquisa in h-mv0301-upc.
            ON 'MOUSE-SELECT-DBLCLICK':U PERSISTENT RUN pi-pesquisa in h-mv0301-upc.
        END TRIGGERS.

    wh-mv0301-ordem-inv:LOAD-MOUSE-POINTER("image/lupa.cur").

    IF VALID-HANDLE(wh-brSon2-mv0301) THEN DO:
        IF wh-brSon2-mv0301:num-selected-rows = 0 THEN
            ASSIGN wh-btTecnico-mv0301:SENSITIVE = NO.
        ELSE
            ASSIGN wh-btTecnico-mv0301:SENSITIVE = YES.

    END.

    DO WHILE VALID-HANDLE(h-objeto):
        IF h-objeto:FILE-NAME = "utp/thinFolder.w" THEN LEAVE.
        ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
    END.
        
    /** Seta pagina em que o programa deve inicializar **/
    RUN setFolder IN h-objeto (INPUT 1).

    RUN piAtualizaInformacao.

END. /* After-Initialize */ 

IF p-ind-event = "AFTER-DELETE":U THEN DO: 
     IF  VALID-HANDLE(wh-brSon2-mv0301)    and
         VALID-HANDLE(wh-btTecnico-mv0301) THEN DO:
         IF wh-brSon2-mv0301:NUM-SELECTED-ROWS = 0 THEN
             ASSIGN wh-btTecnico-mv0301:SENSITIVE = NO.
         ELSE
             ASSIGN wh-btTecnico-mv0301:SENSITIVE = YES.             
     END.
END.

/** Ao ser acionado o evento de adicionar ou alterar ‚ habilitado o campo   **
 ** Status                                                                  **/
IF p-ind-event = "AFTER-ENABLE" THEN DO:
    ASSIGN wh-combo-box-mv0301:SENSITIVE = YES
           wh-mv0301-ordem-inv:SENSITIVE = YES.
END.

/** Quando desabilita o campo dos procedimento do Produto PadrÆo
    o evento ‚ informado, desabilitando tamb‚m o combo-box Status **/
IF p-ind-event = "AFTER-DISABLE" THEN DO:
    ASSIGN wh-combo-box-mv0301:SENSITIVE = NO
           wh-mv0301-ordem-inv:SENSITIVE = NO.
END.

/** Ao navegar pelo registros das ordens de manuten‡Æo roda
    a atualiza‡Æo das onforma‡äes                           **/ 
IF p-ind-event = "AFTER-DISPLAY" THEN DO:

    /** Busca a handle do Browse ***/
    RUN findWidget (INPUT "brSon2", 
                    INPUT "BROWSE", 
                    INPUT wh-fPage2, 
                    OUTPUT wh-brSon2-mv0301).

    IF VALID-HANDLE(wh-combo-box-mv0301) THEN DO:            
        IF NOT VALID-HANDLE(wh-nr-ord-produ-mv0301) THEN DO:
               RUN findWidget (INPUT "nr-ord-produ", 
                               INPUT "FILL-IN", 
                               INPUT p-wgh-frame, 
                               OUTPUT wh-nr-ord-produ-mv0301).
        END.
        RUN piAtualizaInformacao.

        IF wh-brSon2-mv0301:num-selected-rows = 0 THEN
           ASSIGN wh-btTecnico-mv0301:SENSITIVE = NO.
        ELSE
           ASSIGN wh-btTecnico-mv0301:SENSITIVE = YES.
    END.
END.

IF  p-ind-event = "AFTER-CONTROL-TOOL-BAR":U THEN DO:
    /** Busca a handle do Browse ***/
    RUN findWidget (INPUT "brSon2", 
                    INPUT "BROWSE", 
                    INPUT wh-fPage2, 
                    OUTPUT wh-brSon2-mv0301).

    IF VALID-HANDLE(wh-combo-box-mv0301) THEN DO:            
        IF NOT VALID-HANDLE(wh-nr-ord-produ-mv0301) THEN DO:
               RUN findWidget (INPUT "nr-ord-produ", 
                               INPUT "FILL-IN", 
                               INPUT p-wgh-frame, 
                               OUTPUT wh-nr-ord-produ-mv0301).
        END.
        RUN piAtualizaInformacao.

        IF wh-brSon2-mv0301:NUM-SELECTED-ROWS = 0 THEN
           ASSIGN wh-btTecnico-mv0301:SENSITIVE = NO.
        ELSE
           ASSIGN wh-btTecnico-mv0301:SENSITIVE = YES.
    END.
END.

PROCEDURE piAtualizaInformacao:
    /** Para evitar situa‡äes que fiquem sem Handle **/
    IF NOT VALID-HANDLE(wh-nr-ord-produ-mv0301) THEN DO:
        RUN findWidget (INPUT "nr-ord-produ", 
                        INPUT "FILL-IN", 
                        INPUT p-wgh-frame, 
                        OUTPUT wh-nr-ord-produ-mv0301).
    END.

    /* Respons vel pro buscar as informa‡äes do status da Ordem em tela */
    FOR FIRST mmv-ord-status
        WHERE mmv-ord-status.nr-ord-produ = INT(wh-nr-ord-produ-mv0301:SCREEN-VALUE) NO-LOCK:
    END.
    IF AVAIL mmv-ord-status THEN DO:
        ASSIGN wh-combo-box-mv0301:SCREEN-VALUE    = {ydminc/i00ydm001.i 04 mmv-ord-status.idi-status-ord}
               wh-fill-in-data-mv0301:SCREEN-VALUE = STRING(mmv-ord-status.data-sit)
               wh-fill-in-hora-mv0301:SCREEN-VALUE = STRING(mmv-ord-status.hora-sit)
               wh-mv0301-ordem-inv:SCREEN-VALUE    = STRING(mmv-ord-status.num-livre-1, ">>>>>,>>>").
    END.
    ELSE DO:
        IF INT(wh-nr-ord-produ-mv0301:SCREEN-VALUE) <> 0 THEN DO:
            IF NOT AVAIL mmv-ord-status THEN DO:
                  FOR FIRST mmv-ord-manut FIELDS(nr-ord-produ dat-abert hra-abert)
                      WHERE mmv-ord-manut.nr-ord-produ = INT(wh-nr-ord-produ-mv0301:SCREEN-VALUE) NO-LOCK:
                  END.
                  IF AVAIL mmv-ord-manut THEN DO:
                      CREATE mmv-ord-status.
                      ASSIGN mmv-ord-status.nr-ord-produ   = INT(wh-nr-ord-produ-mv0301:SCREEN-VALUE)
                             mmv-ord-status.idi-status-ord = 1  
                             mmv-ord-status.data-sit       = mmv-ord-manut.dat-abert 
                             mmv-ord-status.hora-sit       = mmv-ord-manut.hra-abert.
    
                      ASSIGN wh-combo-box-mv0301:SCREEN-VALUE    = {ydminc/i00ydm001.i 04 1}
                             wh-fill-in-data-mv0301:SCREEN-VALUE = STRING(mmv-ord-manut.dat-abert,"99/99/99")
                             wh-fill-in-hora-mv0301:SCREEN-VALUE = mmv-ord-manut.hra-abert
                             wh-mv0301-ordem-inv:SCREEN-VALUE    = "":U.
                  END.
            END.
        END.
    END.
END.

IF p-ind-event = "AFTER-ADD" THEN DO:
    ASSIGN wh-combo-box-mv0301:SCREEN-VALUE    = {ydminc/i00ydm001.i 04 01}
           wh-fill-in-data-mv0301:SCREEN-VALUE = ""
           wh-fill-in-hora-mv0301:SCREEN-VALUE = ""
           wh-mv0301-ordem-inv:SCREEN-VALUE    = "".
END.

IF p-ind-event = "AFTER-UPDATE" THEN DO:
    /** NÆo permite vincular OM Invest. apos cria‡Æo da OM. **/
    IF VALID-HANDLE(wh-mv0301-ordem-inv) THEN DO:
        IF INT(wh-mv0301-ordem-inv:SCREEN-VALUE) = 0 THEN
            ASSIGN wh-mv0301-ordem-inv:SENSITIVE = NO.
    END.
END.

PROCEDURE findWidget:
    DEFINE INPUT  PARAMETER c-widget-name  AS CHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER c-widget-type  AS CHAR   NO-UNDO.
    DEFINE INPUT  PARAMETER h-start-widget AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER h-widget       AS HANDLE NO-UNDO.
    
    DO WHILE VALID-HANDLE(h-start-widget):
        if h-start-widget:NAME = c-widget-name AND
           h-start-widget:TYPE = c-widget-type THEN DO:
            ASSIGN h-widget = h-start-widget:HANDLE.
            LEAVE.
        END.
    
        IF h-start-widget:TYPE = "field-group":u OR
           h-start-widget:TYPE = "frame":u OR
           h-start-widget:TYPE = "dialog-box":u THEN DO:
            RUN findWidget (INPUT  c-widget-name,
                            INPUT  c-widget-type,
                            INPUT  h-start-widget:FIRST-CHILD,
                            OUTPUT h-widget).
    
            IF VALID-HANDLE(h-widget) THEN
                LEAVE.
        END.
        ASSIGN h-start-widget = h-start-widget:NEXT-SIBLING.
    END.
END PROCEDURE.

PROCEDURE pi-pesquisa:
    
    IF NOT AVAIL param-global THEN
        FIND FIRST param-global NO-LOCK NO-ERROR.

    RUN mvp/mv0301-zoom.p (INPUT p-wgh-object,
                           INPUT param-global.empresa-prin).

END PROCEDURE.

RETURN "OK":U.
