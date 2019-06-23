/*****************************************************************
** Autor...: Gustavo Eduardo Tamanini
** Empresa.: Yamana
** Programa: AB0303-UPC
** UPC cadastrada para programa: AB03034
** Objetivo: 
******************************************************************/
{include/i-prgvrs.i AB0303-UPC 2.06.00.000}

/** Parƒmetros **/
define input parameter p-ind-event  as character     no-undo.
define input parameter p-ind-object as character     no-undo.
define input parameter p-wgh-object as handle        no-undo.
define input parameter p-wgh-frame  as widget-handle no-undo.
define input parameter p-cod-table  as character     no-undo.
define input parameter p-row-table  as rowid         no-undo.
/** Handle **/
define variable h-ab0303-upc        as handle        no-undo.
define variable h-tx-label          as handle        no-undo.
define variable h-tx-label2         as handle        no-undo.
define variable h-tx-label3         as handle        no-undo.
define variable h-ep-codigo         as handle        no-undo.
define variable h-num-docto         as handle        no-undo.
define variable h-fPage1            as handle        no-undo.
DEFINE VARIABLE h-bofr059           AS HANDLE        NO-UNDO.
/** Global **/
define new global shared var wh-fi-espec-0303       as widget-handle no-undo.
define new global shared var wh-fi-horas-espec-0303 as widget-handle no-undo.
define new global shared var wh-fi-dt-situ-0303     as widget-handle no-undo.
define new global shared var wh-fi-hr-situ-0303     as widget-handle no-undo.
DEFINE NEW GLOBAL SHARED VARIABLE wh-fPage2         AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-objeto          AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-button         AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-button2        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-button3        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-combo-box-0303 AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR tx-label-status-0303   AS WIDGET-HANDLE NO-UNDO.
/** Browse **/
DEFINE NEW GLOBAL SHARED VARIABLE qr-ttReservas     AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hdl-ttReservas    AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-ttReservas      AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-bf-ttReservas   AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-browse          AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-field           AS HANDLE        NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h-Column          AS HANDLE        NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wgh-browse-teste  AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wgh-coluna        AS WIDGET-HANDLE NO-UNDO.

/** Vari vel deixar a Window desabilitada ao ser chamado os programas de rela‡Æo**/
DEFINE NEW GLOBAL SHARED VARIABLE wh-frame          AS WIDGET-HANDLE NO-UNDO.

/** Defini‡Æo da tabela para cerregar os dados mostrados no Browse Reservas Materiais  **/
DEFINE TEMP-TABLE ttReservas NO-UNDO LIKE mab-evento-mat
    FIELD c-saldo AS CHARACTER.

DEFINE TEMP-TABLE tt-mab-movto-event NO-UNDO LIKE mab-movto-event
    FIELD r-rowid AS ROWID.

DEFINE BUFFER bf-mab-movto-event FOR mab-movto-event.

RUN findWidget (INPUT "fPage1",    INPUT "FRAME",   INPUT p-wgh-frame, OUTPUT h-fPage1).
RUN findWidget (INPUT "ep-codigo", INPUT "FILL-IN", INPUT h-fPage1,    OUTPUT h-ep-codigo).
RUN findWidget (INPUT "num-docto", INPUT "FILL-IN", INPUT p-wgh-frame, OUTPUT h-num-docto).

/** Pega-se a caracteristica Window e passa para o Handle para desabilitar o**/
/** a janela ao chamar uma das windows espec¡ficas de rela‡Æo com o usu rio **/   
ASSIGN wh-frame  = p-wgh-frame:WINDOW.

/*************************** Vari veis para Tradu‡Æo **********************************/
DEFINE VARIABLE labelStatus AS CHARACTER  NO-UNDO.
/************************************ Tradu‡Æo ****************************************/
{utp/ut-liter.i Status: * R} 
ASSIGN labelStatus = TRIM(RETURN-VALUE).

IF NOT VALID-HANDLE(h-bofr059) THEN DO:
    RUN frbo/bofr059.p PERSISTENT SET h-bofr059.
END.

if  p-ind-event  = "AFTER-INITIALIZE":U and
    p-ind-object = "CONTAINER":U        then do:

    run abp/ab0303-upc.p persistent set h-ab0303-upc (input "",
                                                      input "",
                                                      input p-wgh-object,
                                                      input p-wgh-frame,
                                                      input "",
                                                      input p-row-table).
    /** Label Especialidade **/
    create text h-tx-label
    assign frame        = h-fPage1
           format       = "x(15)"
           width        = 10.14
           screen-value = "Especialidade:":U
           row          = 3.30
           col          = 67.10
           visible      = yes
           font         = 1.

    /** Create Fill-in Especialidade **/
    create fill-in wh-fi-espec-0303
    assign frame     = h-fPage1
           name      = "wh-fi-espec-0303"
           format    = "x(12)"
           width     = 8.86
           height    = 0.88
           row       = 3.25
           col       = 77.40
           visible   = yes
           sensitive = no
           font      = 1
           triggers:
                on "MOUSE-SELECT-DBLCLICK":U  persistent run piZoom in h-ab0303-upc.
                on "F5":U persistent run piZoom in h-ab0303-upc.                
           end.
           wh-fi-espec-0303:load-mouse-pointer("image~\lupa.cur":U).

    /** Label Horas Especialidade **/
    create text h-tx-label2
    assign frame        = h-fPage1
           format       = "x(20)"
           width        = 15.00
           screen-value = "Horas Especialidade:":U
           row          = 4.30
           col          = 62.80
           visible      = yes
           font         = 1.

    /** Create Fill-in Horas Especialidade **/
    create fill-in wh-fi-horas-espec-0303
    assign frame     = h-fPage1
           name      = "wh-fi-horas-espec-0303"
           format    = "x(10)"
           width     = 8.86
           height    = 0.88
           row       = 4.25
           col       = 77.40
           visible   = yes
           sensitive = no
           font      = 1.

    /** Label Data Situacao **/
    CREATE TEXT h-tx-label3
    ASSIGN FRAME        = h-fPage1
           FORMAT       = "x(20)"
           WIDTH        = 15.00
           SCREEN-VALUE = "Situa‡Æo:":U
           ROW          = 1.30
           COL          = 62.50
           VISIBLE      = YES
           FONT         = 1.

    /** Create Fill-in Data Situacao **/
    CREATE FILL-IN wh-fi-dt-situ-0303
    ASSIGN FRAME     = h-fPage1
           NAME      = "wh-fi-dt-situ-0303"
           DATA-TYPE = "DATE"
           FORMAT    = "99/99/99"
           WIDTH     = 9.72
           HEIGHT    = 0.88
           ROW       = 1.25
           COL       = 70.00
           VISIBLE   = yes
           SENSITIVE = no
           FONT      = 1.

    /** Create Fill-in Horas Situacao **/
    CREATE FILL-IN wh-fi-hr-situ-0303
    ASSIGN FRAME     = h-fPage1
           NAME      = "wh-fi-hr-situ-0303"
           DATA-TYPE = "CHARACTER"
           FORMAT    = "99:99"
           WIDTH     = 5.86
           HEIGHT    = 0.88
           ROW       = 1.25
           COL       = 80.3 /* 79.86 */
           VISIBLE   = yes
           SENSITIVE = no
           FONT      = 1.

    /** Label para o Status **/
    CREATE TEXT tx-label-status-0303
    ASSIGN FRAME        = h-fPage1
           FORMAT       = "x(40)"
           WIDTH        = 15
           ROW          = 2.30
           COL          = 62.50
           SCREEN-VALUE = labelStatus 
           VISIBLE      = YES
           FONT         = 1.

    /** Combo-Box status da OM**/
    CREATE COMBO-BOX wh-combo-box-0303
    ASSIGN FRAME              = h-fPage1
           SIDE-LABEL-HANDLE  = tx-label-status-0303:HANDLE
           WIDTH              = 18.00
           ROW                = 2.25
           COL                = 68.3 /* 67.80 */
           FONT               = 1
           VISIBLE            = YES
           SENSITIVE          = NO
           FORMAT             = "x(50)"
           INNER-LINES        = 5
           &IF "{&aplica_facelift}" = "YES" &THEN
                {include/i_fcldin.i wh-fill}
           &ENDIF
           TRIGGERS:
           END TRIGGERS.
    
    /** Para tradu‡Æo dos valores apresentado na Combo-box **/
    ASSIGN wh-combo-box-0303:LIST-ITEMS = {ydminc/i00ydm001.i 03}.

    /** Atualiza dados Especialidade/Horas **/
    run piAtualiza in this-procedure.

    ASSIGN h-objeto = p-wgh-object.

    {include/i_fclpreproc.i} 
    &IF "{&aplica_facelift}" = "YES" &THEN
        {include/i_fcldef.i}
    &ENDIF

    /** cria e instancia Reserva de Materiais **/
    CREATE FRAME wh-fPage2
    ASSIGN COL         = 3.14
           ROW         = 5.46
           WIDTH       = 85.86
           HEIGHT      = 13.79
           NAME        = "Reserva Materiais"
           SIDE-LABELS = YES
           SENSITIVE   = YES
           OVERLAY     = YES
           BGCOLOR     = ?
           BOX         = NO
           THREE-D     = YES.

    &IF "{&aplica_facelift}" = "YES" &THEN
       {include/i_fcldin.i wh-fPage2}
    &ENDIF

    ASSIGN h-objeto = p-wgh-object.
    
    DO WHILE VALID-HANDLE(h-objeto):
        IF  h-objeto:FILE-NAME = "utp/thinFolder.w" THEN LEAVE.
        ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
    END.

    RUN insertFolder IN h-objeto (INPUT ?,
                                  INPUT p-wgh-frame,
                                  INPUT wh-fPage2,
                                  INPUT "Reserva":U).

    /** Cria botÆo Incluir na Frame Reservas**/
    CREATE BUTTON wh-button
    ASSIGN FRAME  = wh-fPage2
           WIDTH     = 10
           HEIGHT    = 1
           ROW       = 12.90
           COL       = 2.55
           FONT      = 1
           VISIBLE   = YES
           SENSITIVE = YES
           LABEL     = "&Incluir":U
           TRIGGERS:
                ON CHOOSE PERSISTENT RUN piRUNESAB0303B-ADD IN h-ab0303-upc.
           END TRIGGERS.

    /** Cria botÆo Modificar na Frame Reservas **/
    CREATE BUTTON wh-button2
    ASSIGN FRAME  = wh-fPage2
           WIDTH     = 10
           HEIGHT    = 1
           ROW       = 12.90
           COL       = 12.70
           FONT      = 1
           VISIBLE   = yes
           SENSITIVE = yes
           LABEL     = "&Modificar":U
           TRIGGERS:
                 ON CHOOSE PERSISTENT RUN piRUNESAB0303B-UP IN h-ab0303-upc.
           END TRIGGERS.

     /** Cria botÆo Eliminar na Frame Reservas**/
    CREATE BUTTON wh-button3
    ASSIGN FRAME  = wh-fPage2
           WIDTH     = 10
           HEIGHT    = 1
           ROW       = 12.90
           COL       = 22.70
           FONT      = 1
           VISIBLE   = yes
           SENSITIVE = yes
           LABEL     = "&Eliminar":U
           TRIGGERS:
                 ON CHOOSE PERSISTENT RUN piRUNESAB0303B-DEL IN h-ab0303-upc.
           END TRIGGERS.

        IF VALID-HANDLE(h-num-docto) THEN DO:
        /** Buffer para criar query da ttReservas **/
            hdl-ttReservas = BUFFER ttReservas:HANDLE.
            CREATE TEMP-TABLE       h-ttReservas.
                                    h-ttReservas:CREATE-LIKE(hdl-ttReservas).
                                    h-ttReservas:TEMP-TABLE-PREPARE("ttReservas").
              h-bf-ttReservas = h-ttReservas:DEFAULT-BUFFER-HANDLE.

            /** limpa temp-table e buffer **/
            IF VALID-HANDLE(h-bf-ttReservas) THEN DO:
                IF h-bf-ttReservas:RECORD-LENGTH > 0 THEN
                   h-bf-ttReservas:EMPTY-TEMP-TABLE().
            END.
            ELSE LEAVE.
            EMPTY TEMP-TABLE ttReservas.
        
            FOR EACH ttReservas:
                h-bf-ttReservas:BUFFER-CREATE.
                h-bf-ttReservas:BUFFER-COPY(BUFFER ttReservas:HANDLE).
            END.
        
            /** cria, abre e posiciona query **/
            CREATE QUERY qr-ttReservas.
                         qr-ttReservas:SET-BUFFERS(h-bf-ttReservas).
                         qr-ttReservas:QUERY-PREPARE("FOR EACH ttReservas").
                         qr-ttReservas:QUERY-OPEN().
                         qr-ttReservas:GET-FIRST().

            /** cria browse para ttReservas **/
            CREATE BROWSE h-browse
            ASSIGN FRAME = wh-fPage2
                   QUERY = qr-ttReservas:HANDLE
                   ROW                = 1.25
                   COL                = 2.50
                   WIDTH              = 82
                   HEIGHT             = 11.60
                   VISIBLE            = YES
                   SENSITIVE          = TRUE
                   READ-ONLY          = NO
                   FONT               = 1
                   SEPARATORS         = TRUE   
                   COLUMN-SCROLLING   = TRUE
                   SCROLLBAR-VERTICAL = TRUE
                   REFRESHABLE        = TRUE.  

            /* colunas do browse */
            ASSIGN h-field = h-bf-ttReservas:BUFFER-FIELD("it-codigo")
                   h-field:VALIDATE-EXPRESSION = "":U
                   h-field:COLUMN-LABEL        = "Item":U
                   h-field:FORMAT = "x(16)"
                   h-Column = h-browse:ADD-LIKE-COLUMN(h-field)
                   h-Column:WIDTH = 16.
                                                                                                        
            ASSIGN h-field = h-bf-ttReservas:BUFFER-FIELD("descricao")
                   h-field:VALIDATE-EXPRESSION = ""
                   h-field:COLUMN-LABEL        = "Descri‡Æo"
                   h-field:FORMAT = "x(30)"
                   h-Column = h-browse:ADD-LIKE-COLUMN(h-field)
                   h-Column:WIDTH = 30.
            
            ASSIGN h-field = h-bf-ttReservas:BUFFER-FIELD("quantidade")
                   h-field:VALIDATE-EXPRESSION = "":U
                   h-field:COLUMN-LABEL        = "Quantidade":U
                   h-Column = h-browse:ADD-LIKE-COLUMN(h-field). 
            
            ASSIGN h-field = h-bf-ttReservas:BUFFER-FIELD("c-saldo")
                   h-field:VALIDATE-EXPRESSION = ""
                   h-field:COLUMN-LABEL        = "Saldo em Estoque":U
                   h-field:FORMAT = "x(16)"
                   h-Column = h-browse:ADD-LIKE-COLUMN(h-field)
                   h-Column:WIDTH = 16.

            RUN piAtualizaBrowse.
            RUN piAtualizaInformacao.

            /** seta p gina em que o programa deve inicializar **/
            RUN setFolder IN h-objeto (INPUT 1).
        END.

        RUN piValidadeRecord IN THIS-PROCEDURE.
END.

/** Ao ser acionado o evento de adicionar ou alterar ‚ habilitado o campo   **
 ** Status                                                                  **/
IF p-ind-event = "AFTER-ENABLE" THEN DO:
    IF VALID-HANDLE(wh-combo-box-0303) THEN
        ASSIGN wh-combo-box-0303:SENSITIVE = YES.
END.

/** Quando desabilita o campo dos procedimento do Produto PadrÆo
    o evento ‚ informado, desabilitando tamb‚m o combo-box Status **/
IF p-ind-event = "AFTER-DISABLE" THEN DO:
    IF VALID-HANDLE(wh-combo-box-0303) THEN
        ASSIGN wh-combo-box-0303:SENSITIVE = NO.
END.

IF  p-ind-event = "AFTER-DISPLAY":U THEN DO:
    RUN piAtualiza IN THIS-PROCEDURE.

    IF  VALID-HANDLE(h-num-docto) AND 
        VALID-HANDLE(h-browse)    THEN
        RUN piAtualizaBrowse.
    IF VALID-HANDLE(wh-combo-box-0303) THEN
        RUN piAtualizaInformacao.

    RUN piValidadeRecord IN THIS-PROCEDURE.
END.

IF  p-ind-event = "AFTER-CONTROL-TOOL-BAR":U THEN DO:
    IF VALID-HANDLE(wh-combo-box-0303) THEN
        RUN piAtualizaInformacao.
END.

IF  p-ind-event = "AFTER-ADD":U     OR
    p-ind-event = "AFTER-COPY":U    OR
    p-ind-event = "AFTER-UPDATE":U  OR
    p-ind-event = "AFTER-DISABLE":U THEN DO:
    IF VALID-HANDLE(h-ep-codigo) THEN DO:
        ASSIGN wh-fi-horas-espec-0303:SENSITIVE = h-ep-codigo:SENSITIVE
               wh-fi-espec-0303:SENSITIVE       = h-ep-codigo:SENSITIVE.
    END.
END.

IF p-ind-event = "AFTER-OPEN-QUERY":U THEN DO:
    IF  VALID-HANDLE(h-num-docto) AND 
        VALID-HANDLE(h-browse)    THEN DO:
        RUN piAtualizaBrowse.
    END.
END.

IF p-ind-event = "AFTER-ADD":U THEN DO:
    IF VALID-HANDLE(wh-combo-box-0303) THEN
        ASSIGN wh-combo-box-0303:SCREEN-VALUE = {ydminc/i00ydm001.i 04 01}.
    IF VALID-HANDLE(wh-fi-hr-situ-0303) THEN
        ASSIGN wh-fi-hr-situ-0303:SCREEN-VALUE = SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) + SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2).
    IF VALID-HANDLE(wh-fi-dt-situ-0303) THEN
        ASSIGN wh-fi-dt-situ-0303:SCREEN-VALUE = STRING(TODAY,"99/99/99").
END.

PROCEDURE piAtualizaInformacao:
    /* Respons vel pro buscar as informa‡äes do status do Evento em tela */
    IF VALID-HANDLE(h-num-docto) THEN DO:
        FOR FIRST mab-event-status
            WHERE mab-event-status.num-docto = INT(h-num-docto:SCREEN-VALUE):
        END.
        IF AVAIL mab-event-status THEN DO:
            IF  VALID-HANDLE(wh-fi-dt-situ-0303) AND
                VALID-HANDLE(wh-combo-box-0303)  AND
                VALID-HANDLE(wh-fi-hr-situ-0303) THEN DO:
                ASSIGN wh-combo-box-0303:SCREEN-VALUE  = {ydminc/i00ydm001.i 04 mab-event-status.idi-status-ord}
                       wh-fi-dt-situ-0303:SCREEN-VALUE = STRING(mab-event-status.data,"99/99/99")
                       wh-fi-hr-situ-0303:SCREEN-VALUE = mab-event-status.hora.
            END.
        END.
        ELSE DO:
            IF VALID-HANDLE(wh-combo-box-0303) THEN
                ASSIGN wh-combo-box-0303:SCREEN-VALUE = {ydminc/i00ydm001.i 04 01}.
            IF VALID-HANDLE(wh-fi-hr-situ-0303) THEN
                ASSIGN wh-fi-hr-situ-0303:SCREEN-VALUE = "":U.
            IF VALID-HANDLE(wh-fi-dt-situ-0303) THEN
                ASSIGN wh-fi-dt-situ-0303:SCREEN-VALUE = "":U.
        END.
    END.
END.

PROCEDURE piRUNESAB0303B-ADD:
    IF VALID-HANDLE(h-num-docto) THEN DO:
        /** Desativa a janela **/
         ASSIGN wh-frame:SENSITIVE = NO.
         RUN abp/esab0303b.w (INPUT "ADD":U,
                              INPUT INT(h-num-docto:SCREEN-VALUE),
                              INPUT "":U,
                              INPUT 0).
         /** Ativa janela ap¢s o processo **/
         ASSIGN wh-frame:SENSITIVE = YES.
         RUN piAtualizaBrowse.
    END.
END.

PROCEDURE piRUNESAB0303B-UP:
    IF  VALID-HANDLE(h-num-docto) AND
        VALID-HANDLE(h-browse)    THEN DO:
        
        IF h-browse:NUM-SELECTED-ROWS > 0 THEN DO:
            ASSIGN wgh-coluna = h-browse:FIRST-COLUMN.
            FOR FIRST mab-evento-mat
                WHERE mab-evento-mat.num-docto = INT(h-num-docto:SCREEN-VALUE)
                AND   mab-evento-mat.it-codigo = wgh-coluna:SCREEN-VALUE NO-LOCK:
            END.
    
            /** Desativa a janela **/
             ASSIGN wh-frame:SENSITIVE = NO.
             RUN abp/esab0303b.w (INPUT "UPDATE":U,
                                  INPUT INT(h-num-docto:SCREEN-VALUE),
                                  INPUT wgh-coluna:SCREEN-VALUE,
                                  INPUT mab-evento-mat.quant).
             /** Ativa janela ap¢s o processo **/
             ASSIGN wh-frame:SENSITIVE = YES.         
             RUN piAtualizaBrowse.
        END.
    END.
END.

PROCEDURE piRUNESAB0303B-DEL:
    IF  VALID-HANDLE(h-num-docto) AND
        VALID-HANDLE(h-browse)    THEN DO:

        IF h-browse:NUM-SELECTED-ROWS > 0 THEN DO:            
            /*--- Exibe mensagem de confirma‡Æo de elimina‡Æo ---*/
            RUN utp/ut-msgs.p (INPUT "SHOW":U, INPUT 550, INPUT "":U).
            IF RETURN-VALUE = "YES":U THEN DO:        
                ASSIGN wgh-coluna = h-browse:FIRST-COLUMN.
                FOR FIRST mab-evento-mat
                    WHERE mab-evento-mat.num-docto = INT(h-num-docto:SCREEN-VALUE)
                    AND   mab-evento-mat.it-codigo = wgh-coluna:SCREEN-VALUE EXCLUSIVE-LOCK:
                    DELETE mab-evento-mat.
                END.
                RUN piAtualizaBrowse.
            END.
        END.
    END.
END.

PROCEDURE piAtualizaBrowse:
    /** limpa temp-table e buffer **/
    IF VALID-HANDLE(h-bf-ttReservas) THEN DO:
        IF  h-bf-ttReservas:RECORD-LENGTH > 0 THEN
            h-bf-ttReservas:EMPTY-TEMP-TABLE().
    END.
    ELSE LEAVE.

    EMPTY TEMP-TABLE ttReservas.

    /***** Carrega dados para a ttReservas ****/
    FOR EACH mab-evento-mat 
        WHERE mab-evento-mat.num-docto = INT(h-num-docto:SCREEN-VALUE) NO-LOCK:
        CREATE ttReservas.
        ASSIGN ttReservas.descricao  = mab-evento-mat.descricao
               ttReservas.it-codigo  = mab-evento-mat.it-codigo
               ttReservas.num-docto  = mab-evento-mat.num-docto
               ttReservas.quantidade = mab-evento-mat.quantidade
               ttReservas.c-saldo    = STRING(mab-evento-mat.saldo,"Sim/NÆo")
               ttReservas.situacao   = mab-evento-mat.situacao.
    END.
    FOR EACH ttReservas:
        h-bf-ttReservas:BUFFER-CREATE.
        h-bf-ttReservas:BUFFER-COPY(BUFFER ttReservas:HANDLE).
    END.
    qr-ttReservas:QUERY-OPEN().
    qr-ttReservas:GET-FIRST().
END.

PROCEDURE piAtualiza:
    IF  VALID-HANDLE(wh-fi-espec-0303)       AND
        VALID-HANDLE(h-num-docto)            AND 
        VALID-HANDLE(wh-fi-horas-espec-0303) THEN DO:
        FOR FIRST mab-evento
            WHERE mab-evento.num-docto = INT(h-num-docto:SCREEN-VALUE) NO-LOCK:
        END.
        IF AVAIL mab-evento THEN
            ASSIGN wh-fi-espec-0303:SCREEN-VALUE       = mab-evento.cod-especialid
                   wh-fi-horas-espec-0303:SCREEN-VALUE = STRING(mab-evento.horas-espec).
        ELSE
            ASSIGN wh-fi-espec-0303:SCREEN-VALUE       = "":U
                   wh-fi-horas-espec-0303:SCREEN-VALUE = "":U.
    END.
END PROCEDURE.

/** Zoom de Especialidade **/
PROCEDURE piZoom:
    DEF VAR hProgramZoom AS HANDLE NO-UNDO.
    {method/zoomFields.i &ProgramZoom="yamzoom/z01yam001.w"
                         &FieldZoom1="cod-especialid"
                         &Frame1="fPage1"
                         &FieldHandle1=wh-fi-espec-0303}
END PROCEDURE.

PROCEDURE piValidadeRecord:
    IF  VALID-HANDLE(h-bofr059)   AND 
        VALID-HANDLE(h-num-docto) THEN DO:
        FOR FIRST bf-mab-movto-event
            WHERE bf-mab-movto-event.num-docto = INT(h-num-docto:SCREEN-VALUE) NO-LOCK:
            CREATE tt-mab-movto-event.
            BUFFER-COPY bf-mab-movto-event TO tt-mab-movto-event.
        END.
        IF AVAIL bf-mab-movto-event THEN DO:
            RUN setRecord IN h-bofr059 (INPUT TABLE tt-mab-movto-event).
            RUN validateRecord IN h-bofr059 (INPUT "UPDATE":U).
            IF  VALID-HANDLE(wh-button)  AND
                VALID-HANDLE(wh-button2) AND
                VALID-HANDLE(wh-button3) THEN
                IF RETURN-VALUE = "NOK" OR bf-mab-movto-event.log-disparad = TRUE THEN
                    ASSIGN wh-button:SENSITIVE  = NO
                           wh-button2:SENSITIVE = NO
                           wh-button3:SENSITIVE = NO.
                ELSE
                    ASSIGN wh-button:SENSITIVE  = YES
                           wh-button2:SENSITIVE = YES
                           wh-button3:SENSITIVE = YES.
        END.
    END.
END PROCEDURE.

PROCEDURE findWidget:
    /*
    * PARAMETROS:
    *   c-widget-name:  nome do widget a ser localizado
    *   c-widget-type:  tipo do widget a ser localizado
    *   h-start-widget: container para procurar o widget
    *   h-widget:       widget encontrado 
    */

    define input  parameter c-widget-name  as char   no-undo.
    define input  parameter c-widget-type  as char   no-undo.
    define input  parameter h-start-widget as handle no-undo.
    define output parameter h-widget       as handle no-undo.

    do while valid-handle(h-start-widget):
        if h-start-widget:name = c-widget-name and
           h-start-widget:type = c-widget-type then do:
            assign h-widget = h-start-widget:handle.
            leave.
        end.

        if h-start-widget:type = "field-group":u or
           h-start-widget:type = "frame":u or
           h-start-widget:type = "dialog-box":u then do:
            run findWidget (input  c-widget-name,
                            input  c-widget-type,
                            input  h-start-widget:first-child,
                            output h-widget).
    
            if valid-handle(h-widget) then
                leave.
        end.
        assign h-start-widget = h-start-widget:next-sibling.
    end.
END PROCEDURE.
