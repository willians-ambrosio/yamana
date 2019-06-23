/**#############################################################**
** Alteraá∆o...: Thiago Coutinho
** Empresa.....: CSX Solution
** Data........: Abril/2012
** Objetivo....: AcrÇscimo dos campos - Num. Cartao Usuario,
**.............: Limite litros e Codigo Produtos.
**#############################################################**/
{include/i-prgvrs.i AB0115A-U01 2.06.00.000}


/** ParÉmetros **/                                    
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID         NO-UNDO.

/** Handle **/
DEFINE VARIABLE h-fPage1        AS HANDLE NO-UNDO.
DEFINE VARIABLE h-nom-usuar     AS HANDLE NO-UNDO.
DEFINE VARIABLE h-cod-usuar     AS HANDLE NO-UNDO.
DEFINE VARIABLE h-des-email     AS HANDLE NO-UNDO.
DEFINE VARIABLE h-idi-tip-usuar AS HANDLE NO-UNDO.
DEFINE VARIABLE h-rect-30       AS HANDLE NO-UNDO.

/** Global **/
DEFINE NEW GLOBAL SHARED VAR wh-text-cod-cart-usuar     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-ab0115a-cod-cart-usuar  AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-text-lim-litros         AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-ab0115a-lim-litros      AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-text-cod-produto        AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-ab0115a-cod-produto     AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-text-email              AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE c-lista AS CHARACTER INIT '001 - Diesel, 002 - Gasolina, 003 - Etanol, 008 - Diesel Adi., 016 - Gasol. Adi., 032 - Flex, 063 - Todos'  NO-UNDO.

RUN findWidget (INPUT "cod-usuar",      INPUT "FILL-IN",    INPUT p-wgh-frame, OUTPUT h-cod-usuar).
RUN findWidget (INPUT "des-email",      INPUT "FILL-IN",    INPUT p-wgh-frame, OUTPUT h-des-email).
RUN findWidget (INPUT "idi-tip-usuar",  INPUT "RADIO-SET",  INPUT p-wgh-frame, OUTPUT h-idi-tip-usuar).
RUN findWidget (INPUT "RECT-30",        INPUT "RECTANGLE",  INPUT p-wgh-frame, OUTPUT h-rect-30).
RUN findWidget (INPUT "fpage0",         INPUT "FRAME",      INPUT p-wgh-frame, OUTPUT h-fPage1).
RUN findWidget (INPUT "fi-nom-usuar",   INPUT "FILL-IN",    INPUT h-fPage1,    OUTPUT h-nom-usuar).

/* RUN pi-msg. */

IF  p-ind-event  = "AFTER-INITIALIZE":U AND
    p-ind-object = "CONTAINER":U        THEN DO:

    /*------------Text Numero Cartao Usuario-------*/
    CREATE TEXT wh-text-cod-cart-usuar
        ASSIGN HEIGHT            = 0.88
               WIDTH             = 13
               FRAME             = h-fPage1
               ROW               = 2.9
               COL               = 54
               FORMAT            = "X(17)"
               SCREEN-VALUE      = "Num. Cart. Usuar:"
               SENSITIVE         = FALSE
               VISIBLE           = TRUE.

    /*------------Campo codigo cartao usuar--------*/
    CREATE FILL-IN wh-ab0115a-cod-cart-usuar
        ASSIGN FRAME              = h-fPage1
               SIDE-LABEL-HANDLE  = wh-text-cod-cart-usuar:HANDLE
               FORMAT             = "x(19)"                                                         
               NAME               = "f-cod-card-usuar"                                                    
               WIDTH              = 20                                                              
               HEIGHT             = 0.88                                                            
               ROW                = 2.9
               COL                = 67
               VISIBLE            = YES                                                             
               SENSITIVE          = yes                                                              
               FONT               = 1.

    /*------------label do Codigo produto----------*/
    CREATE TEXT wh-text-cod-produto
        ASSIGN HEIGHT            = 0.88
               WIDTH             = 10
               FRAME             = h-fPage1
               ROW               = 4
               COL               = 57
               FORMAT            = "X(13)"
               SCREEN-VALUE      = "Cod. Produto:"
               SENSITIVE         = FALSE
               VISIBLE           = TRUE.

    /*-------------Combo-box Cod Prod------------*/
    CREATE COMBO-BOX wh-ab0115a-cod-produto
        ASSIGN FRAME             = h-fPage1
               SIDE-LABEL-HANDLE = wh-text-cod-produto
               SCREEN-VALUE      = "001 - Diesel"
               ROW               = 4
               COL               = 67
               WIDTH             = 20
               DATA-TYPE         = "CHARACTER":U
               FORMAT            = "X(20)":U
               LIST-ITEMS        = c-lista
               INNER-LINES       = 7
               SENSITIVE         = TRUE
               VISIBLE           = TRUE.

    /*-------------Label Lim. Listros-----------*/
    CREATE TEXT wh-text-lim-litros
        ASSIGN HEIGHT            = 0.88
               WIDTH             = 9
               FRAME             = h-fPage1
               ROW               = 5
               COL               = 69
               FORMAT            = "X(12)"
               SCREEN-VALUE      = "Lim. Litros:"
               SENSITIVE         = FALSE
               VISIBLE           = TRUE.

    /*------------Campo do Limite Listros--------*/
    CREATE FILL-IN wh-ab0115a-lim-litros
        ASSIGN FRAME              = h-fPage1
               SIDE-LABEL-HANDLE  = wh-text-lim-litros:HANDLE
               DATA-TYPE          = "DECIMAL":U
               FORMAT             = ">>>>>>9.999"
               NAME               = "f-lim-litros"                                                    
               WIDTH              = 10                                                              
               HEIGHT             = 0.88                                                            
               ROW                = 5
               COL                = 77
               VISIBLE            = YES                                                             
               SENSITIVE          = yes                                                              
               FONT               = 1.


    IF VALID-HANDLE(h-des-email) THEN DO:

        /*--------------------Texto Email---------------------*/
        CREATE TEXT wh-text-email
            ASSIGN HEIGHT            = 0.88
                   WIDTH             = 5
                   FRAME             = h-fPage1
                   ROW               = h-des-email:ROW
                   COL               = 4
                   FORMAT            = "X(7)"
                   SCREEN-VALUE      = "E-mail:"
                   SENSITIVE         = FALSE
                   VISIBLE           = TRUE.

        /*----------------------E-MAIL-------------------------*/
        ASSIGN h-des-email:SIDE-LABEL-HANDLE  = wh-text-email:HANDLE 
               h-des-email:FORMAT             = "X(40)"                                                         
               h-des-email:NAME               = "f-des-email"                                                    
               h-des-email:WIDTH              = 43.5                                                              
               h-des-email:HEIGHT             = h-des-email:HEIGHT                                                            
               h-des-email:ROW                = h-des-email:ROW
               h-des-email:COL                = h-des-email:COL - 14
               h-des-email:VISIBLE            = YES                                                             
               h-des-email:SENSITIVE          = yes                                                              
               h-des-email:FONT               = 1.

    END.
    IF VALID-HANDLE(h-idi-tip-usuar) AND
       valid-handle(h-rect-30)THEN DO:

        /*---------------------RADIO-SET-----------------------*/
        ASSIGN h-idi-tip-usuar:RADIO-BUTTONS  = h-idi-tip-usuar:RADIO-BUTTONS
               h-idi-tip-usuar:NAME           = "idi-tip-usuar-u01"
               h-idi-tip-usuar:WIDTH          = h-idi-tip-usuar:WIDTH                                                              
               h-idi-tip-usuar:HEIGHT         = h-idi-tip-usuar:HEIGHT                                                            
               h-idi-tip-usuar:ROW            = h-idi-tip-usuar:ROW
               h-idi-tip-usuar:COL            = h-idi-tip-usuar:COL - 19
               h-idi-tip-usuar:SENSITIVE      = yes.

        /*---------------------RETANGULO-----------------------*/
        assign h-rect-30:width         = h-rect-30:WIDTH
               h-rect-30:height        = h-rect-30:HEIGHT
               h-rect-30:row           = h-rect-30:ROW
               h-rect-30:col           = h-rect-30:COL - 19.

    END.

    RUN piAtualizar IN THIS-PROCEDURE.

END.

IF  p-ind-event  = "BEFORE-ASSIGN":U AND
    p-ind-object = "CONTAINER":U     THEN DO:

    IF VALID-HANDLE(wh-ab0115a-cod-cart-usuar)  AND
       VALID-HANDLE(wh-ab0115a-cod-produto)     AND
       VALID-HANDLE(wh-ab0115a-lim-litros)      AND
       VALID-HANDLE(h-cod-usuar)                THEN DO:

        IF wh-ab0115a-cod-cart-usuar:SCREEN-VALUE <> "" THEN DO:

            FIND FIRST usuar_mestre
                 WHERE usuar_mestre.cod_usuar = h-cod-usuar:SCREEN-VALUE
                 NO-LOCK NO-ERROR.
            IF AVAIL usuar_mestre THEN DO:

                IF LENGTH(wh-ab0115a-cod-cart-usuar:SCREEN-VALUE) = 19 THEN DO:

                    IF wh-ab0115a-lim-litros:SCREEN-VALUE >= '1' THEN DO:

                        FIND FIRST mab-usuar-frota-ext
                             WHERE mab-usuar-frota-ext.cod-cart-usuar   = wh-ab0115a-cod-cart-usuar:SCREEN-VALUE
                               AND mab-usuar-frota-ext.cod-usuar        = h-cod-usuar:SCREEN-VALUE
                             NO-ERROR.
                        IF AVAIL mab-usuar-frota-ext THEN DO:

                            ASSIGN mab-usuar-frota-ext.cod-usuar        = h-cod-usuar:SCREEN-VALUE
                                   mab-usuar-frota-ext.lim-litros       = DEC(wh-ab0115a-lim-litros:SCREEN-VALUE)
                                   mab-usuar-frota-ext.cod-produto      = SUBSTRING(TRIM(wh-ab0115a-cod-produto:SCREEN-VALUE),1,4)
                                   .
                        END.
                        ELSE DO:
                            CREATE mab-usuar-frota-ext.
                            ASSIGN mab-usuar-frota-ext.cod-cart-usuar   = wh-ab0115a-cod-cart-usuar:SCREEN-VALUE
                                   mab-usuar-frota-ext.cod-usuar        = h-cod-usuar:SCREEN-VALUE
                                   mab-usuar-frota-ext.lim-litros       = DEC(wh-ab0115a-lim-litros:SCREEN-VALUE)
                                   mab-usuar-frota-ext.cod-produto      = SUBSTRING(TRIM(wh-ab0115a-cod-produto:SCREEN-VALUE),1,4)
                                   .

                        END.

                    END. /* IF wh-ab0115a-lim-litros:SCREEN-VALUE > '0' THEN DO: */
                    ELSE DO:
                        RUN utp/ut-msgs.p ('show',17006,'Quantidade de litros!~~Deve ser informado uma quantidade de litros superior a 0,00.').
                        APPLY 'ENTRY':U TO wh-ab0115a-cod-cart-usuar.
                        RETURN ERROR.

                    END.

                END. /* IF LENGTH(wh-ab0115a-cod-cart-usuar:SCREEN-VALUE) = 19 THEN DO: */
                ELSE DO:
                    RUN utp/ut-msgs.p ('show',17006,'N£mero Cart∆o Usuario pequeno!~~Deve ser informado o Numero do Cart∆o do Usu†rio com 19 caracteres.').
                    APPLY 'ENTRY':U TO wh-ab0115a-cod-cart-usuar.
                    RETURN ERROR.

                END.

            END. /* IF AVAIL usuar_mestre THEN DO: */

        END. /** IF wh-ab0115a-cod-cart-usuar:SCREEN-VALUE <> "" THEN DO: **/
        ELSE DO:
            RUN utp/ut-msgs.p ('show',17006,'N£mero Cart∆o Usuario em branco!~~Deve ser informado o Numero do Cart∆o do Usu†rio.').
            APPLY 'ENTRY':U TO wh-ab0115a-cod-cart-usuar.
            RETURN ERROR.

        END.
    
    END.

END.

PROCEDURE piAtualizar:

DEF VAR i-aux AS INT NO-UNDO.

    IF  VALID-HANDLE(h-cod-usuar) THEN DO:

        FIND FIRST mab-usuar-frota-ext
             WHERE mab-usuar-frota-ext.cod-usuar   = h-cod-usuar:SCREEN-VALUE
             NO-LOCK NO-ERROR.
        IF AVAIL mab-usuar-frota-ext THEN DO:
            IF mab-usuar-frota-ext.cod-produto = '001' THEN
                ASSIGN i-aux = 1.
            ELSE IF mab-usuar-frota-ext.cod-produto = '002' THEN
                ASSIGN i-aux = 2.
            ELSE IF mab-usuar-frota-ext.cod-produto = '003' THEN
                ASSIGN i-aux = 3.
            ELSE IF mab-usuar-frota-ext.cod-produto = '008' THEN
                ASSIGN i-aux = 4.
            ELSE IF mab-usuar-frota-ext.cod-produto = '016' THEN
                ASSIGN i-aux = 5.
            ELSE IF mab-usuar-frota-ext.cod-produto = '032' THEN
                ASSIGN i-aux = 6.
            ELSE IF mab-usuar-frota-ext.cod-produto = '063' THEN
                ASSIGN i-aux = 6.
            ELSE 
                ASSIGN i-aux = 1.

            ASSIGN wh-ab0115a-cod-cart-usuar:SCREEN-VALUE   = mab-usuar-frota-ext.cod-cart-usuar
                   wh-ab0115a-lim-litros:SCREEN-VALUE       = STRING(mab-usuar-frota-ext.lim-litros)
                   wh-ab0115a-cod-produto:SCREEN-VALUE      = entry(i-aux,wh-ab0115a-cod-produto:LIST-ITEMS)
                   wh-ab0115a-cod-cart-usuar:SENSITIVE      = NO
                   .
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

procedure pi-msg:
message "p-ind-event..:" p-ind-event                  skip 
        "p-ind-object.:" p-ind-object                 skip 
        "p-cod-table..:" STRING(p-cod-table)          skip 
        "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    skip 
        "p-wgh-frame..:" STRING(p-wgh-frame)          skip 
        "p-row-table..:" string(p-row-table)          skip 
        VIEW-AS ALERT-BOX INFO BUTTONS OK.                    
END PROCEDURE.
