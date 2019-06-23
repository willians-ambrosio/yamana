/********************************************************************************
** PROGRAMA : upc/upsec001zb.p
** DESCRICAO: upc atualizar grupo de seguranáa - controle de conflitos
** HISTORICO: rde - 16/04/2018
********************************************************************************/

def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-row-table  as ROWID         no-undo.

DEF NEW GLOBAL SHARED VAR ngs_h_aux AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h_frame             AS WIDGET-HANDLE NO-UNDO.

DEF TEMP-TABLE tt-gru-conflito
    FIELD cod_grp_usuar_base LIKE prog_dtsul_segur.cod_grp_usuar COLUMN-LABEL "Gru Base"
    FIELD cod_grp_usuar      LIKE prog_dtsul_segur.cod_grp_usuar COLUMN-LABEL "Grupo Conflito"
    FIELD cod_prog_dtsul     LIKE prog_dtsul_segur.cod_prog_dtsul
    INDEX tt-gru-conflito-i1 IS PRIMARY UNIQUE
    cod_grp_usuar_base
    cod_grp_usuar
    cod_prog_dtsul
    INDEX tt-gru-conflito-i2 IS UNIQUE
    cod_grp_usuar
    cod_grp_usuar_base
    cod_prog_dtsul.

DEF BUFFER bf_prog_dtsul_segur FOR prog_dtsul_segur.
DEF BUFFER bf_grp_usuar        FOR grp_usuar.

DEF BUFFER bf2_grp_usuar       FOR grp_usuar.

DEF VAR c-msg AS CHAR NO-UNDO.
def var c-objeto as char no-undo.
DEF VAR c-just AS CHAR NO-UNDO.
DEFINE VARIABLE c-req AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-conflito AS CHARACTER   NO-UNDO.

DEF VAR vlo-erro AS LOG NO-UNDO.
DEFINE VARIABLE c-origem AS CHARACTER   NO-UNDO.

def new global shared var v_cod_usuar_corren   like usuar_mestre.cod_usuario no-undo.

&SCOPED-DEFINE NEW NEW GLOBAL

DEFINE {&NEW} SHARED VARIABLE ngs-just-esmen012 AS CHARACTER FORMAT "x(120)":U INITIAL ? 
     LABEL "Justificativa" 
     VIEW-AS FILL-IN 
     SIZE 70 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE ngs-req-acesso-esmen012 AS CHARACTER FORMAT "REQ9999999":U INITIAL ? 
     LABEL "Requisiá∆o" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

ASSIGN c-objeto   = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").

/* MESSAGE "NOVO Evento " p-ind-event  SKIP   */
/*     "Objeto     " p-ind-object SKIP        */
/*     "nome obj   " c-objeto     SKIP        */
/*     "Frame      " p-wgh-frame  SKIP        */
/*     "Nome Frame " p-wgh-frame:NAME SKIP    */
/*     "Tabela     " p-cod-table  SKIP        */
/*     "ROWID      " STRING(p-row-table) SKIP */
/*     VIEW-AS ALERT-BOX INFORMATION.         */

IF p-ind-event  = 'valor_registro' AND 
   p-cod-table  = 'grp_usuar'      AND 
   p-row-table <> ?                THEN DO:

    FIND FIRST bf2_grp_usuar WHERE
        ROWID(bf2_grp_usuar) = p-row-table NO-LOCK NO-ERROR.
    IF AVAIL bf2_grp_usuar THEN DO:

        RUN pi-retorno-origem (OUTPUT c-origem).

        ASSIGN vlo-erro = NO.

        /* pesquisa programa principal - base */
        FOR EACH ctrl-conflito-it NO-LOCK WHERE
            ctrl-conflito-it.cod_prog_dtsul_base = p-ind-object /* nome do programa que est† posicionado pra ser adicionado um grupo de seguranáa a ele */
            BREAK BY ctrl-conflito-it.cod_prog_dtsul_base.
        
            FOR each prog_dtsul_segur NO-LOCK
                where prog_dtsul_segur.cod_prog_dtsul = ctrl-conflito-it.cod_prog_dtsul_base ,
                first grp_usuar no-lock
                where grp_usuar.cod_grp_usuar = prog_dtsul_segur.cod_grp_usuar
                By prog_dtsul_segur.cod_prog_dtsul.
        
                FOR each bf_prog_dtsul_segur NO-LOCK
                    where bf_prog_dtsul_segur.cod_prog_dtsul = ctrl-conflito-it.cod_prog_dtsul,
                    first bf_grp_usuar no-lock
                    where bf_grp_usuar.cod_grp_usuar = bf_prog_dtsul_segur.cod_grp_usuar
                    By bf_prog_dtsul_segur.cod_prog_dtsul.
        
                    FIND FIRST tt-gru-conflito WHERE
                        tt-gru-conflito.cod_grp_usuar_base = prog_dtsul_segur.cod_grp_usuar    AND 
                        tt-gru-conflito.cod_grp_usuar      = bf_prog_dtsul_segur.cod_grp_usuar AND 
                        tt-gru-conflito.cod_prog_dtsul     = bf_prog_dtsul_segur.cod_prog_dtsul NO-LOCK NO-ERROR.
        
                    IF NOT AVAIL tt-gru-conflito THEN DO:
        
                        CREATE tt-gru-conflito.
                        ASSIGN tt-gru-conflito.cod_grp_usuar_base = prog_dtsul_segur.cod_grp_usuar    
                               tt-gru-conflito.cod_grp_usuar      = bf_prog_dtsul_segur.cod_grp_usuar
                               tt-gru-conflito.cod_prog_dtsul     = bf_prog_dtsul_segur.cod_prog_dtsul.
                    END.
                END.
            END.
        END.


        /* pesquisa down-up - programa conflito para programa base */
        FOR EACH ctrl-conflito-it NO-LOCK WHERE
            ctrl-conflito-it.cod_prog_dtsul = p-ind-object /* nome do programa que est† posicionado pra ser adicionado um grupo de seguranáa a ele */
            BREAK BY ctrl-conflito-it.cod_prog_dtsul_base.
        
            FOR each prog_dtsul_segur NO-LOCK
                where prog_dtsul_segur.cod_prog_dtsul = ctrl-conflito-it.cod_prog_dtsul ,
                first grp_usuar no-lock
                where grp_usuar.cod_grp_usuar = prog_dtsul_segur.cod_grp_usuar
                By prog_dtsul_segur.cod_prog_dtsul.
        
                FOR each bf_prog_dtsul_segur NO-LOCK
                    where bf_prog_dtsul_segur.cod_prog_dtsul = ctrl-conflito-it.cod_prog_dtsul_base,
                    first bf_grp_usuar no-lock
                    where bf_grp_usuar.cod_grp_usuar = bf_prog_dtsul_segur.cod_grp_usuar
                    By bf_prog_dtsul_segur.cod_prog_dtsul.
        
                    FIND FIRST tt-gru-conflito WHERE
                        tt-gru-conflito.cod_grp_usuar_base = prog_dtsul_segur.cod_grp_usuar    AND 
                        tt-gru-conflito.cod_grp_usuar      = bf_prog_dtsul_segur.cod_grp_usuar AND 
                        tt-gru-conflito.cod_prog_dtsul     = bf_prog_dtsul_segur.cod_prog_dtsul NO-LOCK NO-ERROR.
        
                    IF NOT AVAIL tt-gru-conflito THEN DO:
        
                        CREATE tt-gru-conflito.
                        ASSIGN tt-gru-conflito.cod_grp_usuar_base = prog_dtsul_segur.cod_grp_usuar    
                               tt-gru-conflito.cod_grp_usuar      = bf_prog_dtsul_segur.cod_grp_usuar
                               tt-gru-conflito.cod_prog_dtsul     = bf_prog_dtsul_segur.cod_prog_dtsul.
                    END.
                END.
            END.
        END.            

        ASSIGN c-msg = ''.

        FOR EACH tt-gru-conflito WHERE
            tt-gru-conflito.cod_grp_usuar = bf2_grp_usuar.cod_grp_usuar NO-LOCK
            BREAK BY tt-gru-conflito.cod_prog_dtsul.

            IF FIRST-OF(tt-gru-conflito.cod_prog_dtsul) THEN DO:

                FIND FIRST prog_dtsul WHERE
                    prog_dtsul.cod_prog_dtsul = tt-gru-conflito.cod_prog_dtsul NO-LOCK NO-ERROR.
    
                ASSIGN c-msg = c-msg + "Conflito com o programa: " + CAPS(tt-gru-conflito.cod_prog_dtsul) + CHR(13) + 
                               "GRUPO " + tt-gru-conflito.cod_grp_usuar_base + " X " + tt-gru-conflito.cod_grp_usuar + CHR(13) + 
                                "PROGRAMA: " + p-ind-object + " X " + tt-gru-conflito.cod_prog_dtsul 
                       c-conflito = tt-gru-conflito.cod_grp_usuar_base + " X " + tt-gru-conflito.cod_grp_usuar.
            END.
        END.

        IF c-msg <> '' AND vlo-erro = NO THEN DO:

            RUN utp/ut-msgs.p ("show",  27100,
                               "Conflito de acesso grupo " + tt-gru-conflito.cod_grp_usuar_base + " X " + tt-gru-conflito.cod_grp_usuar  + "! Foráar o cadastro, informando uma justificativa?" +
                               "~~" + c-msg).                     

            ASSIGN c-just = ''
                   c-req  = ''.

            IF RETURN-VALUE = 'yes' THEN DO:

                ASSIGN ngs-just-esmen012 = ''
                       ngs-req-acesso-esmen012 = ''.

                RUN esp/esmen012-g03.w (OUTPUT c-just,
                                        OUTPUT c-req,
                                        INPUT p-ind-object,
                                        INPUT c-conflito,
                                        INPUT "programa").

                IF c-just = '' OR c-just = ? THEN DO:

                    MESSAGE "Justificativa Ç obrigat¢ria !" SKIP(1)
                        "Acesso n∆o ser† concedido !"
                        VIEW-AS ALERT-BOX ERROR.

                    ASSIGN vlo-erro = YES.
                END.

                /* dpc */
                IF c-req = '' OR c-req = ? THEN DO:

                    MESSAGE "Requisiá∆o Ç obrigat¢ria!" SKIP(01)
                             "Acesso n∆o ser† concedido!"
                        VIEW-AS ALERT-BOX ERROR.

                    ASSIGN vlo-erro = YES.
                END.

                IF NOT vlo-erro THEN DO:

                    FOR EACH tt-gru-conflito WHERE
                        tt-gru-conflito.cod_grp_usuar = bf2_grp_usuar.cod_grp_usuar NO-LOCK
                        BREAK BY tt-gru-conflito.cod_prog_dtsul.
            
                        IF FIRST-OF(tt-gru-conflito.cod_prog_dtsul) THEN DO:
            
                            FIND FIRST prog_dtsul 
                                WHERE prog_dtsul.cod_prog_dtsul = tt-gru-conflito.cod_prog_dtsul NO-LOCK NO-ERROR.

                            FOR EACH usuar_grp_usuar NO-LOCK 
                               WHERE usuar_grp_usuar.cod_grp_usuar = bf2_grp_usuar.cod_grp_usuar:

                                IF NOT CAN-FIND(FIRST ctrl-conflito-force NO-LOCK 
                                                WHERE ctrl-conflito-force.usuario_add        = usuar_grp_usuar.cod_usuario
                                                AND   ctrl-conflito-force.cod_grp_usuar_add  = bf2_grp_usuar.cod_grp_usuar
                                                AND   ctrl-conflito-force.data_aprov         = TODAY
                                                AND   ctrl-conflito-force.hora_aprov         = TIME) THEN DO:
    
                                    CREATE ctrl-conflito-force.
                                    ASSIGN ctrl-conflito-force.cod_grp_usuar_add       = bf2_grp_usuar.cod_grp_usuar
                                           ctrl-conflito-force.cod_prog_dtsul_add      = p-ind-object
                                           ctrl-conflito-force.usuario_aprov           = v_cod_usuar_corren
                                           ctrl-conflito-force.usuario_add             = usuar_grp_usuar.cod_usuario
                                           ctrl-conflito-force.cod_prog_dtsul_base     = p-ind-object 
                                           ctrl-conflito-force.cod_prog_dtsul_conflito = tt-gru-conflito.cod_prog_dtsul
                                           ctrl-conflito-force.data_aprov              = TODAY
                                           ctrl-conflito-force.hora_aprov              = TIME
                                           ctrl-conflito-force.motivo_aprov            = c-just
                                           ctrl-conflito-force.requisicao              = c-req
                                           ctrl-conflito-force.tipo                    = 1 /* foráada */
                                           ctrl-conflito-force.cod_prog_dtsul_conflito = tt-gru-conflito.cod_prog_dtsul
                                           ctrl-conflito-force.origem                  = c-origem + ";Seguranáa Programas".
                                END.
                            END.
                        END.
                    END.

                    RETURN "OK".
                END.
            END.
            ELSE DO:

                MESSAGE "Acesso n∆o ser† concedido !!!"
                        VIEW-AS ALERT-BOX ERROR.

                ASSIGN vlo-erro = YES.
            END.
        END.

        /* Solicita apenas nr da requisiá∆o qdo n∆o h† conflito de acesso */
        /* Solicita apenas o nr da requisiá∆o qdo n∆o h† conflito de acesso */
        IF c-msg = "" THEN DO:
    
            ASSIGN c-req  = ''
                   vlo-erro = NO.
    
            ASSIGN ngs-req-acesso-esmen012 = ''.
    
            RUN esp/esmen012-g04.w (OUTPUT c-req,
                                    INPUT p-ind-object,
                                    INPUT bf2_grp_usuar.cod_grp_usuar,
                                    INPUT "Programa").
            /* dpc */
            IF c-req = '' OR c-req = ? THEN DO:
    
                MESSAGE "N£mero da requisiá∆o Ç obrigat¢rio!" SKIP(01)
                         "Acesso n∆o ser† concedido!"
                    VIEW-AS ALERT-BOX ERROR.
    
                ASSIGN vlo-erro = YES.
            END.
            ELSE DO:

                FOR EACH usuar_grp_usuar NO-LOCK 
                       WHERE usuar_grp_usuar.cod_grp_usuar = bf2_grp_usuar.cod_grp_usuar:

                    IF NOT CAN-FIND(FIRST ctrl-conflito-force NO-LOCK 
                                    WHERE ctrl-conflito-force.usuario_add        = usuar_grp_usuar.cod_usuario
                                    AND   ctrl-conflito-force.cod_grp_usuar_add  = bf2_grp_usuar.cod_grp_usuar
                                    AND   ctrl-conflito-force.data_aprov         = TODAY
                                    AND   ctrl-conflito-force.hora_aprov         = TIME) THEN DO:

                        CREATE ctrl-conflito-force.
                        ASSIGN ctrl-conflito-force.cod_grp_usuar_add       = bf2_grp_usuar.cod_grp_usuar
                               ctrl-conflito-force.cod_prog_dtsul_add      = p-ind-object
                               ctrl-conflito-force.usuario_aprov           = v_cod_usuar_corren
                               ctrl-conflito-force.usuario_add             = usuar_grp_usuar.cod_usuario
                               ctrl-conflito-force.data_aprov              = TODAY
                               ctrl-conflito-force.hora_aprov              = TIME
                               ctrl-conflito-force.motivo_aprov            = "Liberaá∆o de acesso sem conflito, por requisiá∆o"
                               ctrl-conflito-force.requisicao              = c-req
                               ctrl-conflito-force.tipo                    = 2 /* normal */
                               ctrl-conflito-force.cod_prog_dtsul_conflito = ""
                               ctrl-conflito-force.origem                  = c-origem + ";Seguranáa Programas".
                    END.
                END.
            END.
        END.

        IF vlo-erro = YES THEN DO:

            RETURN ERROR "NOK".
        END.
    
    END. /* IF AVAIL bf2_grp_usuar */
    
END.


{upc\upsec000.i} /*definicao da procedura pi-retorna-origem */

RETURN "OK".

/* fim - upsec001zb.p */

