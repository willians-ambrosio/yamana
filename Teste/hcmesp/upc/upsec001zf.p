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
DEFINE VARIABLE c-origem            AS CHARACTER   NO-UNDO.
{upc\upsec000.i} /*definicao da procedura pi-retorna-origem */

DEF TEMP-TABLE tt-gru-conflito
    FIELD cod_grp_usuar_base LIKE prog_dtsul_segur.cod_grp_usuar COLUMN-LABEL "Gru Base"
    FIELD cod_grp_usuar      LIKE prog_dtsul_segur.cod_grp_usuar COLUMN-LABEL "Grupo Conflito"
    FIELD cod_prog_dtsul     LIKE prog_dtsul_segur.cod_prog_dtsul
    FIELD cod_prog_dtsul_base LIKE prog_dtsul_segur.cod_prog_dtsul
    INDEX tt-gru-conflito-i1 IS PRIMARY UNIQUE
    cod_grp_usuar_base
    cod_grp_usuar
    cod_prog_dtsul_base
    cod_prog_dtsul.

DEF TEMP-TABLE tt-conflito
    FIELD cod_grupo_base LIKE prog_dtsul_segur.cod_grp_usuar
    FIELD cod_gru_usu    LIKE prog_dtsul_segur.cod_grp_usuar
    FIELD cod_prog_dtsul_base LIKE prog_dtsul_segur.cod_prog_dtsul
    FIELD cod_prog_dtsul      LIKE prog_dtsul_segur.cod_prog_dtsul
   INDEX idx0 IS PRIMARY UNIQUE
    cod_grupo_base
    cod_gru_usu.

DEF BUFFER bf_prog_dtsul_segur  FOR prog_dtsul_segur.
DEF BUFFER bf_usuar_grp_usuar   FOR usuar_grp_usuar.

DEF BUFFER bf2_grp_usuar       FOR usuar_grp_usuar.

DEF VAR c-msg AS CHAR NO-UNDO.
def var c-objeto as char no-undo.
DEF VAR c-just AS CHAR NO-UNDO.
DEFINE VARIABLE c-req AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-time AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-conflito AS CHARACTER   NO-UNDO.

DEF VAR vlo-erro AS LOG NO-UNDO.

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

DEFINE NEW GLOBAL SHARED VARIABLE upsec_cod_usuario AS CHARACTER NO-UNDO.

ASSIGN c-objeto   = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").

/* MESSAGE "NOVO Evento     " p-ind-event  SKIP */
/*     "Objeto     " p-ind-object SKIP          */
/*     "nome obj   " c-objeto     SKIP          */
/*     "Frame      " p-wgh-frame  SKIP          */
/*     "Nome Frame " p-wgh-frame:NAME SKIP      */
/*     "Tabela     " p-cod-table  SKIP          */
/*     "ROWID      " STRING(p-row-table) SKIP   */
/*     VIEW-AS ALERT-BOX INFORMATION.           */

/* Grava o c¢digo do usu†rio */
IF p-ind-event  = 'Codigo_Usuario' THEN DO:

    ASSIGN upsec_cod_usuario = p-ind-object.
END.


IF p-ind-event  = 'Grupo_Usuario' AND 
   p-cod-table  = 'grp_usuar'     AND 
   p-row-table <> ?               THEN DO:

    EMPTY TEMP-TABLE  tt-gru-conflito.
    EMPTY TEMP-TABLE tt-conflito.

    RUN pi-retorno-origem (OUTPUT c-origem).

    /* Verifica cadastro de conflitos pesquisa programas em conflito - Base*/
    FOR EACH bf_prog_dtsul_segur NO-LOCK 
        WHERE bf_prog_dtsul_segur.cod_grp_usuar = p-ind-object:
        
        FOR EACH ctrl-conflito-it NO-LOCK WHERE
                 ctrl-conflito-it.cod_prog_dtsul_base = bf_prog_dtsul_segur.cod_prog_dtsul:

            /* programas conflito */
            FIND FIRST tt-gru-conflito 
               WHERE tt-gru-conflito.cod_grp_usuar_base = bf_prog_dtsul_segur.cod_grp_usuar    
                AND tt-gru-conflito.cod_prog_dtsul_base = ctrl-conflito-it.cod_prog_dtsul_base
                AND tt-gru-conflito.cod_prog_dtsul      = ctrl-conflito-it.cod_prog_dtsul NO-ERROR.
            IF NOT AVAIL tt-gru-conflito THEN DO:
    
                CREATE tt-gru-conflito.
                ASSIGN tt-gru-conflito.cod_grp_usuar_base  = bf_prog_dtsul_segur.cod_grp_usuar    
                       tt-gru-conflito.cod_prog_dtsul_base = ctrl-conflito-it.cod_prog_dtsul_base    
                       tt-gru-conflito.cod_prog_dtsul      = ctrl-conflito-it.cod_prog_dtsul.
            END.    
        END.

         FOR EACH ctrl-conflito-it NO-LOCK WHERE
                  ctrl-conflito-it.cod_prog_dtsul = bf_prog_dtsul_segur.cod_prog_dtsul:

            /* programas conflito */
            FIND FIRST tt-gru-conflito 
               WHERE tt-gru-conflito.cod_grp_usuar_base = bf_prog_dtsul_segur.cod_grp_usuar    
                AND tt-gru-conflito.cod_prog_dtsul_base = ctrl-conflito-it.cod_prog_dtsul_base
                AND tt-gru-conflito.cod_prog_dtsul      = ctrl-conflito-it.cod_prog_dtsul NO-ERROR.
            IF NOT AVAIL tt-gru-conflito THEN DO:
    
                CREATE tt-gru-conflito.
                ASSIGN tt-gru-conflito.cod_grp_usuar_base  = bf_prog_dtsul_segur.cod_grp_usuar    
                       tt-gru-conflito.cod_prog_dtsul_base = ctrl-conflito-it.cod_prog_dtsul_base    
                       tt-gru-conflito.cod_prog_dtsul      = ctrl-conflito-it.cod_prog_dtsul.
            END.  
        END.
    END.

    /* Verifica se nos grupos atuais do usu†rio h† conflito */
    FOR EACH bf_usuar_grp_usuar NO-LOCK WHERE
             bf_usuar_grp_usuar.cod_usuario = upsec_cod_usuario,
        EACH bf_prog_dtsul_segur NO-LOCK 
        WHERE bf_prog_dtsul_segur.cod_grp_usuar = bf_usuar_grp_usuar.cod_grp_usuar:

         FIND FIRST tt-gru-conflito WHERE
                    tt-gru-conflito.cod_prog_dtsul_base = bf_prog_dtsul_segur.cod_prog_dtsul NO-ERROR.
         IF AVAIL tt-gru-conflito THEN DO:
             CREATE tt-conflito.
             ASSIGN tt-conflito.cod_grupo_base      = tt-gru-conflito.cod_grp_usuar_base
                    tt-conflito.cod_gru_usu         = bf_prog_dtsul_segur.cod_grp_usuar
                    tt-conflito.cod_prog_dtsul_base = tt-gru-conflito.cod_prog_dtsul_base 
                    tt-conflito.cod_prog_dtsul      = tt-gru-conflito.cod_prog_dtsul.     
             LEAVE.
         END.

         FIND FIRST tt-gru-conflito WHERE
                    tt-gru-conflito.cod_prog_dtsul = bf_prog_dtsul_segur.cod_prog_dtsul NO-ERROR.
         IF AVAIL tt-gru-conflito THEN DO:
             CREATE tt-conflito.
             ASSIGN tt-conflito.cod_grupo_base      = tt-gru-conflito.cod_grp_usuar_base
                    tt-conflito.cod_gru_usu         = bf_prog_dtsul_segur.cod_grp_usuar
                    tt-conflito.cod_prog_dtsul_base = tt-gru-conflito.cod_prog_dtsul_base 
                    tt-conflito.cod_prog_dtsul      = tt-gru-conflito.cod_prog_dtsul.     
             LEAVE.
         END.
    END.
    ASSIGN c-msg    = ''
           vlo-erro = NO.

    FOR EACH tt-conflito:
            ASSIGN c-msg = c-msg + "Conflito com o grupo: " + CAPS(tt-conflito.cod_grupo_base) + " X " + CAPS(tt-conflito.cod_gru_usu) + CHR(13) + 
                                   "Programa: " + tt-conflito.cod_prog_dtsul_base + " X " + tt-conflito.cod_prog_dtsul + CHR(13)
                   c-conflito = CAPS(tt-conflito.cod_grupo_base) + " X " + CAPS(tt-conflito.cod_gru_usu).
    END.

    IF c-msg <> '' AND vlo-erro = NO THEN DO:

        RUN utp/ut-msgs.p ("show",  27100,
                           "Conflito de acesso grupo! Foráar o cadastro, informando uma justificativa?" +
                           "~~" + c-msg).                     

        ASSIGN c-just = ''
               c-req  = ''.

        IF RETURN-VALUE = 'yes' THEN DO:

            ASSIGN ngs-just-esmen012 = ''
                   ngs-req-acesso-esmen012 = ''.

            RUN esp/esmen012-g03.w (OUTPUT c-just,
                                    OUTPUT c-req,
                                    INPUT upsec_cod_usuario,
                                    INPUT c-conflito,
                                    INPUT "usuario").

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

                ASSIGN i-time = TIME.
                FOR EACH tt-conflito: 

                    IF NOT CAN-FIND(FIRST ctrl-conflito-force NO-LOCK 
                                    WHERE ctrl-conflito-force.usuario_add        = upsec_cod_usuario
                                    AND   ctrl-conflito-force.cod_grp_usuar_add  = p-ind-object
                                    AND   ctrl-conflito-force.data_aprov         = TODAY
                                    AND   ctrl-conflito-force.hora_aprov         = i-time) THEN DO:

                        CREATE ctrl-conflito-force.
                        ASSIGN ctrl-conflito-force.cod_grp_usuar_add       = p-ind-object
                               ctrl-conflito-force.cod_grp_conflito        = tt-conflito.cod_grupo_base
                               ctrl-conflito-force.cod_prog_dtsul_add      = ""
                               ctrl-conflito-force.cod_prog_dtsul_base     = tt-conflito.cod_prog_dtsul_base 
                               ctrl-conflito-force.cod_prog_dtsul_conflito = tt-conflito.cod_prog_dtsul
                               ctrl-conflito-force.usuario_aprov           = v_cod_usuar_corren
                               ctrl-conflito-force.usuario_add             = upsec_cod_usuario
                               ctrl-conflito-force.data_aprov              = TODAY
                               ctrl-conflito-force.hora_aprov              = i-time
                               ctrl-conflito-force.motivo_aprov            = c-just
                               ctrl-conflito-force.requisicao              = c-req
                               ctrl-conflito-force.tipo                    = 1 /* foráada */
                               ctrl-conflito-force.cod_prog_dtsul_conflito = tt-conflito.cod_prog_dtsul
                               ctrl-conflito-force.origem                  = c-origem + ";Seguranáa Usu†rios".
                    END.
                END.

                RETURN "OK".
            END.
        END. /* IF RETURN-VALUE = 'yes' THEN DO:*/
        ELSE DO:

            MESSAGE "Acesso n∆o ser† concedido!!!"
                    VIEW-AS ALERT-BOX ERROR.

            ASSIGN vlo-erro = YES.
        END.
    END. /* IF c-msg <> '' AND vlo-erro = NO */
    /* fim tratamento para conflito */


    /* Solicita apenas nr da requisiá∆o qdo n∆o h† conflito de acesso */
    IF NOT CAN-FIND(FIRST tt-conflito) THEN DO:

        ASSIGN c-req  = ''
               vlo-erro = NO
               ngs-req-acesso-esmen012 = ''.

        RUN esp/esmen012-g04.w (OUTPUT c-req,
                                INPUT upsec_cod_usuario,
                                INPUT p-ind-object,
                                INPUT "usuario").

        /* dpc */
        IF c-req = '' OR c-req = ? THEN DO:

            MESSAGE "N£mero da requisiá∆o Ç obrigat¢rio!" SKIP(01)
                     "Acesso n∆o ser† concedido!"
                VIEW-AS ALERT-BOX ERROR.

            ASSIGN vlo-erro = YES.
        END.
        ELSE DO:
            ASSIGN i-time = TIME.

            IF NOT CAN-FIND(FIRST ctrl-conflito-force NO-LOCK 
                            WHERE ctrl-conflito-force.usuario_add        = upsec_cod_usuario
                            AND   ctrl-conflito-force.cod_grp_usuar_add  = p-ind-object
                            AND   ctrl-conflito-force.data_aprov         = TODAY
                            AND   ctrl-conflito-force.hora_aprov         = i-time) THEN DO:

                CREATE ctrl-conflito-force.
                ASSIGN ctrl-conflito-force.cod_grp_usuar_add       = p-ind-object
                       ctrl-conflito-force.cod_prog_dtsul_add      = ""
                       ctrl-conflito-force.usuario_aprov           = v_cod_usuar_corren
                       ctrl-conflito-force.usuario_add             = upsec_cod_usuario
                       ctrl-conflito-force.data_aprov              = TODAY
                       ctrl-conflito-force.hora_aprov              = i-time
                       ctrl-conflito-force.motivo_aprov            = "Liberaá∆o de acesso sem conflito, por requisiá∆o"
                       ctrl-conflito-force.requisicao              = c-req
                       ctrl-conflito-force.tipo                    = 2 /* normal */
                       ctrl-conflito-force.cod_prog_dtsul_conflito = ""
                       ctrl-conflito-force.origem                  = c-origem + ";Seguranáa Usu†rios".
            END.
        END.
    END.

    IF vlo-erro = YES THEN DO:

        RETURN ERROR "NOK".
    END.
END.

RETURN "OK".

/* fim - upsec001zf.p */

