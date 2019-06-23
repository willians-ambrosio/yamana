/**************************************************************************
 ** Programa: upc/esfgl702ca-u00.p (add_lancto_ctbl e mod_lancto_ctbl)
 ** Objetivo: Desabilitar os campos zoom e cen rio
 ** Autor   : Bruno Bertulli - DSC
 ** Data    : 31/05/2013
 ** Cliente : Yamana
 **************************************************************************/
{tools/fc-handle-obj.i}
{tools/fc-falso.i}

DEF NEW GLOBAL SHARED var v_cod_usuar_corren as CHARACTER format "x(12)":U label "Usu rio Corrente" column-label "Usu rio Corrente" no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

define input parameter p_ind_event    as character     no-undo.
define input parameter p_ind_object   as character     no-undo.
define input parameter p_wgh_object   as widget-handle no-undo.
define input parameter p_wgh_frame    as widget-handle no-undo.
define input parameter p_cod_table    as character     no-undo.
define input parameter p_rec_table    as recid         no-undo.

def new global shared var wh-fgl702ca-cod_cenar_ctbl        as widget-handle no-undo.
def new global shared var wh-fgl702ca-bt_ok                 as widget-handle no-undo.
def new global shared var wh-fgl702ca-bt_ok-f               as widget-handle no-undo.
def new global shared var wh-fgl702ca-bt_sav                as widget-handle no-undo.
def new global shared var wh-fgl702ca-bt_sav-f              as widget-handle no-undo.

DEFINE  VARIABLE c-handle-obj  AS CHARACTER          NO-UNDO.
    
DEFINE VARIABLE l-Bloqueia  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cGrupo      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCont       AS INTEGER     NO-UNDO.

/* ===> Main Block <=== */

/* MESSAGE 'p_ind_event  ' p_ind_event   skip */
/*         'p_ind_object ' p_ind_object  skip */
/*         'p_wgh_object ' p_wgh_object  skip */
/*         'p_wgh_frame  ' p_wgh_frame   skip */
/*         'p_cod_table  ' p_cod_table   skip */
/*         'p_rec_table  ' p_rec_table   skip */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
  
IF p_ind_event = "INITIALIZE" then do:
    c-handle-obj               = fc-handle-obj("cod_cenar_ctbl,bt_ok,bt_sav",p_wgh_frame).
    wh-fgl702ca-cod_cenar_ctbl = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    wh-fgl702ca-bt_ok          = WIDGET-HANDLE(ENTRY(2,c-handle-obj)) NO-ERROR.
    wh-fgl702ca-bt_sav         = WIDGET-HANDLE(ENTRY(3,c-handle-obj)) NO-ERROR.

    wh-fgl702ca-bt_ok-f        = fc-falso(wh-fgl702ca-bt_ok, wh-fgl702ca-bt_ok:FRAME, "").
    wh-fgl702ca-bt_ok-f:LOAD-IMAGE-UP(wh-fgl702ca-bt_ok:IMAGE-UP).
    wh-fgl702ca-bt_ok-f:MOVE-TO-TOP().
    wh-fgl702ca-bt_ok:VISIBLE  = NO.
    wh-fgl702ca-bt_sav-f       = fc-falso(wh-fgl702ca-bt_sav, wh-fgl702ca-bt_sav:FRAME, "").
    wh-fgl702ca-bt_sav-f:LOAD-IMAGE-UP(wh-fgl702ca-bt_sav:IMAGE-UP).
    wh-fgl702ca-bt_sav-f:MOVE-TO-TOP().
    wh-fgl702ca-bt_sav:VISIBLE = NO.
  
    ON 'CHOOSE':U OF wh-fgl702ca-bt_ok-f PERSISTENT RUN upc\esfgl702ca-u00.p
       ("choose",
        "bt_ok-f",
        p_wgh_object,
        p_wgh_frame,
        p_cod_table,
        p_rec_table).

    ON 'CHOOSE':U OF wh-fgl702ca-bt_sav-f PERSISTENT RUN upc\esfgl702ca-u00.p
       ("choose",
        "bt_sav-f",
        p_wgh_object,
        p_wgh_frame,
        p_cod_table,
        p_rec_table).

END.

IF  p_ind_event  = "choose" AND
   (p_ind_object = "bt_ok-f" OR p_ind_object = "bt_sav-f") THEN DO:

    FIND FIRST es-param-cenario NO-LOCK NO-ERROR.

    ASSIGN l-Bloqueia = YES
           iCont      = 1.

    DO iCont = 1 TO NUM-ENTRIES (es-param-cenario.grupos,","):
        FIND FIRST usuar_grp_usuar NO-LOCK
            WHERE usuar_grp_usuar.cod_usuar     = v_cod_usuar_corren
            AND   usuar_grp_usuar.cod_grp_usuar = ENTRY(iCont,es-param-cenario.grupos,",") NO-ERROR.
        IF AVAILABLE usuar_grp_usuar THEN
            ASSIGN l-Bloqueia = NO.
    END.

    IF INDEX (es-param-cenario.empresas,v_cod_empres_usuar) > 0 AND l-Bloqueia = YES THEN DO:

        IF wh-fgl702ca-cod_cenar_ctbl:SCREEN-VALUE <> es-param-cenario.cod_cenar_ctbl_destino THEN DO:
            RUN utp/ut-msgs.p (INPUT "show":U, 
                               INPUT 17006, 
                               INPUT "Cen rio diferente de < " + es-param-cenario.cod_cenar_ctbl_destino + " >!" + "~~" +
                                     "Parƒmetro de equaliza‡Æo de cen rio Ativo. Somente permitido lan‡amentos com cen rio igual a < " + es-param-cenario.cod_cenar_ctbl_destino + " > .":u).
            APPLY "ENTRY" TO wh-fgl702ca-cod_cenar_ctbl.
            RETURN "NOK":U.
        END.
        ELSE DO:
            IF p_ind_object = "bt_ok-f" THEN DO:
                APPLY "choose" TO wh-fgl702ca-bt_ok.
                RETURN "OK":U.
            END.
            ELSE DO:
                APPLY "choose" TO wh-fgl702ca-bt_sav.
                RETURN "OK":U.
            END.
        END.
    END.
    ELSE DO:
        IF p_ind_object = "bt_ok-f" THEN DO:
            APPLY "choose" TO wh-fgl702ca-bt_ok.
            RETURN "OK":U.
        END.
        ELSE DO:
            APPLY "choose" TO wh-fgl702ca-bt_sav.
            RETURN "OK":U.
        END.
    END.
END.

