/* ================================================
=== Programa.....: upc/esutb119ba-u00.p
=== Prog.Cadastro: bas_trad_cta_ctbl_ext (prgint/utb/utb119ba)
=== Autor........: Bruno Bertulli (DSC)
=== Data.........: 07/06/2013
=== Descri‡Æo....: Replica conta - Dele‡Æo
=================================================*/    

{tools/fc-handle-obj.i}
{tools/fc-falso.i}

def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-rec-table  as recid         no-undo. 

def new global shared var wh-utb119ba-bt_era3                 as widget-handle no-undo.
def new global shared var wh-utb119ba-bt_era3-f               as widget-handle no-undo.
def new global shared var wh-utb119ba-br-table                as widget-handle no-undo.
def new global shared var wh-utb119ba-cod_unid_organ          as widget-handle no-undo.
def new global shared var wh-utb119ba-cod_matriz_trad_cta_ext as widget-handle no-undo.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

DEFINE  VARIABLE c-handle-obj  AS CHARACTER          NO-UNDO.

DEFINE BUFFER bf-ori-trad_cta_ctbl_ext FOR trad_cta_ctbl_ext.
DEFINE BUFFER bf-des-trad_cta_ctbl_ext FOR trad_cta_ctbl_ext.

DEFINE VARIABLE h-acomp     AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-conta-ext AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-conta     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-handle    AS HANDLE      NO-UNDO.

/* ===> Main Block <=== */

/* MESSAGE 'p-ind-event  ' p-ind-event   skip */
/*         'p-ind-object ' p-ind-object  skip */
/*         'p-wgh-object ' p-wgh-object  skip */
/*         'p-wgh-frame  ' p-wgh-frame   skip */
/*         'p-cod-table  ' p-cod-table   skip */
/*         'p-rec-table  ' p-rec-table   skip */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */

if  p-ind-event = "INITIALIZE" then do:
    c-handle-obj               = fc-handle-obj("bt_era3,br_bac_trad_cta_ctbl_ext,cod_unid_organ,cod_matriz_trad_cta_ext",p-wgh-frame).
    wh-utb119ba-bt_era3        = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    wh-utb119ba-br-table       = WIDGET-HANDLE(ENTRY(2,c-handle-obj)) NO-ERROR.
    wh-utb119ba-cod_unid_organ = WIDGET-HANDLE(ENTRY(3,c-handle-obj)) NO-ERROR.
    wh-utb119ba-cod_matriz_trad_cta_ext = WIDGET-HANDLE(ENTRY(4,c-handle-obj)) NO-ERROR.

    wh-utb119ba-bt_era3-f      = fc-falso(wh-utb119ba-bt_era3, wh-utb119ba-bt_era3:FRAME, "").
    wh-utb119ba-bt_era3-f:LOAD-IMAGE-UP(wh-utb119ba-bt_era3:IMAGE-UP).
    wh-utb119ba-bt_era3-f:MOVE-TO-TOP().
    wh-utb119ba-bt_era3:VISIBLE  = NO.
  
    ON 'CHOOSE':U OF wh-utb119ba-bt_era3-f PERSISTENT RUN upc\esutb119ba-u00.p
       ("choose",
        "bt_era3-f",
        p-wgh-object,
        p-wgh-frame,
        p-cod-table,
        p-rec-table).

end.

IF  p-ind-event  = "choose"    AND
    p-ind-object = "bt_era3-f" THEN DO:

    assign h-handle    = wh-utb119ba-br-table:get-browse-column(1)
           c-conta-ext = h-handle:screen-value.

    assign h-handle    = wh-utb119ba-br-table:get-browse-column(4)
           c-conta     = h-handle:screen-value.

    APPLY "choose" TO wh-utb119ba-bt_era3.

    FIND FIRST bf-ori-trad_cta_ctbl_ext NO-LOCK
        WHERE bf-ori-trad_cta_ctbl_ext.cod_unid_organ          = wh-utb119ba-cod_unid_organ:SCREEN-VALUE
        AND   bf-ori-trad_cta_ctbl_ext.cod_matriz_trad_cta_ext = wh-utb119ba-cod_matriz_trad_cta_ext :SCREEN-VALUE 
        AND   bf-ori-trad_cta_ctbl_ext.cod_cta_ctbl_ext        = c-conta-ext
        AND   bf-ori-trad_cta_ctbl_ext.cod_cta_ctbl            = REPLACE (c-conta,".","")
        NO-ERROR.
    IF NOT AVAILABLE bf-ori-trad_cta_ctbl_ext THEN DO:

        run utp/ut-msgs.p (input "show",
                           input 27100,
                           input "Eliminar de TODAS as matrizes?~~Deseja eliminar a conta < " + c-conta-ext + " > de todas as Matrizes?").
        IF LOGICAL (RETURN-VALUE) = YES THEN DO:
    
            run utp/ut-acomp.p persistent set h-acomp.
    
            run pi-inicializar in h-acomp (input "Eliminando...").
    
            FOR EACH matriz_trad_cta_ctbl_ext NO-LOCK:
                FIND FIRST bf-des-trad_cta_ctbl_ext EXCLUSIVE-LOCK
                    WHERE bf-des-trad_cta_ctbl_ext.cod_unid_organ          = matriz_trad_cta_ctbl_ext.cod_unid_organ         
                    AND   bf-des-trad_cta_ctbl_ext.cod_matriz_trad_cta_ext = matriz_trad_cta_ctbl_ext.cod_matriz_trad_cta_ext
                    AND   bf-des-trad_cta_ctbl_ext.cod_cta_ctbl_ext        = c-conta-ext
                    AND   bf-des-trad_cta_ctbl_ext.cod_cta_ctbl            = REPLACE (c-conta,".","")
                    NO-ERROR.

                IF AVAILABLE bf-des-trad_cta_ctbl_ext THEN DO:
                    DELETE bf-des-trad_cta_ctbl_ext.
                END.
            END.
    
            run pi-finalizar in h-acomp. 
        END.
    END.
END.

