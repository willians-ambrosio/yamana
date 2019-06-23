/* ================================================
=== Programa.....: upc/esfgl900zo-u01.p
=== Prog.Cadastro: api_lote_ctbl_recebto_2 (prgfin/fgl/fgl900zo.py)
=== Autor........: Bruno Bertulli (DSC)
=== Data.........: 28/05/2013
=== Descri‡Æo....: Alterar cen rio do lote
=================================================*/    

/* ===> Definitions <=== */

def temp-table tt_epc no-undo
    field cod_event     as char
    field cod_parameter as char
    field val_parameter as char
    index id is primary cod_parameter cod_event.

def input        param p-ind-event as char no-undo.
def input-output param table for tt_epc.

def var i-num_lote_ctbl as int no-undo.

DEF NEW GLOBAL SHARED var v_cod_usuar_corren as CHARACTER format "x(12)":U label "Usu rio Corrente" column-label "Usu rio Corrente" no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

/* ===> Main Block <=== */

FIND FIRST es-param-cenario NO-LOCK NO-ERROR.

IF INDEX (es-param-cenario.empresas,v_cod_empres_usuar) > 0 THEN DO:

    for each tt_epc
       where ( tt_epc.cod_event     = 'fgl900zl':U OR tt_epc.cod_event     = 'fgl900zo':U)
         and   tt_epc.cod_parameter = 'N£mero do Lote':U:
        assign i-num_lote_ctbl = int(tt_epc.val_parameter).
    end.

    find first lote_ctbl no-lock
         where lote_ctbl.num_lote_ctbl = i-num_lote_ctbl no-error.
    if  avail lote_ctbl AND INDEX (es-param-cenario.modulos,lote_ctbl.cod_modul_dtsul) > 0 THEN DO:
        FOR EACH lancto_ctbl OF lote_ctbl:
            FOR EACH item_lancto_ctbl OF lancto_ctbl:
                IF lote_ctbl.cod_modul_dtsul = "FAS" THEN DO:
                    IF item_lancto_ctbl.cod_cenar_ctbl = es-param-cenario.cod_cenar_ctbl_origem THEN
                        ASSIGN item_lancto_ctbl.cod_cenar_ctbl = es-param-cenario.cod_cenar_ctbl_destino.
                    ELSE IF item_lancto_ctbl.cod_cenar_ctbl <> es-param-cenario.cod_cenar_ctbl_destino THEN
                        DELETE item_lancto_ctbl.   
                END.
                ELSE DO:
                    ASSIGN item_lancto_ctbl.cod_cenar_ctbl = es-param-cenario.cod_cenar_ctbl_destino.
                END.
            END.
    
            IF lote_ctbl.cod_modul_dtsul = "FAS" THEN DO:
                IF lancto_ctbl.cod_cenar_ctbl = es-param-cenario.cod_cenar_ctbl_origem THEN
                    ASSIGN lancto_ctbl.cod_cenar_ctbl = es-param-cenario.cod_cenar_ctbl_destino.
                ELSE IF lancto_ctbl.cod_cenar_ctbl <> es-param-cenario.cod_cenar_ctbl_destino THEN
                    DELETE lancto_ctbl.
            END.
            ELSE DO:
                ASSIGN lancto_ctbl.cod_cenar_ctbl = es-param-cenario.cod_cenar_ctbl_destino.
            END.
        END.
    END.
end.

