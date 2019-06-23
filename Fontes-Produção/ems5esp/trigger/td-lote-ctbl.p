/*------------------------------------------------------------------------
    File        : td-lote_ctbl
    Purpose     :

    Syntax      :

    Description : Trigger para eliminar lotes cont†beis relacionados

    Author(s)   : RogÇrio Dias
    Created     : 
    Notes       :
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ------- Definiá∆o de ParÉmetros ------ */
DEF PARAMETER BUFFER b-lote_ctbl     FOR lote_ctbl.
 
/* ------- Definiá∆o de Buffer ------ */
DEF BUFFER buf_lote_ctbl     FOR lote_ctbl.
DEF BUFFER b_lancto_ctbl_rec FOR lancto_ctbl.

/* ------- Definiá∆o de Vari†veis ------ */
DEF VAR v_cod_empres_usuar_aux  AS CHARACTER FORMAT "x(3)":U LABEL "Empresa" COLUMN-LABEL "Empresa" NO-UNDO.
DEF VAR v_nom_usuar_corren_aux  AS CHARACTER NO-UNDO.
/* DEF VAR h-acomp                 AS HANDLE NO-UNDO. */
DEF VAR c-arquivo               AS CHARACTER NO-UNDO.
DEF VAR c-key-value             AS CHARACTER NO-UNDO.

DEF NEW GLOBAL SHARED VAR v_cod_empres_usuar AS CHARACTER FORMAT "x(3)":U LABEL "Empresa" COLUMN-LABEL "Empresa" NO-UNDO.
DEF NEW GLOBAL SHARED var v_cod_usuar_corren as CHARACTER format "x(12)":U label "Usu†rio Corrente" column-label "Usu†rio Corrente" no-undo.

/* ------- Definiá∆o de TEmp-tables ------ */
DEF TEMP-TABLE tt_log_erro NO-UNDO
    FIELD ttv_num_cod_erro  AS INTEGER   FORMAT ">>>>,>>9"  LABEL "N£mero"         COLUMN-LABEL "N£mero"
    FIELD ttv_des_msg_ajuda AS CHARACTER FORMAT "x(40)"     LABEL "Mensagem Ajuda" COLUMN-LABEL "Mensagem Ajuda"
    FIELD ttv_des_msg_erro  AS CHARACTER FORMAT "x(60)"     LABEL "Mensagem Erro"  COLUMN-LABEL "Inconsistància".


/**
-db ems5       -ld ems5       -N tcp -H ydmprg02 -S 35000
-db ems5_esp   -ld ems5esp    -N tcp -H ydmprg02 -S 35100
**/

/*---------Conex∆o de Banco----------------------------*/
/* IF NOT CONNECTED('ems5') THEN                                           */
/*     CONNECT -db ems5        -ld ems5       -S 35000 -H ydmprg02 -N tcp. */
/*                                                                         */
/* IF NOT CONNECTED('ems5esp') THEN                                        */
/*     CONNECT -db ems5_esp    -ld ems5esp    -S 35100 -H ydmprg02 -N tcp. */

/* ------ Inicializaá∆o das rotinas de acompanhamento ------- */
/* RUN utp/ut-acomp.p persistent set h-acomp.                               */
/* RUN pi-inicializar in h-acomp (INPUT "Localizando Lote Relacionado..."). */


FIND FIRST usuar_mestre WHERE usuar_mestre.cod_usuario = v_cod_usuar_corren NO-LOCK NO-ERROR.
IF AVAIL usuar_mestre THEN
    ASSIGN v_nom_usuar_corren_aux = usuar_mestre.nom_usuario.

DEF STREAM str-rp.
 
/* ------ Testa se o lote foi desatualizado. Se for o lote destino amarrado ao lote origem ser† apagado -------- */


    /* ---------- tabela espec°fica que armazena os lotes copiados para outra empresa ----------*/
    FIND FIRST es_cons_lotes EXCLUSIVE-LOCK WHERE es_cons_lotes.num_lote_orig    = b-lote_ctbl.num_lote_ctbl
                                              AND es_cons_lotes.cod_empresa_orig = b-lote_ctbl.cod_empresa NO-ERROR.
    IF AVAIL es_cons_lotes THEN DO:
        FIND FIRST buf_lote_ctbl WHERE buf_lote_ctbl.num_lote_ctbl = es_cons_lotes.num_lote_dest
                                   AND buf_lote_ctbl.cod_empresa   = es_cons_lotes.cod_empresa_dest EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL buf_lote_ctbl THEN DO:

/*             RUN pi-acompanhar in h-acomp ("Encontrado Lote: " + STRING(buf_lote_ctbl.num_lote_ctbl)). */

            ASSIGN v_cod_empres_usuar_aux = v_cod_empres_usuar
                   v_cod_empres_usuar     = buf_lote_ctbl.cod_empresa.

/*             RUN pi-acompanhar in h-acomp ("Eliminando Lote: " + STRING(buf_lote_ctbl.num_lote_ctbl)). */

            /* ----- Executa api de elmininaá∆o de lotes -------- */
            run prgfin/fgl/fgl201za.py (Input 1,
                                        Input buf_lote_ctbl.num_lote_ctbl,
                                        Input 0,
                                        Input "Eliminar",
                                        output table tt_log_erro).
            IF CAN-FIND(first tt_log_erro) THEN DO:

                ASSIGN c-arquivo = session:TEMP-DIRECTORY + "log_trg_lote.txt".
                OUTPUT STREAM str-rp to value(c-arquivo) NO-ECHO NO-CONVERT.

                PUT STREAM str-rp  FILL('-',132) FORMAT 'X(132)'  SKIP(1)
                                   'LOG ELIMININAÄ«O DE LOTES RELACIONADOS ' AT 01 SKIP (1)
                                   FILL('-',132) FORMAT 'X(132)'  SKIP
                                   'Lote Cont†bil'  AT 01
                                   'Nr. Erro'       AT 20
                                   'Descriá∆o Erro' AT 30
                                   'Ajuda Erro'     AT 150 SKIP
                                   FILL('-',132) FORMAT 'X(132)' SKIP.

                FOR EACH tt_log_erro:
                    PUT STREAM str-rp string(buf_lote_ctbl.num_lote_ctbl)           AT 01
                                      tt_log_erro.ttv_num_cod_erro                  AT 20
                                      tt_log_erro.ttv_des_msg_erro  FORMAT 'x(110)' AT 30
                                      tt_log_erro.ttv_des_msg_ajuda FORMAT 'X(110)' AT 150 SKIP.
                END.
                OUTPUT STREAM str-rp CLOSE.

                ASSIGN c-key-value = "Notepad.exe":U.
                RUN winexec (INPUT c-key-value + chr(32) + c-arquivo, input 1).

            END.            
            ASSIGN v_cod_empres_usuar = v_cod_empres_usuar_Aux.
            ASSIGN es_cons_lotes.resultado = 'Lote: ' + STRING(es_cons_lotes.num_lote_dest) + ' foi eliminado. Motivo: Lote origem sofreu alteraá‰es (Desatualizaá∆o / Eliminaá∆o) - Usu†rio: ' + v_nom_usuar_corren_aux + ' - Data: ' + STRING(TODAY,'99/99/9999') + ' - ' + STRING(TIME,'HH:MM:SS')
                   es_cons_lotes.num_lote_dest  = 0
                   es_cons_lotes.des_lote_dest  = ''
                   es_cons_lotes.dt_lancto_dest = ? 
                   es_cons_lotes.dt_lote_dest   = ?.
        END.
    END.

/*---------Desconectar Bancos do ems5------------------*/
/* IF CONNECTED('ems5') THEN DISCONNECT ems5 NO-ERROR.       */
/* IF CONNECTED('ems5esp') THEN DISCONNECT ems5esp NO-ERROR. */

/* RUN pi-finalizar IN h-acomp. */

procedure WinExec external "kernel32.dll":U:
    def input param prg_name  as char.
    def input param prg_style as short.
end procedure.

