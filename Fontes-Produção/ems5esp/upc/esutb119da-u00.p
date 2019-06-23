/* ================================================
=== Programa.....: upc/esutb119da-u00.p
=== Prog.Cadastro: add_trad_cta_ctbl_ext (prgint/utb/utb119da.p)
=== Autor........: Bruno Bertulli (DSC)
=== Data.........: 04/06/2013
=== Descri‡Æo....: Replica conta
=================================================*/    

{tools/fc-handle-obj.i}
{tools/fc-falso.i}

def input param p-ind-event  as char          no-undo.
def input param p-ind-object as char          no-undo.
def input param p-wgh-object as handle        no-undo.
def input param p-wgh-frame  as widget-handle no-undo.
def input param p-cod-table  as char          no-undo.
def input param p-rec-table  as recid         no-undo. 

def new global shared var wh-esutb119da-u00-replica as handle no-undo.
def new global shared var wh-utb119da-dt-ini as handle no-undo.
def new global shared var wh-utb119da-dt-fim as handle no-undo.
def new global shared var wh-utb119da-tipo   as handle no-undo.

def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.

DEFINE BUFFER bf-ori-trad_cta_ctbl_ext FOR trad_cta_ctbl_ext.
DEFINE BUFFER bf-des-trad_cta_ctbl_ext FOR trad_cta_ctbl_ext.

def var v_dat_final
    as date
    format "99/99/9999":U
    label "Data Final"
    column-label "Data Final"
    no-undo.
def var v_dat_inicial
    as date
    format "99/99/9999":U
    label "Data Inicial"
    no-undo.
def var v_num_natur_sped
    as integer
    format ">>>>,>>9":U
    no-undo.

DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.
DEFINE VARIABLE c-handle-obj AS CHARACTER   NO-UNDO.

/* ===> Functions <=== */

/* Begin_Include: i_declara_SetEntryField */
FUNCTION SetEntryField RETURNS CHARACTER (input p_num_posicao     AS INTEGER,
                                          input p_cod_campo       AS CHARACTER,
                                          input p_cod_separador   AS CHARACTER,
                                          input p_cod_valor       AS CHARACTER):

/* ************* Parametros da FUN°€O *******************************
** Fun»’o para tratamento dos Entries dos c½digos livres
** 
**  p_num_posicao     - Nœmero do Entry / Posi»’o que serÿ atualizado
**  p_cod_campo       - Campo / Variÿvel que serÿ atualizada
**  p_cod_separador   - Separador que serÿ utilizado
**  p_cod_valor       - Valor que serÿ atualizado no Entry passado 
*******************************************************************/

    def var v_num_cont        as integer initial 0 no-undo.
    def var v_num_entries_ini as integer initial 0 no-undo.

    /* ** No progress a menor Entry ² 1 ***/
    if p_num_posicao <= 0 then 
       assign p_num_posicao = 1.       

    /* ** Caso o Campo contenha um valor invÿlido, este valor serÿ convertido para Branco
         para possibilitar os cÿlculo ***/
    if p_cod_campo = ? then do:
       assign p_cod_campo = "" /* l_*/ .
    end.

    assign v_num_entries_ini = num-entries(p_cod_campo,p_cod_separador) + 1 .    
    if p_cod_campo = "" /* l_*/  then do:
       assign v_num_entries_ini = 2.
    end.

    do v_num_cont =  v_num_entries_ini to p_num_posicao :
       assign p_cod_campo = p_cod_campo + p_cod_separador.
    end.

    assign entry(p_num_posicao,p_cod_campo,p_cod_separador) = p_cod_valor.

    RETURN p_cod_campo.

END FUNCTION.


/* End_Include: i_declara_SetEntryField */


/* Begin_Include: i_declara_GetEntryField */
FUNCTION GetEntryField RETURNS CHARACTER (input p_num_posicao     AS INTEGER,
                                          INPUT p_cod_campo       AS CHARACTER,
                                          input p_cod_separador   AS CHARACTER):

/* ************* Parametros da FUN°€O *******************************
** Fun»’o para tratamento dos Entries dos c½digos livres
** 
**  p_num_posicao     - Nœmero do Entry que serÿ atualizado
**  p_cod_campo       - Campo / Variÿvel que serÿ atualizada
**  p_cod_separador   - Separador que serÿ utilizado
*******************************************************************/

    if  p_num_posicao <= 0  then do:
        assign p_num_posicao  = 1.
    end.
    if num-entries(p_cod_campo,p_cod_separador) >= p_num_posicao  then do:
       return entry(p_num_posicao,p_cod_campo,p_cod_separador).
    end.
    return "" /*l_*/ .

END FUNCTION.


/* ===> Main Block <=== */

/* MESSAGE 'p-ind-event  ' p-ind-event   skip */
/*         'p-ind-object ' p-ind-object  skip */
/*         'p-wgh-object ' p-wgh-object  skip */
/*         'p-wgh-frame  ' p-wgh-frame   skip */
/*         'p-cod-table  ' p-cod-table   skip */
/*         'p-rec-table  ' p-rec-table   skip */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */

if  p-ind-event = "INITIALIZE" then do:

    c-handle-obj       = fc-handle-obj("v_dat_inicial,v_dat_final,v_cod_natur_sped",p-wgh-frame).
    wh-utb119da-dt-ini = WIDGET-HANDLE(ENTRY(1,c-handle-obj)) NO-ERROR.
    wh-utb119da-dt-fim = WIDGET-HANDLE(ENTRY(2,c-handle-obj)) NO-ERROR.
    wh-utb119da-tipo   = WIDGET-HANDLE(ENTRY(3,c-handle-obj)) NO-ERROR.

    create toggle-box wh-esutb119da-u00-replica
    assign frame        = p-wgh-frame
           row          = 1.5
           column       = 45
           visible      = true
           sensitive    = true
           screen-value = "yes"
           label        = "Replicar para todas as matrizes".
end.

IF p-ind-event  = "VALIDATE" AND
   p-ind-object = "viewer" THEN DO:

    IF LOGICAL (wh-esutb119da-u00-replica:SCREEN-VALUE) = YES THEN DO:
        run utp/ut-msgs.p (input "show",
                           input 27100,
                           input "Replicar para TODAS as matrizes?~~Deseja replicar para todas as Matrizes?").
        IF LOGICAL (RETURN-VALUE) = YES THEN DO:

            run utp/ut-acomp.p persistent set h-acomp.

            run pi-inicializar in h-acomp (input "Iniciando C¢pia...").

            FIND FIRST bf-ori-trad_cta_ctbl_ext NO-LOCK
                WHERE RECID (bf-ori-trad_cta_ctbl_ext) = p-rec-table NO-ERROR.
            IF AVAILABLE bf-ori-trad_cta_ctbl_ext THEN DO:
                FOR EACH matriz_trad_cta_ctbl_ext NO-LOCK:
                    FIND FIRST bf-des-trad_cta_ctbl_ext EXCLUSIVE-LOCK
                        WHERE bf-des-trad_cta_ctbl_ext.cod_unid_organ          = matriz_trad_cta_ctbl_ext.cod_unid_organ         
                        AND   bf-des-trad_cta_ctbl_ext.cod_matriz_trad_cta_ext = matriz_trad_cta_ctbl_ext.cod_matriz_trad_cta_ext
                        AND   bf-des-trad_cta_ctbl_ext.cod_cta_ctbl_ext        = bf-ori-trad_cta_ctbl_ext.cod_cta_ctbl_ext       
                        NO-ERROR.
                    IF NOT AVAILABLE bf-des-trad_cta_ctbl_ext THEN DO:

                        RUN pi-acompanhar IN h-acomp (INPUT "Criando: " + matriz_trad_cta_ctbl_ext.cod_unid_organ + " - " + matriz_trad_cta_ctbl_ext.cod_matriz_trad_cta_ext).

                        CREATE bf-des-trad_cta_ctbl_ext.
                        BUFFER-COPY bf-ori-trad_cta_ctbl_ext EXCEPT cod_unid_organ cod_matriz_trad_cta_ext cod_cta_ctbl_ext TO bf-des-trad_cta_ctbl_ext.
                        ASSIGN bf-des-trad_cta_ctbl_ext.cod_unid_organ          = matriz_trad_cta_ctbl_ext.cod_unid_organ
                               bf-des-trad_cta_ctbl_ext.cod_matriz_trad_cta_ext = matriz_trad_cta_ctbl_ext.cod_matriz_trad_cta_ext
                               bf-des-trad_cta_ctbl_ext.cod_cta_ctbl_ext        = bf-ori-trad_cta_ctbl_ext.cod_cta_ctbl_ext
                               .

                        assign v_dat_inicial    = DATE (wh-utb119da-dt-ini:screen-value)
                               v_dat_final      = DATE (wh-utb119da-dt-fim:screen-value)
                               v_num_natur_sped = INT  (SUBSTRING (wh-utb119da-tipo:screen-value,1,1)).

                        &if  '{&emsfin_version}' >= '5.07a' &then
                            assign bf-des-trad_cta_ctbl_ext.dat_inic_valid              = v_dat_inicial
                                   bf-des-trad_cta_ctbl_ext.dat_fim_valid               = v_dat_final
                                   bf-des-trad_cta_ctbl_ext.num_natur_trad_cta_ctbl_ext = v_num_natur_sped.
                        &else
                            assign bf-des-trad_cta_ctbl_ext.cod_livre_1 = SetEntryField(1,bf-des-trad_cta_ctbl_ext.cod_livre_1,chr(10),string(v_dat_inicial,'99/99/9999'))
                                   bf-des-trad_cta_ctbl_ext.cod_livre_1 = SetEntryField(2,bf-des-trad_cta_ctbl_ext.cod_livre_1,chr(10),string(v_dat_final,'99/99/9999'))
                                   bf-des-trad_cta_ctbl_ext.cod_livre_1 = SetEntryField(3,bf-des-trad_cta_ctbl_ext.cod_livre_1,chr(10),string(v_num_natur_sped)).
                        &endif
                    END.
                    ELSE DO:
                        IF RECID(bf-ori-trad_cta_ctbl_ext) <> RECID(bf-des-trad_cta_ctbl_ext) THEN DO:

                            RUN pi-acompanhar IN h-acomp (INPUT "Alterando: " + matriz_trad_cta_ctbl_ext.cod_unid_organ + " - " + matriz_trad_cta_ctbl_ext.cod_matriz_trad_cta_ext).

                            assign v_dat_inicial    = DATE (wh-utb119da-dt-ini:screen-value)
                                   v_dat_final      = DATE (wh-utb119da-dt-fim:screen-value)
                                   v_num_natur_sped = INT  (SUBSTRING (wh-utb119da-tipo:screen-value,1,1)).

                            &if  '{&emsfin_version}' >= '5.07a' &then
                                assign bf-des-trad_cta_ctbl_ext.dat_inic_valid              = v_dat_inicial
                                       bf-des-trad_cta_ctbl_ext.dat_fim_valid               = v_dat_final
                                       bf-des-trad_cta_ctbl_ext.num_natur_trad_cta_ctbl_ext = v_num_natur_sped.
                            &else
                                assign bf-des-trad_cta_ctbl_ext.cod_livre_1 = SetEntryField(1,bf-des-trad_cta_ctbl_ext.cod_livre_1,chr(10),string(v_dat_inicial,'99/99/9999'))
                                       bf-des-trad_cta_ctbl_ext.cod_livre_1 = SetEntryField(2,bf-des-trad_cta_ctbl_ext.cod_livre_1,chr(10),string(v_dat_final,'99/99/9999'))
                                       bf-des-trad_cta_ctbl_ext.cod_livre_1 = SetEntryField(3,bf-des-trad_cta_ctbl_ext.cod_livre_1,chr(10),string(v_num_natur_sped)).
                            &endif

                            ASSIGN bf-des-trad_cta_ctbl_ext.cod_ccusto_ext       = bf-ori-trad_cta_ctbl_ext.cod_ccusto_ext      
                                   bf-des-trad_cta_ctbl_ext.cod_ccusto           = bf-ori-trad_cta_ctbl_ext.cod_ccusto          
                                   bf-des-trad_cta_ctbl_ext.cod_cta_ctbl         = bf-ori-trad_cta_ctbl_ext.cod_cta_ctbl        
                                   bf-des-trad_cta_ctbl_ext.cod_estab            = bf-ori-trad_cta_ctbl_ext.cod_estab           
                                   bf-des-trad_cta_ctbl_ext.cod_estab_ext        = bf-ori-trad_cta_ctbl_ext.cod_estab_ext       
/*                                    bf-des-trad_cta_ctbl_ext.cod_livre_1          = bf-ori-trad_cta_ctbl_ext.cod_livre_1 */
                                   bf-des-trad_cta_ctbl_ext.cod_livre_2          = bf-ori-trad_cta_ctbl_ext.cod_livre_2         
                                   bf-des-trad_cta_ctbl_ext.cod_plano_ccusto     = bf-ori-trad_cta_ctbl_ext.cod_plano_ccusto    
                                   bf-des-trad_cta_ctbl_ext.cod_plano_cta_ctbl   = bf-ori-trad_cta_ctbl_ext.cod_plano_cta_ctbl  
                                   bf-des-trad_cta_ctbl_ext.cod_sub_cta_ctbl_ext = bf-ori-trad_cta_ctbl_ext.cod_sub_cta_ctbl_ext
                                   bf-des-trad_cta_ctbl_ext.cod_unid_negoc       = bf-ori-trad_cta_ctbl_ext.cod_unid_negoc      
                                   bf-des-trad_cta_ctbl_ext.cod_unid_negoc_ext   = bf-ori-trad_cta_ctbl_ext.cod_unid_negoc_ext  
                                   bf-des-trad_cta_ctbl_ext.dat_livre_1          = bf-ori-trad_cta_ctbl_ext.dat_livre_1         
                                   bf-des-trad_cta_ctbl_ext.dat_livre_2          = bf-ori-trad_cta_ctbl_ext.dat_livre_2         
                                   bf-des-trad_cta_ctbl_ext.des_cta_ctbl_ext     = bf-ori-trad_cta_ctbl_ext.des_cta_ctbl_ext    
                                   bf-des-trad_cta_ctbl_ext.log_livre_1          = bf-ori-trad_cta_ctbl_ext.log_livre_1         
                                   bf-des-trad_cta_ctbl_ext.log_livre_2          = bf-ori-trad_cta_ctbl_ext.log_livre_2         
                                   bf-des-trad_cta_ctbl_ext.num_livre_1          = bf-ori-trad_cta_ctbl_ext.num_livre_1         
                                   bf-des-trad_cta_ctbl_ext.num_livre_2          = bf-ori-trad_cta_ctbl_ext.num_livre_2         
                                   bf-des-trad_cta_ctbl_ext.val_livre_1          = bf-ori-trad_cta_ctbl_ext.val_livre_1         
                                   bf-des-trad_cta_ctbl_ext.val_livre_2          = bf-ori-trad_cta_ctbl_ext.val_livre_2         
                                   .
                        END.
                    END.
                END.
            END.

            run pi-finalizar in h-acomp. 

        END.
    END.
END.


