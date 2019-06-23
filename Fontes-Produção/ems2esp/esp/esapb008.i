
def var i-opcao        as integer                    no-undo.
def var c-cod-emitente like movto-estoq.cod-emitente no-undo.
def var c-nro-docto    like movto-estoq.nro-docto    no-undo.
def var c-serie-docto  like movto-estoq.serie-docto  no-undo.
def var i-tipo-docto   as integer                    no-undo.
DEF VAR gr-docum-aux    AS ROWID.

MESSAGE "{1}"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

DEFINE BUFFER bf-docum-est      FOR docum-est.
DEFINE BUFFER bf-dupli-apagar   FOR dupli-apagar.
DEFINE BUFFER bf-despesa-aces   FOR despesa-aces.

IF AVAIL tit_ap THEN
    ASSIGN c-cod-emitente   =   tit_ap.cdn_fornecedor 
           c-nro-docto      =   tit_ap.cod_tit_ap   
           c-serie-docto    =   tit_ap.cod_ser_doct.

if  avail ems5.espec_docto then do:
    case espec_docto.ind_tip_espec_docto:
        
        when "Previs∆o" /*l_previsao*/  then
            assign i-tipo-docto = 1.
        when "Normal" /*l_normal*/  then
            assign i-tipo-docto = 2.
        when "" /*l_null*/  then
            assign i-tipo-docto = 3.
     END.
 END.

 
&IF "{&bf_mat_versao_ems}" >= "2.07" &THEN
    def  var i-pais-impto-usuar as CHARACTER NO-UNDO. 
&ELSE
   def  var v_cod_pais_empres_usuar as CHARACTER NO-UNDO.
&ENDIF



find first param-global no-lock no-error.
if  avail param-global then
    if  param-global.modulo-re = no then do:
        /*** MENSAGEM recebimento n“o implantado ***/
        run utp/ut-msgs.p (input "show":U, input 25539, input " ").            
        return "NOK":U.
    end.

if  i-pais-impto-usuario <> 1 
&IF "{&bf_mat_versao_ems}" >= "2.07" &THEN
    AND i-pais-impto-usuar <> "bra":U THEN DO:
&ELSE
   AND v_cod_pais_empres_usuar <> "bra":U THEN DO:
&ENDIF                                        
    if  i-tipo-docto = 1 then
        find bf-docum-est where
             bf-docum-est.cod-emitente = c-cod-emitente and
             bf-docum-est.nro-docto    = c-nro-docto    and
             bf-docum-est.serie-docto  = c-serie-docto  and
             bf-docum-est.nff          = no no-lock no-error.
    if  i-tipo-docto = 2 or
        i-tipo-docto = 3 then
        find bf-docum-est where
             bf-docum-est.cod-emitente = c-cod-emitente and
             bf-docum-est.nro-docto    = c-nro-docto    and
             bf-docum-est.serie-docto  = c-serie-docto  and
             bf-docum-est.nff          = yes no-lock no-error.
    if avail bf-docum-est then 
        assign gr-docum-aux = rowid(bf-docum-est).
    else do:
        /*Quando nío encontra o documento, fazer nova busca utilizando como numero do documento 
          a posiªío livre onde ≤ gravado o numero do embarque*/
        if  i-tipo-docto = 1 then
        find bf-docum-est where
             bf-docum-est.cod-emitente = c-cod-emitente and
             trim(substr(bf-docum-est.char-1,1,12)) = c-nro-docto    and
             bf-docum-est.serie-docto  = c-serie-docto  and
             bf-docum-est.nff          = no no-lock no-error.
        if  i-tipo-docto = 2 or
            i-tipo-docto = 3 then
            find bf-docum-est where
                 bf-docum-est.cod-emitente = c-cod-emitente and
                 trim(substr(bf-docum-est.char-1,1,12)) = c-nro-docto    and
                 bf-docum-est.serie-docto  = c-serie-docto  and
                 bf-docum-est.nff          = yes no-lock no-error.
        if avail bf-docum-est then 
            assign gr-docum-aux = rowid(bf-docum-est).
        ELSE DO:
            /*** MENSAGEM nota fiscal n“o encontrada ***/
            /* Inicio -- Projeto Internacional */
            {utp/ut-liter.i "Nota_Fiscal" *}
            run utp/ut-msgs.p (input "show":U, input 56, input RETURN-VALUE).
            return "NOK":U.
        END.
    end.
END.
ELSE DO:
    FIND FIRST bf-dupli-apagar
        WHERE bf-dupli-apagar.cod-emitente = c-cod-emitente
          AND bf-dupli-apagar.nro-docto    = c-nro-docto
          AND bf-dupli-apagar.serie-docto  = c-serie-docto  NO-LOCK NO-ERROR.
    IF  AVAIL bf-dupli-apagar THEN DO:
        FIND bf-docum-est
            WHERE bf-docum-est.cod-emitente = bf-dupli-apagar.cod-emitente
              AND bf-docum-est.serie-docto  = bf-dupli-apagar.serie-docto 
              AND bf-docum-est.nro-docto    = bf-dupli-apagar.nro-docto
              AND bf-docum-est.nat-operacao = bf-dupli-apagar.nat-operacao NO-LOCK NO-ERROR.
        IF  AVAIL bf-docum-est THEN
            ASSIGN gr-docum-aux = ROWID(bf-docum-est).
    END.
    ELSE DO:
        FIND FIRST  bf-despesa-aces
            WHERE  bf-despesa-aces.ser-docto-ac = c-serie-docto
              AND  bf-despesa-aces.nro-docto-ac = c-nro-docto
              AND  bf-despesa-aces.cod-forn-ac  = c-cod-emitente NO-LOCK NO-ERROR.
        IF  AVAIL  bf-despesa-aces THEN DO:
            FIND bf-docum-est
                WHERE bf-docum-est.cod-emitente =  bf-despesa-aces.cod-emitente
                  AND bf-docum-est.serie-docto  =  bf-despesa-aces.serie-docto 
                  AND bf-docum-est.nro-docto    =  bf-despesa-aces.nro-docto
                  AND bf-docum-est.nat-operacao =  bf-despesa-aces.nat-operacao NO-LOCK NO-ERROR.
            IF  AVAIL bf-docum-est THEN
                ASSIGN gr-docum-aux = ROWID(bf-docum-est).
        END.
    END.
END.

