/*******************************************************************************
**   PROGRAMA.: esfp3020rp
**   OBJETIVO.: Gera Hist¢rico de Beneficios pelo Movto Calculado
**   AUTOR....: Eduardo Brito
**   EMPRESA..: para Robison
**   DATA.....: Fevereiro/2014
**   VERSAO...: HCM 2.10
*******************************************************************************/
DEFINE VARIABLE i-cont      AS INT      NO-UNDO.
DEFINE VARIABLE j-cont      AS INT      NO-UNDO.
DEFINE VARIABLE i-histor    AS INT      NO-UNDO.
DEFINE VARIABLE i-cdn_ben   AS INT      NO-UNDO.
DEFINE VARIABLE h-acomp     AS HANDLE   NO-UNDO.
DEFINE VARIABLE c-ano-mes   AS CHAR 	NO-UNDO.

DEFINE BUFFER b-histor_benefic FOR histor_benefic.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.


DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino           AS INTEGER
    FIELD arquivo           AS CHAR FORMAT "x(35)"
    FIELD usuario           AS CHAR FORMAT "x(12)"
    FIELD data-exec         AS DATE
    FIELD hora-exec         AS INTEGER
    FIELD classifica        AS INTEGER
    FIELD desc-classifica   AS CHAR FORMAT "x(40)"
    FIELD modelo-rtf        AS CHAR FORMAT "x(35)"
    FIELD l-habilitaRtf     AS LOG
    FIELD i-estabel-i       AS char  
    FIELD i-estabel-f       AS char  
    FIELD i-matricula-i     AS INT  
    FIELD i-matricula-f     AS INT  
    FIELD i-ano-refer-i     AS INT 
    FIELD i-ano-refer-f     AS INT 
    FIELD i-mes-refer-i     AS INT 
    FIELD i-mes-refer-f     AS INT
    FIELD cdn_empresa-ini   LIKE histor_benefic.cdn_empresa
    FIELD cdn_empresa-fim   LIKE histor_benefic.cdn_empresa.

define temp-table tt-digita no-undo
    field cdn_event_fp  like event_fp.cdn_event_fp     
    field des_event_fp  like event_fp.des_event_fp  
    field cdn_beneficio like beneficio.cdn_beneficio 
    field des_beneficio like beneficio.des_beneficio.



DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

FOR EACH tt-raw-digita:
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + 'fp_log.txt').
PUT ' LOG - Histotico de Beneficios ' AT 01  SKIP
    FILL('-',100) FORMAT 'x(100)' SKIP.

RUN utp/ut-acomp.p persistent SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando").

FOR EACH movto_calcul_func USE-INDEX mvtclclf_id NO-LOCK
   WHERE movto_calcul_func.cdn_empresa       >= tt-param.cdn_empresa-ini
     AND movto_calcul_func.cdn_empresa       <= tt-param.cdn_empresa-fim
     AND movto_calcul_func.cdn_estab         >= tt-param.i-estabel-i
     AND movto_calcul_func.cdn_estab         <= tt-param.i-estabel-f
     AND movto_calcul_func.cdn_funcionario   >= tt-param.i-matricula-i
     AND movto_calcul_func.cdn_funcionario   <= tt-param.i-matricula-f
     AND movto_calcul_func.num_ano_refer_fp  >= tt-param.i-ano-refer-i
     AND movto_calcul_func.num_ano_refer_fp  <= tt-param.i-ano-refer-f
     AND movto_calcul_func.num_mes_refer_fp  >= tt-param.i-mes-refer-i
     AND movto_calcul_func.num_mes_refer_fp  <= tt-param.i-mes-refer-f
     AND movto_calcul_func.idi_tip_fp        = 1,
    EACH funcionario OF movto_calcul_func NO-LOCK:  

    /*
   FIND FIRST movto_integr_benefic_fp NO-LOCK
         WHERE movto_integr_benefic_fp.cdn_empresa      = movto_calcul_func.cdn_empresa      
           AND movto_integr_benefic_fp.cdn_estab        = movto_calcul_func.cdn_estab        
           AND movto_integr_benefic_fp.cdn_funcionario  = movto_calcul_func.cdn_funcionario  
           AND movto_integr_benefic_fp.num_ano_refer_fp = movto_calcul_func.num_ano_refer_fp 
           AND movto_integr_benefic_fp.num_mes_refer_fp = movto_calcul_func.num_mes_refer_fp NO-ERROR.
     */

    FIND FIRST histor_benefic USE-INDEX hstrbnfc_id
         WHERE histor_benefic.cdn_empresa       = movto_calcul_func.cdn_empresa             
           AND histor_benefic.cdn_estab         = movto_calcul_func.cdn_estab               
           AND histor_benefic.cdn_funcionario   = movto_calcul_func.cdn_funcionario         
           AND histor_benefic.num_ano_refer_fp  = movto_calcul_func.num_ano_refer_fp         
           AND histor_benefic.num_mes_lote_movt = movto_calcul_func.num_mes_refer_fp NO-ERROR.  

    IF NOT AVAIL histor_benefic THEN DO:

        CREATE histor_benefic.
        ASSIGN histor_benefic.cdn_empresa       = movto_calcul_func.cdn_empresa             
               histor_benefic.cdn_estab         = movto_calcul_func.cdn_estab               
               histor_benefic.cdn_funcionario   = movto_calcul_func.cdn_funcionario         
               histor_benefic.num_ano_refer_fp  = movto_calcul_func.num_ano_refer_fp         
               histor_benefic.num_mes_lote_movt = movto_calcul_func.num_mes_refer_fp 
               histor_benefic.num_seq_movto_ben = 1.

    END.                                      
    
    bl-movto:
    DO i-cont = 1 TO 30:

        RUN pi-acompanhar IN h-acomp (INPUT "Evento: " +  movto_calcul_func.cdn_event_fp[i-cont] ).

        IF movto_calcul_func.cdn_event_fp[i-cont] = '' THEN NEXT.

        IF NOT CAN-FIND(FIRST tt-digita WHERE tt-digita.cdn_event_fp  = movto_calcul_func.cdn_event_fp[i-cont]) THEN NEXT.

        RUN pi-beneficio-evento.

        IF  i-cdn_ben = 0 THEN NEXT.

        ASSIGN i-histor = 0.

        DO j-cont = 1 TO 20:

           IF  histor_benefic.cdn_event_fp[j-cont] = '' THEN 
               ASSIGN i-histor = j-cont.

           IF  histor_benefic.cdn_event_fp[j-cont] = movto_calcul_func.cdn_event_fp[i-cont] THEN DO:
               ASSIGN i-histor = j-cont.
               LEAVE.
           END.

        END.

        IF  i-histor <> 0 THEN DO:

              ASSIGN histor_benefic.cdn_benefici[i-histor]      = i-cdn_ben
                     histor_benefic.cdn_event_fp[i-histor]      = movto_calcul_func.cdn_event_fp[i-cont]
                     histor_benefic.val_calcul_efp[i-histor]    = movto_calcul_func.val_calcul_efp[i-cont]
                     histor_benefic.num_identif_quant[i-histor] = 1
                     histor_benefic.qti_efp                     = 20.

              ASSIGN c-ano-mes = STRING(movto_calcul_func.num_ano_refer_fp,'9999') + STRING(movto_calcul_func.num_mes_refer_fp,'99').
              RUN pi-log. 
              NEXT bl-movto.
          
        END.

    END.
END.

OUTPUT CLOSE.
RUN PI-FINALIZAR IN H-ACOMP.

OS-COMMAND SILENT START VALUE(SESSION:TEMP-DIRECTORY + 'fp_log.txt').


/*
PROCEDURE pi-beneficio-evento:

    DEF VAR x-cont AS INT NO-UNDO.

    DO x-cont = 1 TO 20:
        FIND LAST b-histor_benefic NO-LOCK
            WHERE b-histor_benefic.cdn_event_fp[x-cont] = movto_calcul_func.cdn_event_fp[i-cont] NO-ERROR.

        IF AVAIL b-histor_benefic THEN DO:
            MESSAGE b-histor_benefic.cdn_benefici[x-cont]
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ASSIGN i-cdn_ben = b-histor_benefic.cdn_benefici[x-cont].
            LEAVE.
        END.
    END.

END PROCEDURE.
*/
PROCEDURE pi-beneficio-evento:

    ASSIGN i-cdn_ben = 0.

    FOR EACH tt-digita WHERE tt-digita.cdn_event_fp = movto_calcul_func.cdn_event_fp[i-cont]:

        FIND FIRST efp_emis_inform_rendto
             WHERE efp_emis_inform_rendto.cdn_empresa   = movto_calcul_func.cdn_empresa 
             AND   efp_emis_inform_rendto.cdn_event_fp  = tt-digita.cdn_event_fp
             AND  (efp_emis_inform_rendto.cdn_estab     = movto_calcul_func.cdn_estab
             OR    efp_emis_inform_rendto.cdn_estab     = "*")
             AND   efp_emis_inform_rendto.cdn_beneficio = tt-digita.cdn_beneficio
             NO-LOCK NO-ERROR.
    
        IF  AVAIL efp_emis_inform_rendto THEN
            ASSIGN i-cdn_ben = efp_emis_inform_rendto.cdn_beneficio. 

    END.

END PROCEDURE.

PROCEDURE pi-log:

    PUT 'Funcion rio: '              AT 01
        funcionario.cdn_funcionario  AT 25
        'Estabel'                    AT 35
        funcionario.cdn_estab        AT 44
        'Mes/Ano: '                  AT 50
        c-ano-mes                    AT 60
        'Evento:'                    AT 70 
        movto_calcul_func.cdn_event_fp[i-cont] AT 80
        'Valor:'                     AT 90 
        movto_calcul_func.val_calcul_efp[i-cont] AT 100 SKIP.

END PROCEDURE.

