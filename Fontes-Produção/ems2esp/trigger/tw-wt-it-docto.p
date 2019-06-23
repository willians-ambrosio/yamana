/*-----------------------------------------------------------------------------------
    PROGRAMA : tw-wt-it-docto.p
    OBJETIVO : Trigger de Write para a tabela wt-it-docto
    AUTOR    : Sergio Luiz Neto da Silveira (DSC)
    DATA     : 20/12/2016
-----------------------------------------------------------------------------------*/
{include/i-prgvrs.i tw-wt-it-docto 2.06.00.000}

/* Variaveis de Par≥metros */ 
DEF PARAMETER BUFFER p-table     FOR wt-it-docto.
DEF PARAMETER BUFFER p-old-table FOR wt-it-docto.

DEFINE VARIABLE i-cont AS INTEGER    NO-UNDO.
DEFINE VARIABLE c-acao AS CHARACTER  NO-UNDO.

ASSIGN c-acao = "".

blk:
DO i-cont = 1 TO 11:
   IF PROGRAM-NAME(i-cont) = "CriaWtItDocto dibo/bodi317sd.p" THEN DO:
      ASSIGN c-acao = "ADD".
      LEAVE blk.
   END.
END.
IF PROGRAM-NAME(3) MATCHES  "*FT4004*" AND
   PROGRAM-NAME(4) MATCHES  "*FT4003*" THEN DO:
   ASSIGN c-acao = "UPDATE".

END.

/* MESSAGE c-acao                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */


{UTP/UT-GLOB.I}

DEFINE TEMP-TABLE tt-wt-it-docto LIKE es-wt-it-docto.

EMPTY TEMP-TABLE tt-wt-it-docto.

IF c-acao = "ADD" THEN DO:
   FIND FIRST natur-oper OF p-table
        NO-LOCK NO-ERROR.
   IF AVAILABLE(natur-oper) AND 
      natur-oper.terceiros AND 
     (NOT CAN-FIND(FIRST es-cfop-excecao WHERE es-cfop-excecao.nat-operacao = natur-oper.nat-operacao)) THEN DO:

      CREATE tt-wt-it-docto.
      ASSIGN tt-wt-it-docto.seq-wt-docto      = p-table.seq-wt-docto   
             tt-wt-it-docto.seq-wt-it-docto   = p-table.seq-wt-it-docto   
             tt-wt-it-docto.usuar-atualizacao = c-seg-usuario   
             tt-wt-it-docto.data-atual        = TODAY   
             tt-wt-it-docto.hora-atual        = STRING(TIME,"hh:mm:ss").  
     
      RUN esp/esft0005.w (INPUT-OUTPUT TABLE tt-wt-it-docto).


      IF CAN-FIND(FIRST tt-wt-it-docto
                  WHERE tt-wt-it-docto.cod-usuario = ""
                  NO-LOCK) THEN DO:
         RUN utp/ut-msgs.p (INPUT "show",
                            INPUT 17006,
                            INPUT "Respons†vel para o item n∆o informado").
         RETURN "NOK":U.
      END.   

      IF CAN-FIND(FIRST tt-wt-it-docto
                  WHERE tt-wt-it-docto.cod-usuario-aprov = ""
                  NO-LOCK) THEN DO:
         RUN utp/ut-msgs.p (INPUT "show",
                            INPUT 17006,
                            INPUT "Aprovador para o item n∆o informado").
         RETURN "NOK":U.
      END. 

      FIND FIRST tt-wt-it-docto NO-ERROR.
      CREATE es-wt-it-docto.

      ASSIGN es-wt-it-docto.seq-wt-docto      = tt-wt-it-docto.seq-wt-docto   
             es-wt-it-docto.seq-wt-it-docto   = tt-wt-it-docto.seq-wt-it-docto.

      ASSIGN es-wt-it-docto.usuar-atualizacao = tt-wt-it-docto.usuar-atualizacao
             es-wt-it-docto.data-atual        = tt-wt-it-docto.data-atual       
             es-wt-it-docto.hora-atual        = tt-wt-it-docto.hora-atual
             es-wt-it-docto.cod-usuario       = tt-wt-it-docto.cod-usuario
             es-wt-it-docto.cod-usuario-aprov = tt-wt-it-docto.cod-usuario-aprov.
   END.
END.



IF c-acao = "UPDATE" THEN DO:
   FIND FIRST es-wt-it-docto
        WHERE es-wt-it-docto.seq-wt-docto      = p-table.seq-wt-docto    AND
              es-wt-it-docto.seq-wt-it-docto   = p-table.seq-wt-it-docto 
        EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE(es-wt-it-docto) THEN DO:
      CREATE tt-wt-it-docto.
      ASSIGN tt-wt-it-docto.seq-wt-docto      = es-wt-it-docto.seq-wt-docto   
             tt-wt-it-docto.seq-wt-it-docto   = es-wt-it-docto.seq-wt-it-docto   
             tt-wt-it-docto.cod-usuario       = es-wt-it-docto.cod-usuario
             tt-wt-it-docto.cod-usuario-aprov = es-wt-it-docto.cod-usuario-aprov
             tt-wt-it-docto.usuar-atualizacao = c-seg-usuario   
             tt-wt-it-docto.data-atual        = TODAY   
             tt-wt-it-docto.hora-atual        = STRING(TIME,"hh:mm:ss").   

      RUN esp/esft0005.w (INPUT-OUTPUT TABLE tt-wt-it-docto).

      IF CAN-FIND(FIRST tt-wt-it-docto
                  WHERE tt-wt-it-docto.cod-usuario = ""
                  NO-LOCK) THEN DO:
         RUN utp/ut-msgs.p (INPUT "show",
                            INPUT 17006,
                            INPUT "Respons†vel para o item n∆o informado").
         RETURN "NOK":U.
      END.   

      IF CAN-FIND(FIRST tt-wt-it-docto
                  WHERE tt-wt-it-docto.cod-usuario-aprov = ""
                  NO-LOCK) THEN DO:
         RUN utp/ut-msgs.p (INPUT "show",
                            INPUT 17006,
                            INPUT "Aprovador para o item n∆o informado").
         RETURN "NOK":U.
      END.  

     
      FOR EACH tt-wt-it-docto 
               EXCLUSIVE-LOCK:
         ASSIGN es-wt-it-docto.usuar-atualizacao = c-seg-usuario
                es-wt-it-docto.data-atual        = TODAY
                es-wt-it-docto.hora-atual        = STRING(TIME,'hh:mm:ss')
                es-wt-it-docto.cod-usuario       = tt-wt-it-docto.cod-usuario
                es-wt-it-docto.cod-usuario-aprov = tt-wt-it-docto.cod-usuario-aprov.

         DELETE tt-wt-it-docto.
      END.
   END.
END.

RETURN "OK":U.


