/*****************************************************************************
 ** PROGRAMA..: IN0315B
 ** OBJETIVO..: Carregar tabela hist-alter-inv - hist¢rico de alteraá∆o de investimentos
 ** AUTOR.....: Katia Claudino Warmling
 ** CLIENTE...: Yamana - 20.03.2012
******************************************************************************/
{include/i-prgvrs.i upc\in0315c-esp 2.00.00.000}  /*** 010000 ***/
/*****************************************************************************/
def input param p-ind-event   as char          no-undo.
def input param p-ind-object  as CHAR          no-undo.
def input param p-wgh-object  as handle        no-undo.
def input param p-wgh-frame   as widget-handle no-undo.
def input param p-cod-table   as char          no-undo.
def input param p-row-table   as rowid         no-undo.
/*****************************************************************************/

{utp/ut-glob.i}
DEFINE NEW GLOBAL SHARED TEMP-TABLE tt-ordem-inv LIKE ordem-inv.

DEFINE VARIABLE i-seq          AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-result       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-alter-origem AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-alterado     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-cont         AS INTEGER     NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE i-tipo AS INTEGER NO-UNDO.

def new global shared var c-motivo-alter  like hist-alter-inv.motivo no-undo.




FUNCTION f-retorna-campo-temp-table RETURNS CHAR (INPUT tabela AS CHAR,
                                                  INPUT campo  AS CHAR).

    case tabela:
         when "ordem-inv" then do:
              case campo: 
                   when "vl-verba" then return string (tt-ordem-inv.vl-verba[1]).
                   
                   otherwise return "".
              end case.     
         end.
        otherwise return "".      
  end case.               

END FUNCTION.   

FUNCTION f-retorna-campo-tabela RETURNS CHAR (INPUT tabela AS CHAR,
                                              INPUT campo  AS CHAR).

    case tabela:
         when "ordem-inv" then do:
              case campo:
                   when "vl-verba"        then return string (ordem-inv.vl-verba[1]).  
                   otherwise return "".
              end case.
         end.
         otherwise return "".
    end case.               

END FUNCTION.   

IF  p-ind-event  = "BEFORE-DISPLAY"
AND P-IND-OBJECT = "VIEWER"  THEN DO:

    FIND FIRST ordem-inv
        WHERE ROWID(ordem-inv) = p-row-table NO-LOCK NO-ERROR.

    FOR EACH tt-ordem-inv:
        DELETE tt-ordem-inv.
    END.

    IF NOT CAN-FIND(FIRST tt-ordem-inv) 
    AND AVAIL ordem-inv THEN DO:
        CREATE tt-ordem-inv.
        BUFFER-COPY ordem-inv TO tt-ordem-inv.
    END.
    ASSIGN c-motivo-alter = "".

END.

if  p-ind-event   = "AFTER-END-UPDATE":U
AND p-ind-object  = "VIEWER":U  THEN DO:
                            


    IF c-motivo-alter = "" THEN DO:
    FIND FIRST tt-ordem-inv NO-LOCK NO-ERROR.
    FIND FIRST ordem-inv WHERE ROWID(ordem-inv) = p-row-table NO-LOCK NO-ERROR.

    buffer-compare tt-ordem-inv to ordem-inv
        save result in c-result.

    ASSIGN c-alterado = "".

    IF i-tipo = 0 THEN
        ASSIGN i-tipo = 2.

    IF c-result <> ""  AND i-tipo = 2 AND c-motivo-alter = "" /*AND NOT gl-inclusao*/ THEN DO:
        RUN inp\spp\esin9000a.w.
    END.

    FIND FIRST proj-inv
        WHERE  proj-inv.ep-codigo    = ordem-inv.ep-codigo   
        AND    proj-inv.cod-est-exec = ordem-inv.cod-est-exec
        AND    proj-inv.num-projeto  = ordem-inv.num-projeto NO-LOCK NO-ERROR.


    DO  i-cont = 1 TO NUM-ENTRIES (c-result):
        IF  (entry (i-cont, c-result) = "vl-verba") THEN DO:

            
            FIND last hist-alter-inv
                WHERE hist-alter-inv.cod-estabel-exe  = ordem-inv.cod-est-exec   
                AND   hist-alter-inv.num-ordem        = ordem-inv.num-ordem
                AND   hist-alter-inv.num-proj         = ordem-inv.num-projeto exclusive-lock NO-ERROR.

            if avail hist-alter-inv then
                assign i-seq = hist-alter-inv.sequencia + 1.
            else
                assign i-seq = 1.

                CREATE hist-alter-inv.
                ASSIGN hist-alter-inv.cod-estabel-exe     = ordem-inv.cod-est-exec
                       hist-alter-inv.num-proj            = ordem-inv.num-projeto 
                       hist-alter-inv.num-ordem           = ordem-inv.num-ordem
                       hist-alter-inv.usuario             = c-seg-usuario
                       hist-alter-inv.cod-sit-proj        = ordem-inv.cod-situacao-inv
                       hist-alter-inv.sigla               = IF AVAIL proj-inv THEN proj-inv.sigla ELSE ""
                       hist-alter-inv.data                = today
                       hist-alter-inv.hora                = string(time,"HH:MM:SS")
                       hist-alter-inv.motivo              = c-motivo-alter
                       hist-alter-inv.tipo                = i-tipo
                       hist-alter-inv.sequencia           = i-seq    .
    
                ASSIGN c-alter-origem                     = f-retorna-campo-temp-table ("ordem-inv",entry (i-cont, c-result))
                       c-alterado                         = f-retorna-campo-tabela ("ordem-inv",entry (i-cont, c-result))
                       hist-alter-inv.valor-origem        = dec(c-alter-origem)
                       hist-alter-inv.valor-destino       = dec(c-alterado).

                   
        END.
    END.

    IF I-TIPO = 1 THEN DO:

        FIND last hist-alter-inv
                WHERE hist-alter-inv.cod-estabel-exe  = ordem-inv.cod-est-exec   
                AND   hist-alter-inv.num-ordem        = ordem-inv.num-ordem
                AND   hist-alter-inv.num-proj         = ordem-inv.num-projeto exclusive-lock NO-ERROR.

            if avail hist-alter-inv then
                assign i-seq = hist-alter-inv.sequencia + 1.
            else
                assign i-seq = 1.

        ASSIGN c-motivo-alter = "Inclus∆o de Orden de investimento".

         FIND FIRST hist-alter-inv
                WHERE hist-alter-inv.cod-estabel-exe  = ordem-inv.cod-est-exec   
                AND   hist-alter-inv.num-ordem    = ordem-inv.num-ordem
                AND   hist-alter-inv.num-proj  = ordem-inv.num-projeto NO-LOCK NO-ERROR.

            if not avail hist-alter-inv then do:
            end.

            CREATE hist-alter-inv.
            ASSIGN hist-alter-inv.cod-estabel-exe     = ordem-inv.cod-est-exec
                   hist-alter-inv.num-proj            = ordem-inv.num-projeto 
                   hist-alter-inv.num-ordem           = ordem-inv.num-ordem
                   hist-alter-inv.usuario             = c-seg-usuario
                   hist-alter-inv.cod-sit-proj        = ordem-inv.cod-situacao-inv
                   hist-alter-inv.sigla               = IF AVAIL proj-inv THEN proj-inv.sigla ELSE ""
                   hist-alter-inv.data                = TODAY
                   hist-alter-inv.hora                = string(time,"HH:MM:SS")
                   hist-alter-inv.motivo              = c-motivo-alter
                   hist-alter-inv.tipo                = i-tipo
                   hist-alter-inv.sequencia           = i-seq .

            ASSIGN c-alter-origem                     = f-retorna-campo-temp-table ("ordem-inv","vl-verba")
                   c-alterado                         = f-retorna-campo-tabela ("ordem-inv","vl-verba")
                   hist-alter-inv.valor-origem        = 0
                   hist-alter-inv.valor-destino       = dec(c-alterado).

        END.
    END.
END.

IF  p-ind-event  = "BEFORE-ADD" 
AND p-ind-object = "VIEWER" THEN DO:

    ASSIGN i-tipo = 1.
END.

IF  p-ind-event  = "BEFORE-INITIALIZE" 
AND p-ind-object = "VIEWER" THEN DO:

    ASSIGN i-tipo = 0.
END.


