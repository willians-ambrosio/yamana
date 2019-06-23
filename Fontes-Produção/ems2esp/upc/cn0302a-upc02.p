/*****************************************************************************
* Empresa  : DKP
* Cliente  : YAMANA
* Programa : upc/cn0302a-upc02.p
* Descricao: Criar campo Desc/Glosa 
* Autor    : Ramon - DKP
* Data     : 02/2018
*
******************************************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE        NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER     NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS rowid         NO-UNDO.
  
{utp/ut-glob.i}
 
DEF VAR c-objeto  AS CHAR          NO-UNDO .
DEF VAR h_objeto  AS WIDGET-HANDLE NO-UNDO.
DEF VAR h_frame   AS WIDGET-HANDLE NO-UNDO . 

ASSIGN c-objeto   = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:private-data, "~/").
  
DEF NEW GLOBAL SHARED VAR  wh-nr-contrato      AS  WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR  wh-num-seq-item     AS  WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR  wh-numero-ordem     AS  WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR  wh-num-seq-event    AS  WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR  wh-num-seq-medicao  AS  WIDGET-HANDLE  NO-UNDO.
 
DEF NEW GLOBAL SHARED VAR  wh-val-previsto     AS  WIDGET-HANDLE  NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-vl-glosa-desc     AS WIDGET-HANDLE  NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-txt-vl-glosa-desc AS WIDGET-HANDLE  NO-UNDO.

DEF BUFFER b-medicao-contrat FOR medicao-contrat.
   
 

/**        
MESSAGE 
   "p-ind-event  " p-ind-event  SKIP
   "p-ind-object " p-ind-object SKIP
   "p-wgh-object " p-wgh-object SKIP
   "p-wgh-frame  " p-wgh-frame  SKIP
   "p-cod-table  " p-cod-table  SKIP
   "c-objeto:    " c-objeto     SKIP
   "p-row-table  " string(p-row-table) SKIP
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
 **/
  
/**    
OUTPUT TO c:\temp\CN0302A-EVENTOS.TXT APPEND  .
PUT  "p-ind-event  " p-ind-event  FORMAT "x(30)"   SKIP
     "p-ind-object " p-ind-object FORMAT "x(30)"   SKIP
     "p-wgh-object " p-wgh-object SKIP
     "p-wgh-frame  " p-wgh-frame  SKIP
     "p-cod-table  " p-cod-table  FORMAT "x(30)"   SKIP
     "c-objeto:    " c-objeto     FORMAT "x(30)"   SKIP
     "p-row-table  " string(p-row-table)  SKIP(1) .
OUTPUT CLOSE.
 **/
 

IF  p-ind-event  = "BEFORE-INITIALIZE" 
AND p-ind-object = "VIEWER"     
AND c-objeto     = "v14in582.w"  THEN DO:

    /** 
    MESSAGE "p-ind-event  " p-ind-event  SKIP
            "p-ind-object " p-ind-object SKIP
            "p-wgh-object " p-wgh-object SKIP
            "p-wgh-frame  " p-wgh-frame  SKIP
            "p-cod-table  " p-cod-table  SKIP
            "c-objeto:    " c-objeto     SKIP
            "p-row-table  " string(p-row-table) SKIP
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
     **/
    ASSIGN h_Frame = p-wgh-frame:FIRST-CHILD.   /* pegando o Field-Group */

    ASSIGN h_Frame = h_Frame:FIRST-CHILD.       /* pegando o 1o. Campo */
            
    DO WHILE h_Frame <> ? :

        IF h_frame:TYPE <> "field-group" THEN DO: 

            IF h_Frame:NAME = "i-nr-contrato" THEN DO:
                ASSIGN wh-nr-contrato = h_Frame.
            END.
            IF h_Frame:NAME = "i-num-seq-item" THEN DO:
                ASSIGN wh-num-seq-item = h_Frame.
            END.
            IF h_Frame:NAME = "i-numero-ordem" THEN DO:
                ASSIGN wh-numero-ordem = h_Frame.
            END.
            IF h_Frame:NAME = "num-seq-event" THEN DO:
                ASSIGN wh-num-seq-event = h_Frame.
            END.
            IF h_Frame:NAME = "num-seq-medicao" THEN DO:
                ASSIGN wh-num-seq-medicao = h_Frame.
            END.
 
            IF h_Frame:NAME = "val-previsto" THEN DO:
                 ASSIGN wh-val-previsto = h_Frame.
            END.
    
    
             /**    
            OUTPUT TO c:\temp\upccn0102-FIELDS.TXT  APPEND.
            PUT  /*
             "p-ind-event  " p-ind-event  FORMAT "x(30)"   SKIP
             "p-ind-object " p-ind-object FORMAT "x(30)"   SKIP
             "p-wgh-object " p-wgh-object SKIP
             "p-wgh-frame  " p-wgh-frame  SKIP
             "p-cod-table  " p-cod-table  FORMAT "x(30)"   SKIP
             "c-objeto:    " c-objeto     FORMAT "x(30)"   SKIP
             "p-row-table  " string(p-row-table)  SKIP(1) */
    
             "h_frame:type = " h_frame:TYPE FORMAT "x(20)" SKIP
             "h_Frame:NAME = " h_Frame:NAME FORMAT "x(20)" SKIP  (2).
            OUTPUT CLOSE.
            **/
  
            IF h_frame:TYPE = "FRAME" THEN DO:
  
                ASSIGN h_objeto = h_frame:FIRST-CHILD.

                DO WHILE VALID-HANDLE(h_objeto):

                    IF h_objeto:TYPE <> "field-group" THEN DO:

                        /*
                        IF   h_Frame:NAME  = "fPage1"  
                        AND  h_objeto:NAME = "nat-operacao" THEN
                            ASSIGN WH-NAT-OPERACAO = h_objeto.
                        */ 
                        /**  
                        OUTPUT TO c:\temp\upccd4600-FIELDS.TXT  APPEND.
                        PUT  /*
                         "p-ind-event  " p-ind-event  FORMAT "x(30)"   SKIP
                         "p-ind-object " p-ind-object FORMAT "x(30)"   SKIP
                         "p-wgh-object " p-wgh-object SKIP
                         "p-wgh-frame  " p-wgh-frame  SKIP
                         "p-cod-table  " p-cod-table  FORMAT "x(30)"   SKIP
                         "c-objeto:    " c-objeto     FORMAT "x(30)"   SKIP
                         "p-row-table  " string(p-row-table)  SKIP(1) */
                
                         "h_frame:type = " h_frame:TYPE FORMAT "x(20)" SKIP
                         "h_Frame:NAME = " h_Frame:NAME FORMAT "x(20)" SKIP  
                         "h_frame:type = " h_objeto:TYPE FORMAT "x(20)" SKIP
                         "h_Frame:NAME = " h_objeto:NAME FORMAT "x(20)" SKIP(2).
                        OUTPUT CLOSE.
                         **/
 
                        ASSIGN h_objeto = h_objeto:NEXT-SIBLING.

                    END.
                    ELSE
                        ASSIGN h_objeto = h_objeto:FIRST-CHILD.
                END.
            END.
                                 
            ASSIGN h_Frame = h_Frame:NEXT-SIBLING.

        END. 
        ELSE do:
            ASSIGN h_frame = h_frame:FIRST-CHILD.
        END. 
              
    END.
  
END.
 
 
IF P-ind-event  = "ADD"    AND
   p-ind-object = "VIEWER" AND 
   c-objeto     = "v14in582.w"  THEN DO:
 
  
    IF VALID-HANDLE(wh-txt-vl-glosa-desc) THEN DO:
        ASSIGN wh-txt-vl-glosa-desc:SCREEN-VALUE =  "Valor Desc/Glosa:".
    END.
  
    IF VALID-HANDLE(wh-vl-glosa-desc)   THEN DO:
        ASSIGN wh-vl-glosa-desc:SCREEN-VALUE =  "0".
    END.
 
END.
   


IF  p-ind-event  = "INITIALIZE" 
AND p-ind-object = "VIEWER"     
AND c-objeto     = "v14in582.w"  THEN DO:

    CREATE fill-in wh-vl-glosa-desc
    ASSIGN row          = 12.3
           column       = 21.7
           data-type    = "Decimal"
           format       = ">>>,>>>,>>9.99"
           frame        = p-wgh-frame
           sensitive    = YES
           visible      = yes
           height-chars = 0.88
           width-chars  = 12.

      
    CREATE text wh-txt-vl-glosa-desc
    ASSIGN ROW          = 12.3
           column       = 9.3
           frame        = p-wgh-frame
           sensitive    = yes
           visible      = yes
           height-chars = 0.88
           format       = "X(18)"
           width-chars  = 12
           screen-value = "Valor Desc/Glosa:".
 
END.
 
 
IF  p-ind-event  = "AFTER-ENABLE" 
AND p-ind-object = "VIEWER"  
AND c-objeto     = "v14in582.w" THEN DO:
      
    IF STRING(p-row-table) <> ? THEN DO:
           
        FIND FIRST b-medicao-contrat 
             WHERE ROWID(b-medicao-contrat) = p-row-table 
                   NO-LOCK NO-ERROR.
            
        IF AVAIL b-medicao-contrat
             AND b-medicao-contrat.nr-contrato <> 0 THEN DO:
         
            FIND FIRST es-medicao-contrat NO-LOCK 
                 WHERE es-medicao-contrat.nr-contrato     = b-medicao-contrat.nr-contrato    
                   AND es-medicao-contrat.num-seq-item    = b-medicao-contrat.num-seq-item   
                   AND es-medicao-contrat.numero-ordem    = b-medicao-contrat.numero-ordem   
                   AND es-medicao-contrat.num-seq-event   = b-medicao-contrat.num-seq-event  
                   AND es-medicao-contrat.num-seq-medicao = b-medicao-contrat.num-seq-medicao NO-ERROR.
    
            IF AVAIL es-medicao-contrat THEN 
                ASSIGN wh-vl-glosa-desc:SCREEN-VALUE = STRING(es-medicao-contrat.vl-glosa-desc).
        END.
    END.
END.


IF  VALID-HANDLE(wh-val-previsto)
AND VALID-HANDLE(wh-vl-glosa-desc)   THEN DO:
     
/*     IF  wh-val-previsto:SENSITIVE = YES  THEN      */
/*         ASSIGN wh-vl-glosa-desc:SENSITIVE   = YES. */
/*     ELSE                                           */
/*         ASSIGN wh-vl-glosa-desc:SENSITIVE   = NO.  */
END.

  
IF  p-ind-event  = "END-UPDATE" 
AND p-ind-object = "VIEWER"      
AND c-objeto     =  "v14in582.w" THEN DO:
      
    IF p-row-table <> ? THEN DO:
   
        FIND FIRST b-medicao-contrat 
             WHERE ROWID(b-medicao-contrat) = p-row-table 
                   NO-LOCK NO-ERROR.

        IF AVAIL b-medicao-contrat
             AND b-medicao-contrat.nr-contrato <> 0 THEN DO:

            FIND FIRST es-medicao-contrat 
                 WHERE es-medicao-contrat.nr-contrato     = b-medicao-contrat.nr-contrato
                   AND es-medicao-contrat.num-seq-item    = b-medicao-contrat.num-seq-item   
                   AND es-medicao-contrat.numero-ordem    = b-medicao-contrat.numero-ordem   
                   AND es-medicao-contrat.num-seq-event   = b-medicao-contrat.num-seq-event  
                   AND es-medicao-contrat.num-seq-medicao = b-medicao-contrat.num-seq-medicao
                       NO-LOCK NO-ERROR .

            IF NOT AVAIL es-medicao-contrat THEN DO:

                CREATE es-medicao-contrat.
                ASSIGN es-medicao-contrat.nr-contrato     = b-medicao-contrat.nr-contrato 
                       es-medicao-contrat.num-seq-item    = b-medicao-contrat.num-seq-item    
                       es-medicao-contrat.numero-ordem    = b-medicao-contrat.numero-ordem    
                       es-medicao-contrat.num-seq-event   = b-medicao-contrat.num-seq-event   
                       es-medicao-contrat.num-seq-medicao = b-medicao-contrat.num-seq-medicao .
               
            END.
 
            FIND FIRST es-medicao-contrat 
                 WHERE es-medicao-contrat.nr-contrato     = b-medicao-contrat.nr-contrato
                   AND es-medicao-contrat.num-seq-item    = b-medicao-contrat.num-seq-item   
                   AND es-medicao-contrat.numero-ordem    = b-medicao-contrat.numero-ordem   
                   AND es-medicao-contrat.num-seq-event   = b-medicao-contrat.num-seq-event  
                   AND es-medicao-contrat.num-seq-medicao = b-medicao-contrat.num-seq-medicao
                       NO-ERROR .
 
            IF AVAIL es-medicao-contrat THEN DO:

                 ASSIGN es-medicao-contrat.data-alt      = TODAY
                        es-medicao-contrat.hora-alt      = STRING(TIME,"hh:mm:ss")
                        es-medicao-contrat.usuario-alt   = c-seg-usuario
                        es-medicao-contrat.vl-glosa-desc = DEC(wh-vl-glosa-desc:SCREEN-VALUE) .

            END.

        END.
     
    END.
  
END.
   
RETURN "OK".

 
