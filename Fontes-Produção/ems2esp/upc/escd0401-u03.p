/***************************************************************************
      Filename: escd0401-u03.p
      Comment:  upc cd0401 - Modifica Fornecedores
      Created:  26.02.2016 Joao B. C. Bisneto
****************************************************************************/

def input param p-ind-event   as char          no-undo.
def input param p-ind-object  as CHAR          no-undo.
def input param p-wgh-object  as handle        no-undo.
def input param p-wgh-frame   as widget-handle no-undo.
def input param p-cod-table   as char          no-undo.
def input param p-row-table   as rowid         no-undo.

{utp/ut-glob.i}

def new global shared var wh-rs-trib-pis-cd0401     as widget-handle   no-undo.
def new global shared var wh-rs-trib-cofins-cd0401  as widget-handle   no-undo.
def new global shared var wh-cb-natureza-cd0401     as widget-handle   no-undo.
def new global shared var wh-escd0401-l-cooperativa as widget-handle   no-undo.
def new global shared var h-wgh-frame               as widget-handle   no-undo.

/* MESSAGE "p-ind-event..:" p-ind-event                  SKIP */
/*         "p-ind-object.:" p-ind-object                 SKIP */
/*         "p-cod-table..:" STRING(p-cod-table)          SKIP */
/*         "p-wgh-object.:" p-wgh-object:PRIVATE-DATA    SKIP */
/*         "p-wgh-frame..:" STRING(p-wgh-frame)          SKIP */
/*         "p-row-table..:" STRING(p-row-table) SKIP          */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK.                 */
/* IF NOT VALID-HANDLE(wh-conta-transit-re1001a1   ) THEN */
/*   RUN pi-acha-handle.                                  */

IF NOT VALID-HANDLE(wh-rs-trib-pis-cd0401) THEN
  DO:
    ASSIGN h-wgh-frame = p-wgh-frame.
    RUN utp\esut0001.p(INPUT  p-wgh-frame,                   
                       INPUT "rs-trib-pis",                
                       OUTPUT wh-rs-trib-pis-cd0401). 
    /* depuracao - Bisneto
     IF VALID-HANDLE(wh-rs-trib-pis-cd0401) THEN
       MESSAGE wh-rs-trib-pis-cd0401:NAME
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
  END.
IF NOT VALID-HANDLE(wh-rs-trib-cofins-cd0401) THEN
  DO:
    
    RUN utp\esut0001.p(INPUT p-wgh-frame,                   
                       INPUT "rs-trib-cofins",                
                       OUTPUT wh-rs-trib-cofins-cd0401). 
    /* depuracao - Bisneto
    IF VALID-HANDLE(wh-rs-trib-pis-cd0401) THEN
      MESSAGE wh-rs-trib-cofins-cd0401:NAME
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
  END.
IF NOT VALID-HANDLE(wh-cb-natureza-cd0401) THEN
  DO:
    ASSIGN h-wgh-frame = p-wgh-frame.
    RUN utp\esut0001.p(INPUT  p-wgh-frame,                   
                       INPUT "cb-natureza",                
                       OUTPUT wh-cb-natureza-cd0401). 
  END.

IF NOT VALID-HANDLE(wh-escd0401-l-cooperativa) THEN
  DO:
    ASSIGN h-wgh-frame = p-wgh-frame.
    RUN utp\esut0001.p(INPUT  p-wgh-frame,                   
                       INPUT "l-cooperativa",                
                       OUTPUT wh-escd0401-l-cooperativa). 
  END.


IF VALID-HANDLE(wh-rs-trib-pis-cd0401) THEN
  wh-rs-trib-pis-cd0401:SENSITIVE = NO.
IF VALID-HANDLE(wh-rs-trib-cofins-cd0401) THEN
  wh-rs-trib-cofins-cd0401:SENSITIVE = NO.
IF 
  p-ind-event                            = "ADD"        AND 
  /* p-ind-object                        = "VIEWER"     AND */
  /* p-wgh-frame                         = h-wgh-frame  AND */
  /* p-wgh-object:PRIVATE-DATA           = "advwr\v04ad098.w" AND */
  VALID-HANDLE(wh-rs-trib-pis-cd0401)    = YES          AND
  VALID-HANDLE(wh-rs-trib-cofins-cd0401) = YES          THEN
  DO:
    ASSIGN 
      wh-rs-trib-pis-cd0401   :SCREEN-VALUE = "1"
      wh-rs-trib-cofins-cd0401:SCREEN-VALUE = "1".
  END.


IF 
  p-ind-event                            = "ASSIGN"        AND 
  p-ind-object                        = "VIEWER"     AND 
  /* p-wgh-frame                         = h-wgh-frame  AND */
  /* p-wgh-object:PRIVATE-DATA           = "advwr\v04ad098.w" AND */
  VALID-HANDLE(wh-cb-natureza-cd0401)     = YES          AND
  VALID-HANDLE(wh-rs-trib-pis-cd0401)     = YES          AND
  VALID-HANDLE(wh-rs-trib-cofins-cd0401)  = YES          AND 
  VALID-HANDLE(wh-escd0401-l-cooperativa) = YES  
   THEN DO:
   IF wh-cb-natureza-cd0401:SCREEN-VALUE = "Pessoa Jur¡dica"  THEN DO:
      IF wh-escd0401-l-cooperativa:CHECKED = NO THEN
         ASSIGN wh-rs-trib-pis-cd0401   :SCREEN-VALUE = "1"
                wh-rs-trib-cofins-cd0401:SCREEN-VALUE = "1".
      ELSE 
         ASSIGN wh-rs-trib-pis-cd0401   :SCREEN-VALUE = "2"
                wh-rs-trib-cofins-cd0401:SCREEN-VALUE = "2".
   END.

   IF wh-cb-natureza-cd0401:SCREEN-VALUE = "Pessoa Fisica" THEN DO:
      ASSIGN wh-rs-trib-pis-cd0401   :SCREEN-VALUE = "2"
             wh-rs-trib-cofins-cd0401:SCREEN-VALUE = "2".
   END.
END.
