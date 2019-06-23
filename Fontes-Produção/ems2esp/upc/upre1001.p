
/*****************************************************************
 * DATASUL S.A                                                   *
 *---------------------------------------------------------------*
 * Programa....: UPRE1001                                        *
 * Data........:                                                 *
 * Responsavel.: Raimundo C. Soares                              *
 * Funcao......: Programa UPC de itens de recebimento            *
 *****************************************************************/



/********************************************************************
     PARAMENTROS DE ENTRADA DOS OBJETOS,EVENTOS E TABELAS
*********************************************************************/


def input param p-ind-event      as char          no-undo.
def input param p-ind-object     as char          no-undo.
def input param p-wgh-object     as handle        no-undo.
def input param p-wgh-frame      as widget-handle no-undo.
def input param p-cod-table      as char          no-undo.
def input param p-row-table      as rowid         no-undo.

/*variaveis global*/
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001-bt-itens   AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE wh-re1001-bt-troca-conta   AS WIDGET-HANDLE NO-UNDO.


/*variavel local*/
DEF VAR c-handle-obj             AS CHAR NO-UNDO.
DEF VAR h-objeto                 AS WIDGET-HANDLE NO-UNDO.
DEF VAR wh-re1001-rw-docum-est   AS ROWID.



/*     RUN pi-mensagem. */
IF p-cod-table = "docum-est"  AND p-row-table <> ? THEN do:
   ASSIGN wh-re1001-rw-docum-est = p-row-table.

   RUN pi-verifica-natureza.

END.


IF p-ind-event = "Before-Initialize" AND p-ind-object = "Container" THEN  DO:

   ASSIGN h-objeto = p-wgh-frame:first-child
          h-objeto = h-objeto:FIRST-CHILD.

   do while valid-handle(h-objeto):
      IF h-objeto:TYPE <> "FIELD-GROUP" THEN DO:
         IF h-objeto:NAME = "btDelete" THEN do: /*"fpage1"*/
            ASSIGN wh-re1001-bt-itens = h-objeto.
            LEAVE.
         END.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE
      DO:
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
      END.
   END.
   

   /**
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = "btitens" THEN DO:
         ASSIGN wh-re1001-bt-itens = h-objeto.
         LEAVE.
      END.
      IF  h-objeto:TYPE <> "FIELD-GROUP" THEN DO:
          ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE
          ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
   ***/
/*
   IF VALID-HANDLE(wh-re1001-bt-itens) THEN DO:

      CREATE BUTTON  wh-re1001-bt-troca-conta
      ASSIGN FRAME       = wh-re1001-bt-itens:FRAME
             WIDTH       = wh-re1001-bt-itens:WIDTH
             HEIGHT      = wh-re1001-bt-itens:HEIGHT
             ROW         = wh-re1001-bt-itens:ROW
             COLUMN      = wh-re1001-bt-itens:COLUMN /*+ wh-re1001-bt-itens:WIDTH*/ + 9.5
             TOOLTIP     = "Troca conta contabil"
             HELP        = "" 
             NAME        = "bt-troca-conta"
             SENSITIVE   = YES
             VISIBLE     = YES   
             LABEL       = "Troca"
             FONT        = wh-re1001-bt-itens:FONT
             TRIGGERS:
                ON 'choose':U PERSISTENT RUN upc/UPRE1001-1.p (input "CHOOSE",
                                                               input "troca-conta",
                                                               INPUT p-wgh-object,
                                                               input p-wgh-frame:FRAME, 
                                                               input ?, 
                                                               input wh-re1001-rw-docum-est).
             END triggers.
   END.
*/
END.

PROCEDURE pi-verifica-natureza:

   FIND FIRST docum-est
        WHERE ROWID(docum-est) = wh-re1001-rw-docum-est
        NO-LOCK NO-ERROR.

   IF AVAIL docum-est THEN DO:
      FIND FIRST natur-oper OF docum-est NO-LOCK NO-ERROR.
      IF AVAIL natur-oper AND natur-oper.terceiros  = YES THEN
         ASSIGN wh-re1001-bt-troca-conta:SENSITIVE =  YES
                wh-re1001-bt-troca-conta:VISIBLE   =  YES.
      ELSE
         ASSIGN wh-re1001-bt-troca-conta:SENSITIVE = NO
                wh-re1001-bt-troca-conta:VISIBLE   = NO .
   END.
END PROCEDURE.


PROCEDURE pi-mensagem:
   MESSAGE "p-ind-event  " p-ind-event  SKIP
           "p-ind-object " p-ind-object SKIP
           "p-wgh-object " p-wgh-object SKIP
           "p-wgh-frame  " p-wgh-frame  SKIP
           "p-cod-table  " p-cod-table  SKIP
           "p-row-table  " string(p-row-table)  SKIP
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE.

