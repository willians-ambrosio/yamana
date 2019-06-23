/*****************************************************************************************
**  Programa..: prghur\upc\upc-fp1520-u01
**  Objetivo  : Criar campos Gestor e Nivel na viewer V14py085.w  FP1520;
*****************************************************************************************/
{utp\ut-glob.i}
DEFINE NEW GLOBAL SHARED VAR v_cdn_empres_usuar AS CHAR NO-UNDO.

def input param p-ind-event                      as char               no-undo.
def input param p-ind-object                     as char               no-undo.
def input param p-wgh-object                     as handle             no-undo.
def input param p-wgh-frame                      as widget-handle      no-undo.
def input param p-cod-table                      as char               no-undo.
def input param p-row-table                      as rowid              no-undo.

def var c-objeto                                as char                 no-undo.
def var c-container                             as char                 no-undo.
def var wgh-grupo                               as widget-handle        no-undo.
def var wgh-child                               as widget-handle        no-undo.
def var h-num-niv-sal                           as handle               no-undo.

def var wh-objeto                               as widget-handle        no-undo.
DEF VAR c-folder        AS CHARACTER                NO-UNDO.


DEF NEW GLOBAL SHARED VAR adm-broker-hdl                   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-lotacao-folder                AS WIDGET-HANDLE NO-UNDO.

/*Campos Chave*/
def new global shared var wh-cdn-estab          as widget-handle    no-undo.
def new global shared var wh-cdn-funcionario    as widget-handle    no-undo.

/*Campos a Serem monstrados na Tela*/
def new global shared var wh-label-gestor-nivel         as widget-handle    no-undo.
def new global shared var wh-cdn-gestor                 as widget-handle    no-undo.
def new global shared var wh-nom-gestor                 as widget-handle    no-undo.
def new global shared var wh-nivel-hierarquico          as widget-handle    no-undo.
def new global shared var wh-nom-nivel-hierarq          as widget-handle    no-undo.

DEFINE BUFFER bf-funcionario FOR funcionario.

/* message "Evento...........:" string(p-ind-event)      skip  */
/*         "Objeto...........:" string(p-ind-object)     skip  */
/*         "Handle do Objeto.:" string(p-wgh-object)     skip  */
/*         "Handle da Frame..:" p-wgh-frame:name         skip  */
/*         "Tabela...........:" p-cod-table              skip  */
/*         "Rowid............:" string(p-row-table)      skip  */
/*         "Programa.........:" p-wgh-object:FILE-NAME   SKIP  */
/*         "Objeto...........:" c-objeto.                      */

/* In¡cio */ 
FUNCTION buscarHandleCampo RETURNS WIDGET-HANDLE (INPUT pcCampo   AS CHARACTER,
                                                  INPUT whPointer AS WIDGET-HANDLE).

    DEFINE VARIABLE wh-grupo AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE wh-child AS WIDGET-HANDLE NO-UNDO.

    IF whPointer <> ? THEN 
       wh-grupo = whPointer:FIRST-CHILD. 
    ELSE wh-grupo = p-wgh-frame:FIRST-CHILD.
    
    DO WHILE VALID-HANDLE(wh-grupo):
       CASE wh-grupo:NAME:
            WHEN pcCampo THEN DO:
                 RETURN wh-grupo.
            END.
       END.  
       
       IF wh-grupo:TYPE = "field-group" THEN DO:
          wh-grupo = wh-grupo:FIRST-CHILD.
      END.
       ELSE
          wh-grupo = wh-grupo:NEXT-SIBLING.
   END.

END FUNCTION.

assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").

IF p-ind-event            = "BEFORE-INITIALIZE"  AND
   p-ind-object           = "CONTAINER"          AND
   p-wgh-object:FILE-NAME = "prghur/fpp/fp1520.w"   THEN DO:

    /*Muda o tamanho da Window */
/*     p-wgh-object:CURRENT-WINDOW:WIDTH = 95.  */
/*     p-wgh-frame:WIDTH = 95.                  */
/*     p-wgh-object:CURRENT-WINDOW:HEIGHT = 10.  */
/*     p-wgh-frame:HEIGHT = 10.                  */


    p-wgh-object:CURRENT-WINDOW:HEIGHT = 18.
    p-wgh-frame:HEIGHT = 18.

END.

if p-ind-event = "INITIALIZE" and
   c-objeto    = "v01py085.w" then do:

   assign wgh-grupo = p-wgh-frame:first-child.
    
   do while valid-handle(wgh-grupo):
       assign wgh-child = wgh-grupo:first-child.
   
       do while valid-handle(wgh-child):
           assign wh-objeto = wgh-child:handle.
           case wgh-child:name:
               when 'cdn_estab' then
                   assign wh-cdn-estab = wgh-child:handle.
               when 'cdn_funcionario' then
                   assign wh-cdn-funcionario = wgh-child:handle.
           end case.
           assign wgh-child = wgh-child:next-sibling no-error.
       end.
       leave.
   end.
end.

if  p-ind-object = "VIEWER" then do: 

    if c-objeto     = "v06py085.w" then do:

        RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                               INPUT "PAGE-SOURCE":U,
                                               OUTPUT c-folder).

        ASSIGN wh-lotacao-folder = WIDGET-HANDLE(c-folder) NO-ERROR.

        p-wgh-frame:HEIGHT = 30.
        RUN SET-SIZE IN wh-lotacao-folder (5 , 30) NO-ERROR.

        case p-ind-event:  
            when "before-initialize" then do:
                /* Gestor e Nivel Texto na Tela */
                create text wh-label-gestor-nivel
                assign frame             = p-wgh-frame
                       format            = "x(15)"
                       width             = 13.00
                       height            = 0.88
                       screen-value      = "Gestor e Nivel:"
                       row               = 12.15
                       col               = 9.00.
                
                /* Codigo Gestor */    
                create fill-in wh-cdn-gestor 
                assign frame              = p-wgh-frame
                       name               = "wh-cdn-gestor"
                       data-type          = "INTEGER" 
                       format             = "99999999"
                       width              = 8.00
                       height             = 0.88
                       row                = 12.15
                       col                = 19.60
                       visible            = yes
                       sensitive          = no
                       HELP               = "Codigo do Gestor".
                       
                /* Nome Gestor */    
                create fill-in wh-nom-gestor 
                assign frame              = p-wgh-frame
                       name               = "wh-non-gestor"
                       data-type          = "CHARACTER" 
                       format             = "X(30)"
                       width              = 30.00
                       height             = 0.88
                       row                = wh-cdn-gestor:ROW
                       col                = wh-cdn-gestor:COL + 8
                       visible            = yes
                       sensitive          = no
                       HELP               = "Nome do Gestor".
           

                /* Codigo Hierarquia */    
                create fill-in wh-nivel-hierarquico 
                assign frame              = p-wgh-frame
                       name               = "wh-nivel-hierarquico"
                       data-type          = "integer" 
                       format             = "999"
                       width              = 4.00
                       height             = 0.88
                       row                = wh-nom-gestor:ROW
                       col                = wh-nom-gestor:COL + 30
                       visible            = yes
                       sensitive          = no
                       HELP               = "Codigo Hierarquico".


                create fill-in wh-nom-nivel-hierarq 
                assign frame              = p-wgh-frame
                       name               = "wh-nom-nivel-hierarq"
                       data-type          = "CHARACTER" 
                       format             = "X(20)"
                       width              = 21.00
                       height             = 0.88
                       row                = wh-nivel-hierarquico:ROW
                       col                = wh-nivel-hierarquico:COL + 4
                       visible            = yes
                       sensitive          = no
                       HELP               = "Descri‡Æo da  Hierarquia".

            end.
            when "before-display" then do:
                ASSIGN wh-cdn-gestor:SCREEN-VALUE = ""
                       wh-nom-gestor:SCREEN-VALUE = ""
                       wh-nivel-hierarquico:SCREEN-VALUE = ""
                       wh-nom-nivel-hierarq:SCREEN-VALUE = "".
            END.
            when "display" then do:

                FIND bf-funcionario
                    WHERE bf-funcionario.cdn_empres      = v_cdn_empres_usuar
                      AND bf-funcionario.cdn_estab       = wh-cdn-estab:screen-value
                      AND bf-funcionario.cdn_funcionario = int(wh-cdn-funcionario:screen-value) 
                    NO-LOCK no-error.

                FIND LAST es_HistGestor USE-INDEX idx_GestorIni
                    WHERE es_HistGestor.cdn_empres      = bf-funcionario.cdn_empres
                      AND es_HistGestor.cdn_estab       = bf-funcionario.cdn_estab
                      AND es_HistGestor.cdn_funcionario = bf-funcionario.cdn_funcionario
                    NO-LOCK NO-ERROR.

                FIND es_gestor
                    WHERE es_gestor.cdn_gestor = es_HistGestor.cdn_gestor NO-LOCK NO-ERROR.

                IF AVAIL es_HistGestor THEN DO:
                    ASSIGN wh-cdn-gestor:SCREEN-VALUE = STRING(es_HistGestor.cdn_gestor,"99999999")
                           wh-nom-gestor:SCREEN-VALUE = es_gestor.nom_gestor           
                           wh-nivel-hierarquico:SCREEN-VALUE = STRING(es_gestor.niv_hier_funcnal,"999").

                    FIND niv_hier_funcnal
                        WHERE niv_hier_funcnal.cdn_niv_hier_funcnal = es_Gestor.niv_hier_funcnal NO-LOCK NO-ERROR.

                    ASSIGN wh-nom-nivel-hierarq:SCREEN-VALUE = IF AVAIL niv_hier_funcnal THEN des_niv_hier_funcnal ELSE "".

                END.
                ELSE 
                    ASSIGN wh-cdn-gestor:SCREEN-VALUE = ""
                           wh-nom-gestor:SCREEN-VALUE = ""
                           wh-nivel-hierarquico:SCREEN-VALUE = ""
                           wh-nom-nivel-hierarq:SCREEN-VALUE = "".


            end.
        END CASE.
    end.
end.
