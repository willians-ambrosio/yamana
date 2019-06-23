/*****************************************************************************************
**  Programa..: prghur\upc\fp1510ymup.p - Yamana
**  Objetivo  : Criar campos na viewer V07py085.w  -  FP1500 e fp1520;
**              considerar todos os cargos abaixo de 5000 inativos
**  Observação: Teve lógica da upc upfp1500.p incorporada a sua.
*****************************************************************************************/
/*****************************************************************************************
**  Desabilita campo salario_atual se usu rio nÆo participar de grupo                   **
**  tipo Administra‡Æo Central - tabelas espec¡ficas grp_acesso e                       **
**  usuar_grp_acesso.  (Fabiano BONASSA)                                                **
*****************************************************************************************/
/*
MESSAGE "fp1510ymup.p"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
{utp\ut-glob.i}
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
def var v_log_usuar_espec                       as logical initial no   no-undo.
def var v_dat_aux_histor                        as date                 no-undo.
def var wh-objeto                               as widget-handle        no-undo.
def new global shared var wh-cdn-estab          as widget-handle    no-undo.
def new global shared var wh-cdn-funcionario    as widget-handle    no-undo.
def new global shared var wh-cod-rh-ccusto      as widget-handle    no-undo.
def new global shared var wh-label-desligto_func    as widget-handle    no-undo.
def new global shared var wh-dat_desligto_func        as widget-handle    no-undo.

DEF NEW GLOBAL SHARED VAR dt-demissao-fp1500  AS DATE                       NO-UNDO.

DEF BUFFER b-funcionario FOR funcionario.

/*message "Evento...........:" string(p-ind-event)      skip
        "Objeto...........:" string(p-ind-object)     skip
        "Handle do Objeto.:" string(p-wgh-object)     skip
        "Handle da Frame..:" p-wgh-frame:name         skip
        "Tabela...........:" p-cod-table              skip
        "Rowid............:" string(p-row-table)      skip
        "Programa.........:" p-wgh-object:FILE-NAME   SKIP
        "Objeto...........:" c-objeto.
*/

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

if p-ind-event = "INITIALIZE" and
   c-objeto    = "v46py085.w" then do:

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


if  p-ind-object = "VIEWER"                AND 
    c-objeto     = "v42py085.w"            AND  
    p-ind-event  = "before-initialize"     AND
    NOT VALID-HANDLE(wh-dat_desligto_func) then do:

    /* Nome Superior */
    create text wh-label-desligto_func
    assign frame             = p-wgh-frame
           format            = "x(18)"
           width             = 13.00
           height            = 0.88
           screen-value      = "Desligto Func:"
           row               = 5.17
           col               = 50.20.

                       /* Nome Colaborador */    
    create fill-in wh-dat_desligto_func 
    assign frame              = p-wgh-frame
           name               = "wh-dat_desligto_func"
           data-type          = "date"
           format             = "99/99/9999"
           width              = 10.00
           height             = 0.88
           row                = 5.17
           col                = 61.00
           visible            = yes
           sensitive          = no
           HELP               = "Desligto Func".
           
end.

if  p-ind-object = "VIEWER"             AND    
    c-objeto     = "v42py085.w"         AND 
    p-ind-event  = "display"            AND  
    VALID-HANDLE(wh-dat_desligto_func) then do:
    /*
    MESSAGE 
      "v_cdn_empres_usuar                   " v_cdn_empres_usuar                    skip
      "wh-cdn-estab:screen-value            " wh-cdn-estab:screen-value             skip
      "int(wh-cdn-funcionario:screen-value) " int(wh-cdn-funcionario:screen-value)  skip
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
    find first func_desligto no-lock
        where func_desligto.cdn_empresa     = v_cdn_empres_usuar
          and func_desligto.cdn_estab       = wh-cdn-estab:screen-value
          and func_desligto.cdn_funcionario = int(wh-cdn-funcionario:screen-value) no-error.
    if avail func_desligto then do:
        if func_desligto.dat_desligto <> ? then do:            
            assign 
              wh-dat_desligto_func:screen-value = string(func_desligto.dat_desligto)
              dt-demissao-fp1500                = date(wh-dat_desligto_func:SCREEN-VALUE).
        end.
        else do:
            assign 
              wh-dat_desligto_func:screen-value = ?
              dt-demissao-fp1500                = ?.
        end.
    end.
    else do:
          assign 
            wh-dat_desligto_func:screen-value = ?
            dt-demissao-fp1500                = ?.
    end.

    
END.

/* if  p-ind-object = "VIEWER"             AND      */
/*     c-objeto     = "v43py085.w"         AND      */
/*     p-ind-event  = "enable"             AND      */
/*     VALID-HANDLE(wh-dat_desligto_func) then do:  */
/*     assign wh-dat_desligto_func:sensitive = yes. */
/* END.                                             */
/*                                                  */
/* if  p-ind-object = "VIEWER"             AND      */
/*     c-objeto     = "v43py085.w"         AND      */
/*     p-ind-event  = "disable"            AND      */
/*     VALID-HANDLE(wh-dat_desligto_func) then do:  */
/*     assign wh-dat_desligto_func:sensitive = NO.  */
/* END.                                             */

/* IF                                       */
/*   p-ind-object = "VIEWER"           AND  */
/*   p-ind-event  = "after-end-update" AND  */
/*   c-objeto = "v42py085.w"           THEN */
/*   DO:                                    */
/*     /*                                   */
/*     MESSAGE                              */
/*       "p-ind-object " p-ind-object skip  */
/*       "c-objeto     " c-objeto     skip  */
/*       "p-ind-event  " p-ind-event  skip  */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*     */                                   */
/*   END.                                   */

/* IF                                                                                          */
/*   p-ind-event = "VALIDATE"                    AND                                           */
/*   p-ind-object = "VIEWER"                     AND                                           */
/*   c-objeto     = "v46py085.w"                 AND                                           */
/*   VALID-HANDLE(wh-label-desligto_func)        AND                                           */
/*   VALID-HANDLE(wh-dat_desligto_func)          THEN                                          */
/*   DO:                                                                                       */
/*     ASSIGN dt-demissao-fp1500 = date(wh-dat_desligto_func:SCREEN-VALUE).                    */
/*   END.                                                                                      */
/* IF                                                                                          */
/*   p-ind-event = "after-end-update"                          AND                             */
/*   p-ind-object = "VIEWER"                                   AND                             */
/*   p-wgh-object:PRIVATE-DATA = "object/sopy/vwr/v42py085.w"  THEN                            */
/*   DO:                                                                                       */
/*     FIND FIRST b-funcionario                                                                */
/*       NO-LOCK                                                                               */
/*       WHERE ROWID(b-funcionario) = p-row-table                                              */
/*       NO-ERROR.                                                                             */
/*     IF AVAIL b-funcionario THEN                                                             */
/*       DO:                                                                                   */
/*         ASSIGN wh-dat_desligto_func:SCREEN-VALUE = STRING(dt-demissao-fp1500).              */
/*         /* RUN pi-mensag-conta. */                                                          */
/*         /*-------------------------------------------------------------------------------*/ */
/*         find first func_desligto                                                            */
/*           where func_desligto.cdn_empresa     = b-funcionario.cdn_empresa                   */
/*           and   func_desligto.cdn_estab       = b-funcionario.cdn_estab                     */
/*           and   func_desligto.cdn_funcionario = b-funcionario.cdn_funcionario               */
/*           no-error.                                                                         */
/*         if not avail func_desligto then                                                     */
/*           DO:                                                                               */
/*             create func_desligto.                                                           */
/*             assign                                                                          */
/*               func_desligto.cdn_empresa     = b-funcionario.cdn_empresa                     */
/*               func_desligto.cdn_estab       = b-funcionario.cdn_estab                       */
/*               func_desligto.cdn_funcionario = b-funcionario.cdn_funcionario.                */
/*           END.                                                                              */
/*         /*                                                                                  */
/*         ASSIGN                                                                              */
/*           func_desligto.existe-cta      = l-achou-cta                                       */
/*           func_desligto.usuar_alter     = v_cod_usuar_corren.                               */
/*         */                                                                                  */
/*         IF                                                                                  */
/*           func_desligto.dat_desligto <> date(wh-dat_desligto_func:screen-value) THEN        */
/*               DO:                                                                           */
/*                 ASSIGN                                                                      */
/*                   func_desligto.dat_desligto    = DATE(wh-dat_desligto_func:SCREEN-VALUE).  */
/*                 RUN prghur\upc\fp1500ymupa.p(                                               */
/*                   INPUT ROWID(b-funcionario),                                               */
/*                   INPUT ROWID(func_desligto)).                                              */
/*               END.                                                                          */
/*         /*-------------------------------------------------------------------------------*/ */
/*       END.                                                                                  */
/*   END.                                                                                      */
