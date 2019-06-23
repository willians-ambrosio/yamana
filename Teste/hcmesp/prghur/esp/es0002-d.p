/*******************************************************************************
* Programa: es0002-d.p - Gatilho da trigger delete
* Autor   : Fl vio Capitanio
* Data    : Jun05 - AFVIEW SERVI€OS EMPRESARIAIS
                     fcapitanio@uol.com,br
                     (11) 9756-8761 (11) 8341-1361
*******************************************************************************/
DEFINE PARAM BUFFER p-table     FOR func_turno_trab.

/*DEFINE VARIABLE c-objeto AS CHARACTER      NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(PROGRAM-NAME(3),"~/"),PROGRAM-NAME(3),"~/").
*/

 /*  Message "Passei " Skip
        "p-table    "  p-table.cdn_turno_trab     Skip      
        "p-old-table"  p-old-tabLE.cdn_turno_trab Skip
        View-as Alert-box Information.
   */

If Avail p-table And p-table.cdn_turno_trab = 17
                 And p-table.cdn_turma_trab <= 4 Then
Do:
   Run pi-delete-movto.
End.

Procedure pi-delete-movto:
    Find event_fp Where  event_fp.cdn_event_fp = "146" No-lock No-error.
    If Not Avail event_fp Then
       Message  "Evento 146  nÆo cadastrado "  View-as Alert-box Information.
    
    Find Last func_turno_trab Where
               func_turno_trab.cdn_empresa     = p-table.cdn_empresa And 
               func_turno_trab.cdn_estab       = p-table.cdn_estab    And
               func_turno_trab.cdn_funcionario = p-table.cdn_funcionario And
               func_turno_trab.cdn_turno_trab  = 17 And 
               func_turno_trab.cdn_turma_trab  <= 4 Exclusive-lock No-error.
     If Avail func_turno_trab Then
        delete func_turno_trab.
        
    Find Last movto_fp_control_parc Where
              movto_fp_control_parc.cdn_empresa     = p-table.cdn_empresa And
              movto_fp_control_parc.cdn_estab       = p-table.cdn_estab  And
              movto_fp_control_parc.cdn_funcionario = p-table.cdn_funcionario And
              movto_fp_control_parc.cdn_event_fp    = "146" Exclusive-lock No-error.
             /* movto_fp_control_parc.num_seq_movto_fp_control_parc = 1  */
 If Avail movto_fp_control_parc And movto_fp_control_parc.idi_sit_lancto_movto_parc = 1 Then
    Assign movto_fp_control_parc.idi_sit_lancto_movto_par = 2. /*** fica inativo o movimento **/

End Procedure.
