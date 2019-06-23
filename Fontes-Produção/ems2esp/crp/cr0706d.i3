/***************************************************************************
***
*** crp/cr0706.i3 - Defini»’o das Form's do Relat½rio.
***
***************************************************************************/
def var cab-cod-estabel        like titulo.cod-estabel no-undo.
def var cab-cod-esp            like titulo.cod-esp no-undo.
def var cab-serie              like titulo.serie no-undo.
def var cab-nr-docto           like titulo.nr-docto no-undo.
def var cab-parcela            like titulo.parcela no-undo.
def var cab-cod-port           like titulo.cod-port no-undo.
def var cab-modalidade         as char format "x(2)" label "/M".
def var cab-dt-emissao         like titulo.dt-emissao no-undo.
def var cab-dt-vencimen        as date format "99/99/9999" label "Vencto" no-undo.
def var cab-vl-saldo           like titulo.vl-saldo no-undo.
def var cab-dias               as integer format "->>>>>9" label "Dias"  no-undo.
def var cab-vl-original        like titulo.vl-original no-undo.
def var cab-juros              as decimal  label "Juros" no-undo.
def var cab-total-juros        as decimal  label "Total Saldo" no-undo.

def var de-tot-original        like titulo.vl-original no-undo.
def var de-tot-saldo           like titulo.vl-saldo no-undo.
def var de-tot-juros           like titulo.vl-original no-undo.
def var de-total-saldo         like titulo.vl-saldo no-undo.

def frame f-cab-corpo 
    cab-cod-estabel         at 1
    cab-cod-esp             at 5
    cab-serie               at 9
    cab-nr-docto            at 15
    cab-parcela             at 32
    cab-cod-port            at 35
    cab-modalidade          at 41
    cab-vl-saldo            at 49  format "->>,>>>,>>>,>>9.99"
    cab-juros               at 68  format "->>,>>>,>>>,>>9.99"
    cab-total-juros         at 87  format "->>,>>>,>>>,>>9.99"
    cab-dt-emissao          at 109
    cab-dias                at 120
    skip
    cab-vl-original         at 49  format "->>,>>>,>>>,>>9.99"
    cab-dt-vencimen         at 109
    with no-box width 132 stream-io.

def frame f-cab-corpo-resumido 
    cab-cod-estabel         
    cab-cod-esp             
    cab-serie               
    cab-nr-docto            at 15
    cab-parcela             
    cab-cod-port            
    cab-modalidade          at 41 format "X(2)"       
    cab-vl-saldo            at 44 format "->>,>>>,>>9.99"
    cab-juros                     format "->>,>>9.99"  
    cab-total-juros               format "->>,>>>,>>9.99"
    cab-dt-emissao          
    cab-dias                      format "->>>>9"
    cab-vl-original               format "->>,>>>,>>>,>>9.99"
    cab-dt-vencimen         
    skip(0)
    with no-box width 132 stream-io.

{utp/ut-liter.i Vencto * R}
assign cab-dt-vencimen:label in frame f-cab-corpo = trim(return-value).
assign cab-dt-vencimen:label in frame f-cab-corpo-resumido = trim(return-value).

{utp/ut-liter.i Dias}
assign cab-dias:label in frame f-cab-corpo = trim(return-value).
assign cab-dias:label in frame f-cab-corpo-resumido = trim(return-value).

{utp/ut-liter.i /M}
assign cab-modalidade:label in frame f-cab-corpo = trim(return-value).
assign cab-modalidade:label in frame f-cab-corpo-resumido = substr(trim(return-value),1,2).

{utp/ut-liter.i Juros}
assign cab-juros:label in frame f-cab-corpo = trim(return-value).
assign cab-juros:label in frame f-cab-corpo-resumido = trim(return-value).
                 
{utp/ut-liter.i Total_Saldo}
assign cab-total-juros:label in frame f-cab-corpo = trim(return-value).
assign cab-total-juros:label in frame f-cab-corpo-resumido = trim(return-value).

def frame f-total 
    skip(2)
    fi-total-matriz   colon 80
    fi-saldo-anterior colon 80 
    skip(0)
    with no-box side-labels width 132 stream-io.

{utp/ut-liter.i Total_Saldo_Aberto}
assign fi-total-matriz:label in frame f-total = trim(return-value).

{utp/ut-liter.i Saldo_Anterior_do_Cliente}
assign fi-saldo-anterior:label in frame f-total = trim(return-value).

form
    tt-documento.cod-estabel    at 1 format "x(4)"
    tt-documento.cod-esp        at 5
    tt-documento.serie          at 9 format "x(6)"  
    tt-documento.nr-docto       at 15                
    tt-documento.parcela        at 32 format "x(3)"      
    tt-documento.cod-port       at 35 format ">>>>9"    
    tt-documento.modalidade     at 41
    tt-documento.de-saldo       to 66 format  "->>,>>>,>>>,>>9.99"
    tt-documento.de-juros       to 85 format  "->>,>>>,>>>,>>9.99"
    tt-documento.de-total-juros to 104 format "->>,>>>,>>>,>>9.99"
    tt-documento.dt-emissao     to 116                              
    tt-documento.i-dias         to 126 format "->>>>9"  
    skip
    tt-documento.vl-original    to 66 format "->>,>>>,>>>,>>9.99"
    tt-documento.dt-vencimen    to 116
    skip(1)
    with width 132 no-box down stream-io frame f-documento.

form
    tt-documento.cod-estabel            format "x(4)"
    tt-documento.cod-esp        
    tt-documento.serie                  format "x(5)"  
    tt-documento.nr-docto       at 15                
    tt-documento.parcela                format "x(3)"      
    tt-documento.cod-port               format ">>>>9"    
    tt-documento.modalidade             format ">9"
    tt-documento.de-saldo               format "->>,>>>,>>9.99"
    tt-documento.de-juros               format "->>,>>9.99"
    tt-documento.de-total-juros         format "->>,>>>,>>9.99"
    tt-documento.dt-emissao     
    tt-documento.i-dias                 format "->>>>9"  
    tt-documento.vl-original            format "->>,>>>,>>>,>>9.99"
    tt-documento.dt-vencimen    
    with width 132 no-box down stream-io frame f-documento-resumido.

form
   emitente.cod-emitente
   emitente.nome-abrev skip(2)
   with width 132 no-box down stream-io frame f-emitente.

   
/*--- Fim de Include ---*/
