/***************************************************************************
**
**  escc0407.i3 - Imprime a Narrativa do Item
**
***************************************************************************/

find first narrativa use-index codigo
    where narrativa.it-codigo = tt-ordem.it-codigo no-lock no-error.    
if  avail narrativa then do:
    put skip(1).
    if  narrativa.descricao <> "" then do:
        assign i-salta-narra = 1.
        if  line-counter > 63 then do:
            page.
            {&display}
        end.
        run pi-print-editor (input narrativa.descricao, input 60).
        for each tt-editor with frame f-narrativa:
            if  tt-editor.linha = 1 then
                put unformatted
                    fill("-", 19) at 52
                    c-lb-narra-it at 72
                    fill("-", 20) at 90.
            disp tt-editor.conteudo when avail narrativa
                 with frame f-narrativa.
            down with frame f-narrativa.
        end.
        if  line-counter > 63 then do:
            page.
            {&display}
        end.
    end.
end.

/* escc0407.i3 */
