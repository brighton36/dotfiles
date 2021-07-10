""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Amix's vim:
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set runtimepath+=~/.vim_runtime

source ~/.vim_runtime/vimrcs/basic.vim
source ~/.vim_runtime/vimrcs/filetypes.vim
source ~/.vim_runtime/vimrcs/plugins_config.vim
source ~/.vim_runtime/vimrcs/extended.vim

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Below is basically the  ~/.vim_runtime/my_configs.vim, which, I put inline
" here, due to how our home git works, and how the submodule is assigned to 
" the ~/.vim_runtime file

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Basics
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 80-char width column, line numbers:
set foldcolumn=0
:set colorcolumn=80
set invnumber

" Colors:
set background=light

colorscheme solarized

" Set the background to be transparent
hi Normal guibg=NONE ctermbg=NONE

" Lightline statusbar
" Note that I had to create a custom scheme, in order to support the light 
" colors, with a transparent background. (though it only required a two-line
" change)
source ~/.vim/colorscheme/solarized_light.vim
let g:lightline = { 'colorscheme': 'solarized_light' }

" Tab:
set tabstop=2
set shiftwidth=2

" Buffer Navigation using ctrl-[ and ctrl-]
nnoremap <silent> <C-[> :bp<CR>
nnoremap <silent> <C-]> :bn<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" atm, this just includes the ledger syntax highlighting plugin:
call pathogen#infect('~/.vim/bundle/{}')

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Filetypes
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" C++
let g:ycm_confirm_extra_conf = 0

autocmd BufEnter *.cc :setlocal filetype=cpp

