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

" Lightline statusbar
let g:lightline = { 'colorscheme': 'solarized' }

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
