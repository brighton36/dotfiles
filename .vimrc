" Vundle:
set nocompatible

filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'ap/vim-buftabline'
Plugin 'scrooloose/syntastic'
Plugin 'altercation/vim-colors-solarized'
Plugin 'valloric/youcompleteme'
Plugin 'octol/vim-cpp-enhanced-highlight'
Plugin 'sudar/comments.vim'
Plugin 'ledger/vim-ledger'
Plugin 'posva/vim-vue'

call vundle#end()
filetype plugin indent on

" Basics
set laststatus=2
set ruler
set hidden
set history=1000
set showmatch
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
set shiftwidth=2

" Intuitive backspacing in insert mode
set backspace=indent,eol,start
 
" File-type highlighting and configuration.
" Run :filetype (without args) to see what you may have
" to turn on yourself, or just set them all to be sure.
syntax on
filetype plugin indent on
 
" Highlight search terms...
set hlsearch
set incsearch " ...dynamically as they are typed.

set ignorecase 
set smartcase

" Toggle line numbers
set invnumber

" Since we're fish users ...
set shell=/bin/bash

" An easy shortcut to disable indent on an insert (for pasting from system
" clipboard)
set pastetoggle=<F2>

" I'm testing this at the moment:
:set colorcolumn=80

set background=light
colorscheme solarized

" I seem to have a knack for mis-typing the Q key. this is annoying
map Q <Nop>

" This is for minibuf explorer
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplVSplit = 20

" After reading the Use vim like a pro guide:
set showmode
set showcmd
set wildmenu
set ruler<
"runtim ftplugin/man.vim
set autoindent
"set spelllang=en_us

" Time to disable the arrow keys. It's for my own good:
noremap  <Up> ""
noremap! <Up> <Esc>
noremap  <Down> ""
noremap! <Down> <Esc>
noremap  <Left> ""
noremap! <Left> <Esc>
noremap  <Right> ""
noremap! <Right> <Esc>


" buftabline:
set hidden
nnoremap <C-N> :bnext<CR>
nnoremap <C-P> :bprev<CR>

" Syntastic adjustments for C++17:
let g:syntastic_cpp_compiler_options = ' -std=c++17'
let g:ycm_global_ycm_extra_conf = '/home/cderose/.vim/bundle/youcompleteme/.ycm_extra_conf.py'
autocmd BufEnter *.cc :setlocal filetype=cpp

