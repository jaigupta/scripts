set nocompatible              " be iMproved, required


" autocompletion
set wildmode=longest,list,full
set wildmenu

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" Git wrapper
Plugin 'tpope/vim-fugitive'
" Random helper functions
Plugin 'L9'
Plugin 'ctrlpvim/ctrlp.vim'
" Tags for file (summary with tags)
Plugin 'majutsushi/tagbar'
Plugin 'moll/vim-node'
Plugin 'vim-scripts/SyntaxComplete'
Plugin 'burnettk/vim-angular'
Plugin 'scrooloose/nerdTree'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-surround'
Plugin 'tomtom/tcomment_vim'
Plugin 'benekastah/neomake'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'AndrewRadev/switch.vim'
Plugin 'Chiel92/vim-autoformat'

" Solarized color theme
Plugin 'alteration/vim-colors-solarized'

Plugin 'Shougo/unite.vim'
Plugin 'Shougo/vimproc.vim'  " configure this
Plugin 'Shougo/neco-vim'
Plugin 'honza/vim-snippets'

Plugin 'wincent/terminus'
Plugin 'ryanoasis/vim-devicons'
Plugin 'junegunn/fzf', { 'dir': '~/.fzf' }
Plugin 'junegunn/fzf.vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-pathogen'

" All of your Plugins must be added before the following line
call vundle#end()            " required

filetype off
filetype plugin indent off

call pathogen#infect()
call pathogen#helptags()

filetype on
filetype plugin indent on

set encoding=utf-8
set ruler
set hidden
set confirm
set ignorecase
set smartcase
set noshowmode
set relativenumber number
set tabstop=2 shiftwidth=2 expandtab
set conceallevel=0
set laststatus=2
set wrap linebreak nolist

" leader is ,
let mapleader=","

" backup and undo files in a single location.
set nobackup
set undofile
set undodir='~/.vim/undo//'
" set backupdir='~/.vim/backup//'

syntax enable
set background=dark
colorscheme solarized

" close vim if the only buffer left is NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

nnoremap tn :tabnew<Space>
nnoremap tk :tabnext<CR>
nnoremap tj :tabprev<CR>
nnoremap th :tabfirst<CR>
nnoremap tl :tablast<CR>

" Avoid the Esc key
inoremap jk <Esc>
vnoremap jk <Esc>jk <Esc>

" Navigation
nnoremap <S-j> 30j
nnoremap <S-k> 30k

nnoremap <C-Left> :bnext<CR>
nnoremap <C-Right> :bprevious<CR>

