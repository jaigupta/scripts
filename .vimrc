set nocompatible              " be iMproved, required

" autocompletion
set wildmode=longest,list,full
set wildmenu
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
" call vundle#begin('~/some/path/here')

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
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'tpope/vim-surround'
Plugin 'tomtom/tcomment_vim'
Plugin 'benekastah/neomake'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'AndrewRadev/switch.vim'
Plugin 'Chiel92/vim-autoformat'
Plugin 'easymotion/vim-easymotion'

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

filetype plugin indent off

call pathogen#infect()
call pathogen#helptags()

filetype plugin indent on

" Vim syntax based color highlighting
syntax enable

"Leave some context lines above and below cursor
set scrolloff=5
set mousemodel=extend
" Use X clipboard
set clipboard=unnamed
set clipboard+=unnamed

" Use 256 colors in term
set t_Co=256
" Display status line always
set laststatus=2

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
set foldmethod=indent
set foldnestmax=10
set nofoldenable
set foldlevel=2
set hlsearch
set incsearch

" leader is ,
let mapleader=","

" backup and undo files in a single location.
set undofile                " Save undo's after file closes
set undodir='~/.vim/undo//'
set undolevels=1000         " How many undos
set undoreload=10000        " number of lines to save for undo

set nobackup
set noswapfile
" set backupdir='~/.vim/backup//'
" set directory='~/.vim/swap//'

set background=dark
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
if has('gui_running')
  colorscheme solarized
  " Instead of installing the powerline fonts, I am using my own symbols from
  " unicode.
  let g:airline_left_sep = '»'
  let g:airline_left_sep = '▶'
  let g:airline_right_sep = '«'
  let g:airline_right_sep = '◀'
else
  colorscheme desert
  set encoding=utf-8
endif

" close vim if the only buffer left is NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

nnoremap tn :tabnew<Space>
nnoremap tk :tabnext<CR>
nnoremap tj :tabprev<CR>
nnoremap th :tabfirst<CR>
nnoremap tl :tablast<CR>

let g:session_autoload = 'no'
let g:session_autosave = 'no'
let g:solarized_termcolors=256

" Avoid the Esc key
inoremap jk <Esc>
vnoremap jk <Esc>jk <Esc>

" Navigation
nnoremap <S-j> 30j
nnoremap <S-k> 30k

nnoremap <leader>bn :bnext<CR>
nnoremap <leader>bp :bprevious<CR>

" Start editing file in the same folder
nnoremap <Leader>e :e <C-R>=expand('%:p:h') . '/'<CR>

" Easymotion search start
" ,,s
