set nocompatible              " be iMproved, required

if !has('nvim')
  set encoding=utf-8
endif
" autocompletion
set wildmode=longest,list,full
set wildmenu

set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 9

"Leave some context lines above and below cursor
set scrolloff=5
set mousemodel=extend
" Use X clipboard
if !has('nvim')
  set clipboard=unnamed
  set clipboard+=unnamed
endif

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
" Always display status line`
set laststatus=2
set wrap linebreak nolist
set foldmethod=syntax
set foldnestmax=10
set nofoldenable
set foldlevel=2
set hlsearch
set incsearch
set sessionoptions=buffers

" leader is <Space> and \ for backup
let mapleader='\'
map <Space> \

" backup and undo files in a single location.
set undofile                " Save undo's after file closes
set undodir=~/.vim/undo//,/tmp
set undolevels=1000         " How many undos
set undoreload=10000        " number of lines to save for undo

set backupdir=~/.vim/backup//,/tmp
set directory=~/.vim/swap//,/tmp

set background=dark

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
Plugin 'jiangmiao/auto-pairs'
Plugin 'Zuckonit/vim-airline-tomato'

" Solarized color theme
Plugin 'alteration/vim-colors-solarized'
Plugin 'morhetz/gruvbox'

Plugin 'Shougo/unite.vim'
Plugin 'Shougo/vimproc.vim'  " configure this
Plugin 'Shougo/neco-vim'
" Supertab
Plugin 'ervandew/supertab'

" Snippets
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

Plugin 'wincent/terminus'
Plugin 'junegunn/fzf', { 'dir': '~/.fzf' }
Plugin 'junegunn/fzf.vim'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-pathogen'

" Same as git gutter but has support for perforce.
Plugin 'mhinz/vim-signify'

" All of your Plugins must be added before the following line
call vundle#end()            " required

filetype off
filetype plugin indent off

call pathogen#infect()
call pathogen#helptags()

filetype on
filetype plugin indent on

" Vim syntax based color highlighting, Preferably after pathogen.
syntax enable

let g:airline#extensions#tabline#enabled = 1
if has('gui_running')
  colorscheme solarized
  let g:airline_powerline_fonts = 1
else
  " Instead of installing the powerline fonts, I am using my own symbols from
  " unicode.
  let g:airline_left_sep = '»'
  let g:airline_left_sep = '▶'
  let g:airline_right_sep = '«'
  let g:airline_right_sep = '◀'
  colorscheme desert
endif

" close vim if the only buffer left is NERDTree
let g:NERDTreeDirArrows = 1
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let g:session_autoload = 'no'
let g:session_autosave = 'no'
let g:solarized_termcolors=256

" Disable devicons as fonts are not installed. TODO: install fonts
let g:webdevicons_enable = 0

" Avoid the Esc key
inoremap jk <Esc>

" Navigation
nnoremap <c-j> 30j
nnoremap <c-k> 30k

nnoremap <leader>bn :bnext<CR>
nnoremap <leader>bp :bprevious<CR>

" Start editing file in the same folder
nnoremap <leader>ee :e <C-R>=expand('%:p:h') . '/'<CR>

" Easymotion search start
" ,,s

let g:EclimCompletionMethod = 'omnifunc'

" NERDTree mappings
noremap <leader>nf :NERDTreeFind<CR>
noremap <leader>nt :NERDTreeToggle<CR>

" Eclim
nnoremap <leader>el :LocateFile<CR>
nnoremap <leader>ei :JavaImport<CR>
nnoremap <leader>eo :JavaImportOrganize<CR>
nnoremap <leader>ec :JavaCorrect<CR>

" Window switch
nnoremap <leader>wz :only<CR>
nnoremap <leader>wh :hide<CR>
nnoremap <leader>wv :vsplit<CR>
nnoremap <leader>ws :split<CR>
nnoremap <leader>wo <c-w><c-w>

" Comments
nnoremap <leader>cc :TComment<CR>


