" Disable plugin to auto import loggers. Very irritating.!
let g:EclimLoggingDisabled = 1

set nocompatible
if !has('nvim')
  set encoding=utf-8
endif
" autocompletion
set wildmode=longest,list,full
set wildmenu
set backspace=indent,eol,start

set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 9

"Leave some context lines above and below cursor
set scrolloff=5
set mousemodel=extend
" Use X clipboard
if !has('nvim')
  set clipboard=unnamed
  set clipboard+=unnamed
endif

" Make switch from insert to normal mode more responsive.
set noesckeys
set timeoutlen=1000 ttimeoutlen=0

" Use 256 colors in term
set t_Co=256
" Display status line always
set laststatus=2

set nocompatible
set autoindent
set cindent
set smartindent
set ruler
set hidden
set confirm
set ignorecase
set smartcase
set noshowmode
set number
" Use <leader>cor from unimpaired
" set relativenumber
set tabstop=2 shiftwidth=2 expandtab
set conceallevel=0
" Always display status line`
set laststatus=4
set wrap linebreak nolist
set foldmethod=indent
set foldnestmax=10
set nofoldenable
set foldlevel=4
set hlsearch
set incsearch
set sessionoptions=buffers,blank,curdir,folds,tabpages

set guioptions-=m  " remove menu bar
set guioptions-=T  " remove toolbar
set guioptions-=r  " remove right-hand scroll bar
set guioptions-=L  " remove left-hand scroll bar
set guioptions+=c  " use console confirmation dialogs

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
set noswapfile

set background=dark

" Show margin at 81 columns by default
set colorcolumn=81

" Java files can have 100 characters per line
autocmd bufreadpre *.java setlocal colorcolumn=101

" Start scrolling before we reach the edges of the editing window
set scrolloff=13

" Don't wrap lines
set nowrap

" Allow mouse usage in normal mode
set mouse=n

if !has('nvim')
  " Option has been removed in nvim.
  " Set xterm mouse mode to allow resizing of splits with mouse inside Tmux
  set ttymouse=xterm2
endif

" Use spaces instead of tabs
" set expandtab

" Make trailing whitespace annoyingly highlighted
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" Open splits in a more intuitive way
set splitbelow
set splitright


" Jump to matches when entering regexp
set showmatch

" Eclim disables all the syntastic plugins. We need to enable it for every
" language we want manually.
let g:EclimPythonValidate = 0
let g:EclimJavascriptValidate = 0

" YCM and snippets should play good together.
" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

let g:EclimCompletionMethod = 'omnifunc'

let g:TerminusBracketedPaste = 0

" close vim if the only buffer left is NERDTree
let g:NERDTreeDirArrows = 1
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let g:session_autoload = 'no'
let g:session_autosave = 'yes'
let g:solarized_termcolors=256

" Unite
let g:unite_source_history_yank_enable = 1

" This will be turned back on after loading all the plugins.
filetype off
filetype plugin indent off

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
" Custom switches between text.
Plugin 'AndrewRadev/switch.vim'
" Plugin 'benekastah/neomake'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'jiangmiao/auto-pairs'
Plugin 'majutsushi/tagbar'
Plugin 'scrooloose/nerdTree'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'
" Plugin 'vim-airline/vim-airline'
" Plugin 'vim-airline/vim-airline-themes'
Plugin 'scrooloose/nerdcommenter'

Plugin 'moll/vim-node'
Plugin 'burnettk/vim-angular'

" Solarized color theme
Plugin 'altercation/vim-colors-solarized'
Plugin 'morhetz/gruvbox'

Plugin 'Shougo/unite.vim'
Plugin 'Shougo/vimproc.vim'  " configure this
Plugin 'ervandew/supertab'

" Snippets
Plugin 'vim-scripts/SyntaxComplete'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

Plugin 'w0rp/ale'
Plugin 'nvie/vim-flake8'

Plugin 'wincent/terminus'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'tpope/vim-pathogen'

" Same as git gutter but has support for perforce.
Plugin 'mhinz/vim-signify'

Plugin 'maralla/completor.vim'

Plugin 'dhruvasagar/vim-table-mode'
Plugin 'beautify-web/js-beautify'
Plugin 'leafgarland/typescript-vim'

Plugin 'google/vim-maktaba'
Plugin 'google/vim-codefmt'
" Also add Glaive, which is used to configure codefmt's maktaba flags. See
Plugin 'google/vim-glaive'

"Plugin 'Chiel92/vim-autoformat'

" All of your Plugins must be added before the following line
call vundle#end()            " required

filetype on
filetype plugin indent on

" Vim syntax based color highlighting, Preferably after pathogen.
syntax enable

colorscheme gruvbox
nnoremap <leader>bg :let &background = ( &background == "dark"? "light" : "dark"  )<CR>

autocmd FileType python map <buffer> <F3> :call Flake8()<CR>

"let g:airline#extensions#tabline#enabled = 1
" Instead of installing the powerline fonts, I am using my own symbols from
" unicode.
"let g:airline_left_sep = '»'
"let g:airline_left_sep = '▶'
"let g:airline_right_sep = '«'
"let g:airline_right_sep = '◀'
"let g:airline_theme="term"

" Autoformat on write (vim-autoformat)
" au BufWrite * :Autoformat

" Google formatters
augroup autoformat_settings
  autocmd FileType bzl AutoFormatBuffer buildifier
  autocmd FileType c,cpp,proto,javascript AutoFormatBuffer clang-format
  autocmd FileType dart AutoFormatBuffer dartfmt
  autocmd FileType go AutoFormatBuffer gofmt
  autocmd FileType gn AutoFormatBuffer gn
  autocmd FileType html,css,sass,scss,less,json AutoFormatBuffer js-beautify
  autocmd FileType java AutoFormatBuffer google-java-format
  autocmd FileType python AutoFormatBuffer yapf
  " Alternative: autocmd FileType python AutoFormatBuffer autopep8
  autocmd FileType rust AutoFormatBuffer rustfmt
  autocmd FileType vue AutoFormatBuffer prettier
augroup END

" Basic comman shortcuts
" Note that this is not the leader key mapping.
" Leader key is <Space> with \ as backup.
nnoremap ,w :w<cr>


" Avoid the Esc key
inoremap jk <Esc>

" Auto indent after paste.
nnoremap p p`[v`]

nnoremap <leader>bd :bd<CR>
nnoremap <leader>bc :b:

" Start editing file in the same folder
nnoremap <leader>el :e <C-R>=expand('%:p:h') . '/'<CR>

" Easymotion search start
" ,,s

" NERDTree mappings
noremap <leader>nf :NERDTreeFind<CR>
noremap <leader>nt :NERDTreeToggle<CR>
noremap <leader>tt :TagbarToggle<CR>

" Eclim
nnoremap <leader>ef :LocateFile<CR>
nnoremap <leader>ji :JavaImport<CR>
nnoremap <leader>jc :JavaCorrect<CR>

" Window switch
nnoremap <leader>wz :only<CR>
nnoremap <leader>wh :hide<CR>
nnoremap <leader>wv :vsplit<CR>
nnoremap <leader>ws :split<CR>
nnoremap <leader>wo <c-w><c-w>
nnoremap <leader>wH <c-w><c-w>:hide<cr>
" Use ctrl + <movement key> to navigate splits
nmap <silent> <C-h> :wincmd h<CR>
nmap <silent> <C-j> :wincmd j<CR>
nmap <silent> <C-k> :wincmd k<CR>
nmap <silent> <C-l> :wincmd l<CR>

" Unite
" call unite#filters#matcher_default#use(['matcher_fuzzy'])
" call unite#filters#sorter_default#use(['sorter_rank'])

nnoremap <leader>fa :<C-u>Unite -buffer-name=files   -start-insert file_rec/async:!<cr>
nnoremap <leader>ff :<C-u>Unite -buffer-name=files   -start-insert file<cr>
nnoremap <leader>fr :<C-u>Unite -buffer-name=mru     -start-insert file_mru<cr>
nnoremap <leader>hu :<C-u>Unite -buffer-name=unite_history -start-insert history/unite<cr>
nnoremap <c-e> :<C-u>Unite -buffer-name=command -start-insert command<cr>
nnoremap <c-b> :<C-u>Unite -buffer-name=buffer -start-insert buffer<cr>
nnoremap <leader>fF :<C-u>Unite -buffer-name=function -start-insert function<cr>
nnoremap <leader>ubb :Unite -buffer-name=bookmark bookmark -start-insert<cr>
nnoremap <leader>uba :UniteBookmarkAdd <cr><cr><cr>
nnoremap <leader>fp  :YRShow<cr>

" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  " Play nice with supertab
  let b:SuperTabDisabled=1
  " Enable navigation with control-j and control-k in insert mode
  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
  nmap <buffer> <Esc>   <Plug>(unite_exit)
  imap <buffer><silent><expr> <C-v> unite#do_action('vsplit')
  imap <buffer><silent><expr> <C-s> unite#do_action('split')
endfunction

autocmd FileType c  nnoremap <silent> <buffer> <cr> :CSearchContext<cr>
autocmd FileType java  nnoremap <silent> <buffer> <cr> :JavaSearchContext<cr>

" Remember my last position in file.
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

function! OpenSessionAndEclim(client_name)
  execute "OpenSession ".a:client_name
  execute "ProjectOpen ".a:client_name."-magicjar"
  execute ":e BUILD"
  execute ":PiperLoadActiveAsBuffers"
  execute ":bd ^BUILD"
endfunction
com! -nargs=1 OpenSessionAndEclim call OpenSessionAndEclim(<f-args>)

function! CloseSessionAndEclim()
  let current_session_name = xolox#session#find_current_session()
  execute "SaveSession"
  execute "CloseSession"
  execute ":%bd"
  execute "ProjectClose ".current_session_name."-magicjar"
endfunction
com! CloseSessionAndEclim call CloseSessionAndEclim()

function! SwitchSessionAndEclim(client_name)
  execute ":call CloseSessionAndEclim()"
  execute ":call OpenSessionAndEclim('".a:client_name."')"
endfunction
com! -nargs=1 SwitchSessionAndEclim call SwitchSessionAndEclim(<f-args>)

" Delete the buffer; keep windows; create a scratch buffer if no buffers left
" Essentially closes a buffer
function! s:Kwbd(kwbdStage)
  if(a:kwbdStage == 1)
    if(!buflisted(winbufnr(0)))
      bd!
      return
    endif
    let s:kwbdBufNum = bufnr("%")
    let s:kwbdWinNum = winnr()
    windo call s:Kwbd(2)
    execute s:kwbdWinNum . 'wincmd w'
    let s:buflistedLeft = 0
    let s:bufFinalJump = 0
    let l:nBufs = bufnr("$")
    let l:i = 1
    while(l:i <= l:nBufs)
      if(l:i != s:kwbdBufNum)
        if(buflisted(l:i))
          let s:buflistedLeft = s:buflistedLeft + 1
        else
          if(bufexists(l:i) && !strlen(bufname(l:i)) && !s:bufFinalJump)
            let s:bufFinalJump = l:i
          endif
        endif
      endif
      let l:i = l:i + 1
    endwhile
    if(!s:buflistedLeft)
      if(s:bufFinalJump)
        windo if(buflisted(winbufnr(0))) | execute "b! " . s:bufFinalJump | endif
    else
      enew
      let l:newBuf = bufnr("%")
      windo if(buflisted(winbufnr(0))) | execute "b! " . l:newBuf | endif
  endif
  execute s:kwbdWinNum . 'wincmd w'
endif
if(buflisted(s:kwbdBufNum) || s:kwbdBufNum == bufnr("%"))
  execute "bd! " . s:kwbdBufNum
endif
if(!s:buflistedLeft)
  set buflisted
  set bufhidden=delete
  set buftype=
  setlocal noswapfile
endif
  else
    if(bufnr("%") == s:kwbdBufNum)
      let prevbufvar = bufnr("#")
      if(prevbufvar > 0 && buflisted(prevbufvar) && prevbufvar != s:kwbdBufNum)
        b #
      else
        bn
      endif
    endif
  endif
endfunction

command! Kwbd call s:Kwbd(1)
nnoremap <leader>bc :Kwbd<cr>

" Quick redraw needed when in ssh sessions.
nnoremap <leader>rr :redraw!<cr>

" Highlight all instances of word under cursor, when idle.
" Useful when studying strange source code.
" Type z/ to toggle highlighting on/off.
nnoremap <leader>th :if AutoHighlightToggle()<Bar>set hls<Bar>endif<CR>
function! AutoHighlightToggle()
  let @/ = ''
  if exists('#auto_highlight')
    au! auto_highlight
    augroup! auto_highlight
    setl updatetime=4000
    echo 'Highlight current word: off'
    return 0
  augroup END
else
  augroup auto_highlight
    au!
    au CursorHold * let @/ = '\V\<'.escape(expand('<cword>'), '\').'\>'
  augroup end
  setl updatetime=2000
  echo 'Highlight current word: ON'
  return 1
endif
endfunction

nnoremap <leader>So :OpenSession asq1<cr>
nnoremap <leader>Ss :SaveSession<cr>

function! ProfileStart()
  execute ":profile start /tmp/profile.log"
  execute ":profile func *"
  execute ":profile file *"
endfunction

function! s:Scratch (command, ...)
  redir => lines
  let saveMore = &more
  set nomore
  execute a:command
  redir END
  let &more = saveMore
  call feedkeys("\<cr>")
  new | setlocal buftype=nofile bufhidden=hide nos
endfunction
