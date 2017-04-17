set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin('~/.vim/plugin')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'godlygeek/tabular'
Plugin 'plasticboy/vim-markdown'
Plugin 'mikewest/vimroom'
Plugin 'achalddave/vim-pandoc'
"Bundle 'mikewest/vimroom'
"Bundle 'achalddave/vim-pandoc'

call vundle#end()            " required
filetype plugin indent on    " required

" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ

" This line should not be removed as it ensures that various options are
" properly set to work with the Vim-related packages available in Debian.
runtime! debian.vim

" Terminal with 256 colors
set t_Co=256

" Set colorscheme
"let g:zenburn_transparent = 1
let g:zenburn_high_Contrast = 1
colors zenburn

" Vim5 and later versions support syntax highlighting. Uncommenting the next
" line enables syntax highlighting by default.
syntax on

" If using a dark background within the editing area and syntax highlighting
" turn on this option as well
set background=dark

" Uncomment the following to have Vim load indentation rules and plugins
" according to the detected filetype.
if has("autocmd")
  filetype plugin indent on
endif

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
"set showcmd		" Show (partial) command in status line.
set showmatch		" Show matching brackets.
"set ignorecase		" Do case insensitive matching
"set smartcase		" Do smart case matching
"set incsearch		" Incremental search
"set autowrite		" Automatically save before commands like :next and :make
"set hidden             " Hide buffers when they are abandoned
"set mouse=a		" Enable mouse usage (all modes)

" Source a global configuration file if available
"if filereadable("/etc/vim/vimrc.local")
"  source /etc/vim/vimrc.local
"endif

set expandtab
set shiftwidth=2
set softtabstop=2
set shiftwidth=2
set tabstop=2
"set spell spelllang=en_gb

" Add spell check and automatic wrapping at 72 columns to commit message at git
autocmd FileType gitcommit setlocal spell spelllang=en_gb textwidth=72

" Markdown
" --------
"  Autodetect
autocmd BufNewFile,BufFilePre,BufRead *.{md,markdown,mkd} set filetype=markdown
" Disable folding
let g:vim_markdown_folding_disabled=1
" LaTeX math
let g:vim_markdown_math=1

" Terminal with 256 colors
set t_Co=256


