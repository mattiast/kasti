{ pkgs }:
let
  myVim1 = pkgs.neovim.override {
    extraPython3Packages = ps: with ps; [
      jedi
      mypy
      python-language-server
      pyls-isort
      pyls-black
      pyls-mypy
    ]; 
    configure = {
      customRC = ''
        set t_Co=256
        colorscheme inkpot
        let mapleader = ","
        nnoremap <C-e> 3<C-e>
        nnoremap <C-y> 3<C-y>
        nnoremap <silent> <leader>w :w<cr>

        set wildmenu
        set tabstop=4 shiftwidth=4 softtabstop=4 expandtab
        set wildignorecase
        set ignorecase
        set hidden
        set mouse=a
        set relativenumber
        set diffopt=filler,vertical
        set updatetime=100

        let g:neomake_python_enabled_makers = []
        let g:LanguageClient_serverCommands = {
        \ 'python': ['pyls']
        \ }
        highlight ALEWarning ctermfg=Red ctermbg=Yellow
        highlight ALEError ctermfg=White ctermbg=Red

        nnoremap <silent> <leader>cs :Gstatus<cr>
        nnoremap <silent> <leader>cd :Gdiff<cr>
        nnoremap <silent> <leader>cc :Gcommit<cr>
        vnoremap <leader>cg y:Ggrep <C-R>"
        nnoremap <leader>cg yiw:Ggrep "\<<C-R>"\>"
        nnoremap <leader>cv :copen<cr>

        vnoremap <leader>s "*y
        nnoremap <leader>p "*p
        vnoremap <leader>S "+y
        nnoremap <leader>P "+p
        nnoremap <leader>C :%s///gn<cr>

        nnoremap <silent> <leader>mm :CtrlPMRUFiles<cr>
        nnoremap <silent> <leader>mf :CtrlP<cr>
        nnoremap <silent> <leader>mt :CtrlPTag<cr>
        nnoremap <silent> <leader>mr :CtrlPBufTagAll<cr>
        nnoremap <silent> <leader>a :CtrlPBuffer<cr>
      '';
      packages.myVimPackage = with pkgs.vimPlugins; {
        start = [
          vim-fugitive
          vim-nix
          vim-unimpaired
          gitgutter
          colorsamplerpack
          fzf-vim
          ctrlp
          LanguageClient-neovim
          jedi-vim
          airline
          elm-vim
          neomake
          haskell-vim
        ];
      };     
    };
  };
in myVim1
