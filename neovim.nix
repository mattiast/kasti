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
      customRC = pkgs.lib.readFile ./neovimrc.vim;
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
