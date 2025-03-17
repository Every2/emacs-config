# Emacs config

Minha configuração minimalista do Emacs

# LSP

Primeiramente, baixe o lsp booster. É uma dependencia necessária pro Eglot booster.


```
cargo install emacs-lsp-booster
```


Após isso, abra o emacs e rode `M-x package-vc-install` e coloque a url do repositório do eglot booster `https://github.com/jdtsmith/eglot-booster.git`.


Agora deve funcionar. :) 

# Treesit


Eu uso o treesitter nativo do Emacs. Todos os modos que eu uso (c, cpp, rust, python, bash e toml) estão vinculados no arquivo, mas você precisa baixar manualmente rodando `M-x treesit-install-language-grammar` e colocar os respectivos nomes.

# Fontes


Basta rodar `M-x nerd-icons-install-fonts`

# Debug


Garanta que seu gdb esteja na versão 14 em diante para o `DAPE` funcionar.

# Rust

Como uso apenas o `rust-ts-mode`, ficam faltando algumas opções. Por fora eu uso o [cargo-expand](https://github.com/dtolnay/cargo-expand) para ver as macros.


Garanta que você tenha o rust-analyzer instalado corretamente:

```
rustup component add rust-analyzer
```

# C++

Para C++ garanta que você tenha a libstdc++ mais recente e o clangd instalado. 


Debian/Ubuntu:

```
apt-cache show libstdc++ | grep "dev"
```

Arch Linux:

```
sudo pacman -S clang //instala o clangd
```

# Python

Para python basta adicionar o lsp, como eu uso Arch, essas são as instruções:

```
sudo pacman -S python-lsp-server python-pycodestyle python-pydocstyle python-pyflakes python-pylint python-rope flake8 yapf python-whatthepatch
```

# Vterm

Para conseguir compilar o vterm instale:

```
sudo pacman -S cmake libvterm libtool
```

