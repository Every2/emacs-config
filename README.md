# Dot files

My dotfiles with some tutorials to remember in the future

# Theme

Kanagawa theme is broken, so I downloaded the .el file in the repo and put in ,emacs.d/ folder together with init.el and just load the theme.


# C/C++ LSP

Disclaimer: Currently I'm using arch linux and this step isn't nedded!!

To run the C/C++ lsp is needed the latest version of libstdc/libstdc++(version here) and libstdc/libstdc++(version here)-devel.

In order to achieve this, you can search in your OS. I use openSuse so it's something like:

```
rpm -qa | grep c++  
```

And see which version is installed and try find a newer one if nothings works

```
ls /usr/include/c++/  
//13 14 15
```

# Rust LSP

To Rust lsp works just

```
rustup component add rust-analyzer
```

# DAP

Don't forget to use these commands to dap works in emacs

```
dap-cpptols-setup
dap-codelldb-setup
```

# Elixir lexical

Install lexical


``` 
git clone git@github.com:lexical-lsp/lexical.git
cd lexical
mix deps.get
mix package
``` 

and pass the path in `~/lexical/_build/dev/package/lexical/bin/file.sh` to init.el


# Latex

Install tex-live before and run
```
M-x list-packages and find auctex or M-X auctex
```

