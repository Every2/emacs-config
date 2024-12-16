# Emacs config

My emacs config with some reminders


# C/C++ LSP

Disclaimer: Currently I'm using arch linux and this step isn't nedded!!


Disclaimer2: In Ubuntu 24.04 isn't necessary too.

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


# Latex

Install tex-live before and run
```
M-x list-packages and find auctex or M-X auctex
```

# Vterm

Vterm is used with GUIX package manager. Mention in init.el isn't necessary anymore

