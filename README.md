# .dotfiles
My dotfiles

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

To Rust lsp works just

```
rustup component add rust-analyzer
```
