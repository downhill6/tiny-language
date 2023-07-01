# tiny-language
Learn [My First Language Frontend with LLVM Tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html)


## 编译参数

**chapter 2**

``` bash
clang++ -g -O3 implementing-a-parser-and-ast.cpp `llvm-config --cxxflags`

./a.out
```

**chapter 3**

```bash
clang++ -g -O3 code-generation-to-llvm.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core` -o toy

./toy
```

**chapter 4**

```bash
clang++ -g adding-jit-and-optimizer-support.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o toy

./toy
```

**chapter 5**

```bash
clang++ -g adding-control-flow.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o toy

./toy
```