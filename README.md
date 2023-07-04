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

**chapter 6**

```bash
clang++ -g adding-control-flow.cpp ``llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o toy

./toy
```

自定义操作符示例:

```
# Logical unary not.
def unary!(v)
  if v then
    0
  else
    1;

# Unary negate.
def unary-(v)
  0-v;

# Define > with the same precedence as <.
def binary> 10 (LHS RHS)
  RHS < LHS;

# Binary logical or, which does not short circuit.
def binary| 5 (LHS RHS)
  if LHS then
    1
  else if RHS then
    1
  else
    0;

# Binary logical and, which does not short circuit.
def binary& 6 (LHS RHS)
  if !LHS then
    0
  else
    !!RHS;

# Define = with slightly lower precedence than relationals.
def binary = 9 (LHS RHS)
  !(LHS < RHS | LHS > RHS);

# Define ':' for sequencing: as a low-precedence operator that ignores operands
# and just returns the RHS.
def binary : 1 (x y) y;
```

使用自定义操作符

```
# Logical unary not.
ready> !1;
ready> Evaluated to 0.000000
ready> !0;
ready> Evaluated to 1.000000

# Binary logical and
ready> 1 & 2;
ready> Evaluated to 1.000000
ready> 1 & 0;
ready> Evaluated to 0.000000
```

**chapter 7**

```bash
clang++ -g mutable-variables.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core orcjit native` -O3 -o toy

./toy
```

使用变量

```
ready> var a = 1, b = 2 in (a + b);
ready> Evaluated to 3.000000

def binary : 1 (x y) y;

# Iterative fib.
def fibi(x)
  var a = 1, b = 1, c in
  (for i = 3, i < x in
     c = a + b :
     a = b :
     b = c) :
  b;

# Call it.
fibi(10);
```

**chapter 8**

```bash
clang++ -g -O3 toy.cpp `llvm-config --cxxflags --ldflags --system-libs --libs all` -o toy

./toy
```

把在 REPL 输入的代码编译到目标文件

```
./toy
ready> def average(x y) (x + y) * 0.5;
^D
Wrote output.o
```

测试目标文件

```c++
// test.cpp
#include <iostream>

extern "C" {
    double average(double, double);
}

int main() {
    std::cout << "average of 3.0 and 4.0: " << average(3.0, 4.0) << std::endl;
}
```

把 `test.cpp` 链接到 `outpue.o`, 执行查看结果

```bash
clang++ test.cpp output.o -o test

./test

# 输出
Average of 3.0 and 4.0 is 3.5
```
