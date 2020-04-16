虚拟机设计
=========

- [虚拟机设计](#%e8%99%9a%e6%8b%9f%e6%9c%ba%e8%ae%be%e8%ae%a1)
- [1. 虚拟机存储结构](#1-%e8%99%9a%e6%8b%9f%e6%9c%ba%e5%ad%98%e5%82%a8%e7%bb%93%e6%9e%84)
  - [代码空间](#%e4%bb%a3%e7%a0%81%e7%a9%ba%e9%97%b4)
  - [寄存器空间](#%e5%af%84%e5%ad%98%e5%99%a8%e7%a9%ba%e9%97%b4)
  - [栈空间](#%e6%a0%88%e7%a9%ba%e9%97%b4)
- [2. 虚拟机类型系统](#2-%e8%99%9a%e6%8b%9f%e6%9c%ba%e7%b1%bb%e5%9e%8b%e7%b3%bb%e7%bb%9f)
- [3. 中间表达形式](#3-%e4%b8%ad%e9%97%b4%e8%a1%a8%e8%be%be%e5%bd%a2%e5%bc%8f)
  - [类型信息区](#%e7%b1%bb%e5%9e%8b%e4%bf%a1%e6%81%af%e5%8c%ba)
  - [全局符号区](#%e5%85%a8%e5%b1%80%e7%ac%a6%e5%8f%b7%e5%8c%ba)
  - [函数代码区](#%e5%87%bd%e6%95%b0%e4%bb%a3%e7%a0%81%e5%8c%ba)
- [4. 指令集设计](#4-%e6%8c%87%e4%bb%a4%e9%9b%86%e8%ae%be%e8%ae%a1)
  - [字面量](#%e5%ad%97%e9%9d%a2%e9%87%8f)
  - [寄存器](#%e5%af%84%e5%ad%98%e5%99%a8)
  - [赋值相关指令](#%e8%b5%8b%e5%80%bc%e7%9b%b8%e5%85%b3%e6%8c%87%e4%bb%a4)
  - [数据访问指令](#%e6%95%b0%e6%8d%ae%e8%ae%bf%e9%97%ae%e6%8c%87%e4%bb%a4)
  - [基本运算指令](#%e5%9f%ba%e6%9c%ac%e8%bf%90%e7%ae%97%e6%8c%87%e4%bb%a4)
  - [函数定义相关指令](#%e5%87%bd%e6%95%b0%e5%ae%9a%e4%b9%89%e7%9b%b8%e5%85%b3%e6%8c%87%e4%bb%a4)
  - [函数调用相关指令](#%e5%87%bd%e6%95%b0%e8%b0%83%e7%94%a8%e7%9b%b8%e5%85%b3%e6%8c%87%e4%bb%a4)
  - [流程控制指令](#%e6%b5%81%e7%a8%8b%e6%8e%a7%e5%88%b6%e6%8c%87%e4%bb%a4)

Beren程序运行在Beren语言虚拟机上。想要运行一段Beren源代码，需要先将源代码编译成
中间表达形式，然后启动虚拟机执行中间表达形式。中间表达形式由虚拟机指令序列组成，
如同CPU一样，虚拟机每个时钟周期只能完成一种简单任务，每条指令代表的就是虚拟机在
一个周期内能执行的最简单的任务。虚拟机通过逐条执行指令来运行整个程序。

# 1. 虚拟机存储结构

Beren语言虚拟机的存储结构分为3个部分：

- 代码空间
- 寄存器空间
- 栈空间

## 代码空间

代码空间用于存储虚拟机正在执行的指令序列，即源代码经过编译产生的中间表达形式。
程序计数器保存当前执行的指令位置，每执行完一条指令，程序计数器都会指向下一条
需要待执行指令的位置。

## 寄存器空间

寄存器空间用于存储程序运行时产生的数据，包括全局/局部变量、函数接收的实参、返回值等。
寄存器种类分为**特殊寄存器**和**普通寄存器**，每个寄存器只能保存一个数据元，但是
这个数据元可以拥有任意的类型。

特殊寄存器用于特定的用途，数量有限制，且不能通过寄存器名直接访问，只能由指令间接访问，
比如用于保存函数实参的寄存器、返回值寄存器、闭包比昂量寄存器等。特殊寄存器可以多次赋
值，多次读取，其使用方式根据种类的不同有着不同的限制。

普通寄存器分为**静态寄存器**和**局部变量寄存器**，静态寄存器用于存储全局变量，
局部变量寄存器用于存储局部变量，两者除了命名作用域不同之外再无其他区别。
普通寄存器的数量没有限制，每个寄存器只能被赋值一次
（[SSA](https://en.wikipedia.org/wiki/Static_single_assignment_form)），
但可以被任意次数地读取。

## 栈空间

栈空间用于存储执行中函数的活动记录。在发生新的函数调用时，虚拟机会在栈顶开辟新的空间
保存被调用函数的活动记录；在函数调用结束之后，虚拟机会回收栈顶的空间，将控制权交还给
当前栈顶的函数。

# 2. 虚拟机类型系统

Beren虚拟机的类型系统相对于源代码中的类型系统做出了较大的简化。

# 3. 中间表达形式

Beren编译器输出的中间表达形式由**类型信息区**、**全局符号区**和**函数代码区**
三部分组成。类型信息区保存了源代码中类型和中间表达形式中类型的映射；全局符号区
保存了程序中所有全局变量的符号信息；函数代码区则包含了所有的函数代码。

## 类型信息区

类型信息区存储的映射可以帮助虚拟机从指令序列中的简化类型恢复出源代码中的类型信息。
类型信息包含了元组、不定类型、记录等类型的映射。

```
.variant
.name option
  1: None
  .type nil
  2: Some
  .type any

.variant
.name result
  1: Ok
  .type any
  2: Error 
  .type any

.record
.name box
  1: contents
  .type any

...
```

## 全局符号区

全局符号区是一个由变量名组成的集合，每个变量名代表源代码中的一个全局变量。全局符号区
的作用是方便虚拟机在指令序列中区分全局变量对应的静态寄存器和局部变量对应的局部变量寄
存器。

```
.symbols:
  map
  filter
  fold_left
  fold_right
  ...
```

## 函数代码区

函数代码区是所有函数的指令序列组成的集合。每个函数都由其函数签名、函数体等部分构成。

函数签名包含了函数名、函数的参数类型、返回值类型等信息。函数体则是表示其运算过程的
指令序列。

**函数定义示例**

```
.type:
  int -> int -> int
gcd:
  capture $1, %arg
  setenv %inner_fun_gcd
  makefun %val_1
  return %val_1
  
.env:
  a
.type:
  int -> int
%inner_fun_gcd:
  loadint %val_1, 0
  eqint _, %val_1, a
  jmpfalse .L1
  return a
.L1:
  setarg %arg
  call gcd
  getret %val_2
  mod %val_3, a, %arg
  setarg %val_3
  call %val_2
  getret %val_4
  return %val_4
```

# 4. 指令集设计

## 字面量

字面量以`$`起始，可以是布尔型、整型、字符型或字符串型字面量。

## 寄存器

寄存器由小写字母起始或是以`%`起始的标识符表示，寄存器可以存储任意类型的数值。

## 赋值相关指令

字面量赋值

以下尖括号包围的标识符（`<dest>, <src>, <...>`）均表示寄存器。

```
loadunit <dest>
loadbool <dest>, $true
loadint <dest>, $123
loadchar <dest>, $'a'
loadstr <dest>, $"abc"
```

元组赋值

```
loadint <src-1> $123
loadbool <src-2> $false
loadstr <src-3> "abc"

pushpart <src-1>
pushpart <src-2>
pushpart <src-3>

maketup <dest>
```

不定类型赋值

```
load... <some-value> ...

setnum $1
setval <some-value>

makevariant <dest>
```

记录赋值

```
pushfield <src-1>
pushfield <src-2>
makerecord <dest>
```

寄存器赋值

```
move <dest>, <src>
```

引用赋值

```
assign <dest>, <src>
```


## 数据访问指令

解引用

```
deref <dest>, <src>
```

元组

```
tuplepart <dest>, $1, <src>
tuplepart <dest>, $2, <src>
```

不定类型

```
variantnum <dest>, <src>

variantval <dest>, <src>
```

记录

```
recordfield <dest>, $1, <src>
recordfield <dest>, $2, <src>
```


## 基本运算指令

整型运算

```
intadd <dest>, <src-1>, <src-2>
intsub <dest>, <src-1>, <src-2>
...
```

布尔型运算

```
booland <dest>, <src-1>, <src-2>
boolor <dest>, <src-1>, <src-2>
boolnot <dest>, <src>
```

字符串运算

```
strcat <dest>, <src-1>, <src-2>
strlen <dest>, <src>
strpos <dest>, <src>, <pos>
...
```

逻辑运算

```
intlt <dest>, <src-1>, <src-2>
intlte <dest>, <src-1>, <src-2>
inteq <dest>, <src-1>, <src-2>
...
```


## 函数定义相关指令

函数签名

```
.env:
  var_x
.type:
  int -> int
foo:
  ...
```

闭包变量寄存器

```
name
aaa
xyz
%qwe
%clo
```

函数形参

```
getarg <dest>
```

函数返回

```
setret <src>
```

函数构造

```
makefun <fun-name>, .decl
```

闭包构造

```
capture $1, <src-1>
capture $2, <src-2>
makeclos <closure-name>, .decl
```


## 函数调用相关指令

函数实参传入

```
setarg <src>
```

函数返回值获取

```
getret <dest>
```

函数调用

```
setarg <arg>
call <fun>
getret <dest>
```


## 流程控制指令

条件跳转

```
jmptrue .label
jmpfalse .label
```

无条件跳转

```
jmp .label
```

选择操作

```
select <dest>, <src-1>, <src-2>
```