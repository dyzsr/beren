# 语言介绍

- [语言介绍](#%e8%af%ad%e8%a8%80%e4%bb%8b%e7%bb%8d)
- [1. 程序结构](#1-%e7%a8%8b%e5%ba%8f%e7%bb%93%e6%9e%84)
  - [类型声明](#%e7%b1%bb%e5%9e%8b%e5%a3%b0%e6%98%8e)
  - [数值声明](#%e6%95%b0%e5%80%bc%e5%a3%b0%e6%98%8e)
  - [方法声明](#%e6%96%b9%e6%b3%95%e5%a3%b0%e6%98%8e)
- [2. 类型系统](#2-%e7%b1%bb%e5%9e%8b%e7%b3%bb%e7%bb%9f)
  - [类型声明](#%e7%b1%bb%e5%9e%8b%e5%a3%b0%e6%98%8e-1)
  - [类型表达式](#%e7%b1%bb%e5%9e%8b%e8%a1%a8%e8%be%be%e5%bc%8f)
  - [不定类型](#%e4%b8%8d%e5%ae%9a%e7%b1%bb%e5%9e%8b)
  - [记录类型](#%e8%ae%b0%e5%bd%95%e7%b1%bb%e5%9e%8b)
  - [接口类型](#%e6%8e%a5%e5%8f%a3%e7%b1%bb%e5%9e%8b)
- [3. 表达式](#3-%e8%a1%a8%e8%be%be%e5%bc%8f)
  - [基础类型表达式](#%e5%9f%ba%e7%a1%80%e7%b1%bb%e5%9e%8b%e8%a1%a8%e8%be%be%e5%bc%8f)
  - [复合类型表达式](#%e5%a4%8d%e5%90%88%e7%b1%bb%e5%9e%8b%e8%a1%a8%e8%be%be%e5%bc%8f)
  - [局部变量声明](#%e5%b1%80%e9%83%a8%e5%8f%98%e9%87%8f%e5%a3%b0%e6%98%8e)
  - [函数调用](#%e5%87%bd%e6%95%b0%e8%b0%83%e7%94%a8)
  - [条件表达式](#%e6%9d%a1%e4%bb%b6%e8%a1%a8%e8%be%be%e5%bc%8f)
  - [函数声明](#%e5%87%bd%e6%95%b0%e5%a3%b0%e6%98%8e)
  - [Lambda表达式](#lambda%e8%a1%a8%e8%be%be%e5%bc%8f)
  - [模式匹配表达式](#%e6%a8%a1%e5%bc%8f%e5%8c%b9%e9%85%8d%e8%a1%a8%e8%be%be%e5%bc%8f)
- [4. 接口与方法](#4-%e6%8e%a5%e5%8f%a3%e4%b8%8e%e6%96%b9%e6%b3%95)
- [5. 参数多态泛型](#5-%e5%8f%82%e6%95%b0%e5%a4%9a%e6%80%81%e6%b3%9b%e5%9e%8b)
- [6. 模式匹配](#6-%e6%a8%a1%e5%bc%8f%e5%8c%b9%e9%85%8d)

Beren是一门基于ML语言家族，融合Go语言中接口特性的一门通用编程语言，
自我定位是一种适用于教学活动的编程语言。[项目介绍](./project.md)

# 1. 程序结构

Beren识别以`.brn`作为文件后缀名的程序代码。

每个程序代码都由数段声明组成，可以是类型声明、数值声明或方法声明，均为全局声明。
声明遵循词法顺序，声明在先的符号会被先放入符号表。位置在前的声明中无法引用后面声明
的符号。声明方式分为递归声明和非递归声明两种，递归声明会在处理声明结构之前就将名字
放入符号表，方便处理过程中的递归引用；而非递归声明只在处理完声明结构之后才将名字放
入符号表。

## 类型声明

通过类型声明可以引入现有类型的别名，或是利用现有的类型构造新的类型。

类型声明默认采用递归声明的方式。类型声明的详细介绍可见类型系统部分。

``` ml
type t = int
type t2 = string

type t3 = int * bool
type t4 = char * string

type t5 =
  | Nil
  | Cons of int * t5

type 'a t6 =
  | Nil
  | Cons of 'a * 'a t6

type t7 = int -> int -> bool

type t8 = { a : int; b : string }
```

## 数值声明

数值声明用来定义全局变量，可以用来声明函数，或是其他类型的变量。

数值声明默认采用非递归声明，递归声明则要在`let`关键字之后加上`rec`。

``` ml
let b = true
let i = 123
let c = #'a'
let s = "abc"

let () = print_endline "hello world"

let f x y = x + y

let rec fact = function
  | 0 => 1
  | n => n * fact (n-1)
```

## 方法声明

方法声明为现有类型绑定方法。对类型`t`绑定方法`f`，在后文中可以对类型`t`的实例`a`，
用`a->f`的形式调用方法`f`。

``` ml
type t =
  { a : int ref
  ; b : string
  }

method (x : t) get_a () = !(t.a)
method (x : t) set_a x = t.a := x

method (x : t) get_b () = t.b
```

# 2. 类型系统

Beren的类型系统基于ML，另外加入了类似Go语言中的接口类型。目前支持的类型构造子有：

- 基础类型：`unit`, `bool`, `int`, `char`, `string`
- 积类型：元组`tuple`，记录`record`
- 和类型/不定类型`variants`
- 泛化类型：接口`interface`，参数多态泛型`generics`
- 函数类型`function`

## 类型声明

类型声明形如
```
type [<type-vars>] <name> = <type-construct> [and <name> = <type-expr>] ...
```

其中类型名`name`必须以小写英文字母开头，类型构造体`type-construct`可以是：
类型表达式、不定类型声明、记录声明、接口声明，默认支持递归声明。

如果指定了类型变量`type-vars`，那么构造的类型将是一个参数多态泛型。

## 类型表达式

类型表达式用来构造类型别名、元组、函数、或实例化的泛型。

``` ml
(* 类型别名 *)
type number = int
type text = string

(* 元组类型 *)
type int_pair = int * string

(* 参数多态泛型 *)
type ('a, 'b) pair = 'a * 'b

(* 函数类型 *)
type fn_int_to_string = int -> string
type ('a, 'b) fn_a_to_b = 'a -> 'b

(* 实例化泛型 *)
type int_list = int list
type int_option = int option
```

## 不定类型

不定类型`variants`又称“和类型”，这种类型的变量可以存储限定范围内的多种不同类型的数据，
但在程序运行某一特定时刻，只能存储一种确定类型的数据。不定类型的每一种变体`variant`
都拥有一个名字，这些名字也叫构造函数`value constructor`，用来构造这种不定类型的实例。

``` ml
(* 声明一个不定类型int_or_string，可以存储一个整型或字符串
 * Int和String均为构造函数，用于构造int_or_string类型变量 *)
type int_or_string =
  | Int of int
  | String of string

let print_int_or_string = function
  | Int i => print_int i
  | String s => print_string s

let () =
  let a = Int 123 in
  let b = String "abc" in
  let () = print_int_or_string a in
  let () = print_int_or_string b in
  ()

type 'a option =
  | None
  | Some of 'a

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b
```

## 记录类型

记录类型用于聚合多种零散的数据，和元组的功能相似，属于一种“积类型”。

元组按指定好的顺序聚合数据，使用元组时，需要按照元组定义时的顺序才能获取到正确的数据。
记录则用名字绑定数据。记录类型可以拥有多个值域，每个值域都拥有一个名字。使用记录类型
数据时，可以通过值域名来访问想要的数据。

``` ml
type 'a box = { contents : 'a }

(* 声明一个记录类型item *)
type item =
  { name : string
  ; price : int
  }

(* 定义一个item，然后输出其内容 *)
let () =
  let item = { name="abc"; price=123; } in
  let text = item.name ^ string_of_int item.price in
  print_endline text
```

## 接口类型

Been中接口类型类似与Go语言中的接口，是一种类型泛化的方式。接口中指定了一组方法，
所有这个接口的实例都必须实现这些指定的方法。

``` ml
(* 声明一个接口，要求实现print方法 *)
type printable =
  interface
    print : string -> unit
  end

(* 对字符串类型实现print方法 *)
method (s : string) print text =
  print_endline (s ^ ": " ^ text)

(* 调用接口 *)
let print_hello (printer : printable) =
  printer.print "hello"

let () =
  let s = "someone" in
  print_hello s
```

# 3. 表达式

Beren语言中，let声明用于定义全局变量。


一般的变量声明结构形如：
```
let [rec] <pattern> = <expr> [and <pattern> = <expr>] ...
```

函数的声明形如：
```
let [rec] <name> <pattern> [<pattern> ...] = <expr> [and ...] ...
```

let声明中，等号左端的pattern称作模式，最常见的模式就是一个单独的变量名。
等号右端则是表达式，Beren语言中所有的表达式都拥有自己的类型。

## 基础类型表达式

``` ml
(* 最简单的单元类型，相当于C语言中的void类型 *)
()

(* 整型 *)
123
456

(* 布尔型 *)
true
false

(* 字符型 *)
#'a'
#'c'

(* 字符串类型 *)
"abc"
"def"
```

## 复合类型表达式

``` ml
(* 元组 *)
(1, 2)
(#'c', "haha")

(* 列表 *)
[1, 2, 3, 4]

(* 数组 *)
[|1, 2, 3, 4|]

(* 不定类型 *)
None
Some 123

(* 记录类型 *)
{ a=11; b="yes"; c=true }

```

## 局部变量声明

可以通过`let in`语法引入局部变量，形如

```
let [rec] <pattern> = <expr> in <local_expr>
```

`let`部分语法与全局数值声明的语法一致，区别在于局部声明中`in`部分表示前面声明的变量
只能作用在`in`之后的局部表达式中。

``` ml
(* 声明一个变量a，然后返回a *)
let a = (123, 456) in a

let n = gcd 64 48 in n
```

## 函数调用

``` ml
(* 调用函数f，参数为a和b *)
f a b

(* 假设add3是一个将三个数字相加的函数
 * 保存a b c的加和，然后输出这个值 *)
let n = add3 a b c in print_int n
```

## 条件表达式

条件表达式形如：
```
if <condition> then <expr>

if <condition> then <expr-1> else <expr-2>
```

``` ml
if a < b then a else b

if yes then
  print "yes"
```

## 函数声明

``` ml
(* 最大公约数函数gcd *)
let rec gcd a b =
  if b = 0 then a
  else gcd b (a % b)

(* 阶乘函数 *)
let fact n =
  if n < 0 then 1 else
  let rec iter result = function
  | 0 => result
  | n => iter (n * result) (n - 1)
  in
  iter 1 n
```

## Lambda表达式

``` ml
let f = fun x y -> x + y

(* 以模式匹配的形式定义函数 *)
let rec map f = function
  | [] => []
  | h :: t => f h :: map f t
```

## 模式匹配表达式

模式匹配表达式形如
```
match <expr> with
[ | ] <pattern> => <body>
[ | <pattern> => <body> ]
...
```

``` ocaml
let a = None in
let b = Some 123 in
match (a, b) with
| None, None => "both none"
| None, Some b => "some b"
| Some a, None => "some a"
| Some a, Some b => "some a & b"
```

# 4. 接口与方法

接口继承是一种常见的类型泛化方式，主要存在于不允许类多重继承的面向对象程序语言中。
接口泛化方式是一种针对行为的抽象，通过指定一组方法，来表示所有实现了这些方法的
数据类型。

大多数的拥有接口的语言中，都要求一个接口的自类型必须显式地声明继承该接口，并且实现
接口所规定的方法。而在Go语言中，接口使用了“鸭子类型”继承方式，即所有实现了接口指定
方法的类型都会隐式地继承该接口，即所有子类型都可以自由地存储在接口类型的变量中。

Go语言中最常用的一个接口是`error`类型，任何实现了签名相同的`Error`方法的类型都可以
作为`error`类型使用。

``` go
// error接口
type error interface {
  Error() string
}

// 定义一个实现了Error方法的类型
type MyError struct{}
func (MyError) Error() string { return "my error" }

// 将MyError实例赋给一个error类型接口变量
var myErr error = MyError{}
```

Beren中的使用方式类似于Go语言，采取隐式接口继承的方式，任何实现了接口指定方式的类型
都可以作为该接口的实例。

``` ocaml
(* error类型接口 *)
type error =
  interface
    error_message : unit -> string
  end

(* 声明接口 *)
type handler =
  interface
    handle : context -> (unit, error) result
  end

(* 声明具体类型 *)
type my_handler =
  { name : string
  ; content : string
  }

and my_error = string

(* 绑定接口方法 *)
method (h : my_handler) handle context =
  if ... then
    Ok ()
  else
    Error "my handle error"

and (s : my_error) error_message () = s
```

# 5. 参数多态泛型

通常意义上的泛型`generics`即为参数多态泛型，通过从一种具体的语法结构（类型声明、
函数声明）中消去共同的类型因子来提供类型泛化功能。消去类型因子之后，得到的语法结构
便可作用于各种各样的类型上，而不是只局限在某一特定类型。泛型于接口的最大区别是：
泛型的类型检查完全发生在编译阶段，而接口的类型检查有很大一部分是运行期执行的，具有
较多的动态特性。

``` ocaml
(* 泛型记录 *)
type ('a, 'b, 'c) triple =
  { a : 'a
  ; b : 'b
  ; c : 'c
  }

(* 泛型函数: 'a -> 'a -> bool *)
let f a b =
  a < b
```

# 6. 模式匹配

模式匹配类似于许多语言中的`switch-case`语句，可以进行多分支条件判断，起到类似条件
表达式的流程控制作用。

``` ocaml
match <expr> with
[ | ] <pattern> => <body>
[ | <pattern> => <body> ]
...
```

模式匹配不仅可以用在多路分支选择之处，也可以用在变量声明的地方，还有函数的参数位置。

``` ocaml
let <pattern> = <expr>

fun <pattern> ... => <body>

function
[ | ] <pattern> => <body>
[ | <pattern> => <body> ]
...
```

模式匹配相比于条件表达式功能更强大，以一种看似在构造数据的方式去解构数据。这里的模式
指的是某种数据解构，可以是单独的变量、元组、不定类型的某一变体、某种记录结构、字面量，
又或是这些结构组成的复合结构。模式匹配的是具体的数据，如果给定数据的结构和给定模式的
结构相吻合，则达成匹配。

``` ocaml
(* 利用模式匹配构建函数，length接收的第一个参数是待匹配的数据
 * 有两个分支，一个代表空列表结构，另一个代表表中至少有一个元素，
 * 函数将执行能与给定数据匹配的分支 *)
let rec length = function
 | [] => 0
 | h :: t => 1 + length t

let v = true
let (a, b) = (123, "def")
let Ok v | Error v = Ok "yes"
let { a=a; b=b } = { a=123; b=456 }
```