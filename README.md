一种基于“不定类型”的编程语言设计与实现
===========

- [一种基于“不定类型”的编程语言设计与实现](#%e4%b8%80%e7%a7%8d%e5%9f%ba%e4%ba%8e%e4%b8%8d%e5%ae%9a%e7%b1%bb%e5%9e%8b%e7%9a%84%e7%bc%96%e7%a8%8b%e8%af%ad%e8%a8%80%e8%ae%be%e8%ae%a1%e4%b8%8e%e5%ae%9e%e7%8e%b0)
  - [1.前期准备](#1%e5%89%8d%e6%9c%9f%e5%87%86%e5%a4%87)
    - [未完成的Léa解释器](#%e6%9c%aa%e5%ae%8c%e6%88%90%e7%9a%84l%c3%a9a%e8%a7%a3%e9%87%8a%e5%99%a8)
    - [MyLisp解释器](#mylisp%e8%a7%a3%e9%87%8a%e5%99%a8)
    - [Beren解释器](#beren%e8%a7%a3%e9%87%8a%e5%99%a8)
  - [2.思路来源](#2%e6%80%9d%e8%b7%af%e6%9d%a5%e6%ba%90)
    - [路经一](#%e8%b7%af%e7%bb%8f%e4%b8%80)
    - [路径二](#%e8%b7%af%e5%be%84%e4%ba%8c)
  - [3.语法设计](#3%e8%af%ad%e6%b3%95%e8%ae%be%e8%ae%a1)
    - [语法入口](#%e8%af%ad%e6%b3%95%e5%85%a5%e5%8f%a3)
    - [变量声明](#%e5%8f%98%e9%87%8f%e5%a3%b0%e6%98%8e)
    - [方法声明](#%e6%96%b9%e6%b3%95%e5%a3%b0%e6%98%8e)
    - [类型声明](#%e7%b1%bb%e5%9e%8b%e5%a3%b0%e6%98%8e)
    - [类型构造](#%e7%b1%bb%e5%9e%8b%e6%9e%84%e9%80%a0)
    - [类型运算](#%e7%b1%bb%e5%9e%8b%e8%bf%90%e7%ae%97)
    - [一般表达式](#%e4%b8%80%e8%88%ac%e8%a1%a8%e8%be%be%e5%bc%8f)
    - [模式匹配、匿名函数](#%e6%a8%a1%e5%bc%8f%e5%8c%b9%e9%85%8d%e5%8c%bf%e5%90%8d%e5%87%bd%e6%95%b0)
    - [模式表达式](#%e6%a8%a1%e5%bc%8f%e8%a1%a8%e8%be%be%e5%bc%8f)
    - [中缀（前缀）表达式](#%e4%b8%ad%e7%bc%80%e5%89%8d%e7%bc%80%e8%a1%a8%e8%be%be%e5%bc%8f)
    - [字面量、变量、（构造）函数调用](#%e5%ad%97%e9%9d%a2%e9%87%8f%e5%8f%98%e9%87%8f%e6%9e%84%e9%80%a0%e5%87%bd%e6%95%b0%e8%b0%83%e7%94%a8)
  - [4.项目计划](#4%e9%a1%b9%e7%9b%ae%e8%ae%a1%e5%88%92)
    - [时间表](#%e6%97%b6%e9%97%b4%e8%a1%a8)
    - [开发环境](#%e5%bc%80%e5%8f%91%e7%8e%af%e5%a2%83)

我将实现一种语法基于ML家族语言，融合Go语言动态编程特性的静态类型语言Beren，作为我的毕设课题。

## 1.前期准备

我从中学时开始学习编程，接触多种编程语言后也逐渐有了自己设计一门语言的想法，不过一直没有付诸实现。19年夏天进入字节跳动实习以后，我开始认真考虑这个想法，因为经过大二大三的专业课学习，我已经掌握了实现一门语言的理论基础，剩下待考验的就只有动手能力了。

参考了Go，Rust，Crystal，Typescript等新兴语言后，我逐渐有了心目中的理想型语言：一种执行静态类型检查、编译型，保证类型安全的，支持类型推断、垃圾回收、轻量级线程的，DSL化的通用编程语言。但是以我目前的能力，无法一人实现这样一门语言。我只能将期望调低，阶段式地提升目标，由动态解释型语言做起，逐步升级至静态解释型语言，静态编译型语言，最后到支持高并发的通用编程语言。

### 未完成的Léa解释器

在字节跳动实习的几个月中，我利用闲暇时间，开始了第一次尝试。我给这个项目起名叫Léa，定位是一门动态解释性语言，语法混合了C/C++，Go，Lisp以及Mathematica，作为练手。

当时的想法很天真，很不切实际。我企图在毫无编译器设计经验的情况下就开发一门功能完整的语言，结果注定与期望相距甚远。因为对开发语言的难度缺乏认知，同时也想练习C语言水平，我就选用C开始了这个项目。最初的几周我精神饱满，花了很多时间琢磨语法，定义抽象语法树（AST），并用flex和bison实现了一个parser。

从字符流转换到AST的过程相对简单，随后的AST解释执行的过程显然困难不少。当时我决定用环境模型（environment model）为Léa语言编写一个运行时（runtime）。边写边发现，我需要自己实现哈希表、垃圾回收、语法树节点的深拷贝，还要大量操作void*指针，程序编写得十分不安全。另外，在开始写runtime之前，有许多细节我并没有仔细想好，编写过程中意识到与之前设计的AST结构有不少的冲突，实现难度越写越大。我逐渐明白一定要先大幅降低目标难度，才有可能真正完成第一个项目。等到9月开学的时候，我写出了一个半成品解释器，可以完整解析语法，进行变量定义和一些简单计算，之后便搁置了这个项目。

### MyLisp解释器

随后等到申请学校结束，我又开始了新一次尝试。这回我将目标定的非常低：实现一个功能极其简单的Scheme解释器。这个项目的名字叫MyLisp，也是一门动态解释型语言。在尝到C语言的苦头之后，我决定用一门高级语言来减轻心智负担。在字节跳动实习4个月后，我对Go语言已掌握得比较熟练，于是决定使用Go来实现这个项目。

为了避免在parser花费过多时间，我改变了原先的开发顺序。我先定义好了各个AST节点，然后根据AST来设计runtime的结构，定义了runtime能够处理的值类型，之后再去处理语法。我先搭好各个部分的框架，随后逐步填充。Scheme简单的语法大幅度的缩短了各阶段的编写用时，并且使用高级语言确实带来了效率上的提升。最终完成的MyLisp解释器可以支持布尔型、整型、符号、函数和闭包几种核心类型，虽然运行效率较差，但也基本上达成了设定的目标。接下来就可以开始毕设项目Beren的工作了。

### Beren解释器

Beren作为我的毕设项目，设想中是一门执行静态类型检查，类型安全的单线程解释型语言。有了MyLisp的一些经验，我对于Beren实现成功的可能比较有把握。这个项目对我来说极具挑战，必将耗费大量时间。如果能成功完成，将是我一次极大的锻炼。

## 2.思路来源

再说回我想要设计的语言Beren本身，这门语言以ML家族语言的类型系统为主体，以“不定类型”为核心，另外参考Go语言的接口抽象形式，作为ML家族中的module子语言的简化。这一设计想法来源于两条相互关联的思维路径。

### 路经一

**第一条路径**，从观察编程语言中的错误处理方式开始。19年大三快结束的时候，我开始接触Rust语言；入职字节跳动以后，我接着去学习Go语言。这两门语言共同点是诞生时间不久，拥有许多不同于传统语言的革新特性，比如类型继承方式，以及错误处理。在同事们推荐的文章里，我看到了很多关于Go语言的讨论，许多人都说它的错误处理方式不够优雅，不如Rust中Result类型的处理模式，甚至不如try catch机制。

try catch错误处理机制不区分普通错误和异常，要记清楚一个个异常类型的名字不是一件容易的事。以前我图省事，经常写出一些类似下面的几乎无用的错误处理代码。此外，源于try catch的实现机制，在异常多发的情况下会导致不小的性能损耗。

```Java
try {
  ...
} catch (Exception e) {}
```

Go语言区分可预知的错误和不可预知的异常，普通错误通过函数的最后一个error类型的返回值来获取，异常则使用panic和recover机制来处理。error类型是一个非常简单的接口，只提供一个打印错误信息的方法。

``` Go
type error interface {
  Error() string
}
```

对于一般的网络服务器场景的应用来说，相对轻便简单，将错误作为返回值也不会造成太大的性能损耗。如果需要更为细分的错误类型，可以用Go的类型断言或反射机制来推断error接口的类型。

``` Go
func foo() (int, error) {
	a, b, err := someCalculation()
	if err != nil {
 		return 0, fmt.Errorf("foo: %w", err)
	}
	return a+b, nil
}
```

至于难以预测的异常，可以用panic迅速终止函数的运行。这一机制整体上是不错的，不过很多人认为不够优雅，因为作为返回值的error类型和正常的返回值在同一时间必有一个是浪费的，而且这种错误码返回机制并不保证编程者一定会进行错误处理。

Rust的错误处理与Go语言类似，同样区分可预知的错误和不可预知的异常。Result类型是Rust内置的一种“不定类型”，用于处理可预知的错误。函数正常返回的时候Result会保存正常的返回值，出错的时候则会保存错误信息。

``` Rust
enum<T, E> Result {
  Ok(T),
  Err(E)
}
```

想要使用Result类型返回值的内容，需要先判断到底收到的是Ok还是Err，然后再分别处理，这种做法可以保证错误处理不会缺失。如果遇到了异常，利用panic!宏迅速终止程序运行就好。

``` Rust
fn foo() -> Result<(i32, i32), String> {
  let result = some_calculation();
  match result {
    Ok(a, b) => a + b,
    Err(s) => Err(s)
  }
}
```

正是因为Rust支持enum type，也就是通常所说的“不定类型”，才能够提供这种相较之下最为完善的错误处理方式。

由此，我对Rust中的“不定类型”产生了兴趣。后来又看了一些讨论，其中一条评论令我印象深刻，提到说Rust的设计受了OCaml语言很大的影响，但是语法上并不如OCaml那么自然纯粹。受好奇心的驱使，我又开始学习OCaml，以及同为ML家族的SML，这才明白为什么大家都说Rust像是ML家族的新一代语言。只有在接触ML家族语言为模式匹配专门设计的语法之后，我才体会到这种类型系统，尤其是“不定类型”的优雅和强大。

``` OCaml
type expr =
| Int of int
| Plus of expr * expr
| Times of expr * expr

let rec calc = function
| Int x -> x
| Plus (a, b) -> calc a + calc b
| Times (a, b) -> calc a * (calc b

let () =
  let e = Times (Plus (Int 2, Int 3), Times (Int 4, Int 5)) in
  print_int (calc e)
```

### 路径二

**第二条路径**，从Go语言的接口类型开始。Go语言仓库的proposal中，有许多人支持向Go添加一种数据类型用于专门处理“不定类型”。但是这些proposal大多遭到否决，因为Go的接口已经部分提供了“不定类型”的功能，没有必要添加另一种新的语法元素。

Go语言的接口，配合switch case语言以及类型断言，可以展现出类似模式匹配的效果，可以说Go接口很多时候都可以当作ML中的“不定类型”来使用。

``` Go
type expr interface {
  Expr()
}

type Int int
type Plus struct { a, b expr }
type Times struct { a, b expr }

func (Int) Expr() {}
func (Plus) Expr() {}
func (Times) Expr() {}

func calc(e expr) int {
  switch v := e.(type) {
  case Int:
    return v
  case Plus:
    return calc(v.a) + calc(v.b)
  case Times:
    return calc(v.a) * calc(v.b)
  }
}
```

但是两者之间存在本质区别，Go接口是对行为的抽象，每一个接口类型的变量保存的都是一个虚函数表和具体内容的元组，变量的类型信息和虚函数表存放在一起，在类型断言或者使用反射机制时，类型信息会被用到。ML中的“不定类型”更类似于C语言中的struct套union的结构，是对不同数据组合的抽象。

``` C
struct expr {
  enum {INT, PLUS, TIMES} tag;
  union {
    int value;
    struct { int a, b; } plus;
    struct { int a, b; } times;
  };
};
```

两者在出发点有着较大差别。因此，Go接口只能限定数据的行为，无法限制具体的数据种类，所以它代表的是一个“无限”的类型集合；而ML的“不定类型”只能限定数据种类，定义之后便不能拓展，所以其代表的是一个“有限”的类型集合。这是两者最本质的差别。也有Go语言proposal提出向interface中增设语法，可以将接口支持的类型限制在有限的集合内，基本模拟“不定类型”。但是我认为这一提案不太可能通过，因为它与接口语法的出发点不相符，而且不是十分必要。

从我的角度而言，接口更应作为行为方式的抽象存在，属于数据抽象而非行为抽象的内容，则由“不定类型”来实现。ML语言的数据抽象方式以及语法能发挥出“不定类型”最大的优势，而类似Go接口的行为抽象方式可以更好地组织和复用代码。这两种思想的结合，在目前的我看来，是一件非常美妙的事情。

## 3.语法设计

### 语法入口

```
（语法入口）
program: global_decl_list

（全局声明）
global_decl_list: global_decl
                | global_decl global_decl_list

global_decl: let_decl
           | method_decl
           | type_decl
```

### 变量声明

```
（全局变量或函数的声明）
let_decl: "let" ["rec"] let_binding
        | "let" ["rec"] let_binding "and" let_binding_list
        
let_binding_list: let_binding
                | let_binding "and" let_binding_list
        
let_binding: val_binding
           | fun_binding

（模式匹配绑定变量）
val_binding: pattern "=" expr

（函数签名）
fun_binding: IDENT prototype "=" expr
```

### 方法声明

```
（与类型关联的方法声明）
method_decl: "method" receiver fun_binding
           | "method" receiver fun_binding 
             "and" method_binding_list
             
receiver: pattern_with_type

method_binding_list: fun_binding
                   | fun_binding "and" method_binding_list
```

```
prototype: arg_list
         | arg_list ":" type_expr

arg_list: pattern
        | pattern arg_list
```

### 类型声明

```
（类型声明）
type_decl: "type" type_binding
         | "type" type_binding "and" type_binding_list

type_binding_list: type_binding
                 | type_binding "and" type_binding_list

type_binding: type_param IDENT "=" type_construct

（泛型参数）
type_param: TYPE_SYMBOL （类型参数如'a, 'b, ...）
          | type_param_tuple

（多个泛型参数）
type_param_tuple: "(" TYPE_SYMBOL "," type_symbol_list ")"

type_symbol_list: TYPE_SYMBOL
                | TYPE_SYMBOL "," type_symbol_list
                
```

### 类型构造

```
type_construct: type_expr （声明类型别名）
              | variant_type （声明新的不定类型）
              | record_type （声明新的记录类型）
              | interface_type （声明接口类型）

（不定类型）
variant_type: variant_list
            | "|" variant_list
            
variant_list: variant
            | variant "|" variant_list
            
variant: CAPID （大写字母开头的标识符）
       | CAPID "of" type_expr

（记录类型）
record_type: "{" "}"
           | "{" field_type_list "}"
           | "{" field_type_list "," "}"

（记录中的条目）
field_type_list: field_type
               | field_type "," field_type_list

（条目内容可变或不可变）
field_type: ["mutable"] IDENT "=" type_expr

(接口类型)
interface_type: "interface" "end"
              | "interface" method_type_list "end"
              | "interface" method_type_list "," "end"

method_type_list: method_type
                | method_type "," method_type_list
                
method_type: IDENT ":" must_function_type
```

### 类型运算

```
（类型表达式）
type_expr: type_infix_function

（函数类型）
type_infix_function: type_infix_tuple
                   | must_function_type

must_function_type: type_infix_tuple "->" type_infix_function

（积类型/元组类型）
type_infix_tuple: type_inner_expr
                | must_tuple_type

must_tuple_type: type_inner_expr "*" type_inner_expr
               | type_inner_expr "*" must_tuple_type
                
type_inner_expr: type_terminal
               | type_specialization
               | "(" type_expr ")"

（泛型特化）
type_specialization: type_expr IDENT
         
type_terminal: TYPE_SYMBOL
             | IDENT
```

### 一般表达式

```
(表达式)
expr: local_expr (局部作用域)
    | infix_op（中缀表达式）
    | if_expr
    | match_expr
    | lambda_expr
    | "(" expr ":" type_expr ")" （类型限定）

（let in语法）
local_expr: let_decl "in" expr
```

```
if_expr: "if" expr "then" expr
       | "if" expr "then" expr "else" expr
```

### 模式匹配、匿名函数

```
（模式匹配语法）
match_expr: "match" expr "with" match_list
          | "match" expr "with" "|" match_list
          
match_list: match_branch
          | match_branch "|" match_list

match_branch: pattern "->" expr
```

```
（匿名函数表达式）
lambda_expr: fun_expr
           | function_expr
 
（可多参数的匿名函数）
fun_expr: "fun" prototype "->" expr

（模式匹配函数）
function_expr: "function" match_list
             | "function" "|" match_list
```

### 模式表达式

```
（模式表达式）
pattern: single_pattern
       | single_pattern "|" pattern（多模式）

（单模式）
single_pattern: pattern_infix
              | pattern_with_type

（模式匹配类型限定）
pattern_with_type: "(" pattern ":" type_expr ")"

pattern_infix: pattern_infix_cons

（list拼接表达式的模式匹配）
pattern_infix_cons: inner_pattern
                  | inner_pattern "::" pattern_infix_cons

inner_pattern: pattern_terminal
             | variant_pattern
             | "(" pattern ")"

pattern_terminal: pattern_literal
                | unit
                | tuple_pattern
                | list_pattern
                | array_pattern
                | record_pattern
                | variable_pattern

pattern_literal: BOOL
               | INT
               | "-" INT
               | CHAR
               | STRING

tuple_pattern: "(" pattern "," pattern_item_list ")"

list_pattern: "[" "]"
            | "[" pattern_item_list "]"
            
array_pattern: "[|" "|]"
             | "[|" pattern_item_list "|]"
             
pattern_item_list: pattern
                 | pattern "," pattern_item_list
                 
record_pattern: "{" "}"
              | "{" field_pattern_list "}"
              | "{" field_pattern_list "," "}"
              
field_pattern_list: field_pattern
                  | field_pattern "," field_pattern_list

field_pattern: IDENT "=" pattern
 
（变量匹配） 
variable_pattern: IDENT （变量名）
                | WILDCARD （通配符）

（不定类型构造函数的模式匹配）
variant_pattern: CAPID
               | CAPID pattern_terminal
               | CAPID "(" pattern ")"
```

### 中缀（前缀）表达式

```
（中缀表达式）
infix_op: infix_assign

infix_assign: infix_or
            | binding ":=" infix_assign
            | binding "<-" infix_assign

infix_or: infix_and
        | infix_or "||" infix_and
        
infix_and: infix_cmp
         | infix_and "&&" infix_cmp

infix_cmp: infix_cons
         | infix_cmp "<"  infix_append
         | infix_cmp "<=" infix_append
         | infix_cmp ">"  infix_append
         | infix_cmp ">=" infix_append
         | infix_cmp "="  infix_append
         | infix_cmp "!=" infix_append

infix_append: infix_cons
            | infix_append "@" infix_cons

infix_cons: infix_concat
          | infix_concat "::" infix_cons

infix_concat: infix_plus
            | infix_concat "^" infix_plus

infix_plus: infix_times
          | infix_plus "+" infix_times
          | infix_plus "-" infix_times
          
infix_times: prefix_minus
           | infix_times "*" prefix_minus
           | infix_times "/" prefix_minus
           | infix_times "%" prefix_minus
           
（前缀表达式）
prefix_minus: inner_expr
            | "-" prefix_minus
            | "+" prefix_minus
            
inner_expr: highest_prec （最高优先级的表达式项）
          | call （函数调用）
          | construction （不定类型构造函数调用）
```

### 字面量、变量、（构造）函数调用

```
terminal: literal
        | binding
        | unit
        | tuple
        | list
        | array
        | record

literal: BOOL
       | INT
       | CHAR
       | STRING

binding: ident
       | highest_prec "." ident

ident: IDENT
     | CAPID
       
unit: "(" ")"

tuple: "(" expr "," item_list ")"

list: "[" "]"
    | "[" item_list "]"

array: "[|" "|]"
     | "[|" item_list "|]"

item_list: expr
         | expr "," item_list

record: "{" "}"
      | "{" field_list "}"
      | "{" field_list "," "}"
      
field_list: field_binding
          | field_binding "," field_list

field_binding: IDENT "=" expr
        
call: expr highest_prec

construction: CAPID hightest_prec

highest_prec: terminal
            | "(" expr_list ")"

expr_list: expr
         | expr ";" expr_list （表达式拼接）
```

## 4.项目计划

### 时间表

1. 开题报告+任务书：3.1
2. 语法树（AST）设计：3.11
   1. Token和AST定义：3.4
   2. lexer和parser实现：3.11
3. 类型检查：3.18
4. 运行时（Runtime）设计：4.15
   1. 指令集设计：3.31
   2. 翻译方案设计：4.15
5. LLVM翻译方案设计（量力而为）
6. 毕设论文

### 开发环境

- MacOS
- VS Code
- OCaml