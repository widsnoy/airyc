# airyc

⚠️ **学习项目**：这是我的毕业设计项目，仅供个人学习娱乐

一个玩具编程语言，编译到 LLVM IR。  
基本覆盖所有错误类型的检查，并提供良好的编辑器支持。

## 特性

- 🔗 **多文件编译**：通过 `import` 语句支持模块化开发
- ⚡ **LLVM 后端**：编译到 LLVM IR 和原生可执行文件（基于 LLVM 21.1）
- 🔍 **语言服务器**：LSP 支持，提供引用查找、跳转、诊断等功能 (`bin/language_server`)  
- 🎨 **编辑器集成**：[VSCode](editor/code/), [NVim](https://github.com/widsnoy/airyc.nvim)  
- ✅ **友好的错误提示**：基于 miette 的美观错误报告

### 示例项目 
1. [Naive@FAT](example/fs/): 一个用 airyc 实现的简单 FAT 文件系统 (翻译自我的操作系统作业 c 语言代码)，展示了指针运算、多文件编译等特性）  
2. [Calculator](example/calculator): AI 写的交互式整数计算器，经过词法分析，语法分析，语义分析最后算出结果

## 语法

```text
CompUnit    := {Header}{GlobalDecl}

Header      := 'import' Path  
Path        := String ['::' Ident]  

GlobalDecl  := VarDef | FuncDef | StructDef

Type        := ['const'] PrimitType | Pointer Type | '[' Type ';' Expr ']'
PrimitType  := ScalarType | 'struct' Name
ScalarType  := 'i32' | 'u8' | 'bool' | 'void'
Pointer     := '*' ('mut' | 'const')

VarDef      := 'let' Name ':' Type ['=' InitVal] ';'
InitVal     := Expr | '{' [InitVal {',' InitVal}] '}'

FuncDef     :=  FuncSign (';' | Block)
FuncSign    := 'fn' Name '(' [FuncFParams] ')' ['->' Type]
FuncFParams := FuncFParam {',' FuncFParam} ['...']
FuncFParam  := Name: Type
FuncRParams := Expr {',' Expr}
StructDef   := 'struct' Name '{' [StructField {',' StructField}] '}'
StructField := Name: Type

Block       := '{' {BlockItem} '}'
BlockItem   := VarDef | Stmt

Stmt        := AssignStmt
             | ExprStmt
             | Block
             | IfStmt
             | WhileStmt
             | BreakStmt
             | ContinueStmt
             | ReturnStmt

AssignStmt  := Expr '=' Expr ';'
ExprStmt    := [Expr] ';'
IfStmt      := 'if' '(' Expr ')' Stmt ['else' Stmt]
WhileStmt   := 'while' '(' Expr ')' Stmt
BreakStmt   := 'break' ';'
ContinueStmt:= 'continue' ';'
ReturnStmt  := 'return' [Expr] ';'

Expr        := BinaryExpr
             | UnaryExpr
             | CallExpr
             | ParenExpr
             | PostfixExpr
             | IndexVal
             | Literal

BinaryExpr  := Expr BinaryOp Expr
BinaryOp    := '||' | '&&' | '==' | '!=' 
             | '<' | '>' | '<=' | '>=' 
             | '+' | '-' | '*' | '/' | '%'

UnaryExpr   := UnaryOp Expr
UnaryOp     := '+' | '-' | '!' | '&' | '*'

PostfixExpr := Expr PostfixOp FieldAccess
PostfixOp   := '.' | '->'

CallExpr    := Name '(' [FuncRParams] ')'
ParenExpr   := '(' Expr ')'
IndexVal    := Name {'[' Expr ']'}
FieldAccess := Name {'[' Expr ']'}

Literal     := IntLiteral | char | String | 'null'
Name        := Ident
```

## 语义说明


### Import

#### Import（跨文件引用）

```python
import "stdlib.airy"              // 导入所有符号
import "module.airy" :: foo       // 导入特定函数
import "module.airy" :: Point     // 导入特定结构体
```

- 可以引用：函数声明、结构体定义
- 不能引用：变量
- 会检测循环依赖

#### 外部函数声明

```rust
fn foo();
fn printf(format: *const u8, ...) -> i32;
```

当前 stage0 使用以分号结尾且没有函数体的 `fn ...;` 形式声明外部函数。

### 字符和字符串类型

```rust
// 字符字面量：u8 类型
let c: u8 = 'A';

// 字符串字面量：*const u8 类型
fn printf(format: *const u8, ...);
printf("Hello, World!\n");

// 字符数组：应该用 u8
let str: [u8; 100];
```

### 数组维度顺序

```rust
// [[i32; 3]; 2] 表示：2 个长度为 3 的 i32 数组
let arr: [[i32; 3]; 2] = {{1, 2, 3}, {4, 5, 6}};

// 理解方式：从内到外读
// [[i32; 3]; 2] = [长度为 3 的 i32 数组; 2 个]
```

### 数组 Decay

多维数组索引后，如果结果仍是数组，会自动 decay 成指向元素的指针：

```rust
let arr: [[i32; 3]; 2];
// arr[0] 的类型是 *const i32，arr 的类型是 *const [i32; 3]
```

### 类型转换规则

#### 隐式转换（只允许无损扩展）

```rust
// ✅ 整数无损提升：bool → u8 → i32
let a: u8 = true;  // bool → u8
let b: i32 = 20u8; // u8 → i32

// ❌ 禁止：大类型到小类型
let c: u8 = 100i32; // 错误：TypeMismatch
```

#### 指针转换

```rust
// *void 可以与任何指针类型互转
let p: *mut void = some_ptr;
let q: *mut i32 = p;  // ✅ 允许

// 同类型的指针转换，忽略 const/mut 修饰符
let p: *mut *const i32 = null;
let d: i32 = 1;
let g: *mut i32 = &d;
p = &g;  // ✅ 允许
```

### 指针算术

指针可以与整数进行加减运算，偏移量按**指向的元素大小**计算：

```rust
let arr: [i32; 5] = {10, 20, 30, 40, 50};
let p: *const i32 = &arr[0];

// 指针 + 整数：偏移 n 个元素
let p1: *const i32 = p + 1;  // 偏移 1 个 i32（4 字节）
let p2: *const i32 = p + 2;  // 偏移 2 个 i32（8 字节）

// 指针 - 整数：向前偏移
let p3: *const i32 = p2 - 1;  // 回退 1 个 i32

// 指针 - 指针：返回元素个数差（i32 类型）
let diff: i32 = p2 - p;  // 结果是 2
```

**提示**：`p[x]` 等价于 `*(p + x)`

```rust
let p: *const i32 = &arr[0];

p[0]      // 等价于 *p
p[1]      // 等价于 *(p + 1)
p[2]      // 等价于 *(p + 2)
```
### 指针常量
```rust
let x: i32 = 10;
let y: i32 = 20;

// *const i32: 指针是常量，指向的值可变
let p1: *const i32 = &x;
// p1 = &y;  // ❌ 错误：不能修改指针
*p1 = 30;    // ✅ 正确：可以修改值

// *mut const i32: 指针可变，指向的值是常量
let p2: *mut const i32 = &x;
p2 = &y;     // ✅ 正确：可以修改指针
// *p2 = 40; // ❌ 错误：不能修改值

// 多级指针 *mut *const p 可以看作 *mut (*const p)

```
### 常量表达式

数组大小必须是常量表达式，支持常量折叠：

```rust
let size: const i32 = 10;
let arr1: [i32; size];        // ✅ 正确
let arr2: [i32; size + 5];    // ✅ 正确：常量折叠

let n: i32 = 10;
let arr3: [i32; n];           // ❌ 错误：ConstantExprExpected
```

### Void 类型限制

`void` 只能用于：
1. 函数返回类型：`fn foo() -> void`
2. 指针类型：`*mut void`

不能用于：
- 变量类型：`let x: void;` ❌
- 数组元素类型：`let arr: [void; 10];` ❌
- 不能解引用 void 指针：`let v = *void_ptr;` ❌

### 左值规则

只有以下表达式是左值（可以被赋值）：

```rust
x = 10;           // ✅ 变量
arr[0] = 10;      // ✅ 数组索引
s.field = 10;     // ✅ 结构体字段
ptr->field = 10;  // ✅ 指针访问字段
*ptr = 10;        // ✅ 指针解引用

10 = x;           // ❌ 字面量
foo() = x;        // ❌ 函数调用
(x + y) = 10;     // ❌ 表达式
```

### 递归类型检测

结构体不能直接包含自身（会导致无限大小），必须使用指针：

```rust
// ❌ 错误：RecursiveType
struct Node {
    value: i32,
    next: struct Node
}

// ✅ 正确：使用指针
struct Node {
    value: i32,
    next: *mut struct Node
}
```

### 常见错误

- `TypeMismatch`：类型不匹配
- `ConstantExprExpected`：需要常量表达式但提供了变量
- `AssignToConst`：尝试给常量赋值
- `CircularDependency`：模块循环依赖
- `RecursiveType`：结构体递归定义导致无限大小
- `InvalidVoidUsage`：非法使用 void 类型
- `BreakOutsideLoop` / `ContinueOutsideLoop`：循环控制语句在循环外使用
- `NotALValue`：尝试给非左值赋值
- `VoidPointerDeref`：尝试解引用 void 指针

完整错误列表参见 [error.rs](crates/analyzer/src/error.rs)。

## 错误检查示例

编译器能够检测复杂的语义错误并提供友好的错误提示：

### 递归类型检测

跨文件的递归类型定义：

```rust
// file_a.airy
import "file_b.airy" :: StructB
struct StructA {
    value: i32,
    b: struct StructB
}
```

```rust
// file_b.airy
import "file_a.airy" :: StructA
struct StructB {
    value: i32,
    a: struct StructA
}
```

编译器输出：

```
semantic::recursive_type

  × recursive type `StructA` has infinite size
   ╭─[file_a.airy:3:8]
 2 │ 
 3 │ struct StructA {
   ·        ───┬───
   ·           ╰── here
 4 │     value: i32,
   ╰────
  help: StructA->StructB->StructA
```

### 类型不匹配

整数不能隐式窄化：

```rust
let a: u8 = 10i32;  // 错误
```

编译器输出：

```
semantic::type_mismatch

  × type mismatch: expected u8, found i32
   ╭─[demo.airy:2:18]
 1 │ fn main() -> i32 {
 2 │     let a: u8 = 10i32;
   ·                 ──┬──
   ·                   ╰── here
 3 │     return 0;
   ╰────
```

### 常量表达式检查

数组大小必须是常量表达式：

```rust
let n: i32 = 10;
let arr: [i32; n];  // 错误
```

编译器输出：

```
semantic::constant_expr_expected

  × constant expression expected
   ╭─[demo.airy:3:20]
 2 │     let n: i32 = 10;
 3 │     let arr: [i32; n];
   ·                    ┬
   ·                    ╰── here
 4 │     return 0;
   ╰────
```

### 常量赋值检查

不能给常量赋值：

```rust
let x: const i32 = 10;
x = 20;  // 错误
```

编译器输出：

```
semantic::assign_to_const

  × can't assign to const variable 'x'
   ╭─[demo.airy:3:5]
 2 │     let x: const i32 = 10;
 3 │     x = 20;
   ·     ┬
   ·     ╰── here
 4 │     return 0;
   ╰────
```

## 参考
[Rust](https://rust-lang.org/)  
[rust-analyzer](https://github.com/rust-lang/rust-analyzer)  
[SysY](https://gitlab.eduxiji.net/csc1/nscscc/compiler2021/-/blob/master/SysY%E8%AF%AD%E8%A8%80%E5%AE%9A%E4%B9%89.pdf)    
[北大编译实践在线文档](https://pku-minic.github.io/online-doc/#/)  
[compiler-dev-test-cases](https://github.com/pku-minic/compiler-dev-test-cases/tree/master/testcases)  
